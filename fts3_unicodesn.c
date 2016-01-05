/*
** 2012 Nov 11
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
******************************************************************************
**
** Implementation of the "unicode" full-text-search tokenizer with Snowball stemming
*/

#include "fts3_unicodesn.h"

/* Snowball stemmer */
#include "api.h"

#ifdef SQLITE_ENABLE_FTS4_UNICODE61

#include "fts3Int.h"
#if !defined(SQLITE_CORE) || defined(SQLITE_ENABLE_FTS3)

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "fts3_tokenizer.h"

#include "libstemmer_c/src_c/stem_UTF_8_danish.h"
#include "libstemmer_c/src_c/stem_UTF_8_dutch.h"
#include "libstemmer_c/src_c/stem_UTF_8_english.h"
#include "libstemmer_c/src_c/stem_UTF_8_finnish.h"
#include "libstemmer_c/src_c/stem_UTF_8_french.h"
#include "libstemmer_c/src_c/stem_UTF_8_german.h"
#include "libstemmer_c/src_c/stem_UTF_8_hungarian.h"
#include "libstemmer_c/src_c/stem_UTF_8_italian.h"
#include "libstemmer_c/src_c/stem_UTF_8_norwegian.h"
#include "libstemmer_c/src_c/stem_UTF_8_porter.h"
#include "libstemmer_c/src_c/stem_UTF_8_portuguese.h"
#include "libstemmer_c/src_c/stem_UTF_8_romanian.h"
#include "libstemmer_c/src_c/stem_UTF_8_russian.h"
#include "libstemmer_c/src_c/stem_UTF_8_spanish.h"
#include "libstemmer_c/src_c/stem_UTF_8_swedish.h"
#include "libstemmer_c/src_c/stem_UTF_8_turkish.h"


/*
** The following two macros - READ_UTF8 and WRITE_UTF8 - have been copied
** from the sqlite3 source file utf.c. If this file is compiled as part
** of the amalgamation, they are not required.
*/
#ifndef SQLITE_AMALGAMATION

static const unsigned char sqlite3Utf8Trans1[] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  0x00, 0x01, 0x02, 0x03, 0x00, 0x01, 0x00, 0x00,
};

#define READ_UTF8(zIn, zTerm, c)                           \
  c = *(zIn++);                                            \
  if( c>=0xc0 ){                                           \
    c = sqlite3Utf8Trans1[c-0xc0];                         \
    while( zIn!=zTerm && (*zIn & 0xc0)==0x80 ){            \
      c = (c<<6) + (0x3f & *(zIn++));                      \
    }                                                      \
    if( c<0x80                                             \
        || (c&0xFFFFF800)==0xD800                          \
        || (c&0xFFFFFFFE)==0xFFFE ){  c = 0xFFFD; }        \
  }

#define WRITE_UTF8(zOut, c) {                          \
  if( c<0x00080 ){                                     \
    *zOut++ = (u8)(c&0xFF);                            \
  }                                                    \
  else if( c<0x00800 ){                                \
    *zOut++ = 0xC0 + (u8)((c>>6)&0x1F);                \
    *zOut++ = 0x80 + (u8)(c & 0x3F);                   \
  }                                                    \
  else if( c<0x10000 ){                                \
    *zOut++ = 0xE0 + (u8)((c>>12)&0x0F);               \
    *zOut++ = 0x80 + (u8)((c>>6) & 0x3F);              \
    *zOut++ = 0x80 + (u8)(c & 0x3F);                   \
  }else{                                               \
    *zOut++ = 0xF0 + (u8)((c>>18) & 0x07);             \
    *zOut++ = 0x80 + (u8)((c>>12) & 0x3F);             \
    *zOut++ = 0x80 + (u8)((c>>6) & 0x3F);              \
    *zOut++ = 0x80 + (u8)(c & 0x3F);                   \
  }                                                    \
}

#endif /* ifndef SQLITE_AMALGAMATION */

typedef struct unicode_tokenizer unicode_tokenizer;
typedef struct unicode_cursor unicode_cursor;

typedef struct {
    struct SN_env * (*create)(void);
    void (*close)(struct SN_env *);
    int (*stem)(struct SN_env *);
} stemmer_callbacks;

struct unicode_tokenizer {
  sqlite3_tokenizer base;
  int bRemoveDiacritic;
  int nException;
  int *aiException;
  /* Snowball stemmer */
  stemmer_callbacks stemmer;
};

struct unicode_cursor {
  sqlite3_tokenizer_cursor base;
  const unsigned char *aInput;    /* Input text being tokenized */
  int nInput;                     /* Size of aInput[] in bytes */
  int iOff;                       /* Current offset within aInput[] */
  int iToken;                     /* Index of next token to be returned */
  char *zToken;                   /* storage for current token */
  int nAlloc;                     /* space allocated at zToken */
  struct SN_env *pStemmer;         /* Snowball stemmer */
};


/*
** Destroy a tokenizer allocated by unicodeCreate().
*/
static int unicodeDestroy(sqlite3_tokenizer *pTokenizer){
  if( pTokenizer ){
    unicode_tokenizer *p = (unicode_tokenizer *)pTokenizer;
    sqlite3_free(p->aiException);
    sqlite3_free(p);
  }
  return SQLITE_OK;
}

/*
** As part of a tokenchars= or separators= option, the CREATE VIRTUAL TABLE
** statement has specified that the tokenizer for this table shall consider
** all characters in string zIn/nIn to be separators (if bAlnum==0) or
** token characters (if bAlnum==1).
**
** For each codepoint in the zIn/nIn string, this function checks if the
** sqlite3FtsUnicodeIsalnum() function already returns the desired result.
** If so, no action is taken. Otherwise, the codepoint is added to the 
** unicode_tokenizer.aiException[] array. For the purposes of tokenization,
** the return value of sqlite3FtsUnicodeIsalnum() is inverted for all
** codepoints in the aiException[] array.
**
** If a standalone diacritic mark (one that sqlite3FtsUnicodeIsdiacritic()
** identifies as a diacritic) occurs in the zIn/nIn string it is ignored.
** It is not possible to change the behaviour of the tokenizer with respect
** to these codepoints.
*/
static int unicodeAddExceptions(
  unicode_tokenizer *p,           /* Tokenizer to add exceptions to */
  int bAlnum,                     /* Replace Isalnum() return value with this */
  const char *zIn,                /* Array of characters to make exceptions */
  int nIn                         /* Length of z in bytes */
){
  const unsigned char *z = (const unsigned char *)zIn;
  const unsigned char *zTerm = &z[nIn];
  int iCode;
  int nEntry = 0;

  assert( bAlnum==0 || bAlnum==1 );

  while( z<zTerm ){
    READ_UTF8(z, zTerm, iCode);
    assert( (sqlite3FtsUnicodeIsalnum(iCode) & 0xFFFFFFFE)==0 );
    if( sqlite3FtsUnicodeIsalnum(iCode)!=bAlnum 
     && sqlite3FtsUnicodeIsdiacritic(iCode)==0 
    ){
      nEntry++;
    }
  }

  if( nEntry ){
    int *aNew;                    /* New aiException[] array */
    int nNew;                     /* Number of valid entries in array aNew[] */

    aNew = sqlite3_realloc(p->aiException, (p->nException+nEntry)*sizeof(int));
    if( aNew==0 ) return SQLITE_NOMEM;
    nNew = p->nException;

    z = (const unsigned char *)zIn;
    while( z<zTerm ){
      READ_UTF8(z, zTerm, iCode);
      if( sqlite3FtsUnicodeIsalnum(iCode)!=bAlnum 
       && sqlite3FtsUnicodeIsdiacritic(iCode)==0
      ){
        int i, j;
        for(i=0; i<nNew && aNew[i]<iCode; i++);
        for(j=nNew; j>i; j--) aNew[j] = aNew[j-1];
        aNew[i] = iCode;
        nNew++;
      }
    }
    p->aiException = aNew;
    p->nException = nNew;
  }

  return SQLITE_OK;
}

/*
** Return true if the p->aiException[] array contains the value iCode.
*/
static int unicodeIsException(unicode_tokenizer *p, int iCode){
  if( p->nException>0 ){
    int *a = p->aiException;
    int iLo = 0;
    int iHi = p->nException-1;

    while( iHi>=iLo ){
      int iTest = (iHi + iLo) / 2;
      if( iCode==a[iTest] ){
        return 1;
      }else if( iCode>a[iTest] ){
        iLo = iTest+1;
      }else{
        iHi = iTest-1;
      }
    }
  }

  return 0;
}

/*
** Return true if, for the purposes of tokenization, codepoint iCode is
** considered a token character (not a separator).
*/
static int unicodeIsAlnum(unicode_tokenizer *p, int iCode){
  assert( (sqlite3FtsUnicodeIsalnum(iCode) & 0xFFFFFFFE)==0 );
  return sqlite3FtsUnicodeIsalnum(iCode) ^ unicodeIsException(p, iCode);
}

/* Allow stemmers to be looked up by ISO-639 language code or by (English) name. */
static struct {const char *shortName; const char *longName; stemmer_callbacks stemmer;}
const stemmers[] = {
    {"da", "danish",     {danish_UTF_8_create_env, danish_UTF_8_close_env, danish_UTF_8_stem}},
    {"nl", "dutch",      {dutch_UTF_8_create_env, dutch_UTF_8_close_env, dutch_UTF_8_stem}},
    {"en", "english",    {english_UTF_8_create_env, english_UTF_8_close_env, english_UTF_8_stem}},
    {"fi", "finnish",    {finnish_UTF_8_create_env, finnish_UTF_8_close_env, finnish_UTF_8_stem}},
    {"fr", "french",     {french_UTF_8_create_env, french_UTF_8_close_env, french_UTF_8_stem}},
    {"de", "german",     {german_UTF_8_create_env, german_UTF_8_close_env, german_UTF_8_stem}},
    {"hu", "hungarian",  {hungarian_UTF_8_create_env, hungarian_UTF_8_close_env, hungarian_UTF_8_stem}},
    {"it", "italian",    {italian_UTF_8_create_env, italian_UTF_8_close_env, italian_UTF_8_stem}},
    {"no", "norwegian",  {norwegian_UTF_8_create_env, norwegian_UTF_8_close_env, norwegian_UTF_8_stem}},
    {"porter", "porter", {porter_UTF_8_create_env, porter_UTF_8_close_env, porter_UTF_8_stem}},
    {"pt", "portuguese", {portuguese_UTF_8_create_env, portuguese_UTF_8_close_env, portuguese_UTF_8_stem}},
    {"ro", "romanian",   {romanian_UTF_8_create_env, romanian_UTF_8_close_env, romanian_UTF_8_stem}},
    {"ru", "russian",    {russian_UTF_8_create_env, russian_UTF_8_close_env, russian_UTF_8_stem}},
    {"es", "spanish",    {spanish_UTF_8_create_env, spanish_UTF_8_close_env, spanish_UTF_8_stem}},
    {"sv", "swedish",    {swedish_UTF_8_create_env, swedish_UTF_8_close_env, swedish_UTF_8_stem}},
    {"tr", "turkish",    {turkish_UTF_8_create_env, turkish_UTF_8_close_env, turkish_UTF_8_stem}},
};


static int unicodeSetStemmer(
  unicode_tokenizer *p,
  const char *zIn,                /* Stemmer name (not NUL-terminated) */
  int nIn                         /* Length of z in bytes */
)
{
  for (int i = 0; i < sizeof(stemmers)/sizeof(stemmers[0]); i++) {
    const char *n1 = stemmers[i].shortName, *n2 = stemmers[i].longName;
    if ( (nIn==strlen(n1) &&  memcmp(n1, zIn, nIn)==0) ||
         (nIn==strlen(n2) &&  memcmp(n2, zIn, nIn)==0) ) {
      p->stemmer = stemmers[i].stemmer;
      return SQLITE_OK;
    }
  }
  return SQLITE_ERROR;
}

/*
** Create a new tokenizer instance.
*/
static int unicodeCreate(
  int nArg,                       /* Size of array argv[] */
  const char * const *azArg,      /* Tokenizer creation arguments */
  sqlite3_tokenizer **pp          /* OUT: New tokenizer handle */
){
  unicode_tokenizer *pNew;        /* New tokenizer object */
  int i;
  int rc = SQLITE_OK;

  pNew = (unicode_tokenizer *) sqlite3_malloc(sizeof(unicode_tokenizer));
  if( pNew==NULL ) return SQLITE_NOMEM;
  memset(pNew, 0, sizeof(unicode_tokenizer));
  pNew->bRemoveDiacritic = 1;
  pNew->stemmer.create = NULL;
  pNew->stemmer.close = NULL;
  pNew->stemmer.stem = NULL;

  for(i=0; rc==SQLITE_OK && i<nArg; i++){
    const char *z = azArg[i];
    int n = (int)strlen(z);

    if( n==19 && memcmp("remove_diacritics=1", z, 19)==0 ){
      pNew->bRemoveDiacritic = 1;
    }
    else if( n==19 && memcmp("remove_diacritics=0", z, 19)==0 ){
      pNew->bRemoveDiacritic = 0;
    }
    else if( n>=11 && memcmp("tokenchars=", z, 11)==0 ){
      rc = unicodeAddExceptions(pNew, 1, &z[11], n-11);
    }
    else if( n>=11 && memcmp("separators=", z, 11)==0 ){
      rc = unicodeAddExceptions(pNew, 0, &z[11], n-11);
    }
    else if( n>=8 && memcmp("stemmer=", z, 8)==0 ){
      rc = unicodeSetStemmer(pNew, &z[8], n-8);
    }
    else{
      /* Unrecognized argument */
      rc  = SQLITE_ERROR;
    }
  }

  if( rc!=SQLITE_OK ){
    unicodeDestroy((sqlite3_tokenizer *)pNew);
    pNew = 0;
  }
  *pp = (sqlite3_tokenizer *)pNew;
  return rc;
}

/*
** Prepare to begin tokenizing a particular string.  The input
** string to be tokenized is pInput[0..nBytes-1].  A cursor
** used to incrementally tokenize this string is returned in 
** *ppCursor.
*/
static int unicodeOpen(
  sqlite3_tokenizer *p,           /* The tokenizer */
  const char *aInput,             /* Input string */
  int nInput,                     /* Size of string aInput in bytes */
  sqlite3_tokenizer_cursor **pp   /* OUT: New cursor object */
){
  unicode_tokenizer *pTokenizer;
  unicode_cursor *pCsr;

  pCsr = (unicode_cursor *)sqlite3_malloc(sizeof(unicode_cursor));
  if( pCsr==0 ){
    return SQLITE_NOMEM;
  }
  memset(pCsr, 0, sizeof(unicode_cursor));

  pCsr->aInput = (const unsigned char *)aInput;
  if( aInput==0 ){
    pCsr->nInput = 0;
  }else if( nInput<0 ){
    pCsr->nInput = (int)strlen(aInput);
  }else{
    pCsr->nInput = nInput;
  }

  pTokenizer = (unicode_tokenizer *)p;
  if ( pTokenizer->stemmer.create!=NULL ) {
     pCsr->pStemmer = pTokenizer->stemmer.create();
     if ( pCsr->pStemmer==0 ) {
	sqlite3_free(p);
	return SQLITE_NOMEM;
     }
  }else {
     pCsr->pStemmer = NULL;
  }

  *pp = &pCsr->base;
  UNUSED_PARAMETER(p);
  return SQLITE_OK;
}

/*
** Close a tokenization cursor previously opened by a call to
** simpleOpen() above.
*/
static int unicodeClose(sqlite3_tokenizer_cursor *pCursor){
  unicode_cursor *pCsr = (unicode_cursor *) pCursor;
  if ( pCsr->pStemmer != NULL ) {
     unicode_tokenizer *p = ((unicode_tokenizer *)pCsr->base.pTokenizer);
     p->stemmer.close(pCsr->pStemmer);
  }
  sqlite3_free(pCsr->zToken);
  sqlite3_free(pCsr);
  return SQLITE_OK;
}

/*
** Extract the next token from a tokenization cursor.  The cursor must
** have been opened by a prior call to simpleOpen().
*/
static int unicodeNext(
  sqlite3_tokenizer_cursor *pC,   /* Cursor returned by simpleOpen */
  const char **paToken,           /* OUT: Token text */
  int *pnToken,                   /* OUT: Number of bytes at *paToken */
  int *piStart,                   /* OUT: Starting offset of token */
  int *piEnd,                     /* OUT: Ending offset of token */
  int *piPos                      /* OUT: Position integer of token */
){
  unicode_cursor *pCsr = (unicode_cursor *)pC;
  unicode_tokenizer *p = ((unicode_tokenizer *)pCsr->base.pTokenizer);
  int iCode;
  char *zOut;
  const unsigned char *z = &pCsr->aInput[pCsr->iOff];
  const unsigned char *zStart = z;
  const unsigned char *zEnd;
  const unsigned char *zTerm = &pCsr->aInput[pCsr->nInput];

  /* Scan past any delimiter characters before the start of the next token.
  ** Return SQLITE_DONE early if this takes us all the way to the end of 
  ** the input.  */
  while( z<zTerm ){
    READ_UTF8(z, zTerm, iCode);
    if( unicodeIsAlnum(p, iCode) ) break;
    zStart = z;
  }
  if( zStart>=zTerm ) return SQLITE_DONE;

  zOut = pCsr->zToken;
  do {
    int iOut;

    /* Grow the output buffer if required. */
    if( (zOut-pCsr->zToken)>=(pCsr->nAlloc-4) ){
      char *zNew = sqlite3_realloc(pCsr->zToken, pCsr->nAlloc+64);
      if( !zNew ) return SQLITE_NOMEM;
      zOut = &zNew[zOut - pCsr->zToken];
      pCsr->zToken = zNew;
      pCsr->nAlloc += 64;
    }

    /* Write the folded case of the last character read to the output */
    zEnd = z;
    iOut = sqlite3FtsUnicodeFold(iCode, p->bRemoveDiacritic);
    if( iOut ){
      WRITE_UTF8(zOut, iOut);
    }

    /* If the cursor is not at EOF, read the next character */
    if( z>=zTerm ) break;
    READ_UTF8(z, zTerm, iCode);
  }while( unicodeIsAlnum(p, iCode) 
       || sqlite3FtsUnicodeIsdiacritic(iCode)
  );

  if ( pCsr->pStemmer!=NULL ) {
     SN_set_current(pCsr->pStemmer, (int)(zOut - pCsr->zToken), (unsigned char *)pCsr->zToken);
     if ( p->stemmer.stem(pCsr->pStemmer)<0 ) {
	*paToken = pCsr->zToken;
	*pnToken = (int)(zOut - pCsr->zToken);
     }else {
	pCsr->pStemmer->p[pCsr->pStemmer->l] = '\0';
	*paToken = (char *)pCsr->pStemmer->p;
	*pnToken = pCsr->pStemmer->l;
     }
  }else {
     *paToken = pCsr->zToken;
     *pnToken = (int)(zOut - pCsr->zToken);
  }

  /* Set the output variables and return. */
  pCsr->iOff = (int)(z - pCsr->aInput);
  *piStart = (int)(zStart - pCsr->aInput);
  *piEnd = (int)(zEnd - pCsr->aInput);
  *piPos = pCsr->iToken++;
  return SQLITE_OK;
}

/*
** Set *ppModule to a pointer to the sqlite3_tokenizer_module 
** structure for the unicode tokenizer.
*/
void sqlite3Fts3UnicodeSnTokenizer(sqlite3_tokenizer_module const **ppModule){
  static const sqlite3_tokenizer_module module = {
    0,
    unicodeCreate,
    unicodeDestroy,
    unicodeOpen,
    unicodeClose,
    unicodeNext,
    0,
  };
  *ppModule = &module;
}

#endif /* !defined(SQLITE_CORE) || defined(SQLITE_ENABLE_FTS3) */
#endif /* ifndef SQLITE_ENABLE_FTS4_UNICODE61 */
