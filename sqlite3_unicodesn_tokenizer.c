/*
** 2013 September 22
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
*/
#include <sqlite3.h>

#include "fts3_unicodesn.h"
#include "sqlite3_unicodesn_tokenizer.h"

/*
** Register the tokenizer with FTS3 or FTS4. For use when compiling the tokenizer directly into
** an application, instead of as a separate shared library.
*/
int register_unicodesn_tokenizer(
      sqlite3 *db          /* The database connection */
)
{
  const sqlite3_tokenizer_module *tokenizer;
  int rc;
  sqlite3_stmt *pStmt;
  const char *zSql = "SELECT fts3_tokenizer(?, ?)";

  sqlite3Fts3UnicodeSnTokenizer(&tokenizer);

  rc = sqlite3_prepare_v2(db, zSql, -1, &pStmt, 0);
  if( rc!=SQLITE_OK ){
    return rc;
  }

  sqlite3_bind_text(pStmt, 1, TOKENIZER_NAME, -1, SQLITE_STATIC);
  sqlite3_bind_blob(pStmt, 2, &tokenizer, sizeof(tokenizer), SQLITE_TRANSIENT);
  rc = sqlite3_step(pStmt);
  if( rc!=SQLITE_OK && rc < SQLITE_ROW ){
      return rc;
  }
  return sqlite3_finalize(pStmt);
}
