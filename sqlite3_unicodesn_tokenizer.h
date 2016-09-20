#ifndef _UNICODESN_TOKENIZER_H_
#define _UNICODESN_TOKENIZER_H_

/*
 ** Registers the Unicode Snowball tokenizer as "unicodesn", for use with SQLite's FTS3 or FTS4.
 ** This is for use when compiling the tokenizer directly into an application, instead of as a
 ** separate shared library. Example of usage:
 **   CREATE VIRTUAL TABLE fts USING fts3(text, tokenize=unicodesn "stemmer=russian");
 */
int register_unicodesn_tokenizer(sqlite3 *db);

#endif /* _UNICODESN_TOKENIZER_H_ */
