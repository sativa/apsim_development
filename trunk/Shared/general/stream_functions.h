#if !defined (STREAM_FUNCTIONS_H)
#define STREAM_FUNCTIONS_H

#include <general\mystring.h>
#include <iostream.h>

// ------------------------------------------------------------------
//  Short description:
//    Read in a token.  Skip all leading skip characters and stop when
//    a delimiter character is found.  Returns the character that caused
//    the parsing to stop.

//  Notes:

//  Changes:
//    DPH 21/11/94
//    DPH 17/4/1997 - moved to C++ builder.

// ------------------------------------------------------------------
char Read_token(istream& In_stream,
                const char* Init_skip_chars,
                const char* Delimiter_chars,
                string& Token);

#endif


