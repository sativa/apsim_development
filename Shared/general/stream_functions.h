#if !defined (STREAM_FUNCTIONS_H)
#define STREAM_FUNCTIONS_H

#include <general\general.h>
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
char GENERAL_EXPORT Read_token(istream& In_stream,
                               const char* Init_skip_chars,
                               const char* Delimiter_chars,
                               string& Token);

// ------------------------------------------------------------------
//  Short description:
//     Read in entire contents of a stream and return to caller.

//  Notes:
//     I have used the non-standard read_file method of the string
//     class. If we ever move to the STL version of string then
//     this routine will have to be re-worked.

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void GENERAL_EXPORT Read_stream(istream& In_stream, string& Contents);

// ------------------------------------------------------------------
//  Short description:
//     write contents of input stream to output stream converting
//     to CSV (comma separated values).

//  Notes:
//     assumes that input and output streams are both open.

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void GENERAL_EXPORT Convert_2_CSV(istream& In_stream, ostream& Out_stream);


#endif


