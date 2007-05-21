#include <gobjs\gobjs.h>

#include <iostream.h>

// *******************************************************************
       bool GOBJS_FUNCTION_EXPORT Search_stream(istream& In_stream,
                                 const char *Search_chars);
// *******************************************************************

//  Short description:
//    Search the input stream for the specified search characters.  Return
//    TRUE if found.  FALSE otherwise.  The position of the input stream will
//    be set to the start of the match if found.  In_stream should be BINARY
