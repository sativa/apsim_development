#include <gobjs\gobjs.h>

// *******************************************************************
       void GOBJS_FUNCTION_EXPORT Delay(int Num_seconds);
// *******************************************************************

//  Short description:
//    Delay for a specified number of seconds,

//  Notes:
//    This routine is necessary because the standard C routine "delay"
//    doesn't work with Windows.  A real Bummer that !!
