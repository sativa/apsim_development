#include <consts.h>

#include <gobjs\delay.h>

#include <time.h>   // time_t
#include <stdio.h>  // NULL

// *******************************************************************
       void Delay(int Num_seconds)  {
// *******************************************************************

//  Short description:
//    Delay for a specified number of seconds,

//  Notes:
//    This routine is necessary because the standard C routine "delay"
//    doesn't work with Windows.  A real Bummer that !!

//  Changes:
//    DPH - 2/9/93

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    int Num_seconds;                 // Number of seconds to delay

//  Internal variables
      time_t Start_time;               // Starting time
      time_t Current_time;             // Current time

// -------------------- Executable code section ----------------------

   Start_time = time(NULL);

   do
      Current_time = time(NULL);
   while (Current_time - Start_time < Num_seconds);
   }

