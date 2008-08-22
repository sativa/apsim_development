#include <general/pch.h>
#pragma hdrstop

#ifdef __WIN32__
#include <windows.h>
#endif

#include "DebugHook.h"

void DebugException(void)
   {
   try
      {
      asm int 3
      }
   __except (EXCEPTION_EXECUTE_HANDLER)
      { }
   }
                              
