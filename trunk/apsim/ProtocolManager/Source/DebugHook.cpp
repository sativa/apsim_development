#include <vcl.h>
#pragma hdrstop

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
                              
