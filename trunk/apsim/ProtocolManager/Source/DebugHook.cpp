#include <general\pch.h>
#pragma hdrstop

#include "DebugHook.h"
#include <windows.h>

void DebugException(void)
   {
   try
      {
      asm int 3
      }
   __except (EXCEPTION_EXECUTE_HANDLER)
      { }
   }
                              
