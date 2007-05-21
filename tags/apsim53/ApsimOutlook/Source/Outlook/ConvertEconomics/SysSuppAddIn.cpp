//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ComObj.hpp>
#include "SysSuppAddIn.h"


namespace System
   {
   void __fastcall CheckSafecallResult(HRESULT hr)
      {
      OleCheck(hr); /* since I always design error free systems, I'm not afraid of
                     this */
      }
   }

