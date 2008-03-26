//---------------------------------------------------------------------------

#ifndef SysSuppAddInH
#define SysSuppAddInH

#include <windows.hpp>

//function _CheckAutoResult(ResultCode: HResult): HResult;

extern "C"
   {
   void __fastcall CheckSafecallResult(HRESULT hr);
   }

#endif
