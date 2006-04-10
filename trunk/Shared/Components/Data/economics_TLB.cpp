// ************************************************************************ //
// WARNING                                                                  //
// -------                                                                  //
// The types declared in this file were generated from data read from a     //
// Type Library. If this type library is explicitly or indirectly (via      //
// another type library referring to this type library) re-imported, or the //
// 'Refresh' command of the Type Library Editor activated while editing the //
// Type Library, the contents of this file will be regenerated and all      //
// manual modifications will be lost.                                       //
// ************************************************************************ //

/* File generated on 15/09/99 12:19:58 PM from Type Library described below. */

// ************************************************************************ //
// Type Lib: D:\apsuite\shared\Components\Data\economics.dll
// IID\LCID: {B9DEF7A6-FC39-11D2-8C38-00C04F87307B}\0
// Helpfile: D:\apsuite\shared\Components\Data\economics.dll
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\STDOLE2.TLB)
// ************************************************************************ //

#include <vcl.h>
#pragma hdrstop
#if defined(USING_ATLVCL)
#include <atl\atlvcl.h>
#endif

#include "economics_TLB.h"

#if !defined(__PRAGMA_PACKAGE_SMART_INIT)
#define      __PRAGMA_PACKAGE_SMART_INIT
#pragma package(smart_init)
#endif

namespace Economics_tlb
{


// *********************************************************************//
// GUIDS declared in the TypeLibrary                                    //
// *********************************************************************//
extern "C" const GUID LIBID_economics = {0xB9DEF7A6, 0xFC39, 0x11D2,{ 0x8C, 0x38, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B} };
extern "C" const GUID IID__Gross_Margin = {0xCE394E0E, 0x536A, 0x11D3,{ 0x8C, 0x4D, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B} };
extern "C" const GUID CLSID_Gross_Margin = {0xB9DEF7A8, 0xFC39, 0x11D2,{ 0x8C, 0x38, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B} };



// *********************************************************************//
// COCLASS DEFAULT INTERFACE CREATOR
// (The following methods provide an easy way to create an instance of
//  the default interface, _Gross_Margin, of the CoClass Gross_Margin)
// *********************************************************************//

HRESULT CoGross_Margin::Create(TCOM_Gross_Margin& _intf)
{
  static TInitOle __initializeOle;
  return CoCreateInstance(CLSID_Gross_Margin, IID__Gross_Margin, (LPVOID*)&_intf);
};

TCOM_Gross_Margin CoGross_Margin::Create(void)
{
  TCOM_Gross_Margin _intf;
  Create(_intf);
  return _intf;
};

HRESULT CoGross_Margin::CreateRemote(LPWSTR machineName, TCOM_Gross_Margin& _intf)
{
  static TInitOle __initializeOle;
  COSERVERINFO ServerInfo = { 0, machineName, NULL, 0 };
  MULTI_QI MQI = { &IID__Gross_Margin, NULL, 0 };
  HRESULT _hr = ::CoCreateInstanceEx(CLSID_Gross_Margin, NULL,
                                     CLSCTX_LOCAL_SERVER|
                                     CLSCTX_REMOTE_SERVER|
                                     CLSCTX_INPROC_SERVER,
                                     &ServerInfo, 1, &MQI);
  if (SUCCEEDED(_hr))
    _intf = (_Gross_Margin*)MQI.pItf;
  return _hr;
};

TCOM_Gross_Margin CoGross_Margin::CreateRemote(LPWSTR machineName)
{
  TCOM_Gross_Margin _intf;
  CreateRemote(machineName, _intf);
  return _intf;
};

};     // namespace Economics_tlb
