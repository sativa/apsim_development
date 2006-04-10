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
#ifndef   __economics_TLB_h__
#define   __economics_TLB_h__

#pragma option push -b


#include <sysdefs.h>
#include <utilcls.h>
#include <stdvcl.hpp>
#include <ocxproxy.h>

namespace Economics_tlb
{

// *********************************************************************//
// HelpString: 
// Version:    5.0
// *********************************************************************//


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:      //
//   Type Libraries     : LIBID_xxxx                                    //
//   CoClasses          : CLSID_xxxx                                    //
//   DISPInterfaces     : DIID_xxxx                                     //
//   Non-DISP interfaces: IID_xxxx                                      //
// *********************************************************************//
DEFINE_GUID(LIBID_economics, 0xB9DEF7A6, 0xFC39, 0x11D2, 0x8C, 0x38, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B);
DEFINE_GUID(IID__Gross_Margin, 0xCE394E0E, 0x536A, 0x11D3, 0x8C, 0x4D, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B);
DEFINE_GUID(CLSID_Gross_Margin, 0xB9DEF7A8, 0xFC39, 0x11D2, 0x8C, 0x38, 0x00, 0xC0, 0x4F, 0x87, 0x30, 0x7B);

// *********************************************************************//
// Forward declaration of interfaces defined in Type Library            //
// *********************************************************************//
interface _Gross_Margin;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                     //
// (NOTE: Here we map each CoClass to it's Default Interface            //
// *********************************************************************//
typedef _Gross_Margin Gross_Margin;
// *********************************************************************//
// Interface: _Gross_Margin
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {CE394E0E-536A-11D3-8C4D-00C04F87307B}
// *********************************************************************//
interface _Gross_Margin : public IDispatch
{
public:
  virtual HRESULT STDMETHODCALLTYPE econ_summary(VARIANT* tabset/*[in,out]*/, 
                                                 VARIANT* descript_string/*[in,out]*/, 
                                                 VARIANT* yieldset/*[in,out]*/, 
                                                 VARIANT* grossm/*[in,out]*/) = 0; // [1610809369]
  virtual HRESULT STDMETHODCALLTYPE full_monte(void) = 0; // [1610809370]
  virtual HRESULT STDMETHODCALLTYPE compare_desc(BSTR* desc1/*[in,out]*/, BSTR* desc2/*[in,out]*/, 
                                                 VARIANT_BOOL* Param3/*[out,retval]*/) = 0; // [1610809374]
  virtual HRESULT STDMETHODCALLTYPE compare_soi(VARIANT* desc1/*[in,out]*/, 
                                                VARIANT* desc2/*[in,out]*/, 
                                                VARIANT_BOOL* Param3/*[out,retval]*/) = 0; // [1610809375]
};

// *********************************************************************//
// SmartIntf: TCOM_Gross_Margin
// Interface: _Gross_Margin
// *********************************************************************//
class TCOM_Gross_Margin : public TComInterface<_Gross_Margin>
{
public:
  TCOM_Gross_Margin() {}
  TCOM_Gross_Margin(_Gross_Margin *intf, bool addRef = false) : TComInterface<_Gross_Margin>(intf, addRef) {}
};

// *********************************************************************//
// DispIntf:  _Gross_Margin
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {CE394E0E-536A-11D3-8C4D-00C04F87307B}
// *********************************************************************//
class _Gross_MarginDisp : public TAutoDriver<_Gross_Margin>
{
  typedef TDispId<_Gross_Margin> _TDispID;
public:

  _Gross_MarginDisp()
  {}

  _Gross_MarginDisp& operator=(_Gross_Margin *pintf)
  {
    TAutoDriver<_Gross_Margin>::Bind(pintf);
  }

  HRESULT BindDefault(/*Binds to CoClass Gross_Margin*/)
  {
    return OLECHECK(Bind(CLSID_Gross_Margin));
  }

  HRESULT /*[VT_HRESULT:0]*/  econ_summary(VARIANT* tabset/*[in,out]*/,
                                           VARIANT* descript_string/*[in,out]*/,
                                           VARIANT* yieldset/*[in,out]*/,VARIANT* grossm/*[in,out]*/)
  {
    static _TDispID _dispid(*this, OLETEXT("econ_summary"), DISPID(1610809369));
    TAutoArgs<4> _args;
    _args[1] = tabset /*[VT_VARIANT:1]*/;
    _args[2] = descript_string /*[VT_VARIANT:1]*/;
    _args[3] = yieldset /*[VT_VARIANT:1]*/;
    _args[4] = grossm /*[VT_VARIANT:1]*/;
    return OleFunction(_dispid, _args);
  }

  HRESULT /*[VT_HRESULT:0]*/  full_monte()
  {
    static _TDispID _dispid(*this, OLETEXT("full_monte"), DISPID(1610809370));
    return OleFunction(_dispid);
  }

  HRESULT /*[VT_HRESULT:0]*/  compare_desc(BSTR* desc1/*[in,out]*/,BSTR* desc2/*[in,out]*/,
                                           VARIANT_BOOL* Param3/*[out,retval]*/)
  {
    static _TDispID _dispid(*this, OLETEXT("compare_desc"), DISPID(1610809374));
    TAutoArgs<2> _args;
    _args[1] = desc1 /*[VT_BSTR:1]*/;
    _args[2] = desc2 /*[VT_BSTR:1]*/;
    return OutRetValSetterPtr(Param3 /*[VT_BOOL:1]*/, _args, OleFunction(_dispid, _args));
  }

  HRESULT /*[VT_HRESULT:0]*/  compare_soi(VARIANT* desc1/*[in,out]*/,VARIANT* desc2/*[in,out]*/,
                                          VARIANT_BOOL* Param3/*[out,retval]*/)
  {
    static _TDispID _dispid(*this, OLETEXT("compare_soi"), DISPID(1610809375));
    TAutoArgs<2> _args;
    _args[1] = desc1 /*[VT_VARIANT:1]*/;
    _args[2] = desc2 /*[VT_VARIANT:1]*/;
    return OutRetValSetterPtr(Param3 /*[VT_BOOL:1]*/, _args, OleFunction(_dispid, _args));
  }

};


// *********************************************************************//
// COCLASS DEFAULT INTERFACE CREATOR
// CoClass  : Gross_Margin
// Interface: TCOM_Gross_Margin
// *********************************************************************//
class CoGross_Margin : public CoClassCreator
{
public:
  static TCOM_Gross_Margin Create(void);
  static TCOM_Gross_Margin CreateRemote(LPWSTR machineName);

  static HRESULT Create(TCOM_Gross_Margin& defIntfObj);
  static HRESULT CreateRemote(LPWSTR machineName, TCOM_Gross_Margin& defIntfObj);
};

};     // namespace Economics_tlb

#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using  namespace Economics_tlb;
#endif

#pragma option pop

#endif // __economics_TLB_h__
