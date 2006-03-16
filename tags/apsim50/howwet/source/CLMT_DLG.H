#if !defined(__clmt_dlg_h)              // Sentry, use file only if it's not already included.
#define __clmt_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         clmt_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Climate_dlg (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.

#include "params.h"
#include <owl\listbox.h>

//{{TDialog = Climate_dlg}}
class Climate_dlg : public TDialog {
protected:
	Parameters *Param_ptr;
   TListBox *Site_list_control;

public:
    Climate_dlg (TWindow* parent, Parameters *Param_ptr, TResId resId = IDC_CLIMATE, TModule* module = 0);
    virtual ~Climate_dlg ();


//{{Climate_dlgVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{Climate_dlgVIRTUAL_END}}

//{{Climate_dlgRSP_TBL_BEGIN}}
protected:
    void Ok_button ();
    void EvLButtonDblClk (uint modKeys, TPoint& point);
//{{Climate_dlgRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Climate_dlg);
};    //{{Climate_dlg}}


#endif                                      // __clmt_dlg_h sentry.

