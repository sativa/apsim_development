#if !defined(__so_what_h)              // Sentry, use file only if it's not already included.
#define __so_what_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         so_what.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for So_what_dlg (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include <cl\viewbox.h>

//{{TDialog = So_what_dlg}}
class So_what_dlg : public TDialog {
protected:
	int Screen_number;						// screen number to search for
	int Page_number;							// page number to search for.
	View_box *List_box_ptr;					// pointer to listbox.

public:
    So_what_dlg (TWindow* parent, int Screen_num, int Page_num, TResId resId = IDC_SO_WHAT_DLG, TModule* module = 0);
    virtual ~So_what_dlg ();

//{{So_what_dlgVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{So_what_dlgVIRTUAL_END}}
};    //{{So_what_dlg}}


#endif                                      // __so_what_h sentry.

