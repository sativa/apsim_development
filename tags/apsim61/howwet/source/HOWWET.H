#if !defined(__howwet_h)              // Sentry, use file only if it's not already included.
#define __howwet_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.exe Application
    FILE:         howwet.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Howwet_app (TApplication).      
*/


#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.


//
// FrameWindow must be derived to override Paint for Preview and Print.
//
//{{TDecoratedFrame = SDIDecFrame}}
class SDIDecFrame : public TDecoratedFrame {
public:
    SDIDecFrame (TWindow *parent, const char far *title, TWindow *clientWnd, bool trackMenuSelection = false, TModule *module = 0);
    ~SDIDecFrame ();
};    //{{SDIDecFrame}}


//{{TApplication = Howwet_app}}
class Howwet_app : public TApplication {
private:

public:
    Howwet_app ();
    virtual ~Howwet_app ();

//{{Howwet_appVIRTUAL_BEGIN}}
public:
    virtual void InitMainWindow();
//{{Howwet_appVIRTUAL_END}}

//{{Howwet_appRSP_TBL_BEGIN}}
protected:
    void CmHelpAbout ();
//{{Howwet_appRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Howwet_app);
};    //{{Howwet_app}}


#endif                                      // __howwet_h sentry.
