/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.exe Application
    FILE:         howwet.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Howwet_app (TApplication).      
*/


#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "howwet.h"

#include "how_main.h"                        // Definition of client class.

#include <gobjs\ginifile.h>
GIni_file Howwet_ini_file;      // global ini file object


//{{Howwet_app Implementation}}


//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Howwet_app, TApplication)
//{{Howwet_appRSP_TBL_BEGIN}}
//{{Howwet_appRSP_TBL_END}}
END_RESPONSE_TABLE;


//////////////////////////////////////////////////////////
// Howwet_app
// =====
//
Howwet_app::Howwet_app () : TApplication("Howwet?")
{


    // INSERT>> Your constructor code here.
}


Howwet_app::~Howwet_app ()
{
    // INSERT>> Your destructor code here.
}


//////////////////////////////////////////////////////////
// Howwet_app
// =====
// Application intialization.
//
void Howwet_app::InitMainWindow ()
{
    if (nCmdShow != SW_HIDE)
        nCmdShow = (nCmdShow != SW_SHOWMINNOACTIVE) ? SW_SHOWNORMAL : nCmdShow;

    SDIDecFrame *frame = new SDIDecFrame(0, GetName(), 0, false);

    //
    // Assign ICON w/ this application.
    //
    frame->SetIcon(this, IDI_SDIAPPLICATION);

    //
    // Menu associated with window and accelerator table associated with table.
    //
    frame->AssignMenu(SDI_MENU);
    
    //
    // Associate with the accelerator table.
    //
    frame->Attr.AccelTable = SDI_MENU;

  
    SetMainWindow(frame);

    frame->SetMenuDescr(TMenuDescr(SDI_MENU));

    //
    // Borland Windows custom controls.
    //
    EnableBWCC();
}


//{{SDIDecFrame Implementation}}


SDIDecFrame::SDIDecFrame (TWindow *parent, const char far *title, TWindow *clientWnd, bool trackMenuSelection, TModule *module)
    : TDecoratedFrame(parent, title, clientWnd == 0 ? new Howwet_main(0, "") : clientWnd, trackMenuSelection, module)
{
    // INSERT>> Your constructor code here.

   // Make sure window takes up whole screen.

   int width = GetSystemMetrics(SM_CXSCREEN);
   int height = GetSystemMetrics(SM_CYSCREEN);

   Attr.X = 1;
   Attr.Y = 1;
   Attr.W = width;
   Attr.H = height;
}


SDIDecFrame::~SDIDecFrame ()
{
    // INSERT>> Your destructor code here.

}

int OwlMain (int , char* [])
{
   Howwet_ini_file.Set_file_name("howwet.ini");

   // Ensure floating point is linked in.

   float D;
   D = 0.45;
   D *= 0.67;

    try {
        Howwet_app    app;
        return app.Run();
    }
    catch (xmsg& x) {
        ::MessageBox(0, x.why().c_str(), "Exception", MB_OK);
    }

    return -1;
}


