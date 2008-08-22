#if !defined(__how_main_h)              // Sentry, use file only if it's not already included.
#define __how_main_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.exe Application
    FILE:         how_main.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Howwet_main (TWindow).
*/


#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include "simul.h"
#include "watn_dlg.h"

class RainfallFile;

//{{TWindow = Howwet_main}}
class Howwet_main : public TWindow {
protected:
   TDib *Main_bitmap;
   TBitmap *Logo_bitmap;
   TBitmap *Desc_bitmap;
   TBitmap *Funded_bitmap;
   bool Display_logo_screen;
   Parameters *Param_ptr;
   Simul *Simul_ptr;
	bool Remove_logos;
   bool Display_init_dialog;
   int Screen_height;
   Water_n_dlg* Water_n_dlg_ptr;
   RainfallFile* rainfallFile;

   void setupRainfallMenu();

public:
    Howwet_main (TWindow* parent, const char far* title = 0, TModule* module = 0);
    virtual ~Howwet_main ();

//{{Howwet_mainVIRTUAL_BEGIN}}
public:
    virtual void Paint (TDC& dc, bool erase, TRect& rect);
    virtual void SetupWindow();
//{{Howwet_mainVIRTUAL_END}}

//{{Howwet_mainRSP_TBL_BEGIN}}
protected:
    void Climate ();
    void Exit ();
    void Soil_type_menu ();
    void Soil_conditions ();
    void Add_to_existing_rain ();
    void Create_rain_file ();
    void Delete_rain_file ();
    void About_menu ();
    void Results_table ();
    void Rainfall_analysis ();
    void Rainfall_calender ();
    void Nitrogen_graph_dlg ();
    void Soil_water_graph ();
    void Rainfall_average ();
    void Water_NBalance ();
    void Animated_profile ();
    void Erosion_estimation ();
    void RainfallEntry();
    void RainfallMenuItemClick(WPARAM id);
//{{Howwet_mainRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Howwet_main);
};    //{{Howwet_main}}


#endif                                      // __how_main_h sentry.
