#if !defined(__eros_dlg_h)              // Sentry, use file only if it's not already included.
#define __eros_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         eros_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Erosion_dialog (TDialog).      
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include <owl\edit.h>
#include "params.h"
#include "simul.h"
#include <cl\chartcon.h>

//{{TDialog = Erosion_dialog}}
// ------------------------------------------------------------------
//  Short description:
//    Class for an erosion dialog

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
class Erosion_dialog : public TDialog
   {
   protected:
      TEdit* Slope_edit;               // edit box for slope
      TEdit* Slope_length_edit;        // edit box for slope length
      TEdit* Erodability_edit;         // edit box for erodability
      TEdit* Start_cover_edit;         // edit box for start cover
      TEdit* End_cover_edit;           // edit box for end cover.
      TStatic* Start_date_static;      // Start date static
      TStatic* End_date_static;        // end date static
      TStatic* Runoff_static;          // total runoff static
      TStatic* Soil_loss_static;       // soil loss static
      TStatic* Runoff_text;
      TStatic* Soil_loss_text;

      Parameters* Param_ptr;           // pointer to parameter object
      Simul* Simul_ptr;                // pointer to simulation object.

      float Saved_start_cover;
      float Saved_end_cover;

      Chart_control* Chart_control_ptr1;
      Chart_control* Chart_control_ptr2;

      GColumns All_data;               // data for runoff chart.

      void Set_slope_length_edit (void);
                                       // set the slope length edit box.
      void Setup_charts (void);        // setup the charts.
      void Get_params (float&   Slope,
                       float&   Slope_length,
                       float&   K);
                                       // return current values for params.

   public:
//    Erosion_dialog (TWindow* parent, TResId resId = IDD_EROSION_DLG, TModule* module = 0);
      Erosion_dialog (TWindow*    parent,
                      Parameters* Param_ptr,
                      Simul*      Simul_ptr,
                      TResId      resId = IDD_EROSION_DLG,
                      TModule*    module = 0);
      virtual ~Erosion_dialog ();

//{{Erosion_dialogVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
//{{Erosion_dialogVIRTUAL_END}}

//{{Erosion_dialogRSP_TBL_BEGIN}}
protected:
    void Redraw_graphs ();
    void Help1 ();
    void Help2 ();
    void Help3 ();
    void Help4 ();
    void Help5 ();
//{{Erosion_dialogRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Erosion_dialog);
   };   //{{Erosion_dialog}}


#endif                                      // __eros_dlg_h sentry.

