#if !defined(__watn_dlg_h)              // Sentry, use file only if it's not already included.
#define __watn_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         watn_dlg.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Water_n_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <owl/commctrl.h>


#include "howwet.rh"            // Definition of all resources.

#include "simul.h"
#include <owl\edit.h>
#include <owl\combobox.h>

class RainfallFile;

//{{TDialog = Water_n_dlg}}
class Water_n_dlg : public TDialog {
protected:
	Parameters* Param_ptr;					// pointer to parameters object
	Simul* Simul_ptr;							// pointer to simul object
   RainfallFile* rainfallFile;
	bool In_fill_results;					// are we currently in "Fill_results_boxes" routine?

	TEdit* User_stored_moisture;			// user estimate of stored moisture (mm)
   TEdit* In_crop_rain;                // in crop rain control
	TEdit* Days_to_maturity;				// days to maturity control
	TEdit* Threshold_water;					// threshold water
	TEdit* Water_use_efficiency;			// water use efficiency
	TEdit* Protein;							// protein
	TEdit* User_NO3_supply;					// user estimate of no3 supply

	GString User_stored_moisture_st;		// user estimate of stored moisture (mm)
   GString In_crop_rain_st;            // in crop rain control
	GString Days_to_maturity_st;			// days to maturity control
	GString Threshold_water_st;		   // threshold water
	GString Water_use_efficiency_st;		// water use efficiency
	GString Protein_st;						// protein
	GString User_NO3_supply_st;			// user estimate of no3 supply
   int NO3_Perc_tonne_num;             // saved value of no3_perc_tonne

	TComboBox* NO3_Perc_tonne;				// no3 / %protein/tonne

   TStatic* Stored_moisture;				// stored moisture control
   TStatic* Total_water1;              // total water control
   TStatic* Total_water2;              // total water control
   TStatic* Expected_yield;            // expected yield control
   TStatic* Howwet_NO3_supply;			// howwet estimate of no3 supply
	TStatic* NO3_gap;							// nitrate gap.
	TStatic* Howwet_NO3_demand;			// howwet estimate of no3 demand
   TStatic* Need_surplus;              // needed/surplus static control

	void Fill_results_boxes(void);		// fill all results boxes based on
   												// user input
   float Calc_no3_demand
         (int Days_to_mat,
          float WUE,
          float Prot,
          float kgno3_perc_t);
													// calculate no3 demand.

public:
    Water_n_dlg (TWindow* parent,
                 Parameters* Param_p,
                 Simul* Simul_p,
                 RainfallFile* rainfallFile,
                 TResId resId = ID_WATER_N_DLG,
                 TModule* module = 0);
    virtual ~Water_n_dlg ();

//{{Water_n_dlgVIRTUAL_BEGIN}}
public:
    virtual void SetupWindow ();
    virtual bool CanClose();
//{{Water_n_dlgVIRTUAL_END}}

//{{Water_n_dlgRSP_TBL_BEGIN}}
protected:
    void Days_to_maturity_change (void);
													// The days to maturity edit box has changed.
	 void In_crop_rain_change (void);
													// The in crop rainfall edit box has changed.
    void User_no3_estimate_change ();
    void Help1 ();
    void Help10 ();
    void Help11 ();
    void Help2 ();
    void Help3 ();
    void Help4 ();
    void Help5 ();
    void Help6 ();
    void Help7 ();
    void Help8 ();
    void Help9 ();
    HBRUSH EvCtlColor (HDC dc, HWND hWndChild, uint ctlType);
    void Print();
//{{Water_n_dlgRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Water_n_dlg);
};    //{{Water_n_dlg}}


#endif                                      // __watn_dlg_h sentry.

