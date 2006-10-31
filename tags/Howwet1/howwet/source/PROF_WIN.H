#if !defined(__prof_win_h)              // Sentry, use file only if it's not already included.
#define __prof_win_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         prof_win.h
    AUTHOR:       


    OVERVIEW
    ========
    Class definition for Animated_profile_window (TWindow).
*/

#include <owl\owlpch.h>
#pragma hdrstop


#include "howwet.rh"            // Definition of all resources.
#include "shadeobj.h"
#include "lineobj.h"
#include "simul.h"
#include "params.h"
#include <owl\static.h>
#include <owl\slider.h>

//{{TWindow = Animated_profile_window}}
class Animated_profile_window : public TWindow  {
protected:
	enum Speed_enum {stop, f, ff, fff, r, rr, rrr};

	TSlider *Slide_control;		      	// pointer to slide control
   TBitmap* Our_bitmap;                // our bitmap
	TDC *DC_ptr;								// pointer to DC used for animation
	bool Doing_animation;					// are we doing animation?
   bool Simulation_done;					// is the simulation done?
   int Current_index;                  // current index
   bool Going_forwards;                // are we going forwards?
	TBitmap *Slide_bitmap;					// slide bitmap
	TBitmap *Rain_bitmap;					// bitmap of rain
	TBitmap *Sun_bitmap;						// bitmap of sun
   TStatic *Date_control;              // control for current date.
	TStatic *mm_water_control;				// moisture content control (mm)
	TStatic *perc_water_control;        // moisture contect control (%)
	TStatic *Rain_control;					// rainfall today control (mm)
	TStatic *Rain_total_control;			// total rainfall control (mm)
	TStatic *Runoff_control;				// runoff today control (mm)
	TStatic *Runoff_total_control;		// total runoff control (mm)
   TStatic *Start_date_control;        // start date control
   TStatic *End_date_control;          // end date control

	TFont* Big_font;							// font used for static controls.
	TFont* Tiny_font;
	TFont* Medium_font;

   float Total_rain_so_far;            // total rainfall since start
   float Total_runoff_so_far;          // total runoff since start

	Shade_object Evap_shade;				// shade object for evaporation
	Shade_object Runoff_shade;				// shade object for runoff.
	Shade_object Surface1;					// shade object for surface moisture1
	Shade_object Surface2;					// shade object for surface moisture2
	Line_object Profile1;					// animate profile line and shading1
	Line_object Profile2;  					// animate profile line and shading2
	Simul* Simul_ptr;							// pointer to simulation object.
   Parameters* Param_ptr;              // pointer to parameter object
	Speed_enum Speed_value;					// current speed of animation

   void Start_animation (void);			// Start the animation.
   void End_animation (void);				// End the animation.
   void EvTimer (uint timerId);			// Timer tick from Windows.
   bool Paint_next_point (void);			// Paint the next set of points.  Return
													// TRUE if all went ok.  FALSE if we run
													// out of data.
   void Restart_button ();					// Restart simulation
	void Display_statics (void);			// Display all static controls
	void Ok_button(void);					// Ok button was clicked
   void Forward_button(void);          // forward button was clicked.
   void FF_button(void);               // fast forward button was clicked.
   void Rewind_button(void);           // rewind button was clicked.
   void RR_button(void);               // fast rewind button was clicked.
   void Stop_button(void);             // stop button was clicked.
   void Display_slider_stuff(void);    // Display dates etc in slider window.
	void Recalc(void);						// recalc all accumulated fields.
	void Help1 (void);
	void Help2 (void);
	void Help3 (void);
	void Help4 (void);

public:
//    Animated_profile_window (TWindow* parent, const char far* title = 0, TModule* module = 0);
    Animated_profile_window (TWindow* parent, Simul* Simul_ptr, Parameters* Param_ptr, const char far* title = 0, TModule* module = 0);
    virtual ~Animated_profile_window ();

//{{Animated_profile_windowVIRTUAL_BEGIN}}
public:
    virtual void Paint (TDC& dc, bool erase, TRect& rect);
    virtual void SetupWindow ();
//{{Animated_profile_windowVIRTUAL_END}}

//{{Animated_profile_windowRSP_TBL_BEGIN}}
protected:
    bool EvEraseBkgnd (HDC dc);
    void EvKeyDown (uint key, uint repeatCount, uint flags);
    HBRUSH EvCtlColor (HDC hDC, HWND hWndChild, uint ctlType);
    void EvHScroll(uint scrollCode, uint thumbPos, HWND hWndCtl);
//{{Animated_profile_windowRSP_TBL_END}}
DECLARE_RESPONSE_TABLE(Animated_profile_window);
};    //{{Animated_profile_window}}


#endif                                      // __prof_win_h sentry.

