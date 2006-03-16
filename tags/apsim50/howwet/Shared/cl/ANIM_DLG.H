#if !defined(__anim_dlg_h)              // Sentry, use file only if it's not already included.
#define __anim_dlg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    fileview.apx Application
    FILE:         anim_dlg.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Animated_dlg (TDialog).
*/

//#include <graph\anim_dlg.rh>
#include <cl\chartcon.h>
#include <owl\printer.h>
#include <owl\slider.h>

//{{TDialog = Animated_dlg}}
class huge Animated_dlg : public TDialog {

protected:
   TPrinter *Printer;                  // pointer to printer object
	GDC *DC_ptr;								// pointer to DC used for animation
   bool Doing_animation;					// are we currently doing animation?
	THSlider *Slide_control;	   		// pointer to slide control
	Chart_control *Chart_control_ptr;   // Chart control pointer
	int Speed_value;							// Speed value to use to control animation
													// loop.  0=stop, 10=fastest.
   bool Simulation_done;               // Is the simulation complete?
   int Total_number_of_points;         // Total number of points.
   int Points_per_timer;               // Number of points to display per timer message.
   int Current_point;                  // Current point to be displayed.
   GDate Start_date;                   // date at start of simulation

   bool Paint_next_point (void);       // Paint the next set of points.  Return TRUE
                                       // if all went ok.  FALSE if we run out of data.
   virtual void SetupWindow (void);    // setup window
   void EvHScroll(uint Scroll_code,
                  uint Thumb_pos,
                  HWND Window_ctl); 	// User has changed slide.
   void Start_animation (void);        // Start the animation.
   void End_animation(void);           // End the animation
   void Change_series_visible (bool Make_visible);
                                       // Make all series visible or not visible
                                       // depending on value of make_visible.
   virtual void Display_date (GDate& Date)
      {};                              // Display the date on the screen.
   HBRUSH EvCtlColor (HDC hDC, HWND hWndChild, uint ctlType);
                                       // Change the background colour of the
                                       // slide control.

public:
    Animated_dlg (TWindow* parent, TResId resId, TModule* module = 0);
    virtual ~Animated_dlg ();

	LRESULT Do_animation(WPARAM, LPARAM);
													// chart control is telling us about it's
													// paint message.
   void Set_start_date (GDate& Start_d)
      {
      Start_date = Start_d;
      };                               // set the starting date of simulation
//{{Animated_dlgVIRTUAL_BEGIN}}
public:
//{{Animated_dlgVIRTUAL_END}}

	DECLARE_RESPONSE_TABLE(Animated_dlg);

//{{Animated_dlgRSP_TBL_BEGIN}}
protected:
    void EvTimer (uint timerId);
    bool EvEraseBkgnd (HDC dc);
    void Restart_button ();
    void Print_chart ();
//{{Animated_dlgRSP_TBL_END}}
};    //{{Animated_dlg}}


#endif                                      // __anim_dlg_h sentry.

