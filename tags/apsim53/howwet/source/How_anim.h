#if !defined(__how_anim_h)              // Sentry, use file only if it's not already included.
#define __how_anim_h

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

#include <cl\anim_dlg.h>
#include "params.h"
#include "simul.h"
#include <owl\static.h>

#define MAX_DATA_POINTS   600

class RainfallFile;

//{{TDialog = Animated_dlg}}
class Howwet_animated_dlg : public Animated_dlg  {
private:

protected:
   Parameters *Param_ptr;              // pointer to param
   Simul *Simul_ptr;                   // pointer to simul.
   RainfallFile*  rainfallFile;       
   TStatic* Date_control;              // date control.
   void Print_button(void);            // print button was pressed.
   virtual void Display_date (GDate& Date);
                                       // Display the date on the screen.
public:
    Howwet_animated_dlg (TWindow*      parent,
                         TResId        resId,
                         TModule*      module,
                         Parameters*   Param_p,
                         Simul*        Simul_p,
                         RainfallFile*  rainfallF);

   DECLARE_RESPONSE_TABLE(Howwet_animated_dlg);
   };    //{{Howwet_animated_dlg}}


#endif

