#if !defined(__rain_avg_h)              // Sentry, use file only if it's not already included.
#define __rain_avg_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         nitgraph.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Rain_avg (TDialog).
*/

#include "simul.h"
#include "how_anim.h"

class RainfallFile;

//{{TDialog = Rain_avg_graph}}
class Rain_avg_graph : public Howwet_animated_dlg {
protected:
   GColumns All_data;                  // all data for chart.
   DChart *Rain_avg_chart;             // pointer to sw chart.
   RainfallFile* rainfallFile;

   void Get_all_data (Series_attributes& Series_attr);
                                       // Fill the series attributes structure.
	void SetupWindow(void);   	         // Setup all chart objects
   void Help1(void);                   // help 1 button

public:
    Rain_avg_graph (TWindow* parent,
				        TResId resId,
                    TModule* module,
                    Parameters *Param_ptr,
                    Simul *Simul_ptr,
                    RainfallFile* rainfallFile);
    virtual ~Rain_avg_graph ();

    DECLARE_RESPONSE_TABLE(Rain_avg_graph);
};    //{{rain_avg_graph}}


#endif                                      // __rain_avg_h sentry.
