#if !defined(__nitgraph_h)              // Sentry, use file only if it's not already included.
#define __nitgraph_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         nitgraph.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Nitrogen_graph (TDialog).
*/

#include "simul.h"
#include "how_anim.h"

class RainfallFile;

//{{TDialog = Nitrogen_graph}}
class Nitrogen_graph : public Howwet_animated_dlg {
protected:
   GColumns All_data;                  // all data for chart.
   DChart *Temp_chart;                 // pointer to chart.
   DChart *Water_chart;                // pointer to chart.
   DChart *Nitrate_chart;              // pointer to chart.
   RainfallFile* rainfallFile;

   void Get_all_data (Series_attributes& Series_attr);
                                       // Fill the series attributes structure.
	void SetupWindow(void);   	         // Setup all chart objects
   void Setup_temp_chart(void);        // setup temp chart
   void Setup_water_chart(void);       // setup water chart
   void Setup_nitrate_chart(void);     // setup nitrate chart
   void Help1(void);                   // help 1 button was pressed.

public:
    Nitrogen_graph (TWindow*     parent,
						  TResId       resId,
                    TModule*     module,
                    Parameters*  Param_ptr,
                    Simul*       Simul_ptr,
                    RainfallFile* rainfallFile);
    virtual ~Nitrogen_graph ();

    DECLARE_RESPONSE_TABLE(Nitrogen_graph);
};    //{{Nitrogen_graph}}


#endif                                      // __nitgraph_h sentry.
