#if !defined(__sw_graph_h)              // Sentry, use file only if it's not already included.
#define __sw_graph_h

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         nitgraph.h
    AUTHOR:


    OVERVIEW
    ========
    Class definition for SW_graph (TDialog).
*/

#include "simul.h"
#include "how_anim.h"

class RainfallFile;

//{{TDialog = SW_graph}}
class SW_graph : public Howwet_animated_dlg {
protected:
   GColumns All_data;                  // all data for chart.
   DChart *SW_chart;                   // pointer to sw chart.
   RainfallFile* rainfallFile;

   void Get_all_data (Series_attributes& Series_attr);
                                       // Fill the series attributes structure.
	void SetupWindow(void);   	         // Setup all chart objects
   void Help1(void);                   // help 1 button

public:
    SW_graph (TWindow*     parent,
				  TResId       resId,
              TModule*     module,
              Parameters*  Param_ptr,
              Simul*       Simul_ptr,
              RainfallFile* rainfallFile);
    virtual ~SW_graph ();

    DECLARE_RESPONSE_TABLE(SW_graph);
};    //{{SW_graph}}


#endif                                      // __sw_graph_h sentry.
