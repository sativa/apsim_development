//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
//   Important note about DLL memory management when your DLL uses the
//   static version of the RunTime Library:
//
//   If your DLL exports any functions that pass String objects (or structs/
//   classes containing nested Strings) as parameter or function results,
//   you will need to add the library MEMMGR.LIB to both the DLL project and
//   any other projects that use the DLL.  You will also need to use MEMMGR.LIB
//   if any other projects which use the DLL will be perfomring new or delete
//   operations on any non-TObject-derived classes which are exported from the
//   DLL. Adding MEMMGR.LIB to your project will change the DLL and its calling
//   EXE's to use the BORLNDMM.DLL as their memory manager.  In these cases,
//   the file BORLNDMM.DLL should be deployed along with your DLL.
//
//   To avoid using BORLNDMM.DLL, pass string information using "char *" or
//   ShortString parameters.
//
//   If your DLL uses the dynamic version of the RTL, you do not need to
//   explicitly add MEMMGR.LIB as this will be done implicitly for you
//---------------------------------------------------------------------------
USELIB("general.lib");
USELIB("data.lib");
USEUNIT("low_level\XY_series.cpp");
USEUNIT("low_level\Bar_series.cpp");
USEUNIT("low_level\Brush.cpp");
USEUNIT("low_level\Chart_base.cpp");
USEUNIT("low_level\Drawable.cpp");
USEUNIT("low_level\Font.cpp");
USEUNIT("low_level\Legend_class.cpp");
USEUNIT("low_level\Line.cpp");
USEUNIT("low_level\Plot.cpp");
USEUNIT("low_level\Position.cpp");
USEUNIT("low_level\Rectangle.cpp");
USEUNIT("low_level\Screen.cpp");
USEUNIT("low_level\Series_base.cpp");
USEUNIT("low_level\Text.cpp");
USEUNIT("low_level\Axis.cpp");
USEUNIT("high_level\Scatter_format.cpp");
USEUNIT("high_level\Bar_format.cpp");
USEUNIT("high_level\Depth_chart.cpp");
USEUNIT("high_level\Format_base.cpp");
USEUNIT("high_level\Frequency_chart.cpp");
USEUNIT("high_level\High_level_chart_base.cpp");
USEUNIT("high_level\High_level_screen.cpp");
USEUNIT("high_level\Pred_obs_chart.cpp");
USEUNIT("high_level\Prob_chart.cpp");
USEUNIT("high_level\Scatter_chart.cpp");
USEUNIT("high_level\Apsim_depth_chart.cpp");
USEUNIT("low_level\Pen.cpp");
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
   return 1;
}
//---------------------------------------------------------------------------
