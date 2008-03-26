//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("TAPSTable_form.cpp", APSTable_form);
USEFORM("TAnalysis_form.cpp", Analysis_form);
USEFORM("TSummary_form.cpp", Summary_form);
USEFORM("TProbability_analysis_form.cpp", Probability_analysis_form);
USEFORM("TTime_series_form.cpp", Time_series_form);
USEFORM("TDifference_form.cpp", Difference_form);
USEFORM("TFrequency_form.cpp", Frequency_form);
USEFORM("TPie_frequency_form.cpp", Pie_frequency_form);
USEFORM("txy_form.cpp", XY_form);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
