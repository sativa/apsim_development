//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORM("TPropertyForm.cpp", PropertyForm);
USEFORM("TApsimFileReaderForm.cpp", ApsimFileReaderForm);
USEFORM("TChartForm.cpp", ChartForm);
USEFORM("TDirectorySelectForm.cpp", DirectorySelectForm);
USEFORM("TExcelForm.cpp", ExcelForm);
USEFORM("TFilterForm.cpp", FilterForm);
USEFORM("TImageForm.cpp", ImageForm);
USEFORM("TLibraryForm.cpp", LibraryForm);
USEFORM("TProbabilityForm.cpp", ProbabilityForm);
USEFORM("TREMSForm.cpp", REMSForm);
USEFORM("TShapeForm.cpp", ShapeForm);
USEFORM("TSOIForm.cpp", SOIForm);
USEFORM("TStatsForm.cpp", StatsForm);
USEFORM("TStringsForm.cpp", StringsForm);
USEFORM("TTextForm.cpp", TextForm);
HINSTANCE instanceHandle;
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
   instanceHandle = hinst;
   return 1;
}
//---------------------------------------------------------------------------
