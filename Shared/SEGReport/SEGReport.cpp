//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("TDirectorySelectForm.cpp", DirectorySelectForm);
USEFORM("TLibraryForm.cpp", LibraryForm);
USEFORM("TStringsForm.cpp", StringsForm);
USEFORM("TPropertyForm.cpp", PropertyForm);
USEFORM("TFilterForm.cpp", FilterForm);
USEFORM("TSOIForm.cpp", SOIForm);
USEFORM("TStatsForm.cpp", StatsForm);
USEFORM("TChartForm.cpp", ChartForm);
USEFORM("TImageForm.cpp", ImageForm);
USEFORM("TShapeForm.cpp", ShapeForm);
USEFORM("TTextForm.cpp", TextForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
HINSTANCE instanceHandle;
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
