//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
USEFORM("TDirectorySelectForm.cpp", DirectorySelectForm);
USEFORM("TLibraryForm.cpp", LibraryForm);
USEFORM("TStringsForm.cpp", StringsForm);
USEFORM("TChartForm.cpp", ChartForm);
USEFORM("TImageForm.cpp", ImageForm);
USEFORM("TObjectInspectorForm.cpp", Form1);
USEFORM("TRichTextForm.cpp", RichTextForm);
USEFORM("TSEGTableForm.cpp", SEGTableForm);
USEFORM("TFilterForm.cpp", FilterForm);
USEFORM("TSOIForm.cpp", SOIForm);
USEFORM("TStatsForm.cpp", StatsForm);
USEFORM("TShapeForm.cpp", ShapeForm);
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
