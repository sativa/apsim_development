//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TExcel.h"
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\excel.h>

TExcelApplication *ExcelApp;

using namespace std;
#pragma package(smart_init)

#pragma resource "ComponentRegistration.res"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TExcel::TExcel(TComponent* owner)
   : TSEGTable(owner)
   {
   xlsPageNames = new TStringList;
   xlsPageIndex = -1;
   ExcelApp = NULL;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TExcel::~TExcel()
   {
   delete xlsPageNames;
   }
//---------------------------------------------------------------------------
// Component has loaded - assume the current dir is the reportDirectory.
//---------------------------------------------------------------------------
void __fastcall TExcel::Loaded(void)
   {
   TSEGTable::Loaded();
   reportDirectory = GetCurrentDir();
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current paths will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TExcel::relativeToAbsoluteFile(void)
   {
   if (reportDirectory != "")
      {
      SetCurrentDir(reportDirectory);
      xlsFileName = ExpandFileName(xlsFileName);
      }
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current paths will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TExcel::absoluteToRelativeFile()
   {
   if (reportDirectory != "")
      {
      xlsFileName = ExtractRelativePath(reportDirectory + "\\", xlsFileName);
      }
   }
//---------------------------------------------------------------------------
// set the 'xlsFileName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TExcel::setFilename(AnsiString file)
   {
   if (xlsFileName != file)
      {
      xlsFileName = file;
      vector<string> sheetNames;
      if (reportDirectory == "")
         reportDirectory = GetCurrentDir();

      if (FileExists(xlsFileName))
         {
         relativeToAbsoluteFile();
         getXLSSheetNames(xlsFileName.c_str(), sheetNames);
         absoluteToRelativeFile();
         Stl_2_tstrings(sheetNames, xlsPageNames);
         forceRefresh();
         }
      }
   }
//---------------------------------------------------------------------------
// Called by SEGReport to give components a chance to know the current
// report directory.  Used by ApsimFileReader to use relative paths.
//---------------------------------------------------------------------------
void TExcel::setReportDirectory(AnsiString reportDir)
   {
   relativeToAbsoluteFile();
   reportDirectory = reportDir;
   absoluteToRelativeFile();
   }
//---------------------------------------------------------------------------
// return the 'pageName' property
//---------------------------------------------------------------------------
AnsiString __fastcall TExcel::getPageName(void)
   {
   if (xlsPageIndex >= 0 && xlsPageIndex < xlsPageNames->Count)
      return xlsPageNames->Strings[xlsPageIndex];
   else
      return "";
   }
//---------------------------------------------------------------------------
// set the 'pageName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TExcel::setPageName(AnsiString pageName)
   {
   int index = xlsPageNames->IndexOf(pageName);
   if (xlsPageIndex != index)
      {
      xlsPageIndex = index;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TExcel::createFields(void) throw(runtime_error)
   {
   if (xlsFileName != "" && pageName != "")
      {
      relativeToAbsoluteFile();

      // get a list of pages from XL file
      ExcelApp = new TExcelApplication(NULL);
      ExcelApp->Connect();
      ExcelApp->Workbooks->Open(WideString(fileName));
      ExcelWorksheetPtr worksheet = ExcelApp->Sheets->get_Item((Variant)xlsPageIndex+1);

      vector<string> fieldValues;
      getXLSRow(worksheet, 1, fieldNames);
      getXLSRow(worksheet, 2, fieldValues);

      FieldDefs->Clear();
      addDBFields(this, fieldNames, fieldValues);

      worksheet->Release();
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TExcel::storeRecords(void) throw(runtime_error)
   {
   if (ExcelApp != NULL)
      {
      ExcelWorksheetPtr worksheet;
      try
         {
         worksheet = ExcelApp->Sheets->get_Item((Variant)xlsPageIndex+1);

         vector<string> values;
         int row = 2;
         while (getXLSRow(worksheet, row, values))
            {
            appendDBRecord(this, fieldNames, values);
            row++;
            }
         }
      catch (const runtime_error& err)
         {
         absoluteToRelativeFile();
         // shut down EXCEL.
         worksheet->Release();
         ExcelApp->ActiveWorkbook->Close((Variant)false);
         ExcelApp->Quit();
         ExcelApp->Disconnect();
         delete ExcelApp;
         throw;
         }
      // shut down EXCEL.
      worksheet->Release();
      ExcelApp->ActiveWorkbook->Close((Variant)false);
      ExcelApp->Quit();
      ExcelApp->Disconnect();
      delete ExcelApp;

      absoluteToRelativeFile();
      Sleep(1000);
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TExcel::setProperty(const std::string& propertyName,
                         const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "filename"))
      fileName = propertyValue.c_str();
   else if (Str_i_Eq(propertyName, "pagename"))
      pageName = propertyValue.c_str();
   }

