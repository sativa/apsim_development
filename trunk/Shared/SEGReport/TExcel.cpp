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
// set the 'xlsFileName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TExcel::setFilename(AnsiString file)
   {
   if (xlsFileName != file)
      {
      xlsFileName = file;
      vector<string> sheetNames;
      getXLSSheetNames(xlsFileName.c_str(), sheetNames);
      Stl_2_tstrings(sheetNames, xlsPageNames);
      forceRefresh();
      }
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
void TExcel::createFields(void) throw(runtime_error)
   {
   if (xlsFileName != "" && pageName != "")
      {
      // get a list of pages from XL file
      ExcelApp = new TExcelApplication(NULL);
      ExcelApp->Connect();
      ExcelApp->Workbooks->Open(WideString(fileName));
      ExcelWorksheetPtr worksheet = ExcelApp->Sheets->get_Item((Variant)xlsPageIndex+1);

      vector<string> fieldValues;
      getXLSRow(worksheet, 1, fieldNames);
      getXLSRow(worksheet, 2, fieldValues);
      addDBFields(this, fieldNames, fieldValues);

      worksheet->Release();
      }
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

