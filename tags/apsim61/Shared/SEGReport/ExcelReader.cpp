//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ExcelReader.h"
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include "../general/excel.h"

using namespace std;


TExcelApplication* ExcelApp;
ExcelWorksheetPtr worksheet;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void ExcelReader::createFields(TDataSet* source, TDataSet* result)
   {
   string fileName = getProperty("filename");
   string pageName = getProperty("pagename");
   if (fileName != "" && pageName != "")
      {
      try
         {
         // get a list of pages from XL file
         ExcelApp = new TExcelApplication(NULL);
         ExcelApp->Connect();
         ExcelApp->Workbooks->Open(WideString(fileName.c_str()));
         worksheet = ExcelApp->Sheets->get_Item((Variant)1);

         getXLSRow(worksheet, 1, fieldNames);
         getXLSRow(worksheet, 2, fieldValues);

         addDBFields(result, fieldNames, fieldValues);
         }
      catch (const runtime_error& err)
         {
         worksheet->Release();
         ExcelApp->ActiveWorkbook->Close((Variant)false);
         ExcelApp->Quit();
         ExcelApp->Disconnect();
         delete ExcelApp;
         throw;
         }
      }
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void ExcelReader::process(TDataSet* source, TDataSet* result)
   {
   string fileName = getProperty("filename");
   string pageName = getProperty("pagename");
   if (fileName != "" && pageName != "")
      {
      try
         {
         vector<string> values;
         int row = 2;
         while (getXLSRow(worksheet, row, values))
            {
            appendDBRecord(result, fieldNames, values);
            row++;
            }
         }
      catch (const runtime_error& err)
         {
         worksheet->Release();
         ExcelApp->ActiveWorkbook->Close((Variant)false);
         ExcelApp->Quit();
         ExcelApp->Disconnect();
         delete ExcelApp;
         throw;
         }
      worksheet->Release();
      ExcelApp->ActiveWorkbook->Close((Variant)false);
      ExcelApp->Quit();
      ExcelApp->Disconnect();
      delete ExcelApp;
      Sleep(1000);
      }
   }

