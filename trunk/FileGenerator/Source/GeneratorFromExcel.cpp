//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "GeneratorFromExcel.h"
#include <general\macroSubstFile.h>
#include <general\excel.h>
#include <general\string_functions.h>

#pragma package(smart_init)
#pragma link "Excel_2K_SRVR"
using namespace std;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
GeneratorFromExcel::GeneratorFromExcel(void)
   {

   }
//---------------------------------------------------------------------------
// Parse the spreadsheet
//---------------------------------------------------------------------------
bool parseSpreadsheetTable(MacroSubstFile& macroFile,
                           ExcelWorksheetPtr worksheet,
                           unsigned& row)
   {
   // try and locate macroName.
   vector<string> macroNames;
   while (getXLSRow(worksheet, row, macroNames) && macroNames.size() > 0
          && macroNames[0] == "")
      row++;

   if (macroNames.size() > 0 && macroNames[0] != "")
      {
      string macroName = macroNames[0];
      macroFile.addMacro(macroName);
      row++;
      vector<string> fieldNames;
      getXLSRow(worksheet, row, fieldNames);

      row++;
      vector<string> fieldValues;
      while (getXLSRow(worksheet, row, fieldValues)
             && fieldValues.size() > 0
             && fieldValues[0] != "")
         {
         MacroSubstFile::ValueAttributes attributes;
         for (unsigned col = 0; col != fieldValues.size(); col++)
            {
            if (fieldNames[col] != "")
               attributes.push_back(make_pair(fieldNames[col], fieldValues[col]));
            }
         macroFile.addValue(macroName, attributes);
         row++;
         }
      return true;
      }
   else
      return false;
   }
//---------------------------------------------------------------------------
// Go generate all files given the specified macro spreadsheet file.
//---------------------------------------------------------------------------
void GeneratorFromExcel::go(const std::string& xlsFileName,
                            const std::string& xlsSheetName,
                            const std::string& scriptFileName)
   {
   // change the working directory to the dir of the xls file.
   SetCurrentDir(ExtractFileDir(xlsFileName.c_str()));

   CoInitialize(NULL);

   TExcelApplication* excelApp;
   ExcelWorksheetPtr worksheet;
   ExcelWorksheetPtr generatorScriptSheet;
   try
      {
      excelApp = new TExcelApplication(NULL);
      excelApp->ConnectKind = ckNewInstance;
      excelApp->Connect();
      excelApp->Workbooks->Open(WideString(xlsFileName.c_str()));

      // Locate the generator script sheet and the worksheet.
      if (!getXLSSheet(xlsSheetName, excelApp, worksheet))
         throw runtime_error("Cannot find sheet: " + xlsSheetName);

      if (!getXLSSheet("GeneratorScript", excelApp, generatorScriptSheet))
         {
         worksheet->Release();
         throw runtime_error("Cannot find sheet: GeneratorScript");
         }
      }
   catch (const runtime_error& err)
      {
      excelApp->ActiveWorkbook->Close((Variant)false);
      excelApp->Quit();
      excelApp->Disconnect();
      delete excelApp;
      throw;
      }

   // setup the macroFile object
   MacroSubstFile macroFile;
   ifstream in(scriptFileName.c_str());
   ostringstream contents;
   contents << in.rdbuf();
   macroFile.setContents(contents.str());

//   Excel_2k::ShapesPtr shapes = generatorScriptSheet->get_Shapes();
//   Excel_2k::TextBox* shape = (Excel_2k::TextBox*) shapes->Item((Variant) 1);
//   AnsiString name = shape->Name;
//   Excel_2k::TextBoxPtr textbox(shape);
//   Excel_2k::TextFramePtr textFrame = shape->get_TextFrame();
//   Excel_2k::CharactersPtr characters = textFrame->Characters((Variant)1, (Variant)10);
//   wchar_t* text = shape->Text;


   unsigned row = 2;
   while (parseSpreadsheetTable(macroFile, worksheet, row));

   // shut down EXCEL.
   worksheet->Release();
   generatorScriptSheet->Release();
   excelApp->ActiveWorkbook->Close((Variant)false);
   excelApp->Quit();
   excelApp->Disconnect();
   delete excelApp;

   // generate all files.
   vector<string> filesGenerated;
   macroFile.generateFiles(filesGenerated);

   // display a message box showing which files have been generated.
   string msg;
   Build_string(filesGenerated, ", ", msg);
   msg = "Files generated: " + msg;
   ::MessageBox(NULL, msg.c_str(), "Information", MB_ICONINFORMATION | MB_OK);

   CoUninitialize();
   }


