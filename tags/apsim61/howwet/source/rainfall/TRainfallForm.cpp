//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRainfallForm.h"
#include <ApsimShared\ApsimDataFile.h>
#include <ApsimShared\ApsimDataFileWriter.h>
#include <boost\lexical_cast.hpp>
#include <general\date_functions.h>
#include <general\string_functions.h>
using namespace boost::gregorian;
using namespace boost;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Planner"
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "AdvFileNameEdit"
#pragma resource "*.dfm"
TRainfallForm *RainfallForm;
//---------------------------------------------------------------------------
__fastcall TRainfallForm::TRainfallForm(TComponent* Owner)
   : TForm(Owner)
   {
   dirty = false;
   clearingCells = false;
   }
//---------------------------------------------------------------------------
// setup the form.
//---------------------------------------------------------------------------
void TRainfallForm::setup(const std::string& fileName)
   {
   clear();
   open(fileName);
   }
//---------------------------------------------------------------------------
// User has close the form - save if necessary.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   saveIfNecessary();
   }
//---------------------------------------------------------------------------
// User has changed something in the grid - signal data is dirty.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::gridCellsChanged(TObject *Sender, TRect &R)
   {
   if (!clearingCells)
      dirty = true;
   }
//---------------------------------------------------------------------------
// Clear the grid back to default values.
//---------------------------------------------------------------------------
void TRainfallForm::clear(void)
   {
   clearingCells = true;
   rainfall.erase(rainfall.begin(), rainfall.end());
   grid->ClearRect(1, 1, grid->ColCount-1, grid->RowCount-1);
   clearingCells = false;
   fileName = "";
   Caption = "Rainfall";
   dirty = false;
   }
//---------------------------------------------------------------------------
// open a met file
//---------------------------------------------------------------------------
void TRainfallForm::open(const std::string& file)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = savedCursor;
   ApsimDataFile metFile;
   try
      {
      metFile.open(file);
      date startDate(metFile.getDate());
      metFile.last();
      date endDate(metFile.getDate());

      // find rainfall column.
      ApsimDataFile::iterator rainI = find(metFile.fieldsBegin(),
                                           metFile.fieldsEnd(),
                                           "rain");
      if (rainI == metFile.fieldsEnd())
         throw runtime_error("Cannot find a rainfall column in file " + file);

      clear();                                    

      // fill grid with rainfall values.
      metFile.first();
      date currentDate(startDate);
      while (currentDate <= endDate)
         {
         string rainString = rainI->values[0];
         float rain = lexical_cast<float>(rainString);
         if (rain != 0.0)
            rainfall.insert(make_pair(currentDate, rain));
         metFile.next();
         currentDate = currentDate + date_duration(1);
         }
      populateGrid(startDate.year());
      dirty = false;
      fileName = file;
      Caption = string("Rainfall - " + fileName).c_str();
      YearUpDown->Position = startDate.year();
      }
   catch (const exception& err)
      {
      MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   metFile.close();
   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
// populate rainfall grid.
//---------------------------------------------------------------------------
void TRainfallForm::populateGrid(int year)
   {
   grid->ClearNormalCells();
   gridYear = year;

   Rainfall::iterator rainfallI = rainfall.begin();
   while (rainfallI != rainfall.end() && rainfallI->first.year() < gridYear)
      rainfallI++;

   while (rainfallI != rainfall.end() && rainfallI->first.year() == gridYear)
      {
      date currentDate = rainfallI->first;
      float rain = rainfallI->second;
      int col = rainfallI->first.month();
      int row = rainfallI->first.day();
      grid->Floats[col][row] = rain;
      rainfallI++;
      }

   // Put monthly sums of rainfall in footer.
   float sum = 0.0;
   for (int col = 1; col != grid->ColCount; col++)
      {
      grid->Floats[col][grid->RowCount - 1] = grid->ColumnSum(col,1,grid->RowCount - 2);
      sum += grid->ColumnSum(col,1,grid->RowCount - 2);
      }
   TotalRainLabel->Caption = "Total rain: " + AnsiString(ftoa(sum, 1).c_str()) + "mm";
   dirty = false;
   }
//---------------------------------------------------------------------------
// save rainfall grid.
//---------------------------------------------------------------------------
void TRainfallForm::saveGrid(void)
   {
   if (dirty)
      {
      date startDate = date(gridYear, 1, 1);
      date endDate = date(gridYear, 12, 31);
      date currentDate = startDate;
      for (unsigned col = 1; col <= 12; col++)
         for (unsigned row = 1; row <= 31 && !isCellFixed(col, row); row++)
            {
            rainfall.erase(currentDate);
            if (grid->Cells[col][row] != "")
               rainfall.insert(make_pair(currentDate, grid->Floats[col][row]));
            currentDate = currentDate + date_duration(1);
            }
      }
   }
//---------------------------------------------------------------------------
// Save the data if it is dirty.
//---------------------------------------------------------------------------
void TRainfallForm::saveIfNecessary(void)
   {
   if (dirty)
      {
      if (MessageBox(NULL, "Do you want to save changes?", "Question", MB_ICONQUESTION | MB_YESNO)
          == IDYES)
         SaveButtonClick(NULL);
      }
   }
//---------------------------------------------------------------------------
// save all data in grid back to file.
//---------------------------------------------------------------------------
void TRainfallForm::save(void)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   try
      {
      saveGrid();

      ApsimDataFileWriter writer;
      writer.open(fileName, "weather.met.weather");

      date startDate = rainfall.begin()->first;
      startDate = date(startDate.year(), startDate.month(), 1);
      date endDate = (--rainfall.end())->first;
      endDate = date(endDate.year(), endDate.month(),
                     gregorian_calendar::end_of_month_day(endDate.year(), endDate.month()));
      date previousDate = startDate;

      for (Rainfall::iterator rainfallI = rainfall.begin();
                              rainfallI != rainfall.end();
                              rainfallI++)
         {
         date currentDate = rainfallI->first;
         float rain = rainfallI->second;
         for (date d = previousDate; d != currentDate; d = d + date_duration(1))
            {
            ApsimDataFileWriter::Values values;
            values.push_back(Value("rain", "(mm)", "0.0", ""));
            writer.addTemporalRecord(d, values);
            }

         ApsimDataFileWriter::Values values;
         values.push_back(Value("rain", "(mm)", lexical_cast<string>(rain), ""));
         writer.addTemporalRecord(currentDate, values);
         previousDate = currentDate + date_duration(1);
         }
      Caption = string("Rainfall - " + fileName).c_str();
      writer.close();
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
// User has clicked new.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::NewButtonClick(TObject *Sender)
   {
   saveIfNecessary();
   clear();
   }
//---------------------------------------------------------------------------
// User has clicked open
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::OpenButtonClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      saveIfNecessary();
      open(OpenDialog->FileName.c_str());
      }
   }
//---------------------------------------------------------------------------
// user has clicked save.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::SaveButtonClick(TObject *Sender)
   {
   if (fileName == "")
      {
      if (SaveDialog->Execute())
         fileName = SaveDialog->FileName.c_str();
      else
         return;
      }
   save();
   }
//---------------------------------------------------------------------------
// user has clicked on exit.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::ExitButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------
// return true if cell is fixed.
//---------------------------------------------------------------------------
bool TRainfallForm::isCellFixed(int ACol, int ARow)
   {
   unsigned year = YearUpDown->Position;
   unsigned month = ACol;
   return (ARow > gregorian_calendar::end_of_month_day(year, month));
   }
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::gridIsFixedCell(TObject *Sender, int ARow,
      int ACol, bool &IsFixed)
   {
   IsFixed = isCellFixed(ACol, ARow);
   }
//---------------------------------------------------------------------------
// User has changed the year combo box - save grid contents.
//---------------------------------------------------------------------------
void __fastcall TRainfallForm::YearUpDownChangingEx(TObject *Sender,
      bool &AllowChange, short NewValue, TUpDownDirection Direction)
   {
   saveGrid();
   populateGrid(NewValue);
   }
//---------------------------------------------------------------------------

