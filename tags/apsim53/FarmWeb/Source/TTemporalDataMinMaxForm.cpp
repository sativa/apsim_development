//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTemporalDataMinMaxForm.h"
#include "Data.h"
#include "TWebSession.h"
#include <general\string_functions.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWOutlookBar"
#pragma link "IWCompEdit"
#pragma link "IWCompListbox"
#pragma link "IWTMSCal"
#pragma link "IWCompButton"
#pragma link "IWAdvWebGrid"
#pragma link "IWWebGrid"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompRectangle"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"

using namespace boost::gregorian;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TTemporalDataMinMaxForm::TTemporalDataMinMaxForm(TComponent* Owner)
        : TIWAppForm(Owner), sowDate(pos_infin), startDate(pos_infin)
   {
   startDate = date(day_clock::local_day().year(), 1, 1);

   for (int year = 2003; year <= 2010; year++)
      {
      YearCombo->Items->Add(IntToStr(year) + " (Jan-June)");
      YearCombo->Items->Add(IntToStr(year) + " (July-December)");
      }
   YearCombo->ItemIndex = (startDate.year()-2003) * 2;
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TTemporalDataMinMaxForm::setup(TWebSession* session, Data* d,
                                    const std::string& userN,
                                    const std::string& paddockN,
                                    const std::string& desc,
                                    const std::string& dataNMin,
                                    const std::string& dataNMax,
                                    bool fromGrowerMan)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   description = desc;
   dataNameMin = dataNMin;
   dataNameMax = dataNMax;
   fromGrowerManagement = fromGrowerMan;

   ostringstream prompt;
   prompt << "Please enter " << description << " into the chart below for "
          << " Grower: " << userName << " and Paddock " << paddockName;
   PromptLabel->Caption = prompt.str().c_str();

   SaveButton->Enabled = (webSession->isSaveAllowed());

   fillGrid();
   }
//---------------------------------------------------------------------------
// Fill the grid
//---------------------------------------------------------------------------
void TTemporalDataMinMaxForm::fillGrid(void)
   {
   try
      {
      grid->ClearCells();
      date endDate(pos_infin);
      if (startDate.month() == 1)
         endDate = date(startDate.year(), 6, 30);
      else
         endDate = date(startDate.year(), 12, 31);

      // add in a row heading.
      for (int day = 1; day <= 31; day++)
         grid->Cells[0][day-1] = IntToStr(day);

      // put month labels on every 2nd column title
      for (int col = 1; col <= 12; col += 2)
         {
         int month = startDate.month() + (col-1) / 2;
         grid->Columns->Items[col]->Title = greg_month(month).as_short_string();
         }

      // put minimum values on grid.
      Data::TemporalValues minValues;
      data->getTemporalData(userName, paddockName, dataNameMin,
                            startDate, endDate,
                            minValues);
      for (Data::TemporalValues::iterator value = minValues.begin();
                                          value != minValues.end();
                                          value++)
         {
         int col = value->date.month()*2 - 1;
         if (col > 11)
            col = col - 12;
         int row = value->date.day()-1;
         grid->Cells[col][row] = value->value.c_str();
         }

      // put maximum values on grid.
      Data::TemporalValues maxValues;
      data->getTemporalData(userName, paddockName, dataNameMax,
                            startDate, endDate,
                            maxValues);
      for (Data::TemporalValues::iterator value = maxValues.begin();
                                          value != maxValues.end();
                                          value++)
         {
         int col = value->date.month()*2;
         if (col > 11)
            col = col - 12;
         int row = value->date.day()-1;
         grid->Cells[col][row] = value->value.c_str();
         }
      }
   catch (const exception& err)
      {
      webSession->showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Save the grid
//---------------------------------------------------------------------------
void TTemporalDataMinMaxForm::saveGrid(void)
   {
   if (webSession->isSaveAllowed())
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      try
         {
         date endDate(pos_infin);
         if (startDate.month() == 1)
            endDate = date(startDate.year(), 6, 30);
         else
            endDate = date(startDate.year(), 12, 31);

         // save minimum values.
         Data::TemporalValues minValues;
         for (int col = 1; col <= 12; col += 2)
            {
            for (int row = 0; row < 31; row++)
               {
               if (grid->Cells[col][row] != "")
                  {
                  int day = row+1;
                  int month = startDate.month() + (col-1)/2;
                  date cellDate(startDate.year(), month, day);
                  minValues.push_back(Data::TemporalData(cellDate,
                                                         grid->Cells[col][row].c_str(), ""));
                  }
               }
            }
         data->addTemporalData(userName, paddockName, dataNameMin,
                               startDate, endDate, minValues);

         // save maximum values.
         Data::TemporalValues maxValues;
         for (int col = 2; col <= 12; col += 2)
            {
            for (int row = 0; row < 31; row++)
               {
               if (grid->Cells[col][row] != "")
                  {
                  int day = row+1;
                  int month = startDate.month() + (col-1)/2;
                  date cellDate(startDate.year(), month, day);
                  maxValues.push_back(Data::TemporalData(cellDate,
                                                         grid->Cells[col][row].c_str(), ""));
                  }
               }
            }
         data->addTemporalData(userName, paddockName, dataNameMax,
                               startDate, endDate, maxValues);

         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      Screen->Cursor = savedCursor;
      }
   }
//---------------------------------------------------------------------------
// User has clicked save - save grid.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::SaveButtonClick(TObject *Sender)
   {
   saveGrid();
   }
//---------------------------------------------------------------------------
// User has changed half years - save grid.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::gridGetCellProp(TObject *Sender,
      int row, int col, AnsiString AValue, TIWColor &colour,
      TAlignment &AAlignment, TIWFont *Font)
   {
   if (col == 0)
      colour = clWebLIGHTBLUE;
   else
      {
      int day = row+1;
      int month = startDate.month() + (col-1) / 2;
      int lastDayOfMonth = gregorian_calendar::end_of_month_day(startDate.year(), month);
      if (day > lastDayOfMonth)
         colour = clWebLIGHTBLUE;
      }
   }
//---------------------------------------------------------------------------
// User has clicked on next button.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::YearComboChange(TObject *Sender)
   {
   saveGrid();
   string comboText = YearCombo->Text.c_str();
   string MonthRangeText = splitOffBracketedValue(comboText, '(', ')');
   stripLeadingTrailing(comboText, " ");
   int year = StrToInt(comboText.c_str());
   if (MonthRangeText == "Jan-June")
      startDate = date(year, 1, 1);
   else
      startDate = date(year, 7, 1);
   fillGrid();
   }
//---------------------------------------------------------------------------
// User has clicked on back button.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::BackButtonClick(TObject *Sender)
   {
   saveGrid();
   webSession->showPaddockForm(userName, paddockName, true, fromGrowerManagement);
   }
//---------------------------------------------------------------------------

