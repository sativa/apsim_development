//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTemporalDataForm.h"
#include "Data.h"
#include "TWebSession.h"
#include <boost\date_time\gregorian\gregorian.hpp>
using namespace boost::gregorian;
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
#pragma link "IWTMSEdit"
#pragma resource "*.dfm"

using namespace boost::gregorian;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TTemporalDataForm::TTemporalDataForm(TComponent* Owner)
        : TIWAppForm(Owner), firstAllowableDate(pos_infin), startDate(pos_infin)
   {
   startDate = date(day_clock::local_day().year(), 1, 1);

   for (int year = 2003; year <= 2010; year++)
      YearCombo->Items->Add(IntToStr(year));
      
   YearCombo->ItemIndex = startDate.year()-2003;
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TTemporalDataForm::setup(TWebSession* session, Data* d,
                              const std::string& userN,
                              const std::string& paddockN,
                              const std::string& desc,
                              const std::string& dataN,
                              bool fromGrowerMan)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   dataName = dataN;
   description = desc;
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
void TTemporalDataForm::fillGrid(void)
   {
   try
      {
      grid->ClearCells();
      for (int day = 1; day <= 31; day++)
         grid->Cells[0][day-1] = IntToStr(day);

      Data::TemporalValues values;
      data->getTemporalData(userName, paddockName, dataName,
                            startDate, date(startDate.year(), 12, 31),
                            values);
      for (Data::TemporalValues::iterator value = values.begin();
                                          value != values.end();
                                          value++)
         {
         int col = value->date.month();
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
void TTemporalDataForm::saveGrid(void)
   {
   if (webSession->isSaveAllowed())
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      try
         {
         Data::TemporalValues values;
         for (int col = 1; col <= 12; col++)
            {
            for (int row = 0; row < 31; row++)
               {
               if (grid->Cells[col][row] != "")
                  {
                  int day = row+1;
                  int month = col;
                  values.push_back(Data::TemporalData(date(startDate.year(), month, day),
                                                      grid->Cells[col][row].c_str()));
                  }
               }
            }
         data->addTemporalData(userName, paddockName, dataName,
                               startDate, date(startDate.year(), 12, 31), values);
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
void __fastcall TTemporalDataForm::SaveButtonClick(TObject *Sender)
   {
   saveGrid();
   }
//---------------------------------------------------------------------------
// Colour cells.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataForm::gridGetCellProp(TObject *Sender,
      int row, int col, AnsiString AValue, TIWColor &colour,
      TAlignment &AAlignment, TIWFont *Font)
   {
   if (col == 0)
      colour = clWebLIGHTBLUE;
   else
      {
      int day = row+1;
      int month = col;
      int lastDayOfMonth = gregorian_calendar::end_of_month_day(startDate.year(), month);
      if (day > lastDayOfMonth)
         colour = clWebLIGHTBLUE;
      }
   }
//---------------------------------------------------------------------------
// User has clicked back
//---------------------------------------------------------------------------
void __fastcall TTemporalDataForm::BackButtonClick(TObject *Sender)
   {
   saveGrid();
   webSession->showPaddockForm(userName, paddockName, true, fromGrowerManagement);
   }
//---------------------------------------------------------------------------
void __fastcall TTemporalDataForm::YearComboChange(TObject *Sender)
   {
   saveGrid();
   startDate = date(StrToInt(YearCombo->Text), 1, 1);
   fillGrid();
   }
//---------------------------------------------------------------------------

