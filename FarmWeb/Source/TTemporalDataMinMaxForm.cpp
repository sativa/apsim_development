//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTemporalDataMinMaxForm.h"
#include "Data.h"
#include "TWebSession.h"
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
#pragma resource "*.dfm"

using namespace boost::gregorian;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TTemporalDataMinMaxForm::TTemporalDataMinMaxForm(TComponent* Owner)
        : TIWAppForm(Owner), sowDate(pos_infin), startDate(pos_infin)
   {
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
   sowDate = from_string(data->getProperty(userName, paddockName, "sowdate"));

   if (sowDate.month() <= 6)
      startDate = date(sowDate.year(), 1, 1);
   else
      startDate = date(sowDate.year(), 7, 1);

   fillGrid();
   }
//---------------------------------------------------------------------------
// Fill the grid
//---------------------------------------------------------------------------
void TTemporalDataMinMaxForm::fillGrid(void)
   {
   try
      {
      ostringstream prompt;
      prompt << "Please enter " << description << " into the chart below for "
             << startDate.year() << " (Grower: " << userName << ", Paddock: " << paddockName << ")";
      PromptLabel->Caption = prompt.str().c_str();

      grid->ClearCells();
      date endDate(pos_infin);
      if (startDate.month() == 1)
         endDate = date(startDate.year(), 6, 30);
      else
         endDate = date(startDate.year(), 12, 31);

      // put month labels on every 2nd column title
      for (int col = 0; col < 12; col += 2)
         {
         int month = startDate.month() + col/2;
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
         int col = (value->date.month()-1)*2;
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
         int col = (value->date.month()-1)*2+1;
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
      for (int col = 0; col < 12; col += 2)
         {
         for (int row = 0; row < 31; row++)
            {
            if (grid->Cells[col][row] != "")
               {
               int day = row+1;
               int month = startDate.month() + col/2;
               date cellDate(startDate.year(), month, day);
               minValues.push_back(Data::TemporalData(cellDate,
                                                      grid->Cells[col][row].c_str()));
               }
            }
         }
      data->addTemporalData(userName, paddockName, dataNameMin,
                            startDate, endDate, minValues);

      // save maximum values.
      Data::TemporalValues maxValues;
      for (int col = 1; col < 12; col += 2)
         {
         for (int row = 0; row < 31; row++)
            {
            if (grid->Cells[col][row] != "")
               {
               int day = row+1;
               int month = startDate.month() + col/2;
               date cellDate(startDate.year(), month, day);
               maxValues.push_back(Data::TemporalData(cellDate,
                                                      grid->Cells[col][row].c_str()));
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
   if (startDate.year() < sowDate.year())
      colour = clWebLIGHTBLUE;
   else if (startDate.year() == sowDate.year())
      {
      int sowCol = (sowDate.month()-1)*2;
      int sowRow = sowDate.day()-1;
      if (startDate.year() > 6)
         sowCol = sowCol - 12;
      if (col < sowCol ||
          ( (col == sowCol || col == sowCol+1) && row < sowRow))
         colour = clWebLIGHTBLUE;
      }
   }
//---------------------------------------------------------------------------
// User has clicked on next button.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::NextButtonClick(TObject *Sender)
   {
   saveGrid();
   if (startDate.month() <= 6)
      startDate = date(startDate.year(), 7, 1);
   else
      startDate = date(startDate.year()+1, 1, 1);
   fillGrid();
   }
//---------------------------------------------------------------------------
// User has clicked on previous button.
//---------------------------------------------------------------------------
void __fastcall TTemporalDataMinMaxForm::PreviousButtonClick(
      TObject *Sender)
   {
   saveGrid();
   if (startDate.month() <= 6)
      startDate = date(startDate.year()-1, 7, 1);
   else
      startDate = date(startDate.year(), 1, 1);
   fillGrid();
   }
//---------------------------------------------------------------------------

void __fastcall TTemporalDataMinMaxForm::PaddockButtonClick(
      TObject *Sender)
   {
   saveGrid();
   webSession->showPaddockForm(userName, paddockName, true, fromGrowerManagement);
   }
//---------------------------------------------------------------------------

