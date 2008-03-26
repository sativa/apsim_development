//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDataForm.h"
#include "TWebSession.h"
#include <general\string_functions.h>
#include <general\date_functions.h>
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
__fastcall TDataForm::TDataForm(TComponent* Owner)
        : TIWAppForm(Owner), sowDate(pos_infin), startDate(pos_infin)
   {
   startDate = date(day_clock::local_day().year(), 1, 1);
   for (int year = 2003; year <= 2010; year++)
      YearCombo->Items->Add(IntToStr(year));

   YearCombo->ItemIndex = startDate.year()-2003;
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TDataForm::setup(TWebSession* session, Data* d,
                                    const std::string& userN,
                                    const std::string& paddockN,
                                    bool fromGrowerMan)
   {                   
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   fromGrowerManagement = fromGrowerMan;

   ostringstream prompt;
   prompt << " Grower: " << userName << " and Paddock: " << paddockName;
   PromptLabel->Caption = prompt.str().c_str();

   SaveButton->Enabled = (webSession->isSaveAllowed());

   fillGrid();
   }
//---------------------------------------------------------------------------
// Fill the grid
//---------------------------------------------------------------------------
void TDataForm::fillGrid(void)
   {
   try
      {
      grid->ClearCells();

      // "patch_mint" "patch_maxt"  "min_soilt"  "max_soilt"  "patch_rain"  "patch_irr"  "obs"

      // get all temporal data and put onto grid.
      Data::TemporalValues allData;
      data->getTemporalData(userName, paddockName,
                            startDate, date(startDate.year(), 12, 31), allData);

      int row = -1;
      string previousDate;
      for (Data::TemporalValues::iterator value = allData.begin();
                                          value != allData.end();
                                          value++)
         {
         string date = to_dmy(value->date);
         if (date != previousDate)
            {
            row++;
            grid->Cells[0][row] = date.c_str();
            previousDate = date;
            }
         if (Str_i_Eq(value->type, "patch_mint"))
            grid->Cells[1][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "patch_maxt"))
            grid->Cells[2][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "min_soilt"))
            grid->Cells[3][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "max_soilt"))
            grid->Cells[4][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "patch_rain"))
            grid->Cells[5][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "patch_irr"))
            grid->Cells[6][row] = value->value.c_str();
         else if (Str_i_Eq(value->type, "obs"))
            grid->Cells[7][row] = value->value.c_str();
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
void TDataForm::saveGrid(void)
   {
   if (webSession->isSaveAllowed())
      {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      try
         {
         // save air temp min.
         Data::TemporalValues values;

         int row = 0;
         while (grid->Cells[0][row] != "")
            {
            for (int col = 1; col != 8; col++)
               {
               if (grid->Cells[col][row] != "")
                  {
                  string dataType;
                  switch (col)
                     {
                     case 1: dataType = "patch_mint"; break;
                     case 2: dataType = "patch_maxt"; break;
                     case 3: dataType = "min_soilt"; break;
                     case 4: dataType = "max_soilt"; break;
                     case 5: dataType = "patch_rain"; break;
                     case 6: dataType = "patch_irr"; break;
                     case 7: dataType = "obs"; break;
                     }
                  date cellDate = fromDmyString(grid->Cells[0][row].c_str());
                  values.push_back(Data::TemporalData(cellDate,
                                                      grid->Cells[col][row].c_str(),
                                                      dataType));
                  }
               }
            row++;
            }
         data->addTemporalData(userName, paddockName,
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
void __fastcall TDataForm::SaveButtonClick(TObject *Sender)
   {
   saveGrid();
   }
//---------------------------------------------------------------------------
// User has clicked on year combo
//---------------------------------------------------------------------------
void __fastcall TDataForm::YearComboChange(TObject *Sender)
   {
   saveGrid();
   startDate = date(StrToInt(YearCombo->Text), 1, 1);
   fillGrid();
   }
//---------------------------------------------------------------------------
// User has clicked on back button.
//---------------------------------------------------------------------------
void __fastcall TDataForm::BackButtonClick(TObject *Sender)
   {
   saveGrid();
   webSession->showPaddockForm(userName, paddockName, true, fromGrowerManagement);
   }
//---------------------------------------------------------------------------

