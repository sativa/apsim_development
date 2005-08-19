//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <iterator>

#include <general\TreeNodeIterator.h>
#include <general\xml.h>
#include <general\path.h>
#include <general\io_functions.h>
#include <general\vcl_functions.h>

#include <ApsimShared\ApsimComponentData.h>
#include <ApsimShared\ApsimServiceData.h>
#include <ApsimShared\ApsimSystemData.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ControlFileConverter.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>

#include "TRunForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma link "HTMListB"
#pragma resource "*.dfm"
TRunForm *RunForm;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TRunForm::TRunForm(TComponent* Owner)
   : TForm(Owner)
 {
 }
//---------------------------------------------------------------------------
// Setup form
//---------------------------------------------------------------------------
void TRunForm::setup(ApsimRuns& apsimRuns, bool cons)
   {
   console = cons;
   runs = &apsimRuns;
   runs->getFilesNeedingConversion(filesNeedingConversion);
   if (filesNeedingConversion.size() > 0)
      {
      MainPanel->ActivePage = Page1;
      populatePage1();
      }
   else
      {
      MainPanel->ActivePage = Page3;
      populatePage3();
      }
   }

//---------------------------------------------------------------------------
// populate page 1.
//---------------------------------------------------------------------------
void TRunForm::populatePage1(void)
   {
   ostringstream out;
   copy(filesNeedingConversion.begin(),filesNeedingConversion.end(),
        ostream_iterator<string, char>(out,"\n"));
   ControlFileLabel->Caption = out.str().c_str();
   }
//---------------------------------------------------------------------------
// populate page 2.
//---------------------------------------------------------------------------
void TRunForm::populatePage2()
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   try
      {
      StatusList->Items->Clear();
      TStringList* lines = new TStringList;
      for (unsigned f = 0; f != filesNeedingConversion.size(); f++)
         {
         ControlFileConverter converter;
         converter.convert(filesNeedingConversion[f], (TControlFileConverterEvent)NULL);
         Path log(filesNeedingConversion[f]);
         log.Set_extension(".conversions");
         if (FileExists(log.Get_path().c_str()))
            {
            lines->LoadFromFile(log.Get_path().c_str());
            StatusList->Items->AddStrings(lines);
            }
         }
      delete lines;
      }
   catch (const runtime_error& error)
      {
      StatusList->Items->Add(error.what());
      NextButton->Visible = false;
      }

   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
// populate page 3.
//---------------------------------------------------------------------------
void TRunForm::populatePage3()
   {
   NextButton->Caption = "&Run APSIM";
   fillSimulationList();
   }
//---------------------------------------------------------------------------
// Fill the simulation list.
//---------------------------------------------------------------------------
void TRunForm::fillSimulationList()
   {
   SimulationList->Items->BeginUpdate();

   vector<string> fileNames, simNames;
   runs->getSimulations(fileNames, simNames);
   bool someWereSelected = false;
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      // create list box item.
      TListItem* simNameItem = SimulationList->Items->Add();
      simNameItem->ImageIndex = 1;
      simNameItem->Caption = simNames[f].c_str();

      simNameItem->SubItems->Add(fileNames[f].c_str());

      // select list box item if previously selected by user.
      simNameItem->Selected = previousRuns.wasPreviouslyRun(fileNames[f], simNames[f]);
      someWereSelected = (someWereSelected || simNameItem->Selected);
      }
   if (!someWereSelected)
      for (int i = 0; i != SimulationList->Items->Count; i++)
         SimulationList->Items->Item[0]->Selected = true;

   SimulationList->Items->EndUpdate();
   checkOkButtonState(NULL);
   }
//---------------------------------------------------------------------------
// Next button has been clicked.
//---------------------------------------------------------------------------
void __fastcall TRunForm::NextButtonClick(TObject *Sender)
   {
   MainPanel->ActivePageIndex = MainPanel->ActivePageIndex + 1;
   if (MainPanel->ActivePage == Page2)
      populatePage2();
   else if (MainPanel->ActivePage == Page3)
      populatePage3();
   else if (MainPanel->ActivePage == Page4)
      {
      NextButton->Visible = false;
      CancelButton->Visible = false;
      MainPanel->Visible = false;
      ApsimRuns selectedRuns = saveSelections();
      selectedRuns.runApsim(false, console, OnRunNotifyEvent);
      Close();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::CancelButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------
// Check the ok button state.
//---------------------------------------------------------------------------
void __fastcall TRunForm::checkOkButtonState(TObject *Sender)
   {
   bool somethingSelected = false;
   for (int n = 0; n != SimulationList->Items->Count && !somethingSelected; n++)
      somethingSelected = SimulationList->Items->Item[n]->Selected;

   NextButton->Enabled = somethingSelected;
   }
//---------------------------------------------------------------------------
// return all selected simulations
//---------------------------------------------------------------------------
ApsimRuns TRunForm::saveSelections(void)
   {
   ApsimRuns selectedRuns;
   for (int i = 0; i != SimulationList->Items->Count; i++)
      {
      TListItem* simNameItem = SimulationList->Items->Item[i];
      if (simNameItem->Selected)
         {
         string simName = simNameItem->Caption.c_str();
         string fileName = simNameItem->SubItems->Strings[0].c_str();
         selectedRuns.addSimulation(fileName, simName);
         }
      }
   previousRuns.saveSelectedRunNames(selectedRuns);
   return selectedRuns;
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::simulationListClick(TObject *Sender)
   {
   checkOkButtonState(NULL);
   }
//---------------------------------------------------------------------------
// APSIM is currently running - update screen.
//---------------------------------------------------------------------------
void __fastcall TRunForm::OnRunNotifyEvent(const std::string& simFileName)
   {
   FileNameLabel->Caption = simFileName.c_str();
   }

void __fastcall TRunForm::MinimiseButtonClick(TObject *Sender)
   {
   this->WindowState = wsMinimized;
   }
//---------------------------------------------------------------------------

