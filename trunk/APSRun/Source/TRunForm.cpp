//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRunForm.h"
#include <ApsimShared\ControlFileConverter.h>
#include <general\path.h>
#include <general\io_functions.h>
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include <iterator>
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
// Form has been shown - set everything up.
//---------------------------------------------------------------------------
void __fastcall TRunForm::FormShow(TObject *Sender)
   {
   runs->getFilesNeedingConversion(filesNeedingConversion);

   if (filesNeedingConversion.size() > 0)
      {
      PageControl1->ActivePage = Page1;
      populatePage1();
      }
   else
      {
      PageControl1->ActivePage = Page3;
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
void TRunForm::fillSimulationList(void)
   {
   vector<string> fileNames;
   runs->getFilesToRun(fileNames);
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      TTreeNode* parentNode = simulationList->Items->Add(NULL, fileNames[f].c_str());
      parentNode->ImageIndex = 0;
      parentNode->SelectedIndex = 0;

      bool someSelected = false;
      vector<string> previousSimulations;
      previousRuns.getPreviousRun(fileNames[f], previousSimulations);

      vector<string> names;
      ApsimControlFile con(fileNames[f]);
      con.getAllSectionNames(names);
      for (unsigned n = 0; n != names.size(); n++)
         {
         TTreeNode* node = simulationList->Items->AddChild(parentNode, names[n].c_str());
         node->ImageIndex = 1;
         node->SelectedIndex = 1;
         if (find(previousSimulations.begin(), previousSimulations.end(),
                  names[n]) != previousSimulations.end())
            {
            simulationList->Select(node, TShiftState() << ssCtrl);
            someSelected = true;
            }
         }
      if (!someSelected)
         {
         TTreeNode* node = parentNode->getFirstChild();
         while (node != NULL)
            {
            simulationList->Select(node, TShiftState() << ssCtrl);
            node = node->getNextSibling();
            }
         }
      }
   checkOkButtonState(NULL);
   }
//---------------------------------------------------------------------------
// Next button has been clicked.
//---------------------------------------------------------------------------
void __fastcall TRunForm::NextButtonClick(TObject *Sender)
   {
   PageControl1->ActivePageIndex = PageControl1->ActivePageIndex + 1;
   if (PageControl1->ActivePage == Page2)
      populatePage2();
   else if (PageControl1->ActivePage == Page3)
      populatePage3();
   else if (PageControl1->ActivePage == Page4)
      {
      NextButton->Visible = false;
      CancelButton->Visible = false;
      PageControl1->Visible = false;
      saveSelections();
      runs->runApsim(false);
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
   for (int n = 0; n != simulationList->Items->Count && !somethingSelected; n++)
      somethingSelected = simulationList->Items->Item[n]->Selected;

   NextButton->Enabled = somethingSelected;
   }
//---------------------------------------------------------------------------
// save all selected simulations back to run and previous run objects.
//---------------------------------------------------------------------------
void TRunForm::saveSelections(void)
   {
   TTreeNode* parentNode = simulationList->Items->Item[0];
   while (parentNode != NULL)
      {
      string controlFileName = parentNode->Text.c_str();
      vector<string> simulations;
      TTreeNode* childNode = parentNode->getFirstChild();
      while (childNode != NULL)
         {
         if (childNode->Selected)
            simulations.push_back(childNode->Text.c_str());
         childNode = childNode->getNextSibling();
         }
      runs->setSimulationsToRun(controlFileName, simulations);
      previousRuns.setCurrentRun(controlFileName, simulations);

      parentNode = parentNode->getNextSibling();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::simulationListClick(TObject *Sender)
   {
   checkOkButtonState(NULL);
   }
//---------------------------------------------------------------------------

