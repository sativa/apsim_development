//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRunForm.h"
#include <ApsimShared\ControlFileConverter.h>
#include <ApsimShared\ApsimSettings.h>
#include <general\path.h>
#include <general\io_functions.h>
#include <general\vcl_functions.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma link "HTMListB"
#pragma resource "*.dfm"
TRunForm *RunForm;
//---------------------------------------------------------------------------
__fastcall TRunForm::TRunForm(TComponent* Owner)
   : TForm(Owner)
 {
 }
//---------------------------------------------------------------------------
void __fastcall TRunForm::FormShow(TObject *Sender)
   {
   if (!ControlFileConverter::needsConversion(controlFileName))
      {
      PageControl1->ActivePageIndex = 1;
      NextButtonClick(Sender);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      // write all selected simulations to .ini file
      string configurationName = configurationList->Items->
                           Strings[configurationList->ItemIndex].c_str();
      vector<string> simulations;
      for (int i = 0; i < simulationList->Items->Count; i++)
         if (simulationList->Selected[i])
            simulations.push_back(simulationList->Items->Strings[i].c_str());

      previousRuns.setCurrentRun(controlFileName, configurationName, simulations);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::NextButtonClick(TObject *Sender)
   {
   PageControl1->ActivePageIndex = PageControl1->ActivePageIndex + 1;
   if (PageControl1->ActivePageIndex == 2)
      {
      NextButton->Caption = "&Run APSIM";
      NextButton->ModalResult = mrOk;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::CancelButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------
// Do the control file conversion.
//---------------------------------------------------------------------------
void __fastcall TRunForm::Page2Show(TObject *Sender)
   {
   savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   try
      {
      ControlFileConverter converter;
      converter.convert(controlFileName);
      Path log(controlFileName);
      log.Set_extension(".conversions");
      StatusList->Items->LoadFromFile(log.Get_path().c_str());
      }
   catch (const runtime_error& error)
      {
      StatusList->Items->Add(error.what());
      NextButton->Visible = false;
      }

   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::Page3Show(TObject *Sender)
   {
   fillConfigurationList();
   fillSimulationList();
   setupForm();
   }
//---------------------------------------------------------------------------
void TRunForm::fillConfigurationList(void)
   {
   vector<string> configurationFiles;
   getDirectoryListing(ApsimSettings::getSettingsFolder(), "*.config", configurationFiles);

   // only put the file name without extention into the listbox.
   vector<string> configurationFilesNoExt;
   remove_directory_ext_and_copy< vector<string> > removeDirExt(configurationFilesNoExt);
   for_each(configurationFiles.begin(), configurationFiles.end(), removeDirExt);
   Stl_2_tstrings(configurationFiles, configurationList->Items);
   }
//---------------------------------------------------------------------------
void TRunForm::fillSimulationList(void)
   {
   vector<string> names;
   ApsimControlFile::getAllSectionNames(controlFileName, names);
   Stl_2_tstrings(names, simulationList->Items);
   }
//---------------------------------------------------------------------------
void TRunForm::setupForm(void)
   {
   bool someSelected = false;
   string configurationName;
   vector<string> simulations;
   if (previousRuns.getPreviousRun(controlFileName,
                                   configurationName,
                                   simulations))
      {
      configurationList->ItemIndex = configurationList->Items->
         IndexOf(configurationName.c_str());

      for (unsigned int simNameI = 0; simNameI < simulations.size(); simNameI++)
         {
         int listBoxIndex = simulationList->Items->
             IndexOf(simulations[simNameI].c_str());
         if (listBoxIndex >= 0)
            {
            simulationList->Selected[listBoxIndex] = true;
            someSelected = true;
            }
         }
      }
   if (!someSelected)
      {
      for (int i = 0; i < simulationList->Items->Count; i++)
         simulationList->Selected[i] = true;
      }
   checkOkButtonState(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::checkOkButtonState(TObject *Sender)
   {
   NextButton->Enabled = (configurationList->ItemIndex >= 0);
   }
//---------------------------------------------------------------------------
void TRunForm::getSelectedSimulations(vector<string>& simulations)
   {
   for (int i = 0; i < simulationList->Items->Count; i++)
      {
      if (simulationList->Selected[i])
         {
         simulations.push_back(simulationList->Items->Strings[i].c_str());
         }
      }
   }
//---------------------------------------------------------------------------
std::string TRunForm::getSelectedConfiguration(void)
   {
   return ApsimSettings::getSettingsFolder() + "\\" + configurationList->Items->Strings[configurationList->ItemIndex].c_str();
   }

