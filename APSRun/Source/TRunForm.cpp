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
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma link "HTMListB"
#pragma resource "*.dfm"
TRunForm *RunForm;
//---------------------------------------------------------------------------
__fastcall TRunForm::TRunForm(TComponent* Owner)
   : TForm(Owner), childProcessHandle(NULL)
 {
 }
//---------------------------------------------------------------------------
void __fastcall TRunForm::FormShow(TObject *Sender)
   {
   if (processCmdLine())
      {
      if (!ControlFileConverter::needsConversion(controlFileName))
         {
         PageControl1->ActivePageIndex = 1;
         NextButtonClick(Sender);
         }
      }
   else
      Close();
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::FormCloseQuery(TObject *Sender, bool &CanClose)
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
//---------------------------------------------------------------------------
void __fastcall TRunForm::NextButtonClick(TObject *Sender)
   {
   PageControl1->ActivePageIndex = PageControl1->ActivePageIndex + 1;
   if (PageControl1->ActivePageIndex == 2)
      NextButton->Caption = "&Run APSIM";
   else if (PageControl1->ActivePageIndex == 3)
      {
      NextButton->Visible = false;
      getSelectedSimulations(sections);
      currentSection = 0;
      configurationFile = getSelectedConfiguration();
      if (createSIM)
         {
         Visible = false;
         try
            {
            for (unsigned sim = 0; sim != sections.size(); sim++)
               {
               ApsimControlFile simulation(controlFileName, sections[sim]);
               string simFileName;
               simulation.createSIM(configurationFile, simFileName);
               }
            }
         catch (const runtime_error& err)
            {
            ShowMessage(err.what());
            }
         Close();
         }
      else if (sections.size() == 1)
         {
         Visible = false;
         Timer1->Enabled = true;
         }
      else
         {
         Caption = "Batch running APSIM";
         Timer1->Enabled = true;
         }
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
      converter.convert(controlFileName, &ConverterCallback);
      StatusList->Items->Clear();
      Path log(controlFileName);
      log.Set_extension(".conversions");
      if (FileExists(log.Get_path().c_str()))
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
//---------------------------------------------------------------------------
void __fastcall TRunForm::ConverterCallback(const std::string& section)
   {
   StatusList->Items->Clear();
   AnsiString msg = AnsiString("Converting control file section: ") + section.c_str();
   StatusList->Items->Add(msg);
   Application->ProcessMessages();
   }
// ------------------------------------------------------------------
// This application will be passed either a control file (.CON), a
// run file (.RUN), or a .SIM file depending on what the user has
// right clicked on.  Returns true if we need to continue with this
// form.
// ------------------------------------------------------------------
bool TRunForm::processCmdLine()
   {
   console = false;

   string fileName;
   bool quietRun = false;
   createSIM = false;
   for (int argIndex = 1; argIndex < _argc; argIndex++)
      {
      if (stricmp(_argv[argIndex], "/q") == 0)
         quietRun = true;
      else if (stricmp(_argv[argIndex], "/CreateSIM") == 0)
         createSIM = true;
      else if (stricmp(_argv[argIndex], "/Console") == 0)
         console = true;
      else
         fileName = _argv[argIndex];
      }
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot locate APSIM file: " + fileName);

   // Does the command line contain a control file?
   if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".con") == 0)
      {
      // yes - better ask user for a configuration.
      controlFileName = fileName;
      return true;
      }
   else if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".run") == 0)
      {
      ApsimRunFile run(fileName);
      run.run(quietRun);
      }
   else if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".sim") == 0)
      {
      ApsimSimulationFile::run(fileName, quietRun);
      }
   else
      throw runtime_error("Cannot run APSIM on file: " + fileName);
   return false;
   }
//---------------------------------------------------------------------------
void __fastcall TRunForm::Timer1Timer(TObject *Sender)
   {
   Timer1->Enabled = false;
   if (childProcessHandle == NULL)
      doApsimRun();
   else
      {
      DWORD exitCode;
      GetExitCodeProcess(childProcessHandle, &exitCode);
      if (exitCode != STILL_ACTIVE)
         doApsimRun();
      }
   if (childProcessHandle == NULL)
      Close();
   else
      Timer1->Enabled = true;
   }
//---------------------------------------------------------------------------
void TRunForm::doApsimRun(void)
   {
   if (currentSection != sections.size())
      {
      try
         {
         ApsimControlFile simulation(controlFileName, sections[currentSection]);
         childProcessHandle = simulation.run(configurationFile, console);
         currentSection++;
         }
      catch (const runtime_error& err)
         {
         ShowMessage(err.what());
         }
      }
   else
      childProcessHandle = NULL;
   }
//---------------------------------------------------------------------------

