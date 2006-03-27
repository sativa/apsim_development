//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <iterator>


#include <general\TreeNodeIterator.h>
#include <general\xml.h>
#include <general\path.h>
#include <general\date_class.h>
#include <general\io_functions.h>
#include <general\vcl_functions.h>
#include <general\StringTokenizer.h>

#include <ApsimShared\ApsimComponentData.h>
#include <ApsimShared\ApsimServiceData.h>
#include <ApsimShared\ApsimSystemData.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ControlFileConverter.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include <ApsimShared\ApsimSettings.h>

#include "TRunForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMLabel"
#pragma link "HTMListB"
#pragma link "HTMLText"
#pragma link "AdvMemo"
#pragma link "advmws"
#pragma resource "*.dfm"
TRunForm *RunForm;


__fastcall TRunForm::TRunForm(TComponent* Owner)
   : TForm(Owner)
   //---------------------------------------------------------------------------
   // constructor
   {
   }
   
void TRunForm::setup(ApsimRuns& apsimRuns, bool autoRun)
   //---------------------------------------------------------------------------
   // Setup all form stuff
   {
   ApsimSettings settings;
   settings.read("Apsim|PauseOnComplete", pauseOnComplete);

   paused = false;
   if (autoRun)
      pauseOnComplete = false;

   PauseCheckBox->Checked = pauseOnComplete;

   runs = &apsimRuns;
   runs->getFilesNeedingConversion(filesNeedingConversion);
   if (filesNeedingConversion.size() > 0)
      {
      if (autoRun || runs->count() == 1)
         runs->convertFiles();
      else
         {
         MainPanel->ActivePage = Page1;
         populatePage1();
         }
      }
   else
      {
      MainPanel->ActivePage = Page3;
      populatePage3();
      }

   // Do we automatically start the simulations running?
   if (autoRun || runs->count() == 1)
      {
      MainPanel->ActivePage = Page3;
      populatePage3();
      for (int i = 0; i != SimulationList->Items->Count; i++)
         SimulationList->Items->Item[i]->Selected = true;

      PostMessage(NextButton->Handle, BM_CLICK, 0, 0);
      }
   }

void TRunForm::populatePage1(void)
   //---------------------------------------------------------------------------
   // populate page 1.
   {
   ostringstream out;
   copy(filesNeedingConversion.begin(),filesNeedingConversion.end(),
        ostream_iterator<string, char>(out,"\n"));
   ControlFileLabel->Caption = out.str().c_str();
   }

void TRunForm::populatePage2()
   //---------------------------------------------------------------------------
   // populate page 2.
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

void TRunForm::populatePage3()
   //---------------------------------------------------------------------------
   // populate page 3.
   {
   NextButton->Caption = "&Run APSIM";
   SimulationList->Items->BeginUpdate();

   SimulationList->Items->Clear();
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

void __fastcall TRunForm::NextButtonClick(TObject *Sender)
   //---------------------------------------------------------------------------
   // Next button has been clicked.
   {
   if (MainPanel->ActivePage == Page1)
      {
      populatePage2();
      MainPanel->ActivePageIndex ++;
      }
   else if (MainPanel->ActivePage == Page2)
      {
      populatePage3();
      MainPanel->ActivePageIndex ++;
      }
   else if (MainPanel->ActivePage == Page3)
      {
      saveSelections();
      runSimulations();
      }
   else if (MainPanel->ActivePage == Page4)
      {
      paused = !paused;
      if (paused)
         {
         NextButton->Caption = "Resume";
         Memo1->ScrollBars = Stdctrls::ssBoth;
         Memo1->Lines->Add(" ");
         }
      else
         {
         Memo1->ScrollBars = Stdctrls::ssNone;
         NextButton->Caption = "Pause";
         }
      runs->Pause(paused);
      }
   }

void TRunForm::runSimulations()
   //---------------------------------------------------------
   // Go run all simulations.
   {
   StartDateLabel->Caption = "";
   CurrentDateLabel->Caption = "";
   EndDateLabel->Caption = "";
   MainPanel->ActivePageIndex ++;
   NextButton->Caption = "Pause";
   FinishedLabel->Visible = false;
   ErrorLabel->Visible = false;
   dayCounter = 0;
   Application->ProcessMessages();

   runs->runApsim(false, OnRunNotifyEvent, OnStdoutEvent);

   FinishedLabel->Visible = true;
   NextButton->Visible = false;
   CancelButton->Caption = "Ok";
   CancelButton->Default = true;
   Memo1->ScrollBars = Stdctrls::ssBoth;
   Memo1->Lines->Add(" ");


   if (!pauseOnComplete || !Visible)
      Close();
   }

void __fastcall TRunForm::CancelButtonClick(TObject *Sender)
   //---------------------------------------------------------------------------
   // User has hit cancel - better stop APSIM.
   {
   runs->StopApsim();
   Close();
   }

void __fastcall TRunForm::checkOkButtonState(TObject *Sender)
   //---------------------------------------------------------------------------
   // Only enable the ok button if at least one simulation is selected.
   {
   bool somethingSelected = false;
   for (int n = 0; n != SimulationList->Items->Count && !somethingSelected; n++)
      somethingSelected = SimulationList->Items->Item[n]->Selected;

   NextButton->Enabled = somethingSelected;
   }

void TRunForm::saveSelections(void)
   //---------------------------------------------------------------------------
   // Save all selected simulations back to run object.
   {
   runs->clearSimulations();
   for (int i = 0; i != SimulationList->Items->Count; i++)
      {
      TListItem* simNameItem = SimulationList->Items->Item[i];
      if (simNameItem->Selected)
         {
         string simName = simNameItem->Caption.c_str();
         string fileName = simNameItem->SubItems->Strings[0].c_str();
         runs->addSimulation(fileName, simName);
         }
      }
   previousRuns.saveSelectedRunNames(*runs);
   }

void __fastcall TRunForm::simulationListClick(TObject *Sender)
   //---------------------------------------------------------------------------
   // Everytime a user clicks on the simulation list somewhere,
   // enable or disable the ok button.
   {
   checkOkButtonState(NULL);
   }

void __fastcall TRunForm::OnRunNotifyEvent(const std::string& simFileName)
   //---------------------------------------------------------------------------
   // An APSIM simulation is about to run. Display title and extract start/end dates.
   {
   GDate d;
   string caption = "Simulation progress for: ";
   caption += simFileName;
   caption.erase(caption.find(".sim"));
   FileNameLabel->Caption = caption.c_str();

   XMLDocument simDoc(simFileName);

   XMLNode i = findNodeWithName(simDoc.documentElement(), "clock");
   XMLNode id = findNode(i, "initdata");
   XMLNode sdat = findNode(id, "start_date");
   d.Read (sdat.getValue().c_str());
   simStartJDay = d.Get_jday();

   XMLNode edat = findNode(id, "end_date");
   d.Read (edat.getValue().c_str());
   simEndJDay = d.Get_jday();

   if (simStartJDay == 0 || simEndJDay == 0)
      ProgressBar->Visible = false;
   else
      {
      ProgressBar->Step = 1;
      ProgressBar->Min = 0;
      int length = simEndJDay - simStartJDay;
      ProgressBar->Max = (length < 0) ? 0 : length;

      ProgressBar->Position = 0;

      StartDateLabel->Caption = sdat.getValue().c_str();
      EndDateLabel->Caption = edat.getValue().c_str();
      }
   }



void TRunForm::addLine(const string& line)
   //---------------------------------------------------------------------------
   // Add a line to the memo box. Make sure it doesn't overflow.
   {
   static const int MAX_LINES = 5000;
   if (Memo1->Lines->Count == MAX_LINES)
      {
      Memo1->Lines->BeginUpdate();
      for (int i = MAX_LINES; i >= 0 / 2; i--)
         Memo1->Lines->Delete(i);
      Memo1->Lines->EndUpdate();
      }
   Memo1->Lines->Add(line.c_str());
   }

void __fastcall TRunForm::OnStdoutEvent(const string& text)
   //---------------------------------------------------------------------------
   // Apsim has given us some stdout data. Note the text chunk may not be
   // aligned with line breaks; expect (a few) partial lines.
   {
   if (MainPanel->ActivePage != Page4)
      MainPanel->ActivePage = Page4;

   // Parse dates out of text. Use the dates to update the progress bar.
   GDate currentDate;
   StringTokenizer st(text, "\r\n", false);
   while (st.hasMoreTokens())
      {
      string line =  st.nextToken();
      if (line.find("Date:") != string::npos)
         {
         string DateString = line.substr(5);
         replaceAll(DateString, " ", "");
         // 'Date' lines. Update 'indicator' dials
         CurrentDateLabel->Caption = DateString.c_str();
         currentDate.Read(DateString.c_str());
         if (ProgressBar->Visible)
            {
            int currpos = currentDate.Get_jday() - simStartJDay;
            currpos = (currpos < ProgressBar->Min) ? ProgressBar->Min : currpos;
            currpos = (currpos > ProgressBar->Max) ? ProgressBar->Max : currpos;
            ProgressBar->Position = currpos;
            }
         }
      else if (line != "" || st.hasMoreTokens())
         addLine(line);
      }

   if (!ErrorLabel->Visible)
      {
      if (text.find("APSIM  Fatal") != string::npos)
         ErrorLabel->Visible = true;
      else if (text.find("APSIM Warning") != string::npos)
         {
         ErrorLabel->Visible = true;
         ErrorLabel->Caption = "warnings found";
         }
      }
   }

void __fastcall TRunForm::FormCloseQuery(TObject *Sender, bool &CanClose)
   //-------------------------------------------
   // form is about to close - shut down apsim
   {
   runs->StopApsim();
   }

void __fastcall TRunForm::PauseCheckBoxClick(TObject *Sender)
   // ----------------------------------------------------------------
   // User has clicked on the "Pause on simulation complete" checkbox
   // Save this setting to the apsim.ini.
   {
   string YesNoValue;
   if (PauseCheckBox->Checked)
      YesNoValue = "yes";
   else
      YesNoValue = "no";

   ApsimSettings settings;
   settings.write("Apsim|PauseOnComplete", YesNoValue);
   pauseOnComplete = PauseCheckBox->Checked;
   }

void __fastcall TRunForm::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)
   //---------------------------------------------------------------------------
   // Look for a return key and send next button. If ESC, then close down.
   {
   if (Key == VK_RETURN)
      NextButtonClick(NULL);
   else if (Key == VK_ESCAPE)
      Close();
   }
//---------------------------------------------------------------------------

