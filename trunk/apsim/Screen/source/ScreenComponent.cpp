#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ScreenComponent.h"
#include <ComponentInterface\MessageDataExt.h>
#include <ComponentInterface\ApsimVariant.h>
#include <ComponentInterface\datatypes.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimComponentData.h>
#include <general\math_functions.h>
#include <general\stl_functions.h>
#include <general\vcl_functions.h>
#include <general\date_class.h>
#include <general\StringTokenizer.h>
#include <sstream>
#include <iomanip>
#include <variant.h>
#include "TScreenForm.h"
#pragma package(smart_init)
using namespace std;
using namespace protocol;
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the SCREEN module
// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new ScreenComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScreenComponent::ScreenComponent(void)
   {
   inDiaryState = false;
   startDateJDay = 0;
   HWND parentHandle = FindWindow(NULL, "Running APSIM...");
   char parentCaption[100];
   GetWindowText(parentHandle, parentCaption, sizeof(parentCaption));
   if (strcmpi(parentCaption, "Running APSIM...") != 0)
      ScreenForm = new TScreenForm((TComponent*)NULL);

   else
      {
      ScreenForm = new TScreenForm((HWND)parentHandle);
      ScreenForm->BorderIcons.Clear();
      ScreenForm->BorderStyle = bsNone;
      ScreenForm->Caption = "";
      }
   ScreenForm->Show();

   ScreenForm->setup();
   ScreenForm->Left = 168;
   ScreenForm->Top = 24;
   Application->ProcessMessages();
   }
// ------------------------------------------------------------------
// destructor.
// ------------------------------------------------------------------
ScreenComponent::~ScreenComponent(void)
   {
   ScreenForm->simulationHasFinished();
   if (ScreenForm->CancelButton->Caption == "Close")
      {
      MSG msg;
      while (ScreenForm->Visible && GetMessage(&msg, 0, 0, 0)
             && ScreenForm->PauseCheckBox->Checked
             && msg.message != WM_CLOSE_CLICKED_MSG)
         {
         if (!IsDialogMessage (ScreenForm->Handle, &msg))
            {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
            }
         }
      }

   delete ScreenForm;
   }
// ------------------------------------------------------------------
// INIT1
// ------------------------------------------------------------------
void ScreenComponent::doInit1(const FString& sdml)
   {
   Component::doInit1(sdml);

   static const char* doubleDDML = "<type kind=\"double\"\\>";
   static const char* stringDDML = "<type kind=\"string\"\\>";

   tickID = addRegistration(RegistrationType::respondToEvent, "tick", timeTypeDDML);
   prepareID = addRegistration(RegistrationType::respondToEvent, "prepare", "");
   titleID = addRegistration(RegistrationType::get, "title", stringDDML);
   externalErrorID = addRegistration(RegistrationType::respondToEvent, "error", "");
   startDateID = addRegistration(RegistrationType::get, "simulation_start_date", doubleDDML);
   endDateID = addRegistration(RegistrationType::get, "simulation_end_date", doubleDDML);

   screenOutput = Str_i_Eq(componentData->getProperty("parameters", "screen_output"), "on");
   if (screenOutput)
      {
//      ScreenForm->Height = 466;
      summaryFileWriteID = addRegistration(RegistrationType::respondToEvent, "summaryFileWrite", "");
      }
   }
// ------------------------------------------------------------------
// Get start and end dates
// ------------------------------------------------------------------
void ScreenComponent::getStartEndDate(void)
   {
   // put title on form.
   protocol::Variant* variant;
   bool ok = getVariable(titleID, variant);
   if (ok)
      {
      string title;
      variant->unpack(title);
      ScreenForm->TitleLabel->Caption = AnsiString("Simulation progress for: ") + title.c_str();
      }
   string st;
   ok = getVariable(startDateID, variant);
   if (ok)
      {
      double num;
      variant->unpack(num);
      startDateJDay = num;
      GDate d;
      d.Set(startDateJDay);
      ostringstream out;
      d.Write(out);
      ScreenForm->StartDateLabel->Caption = out.str().c_str();
      }
   ok = getVariable(endDateID, variant);
   if (ok)
      {
      double num;
      variant->unpack(num);
      endDateJDay = num;
      GDate d;
      d.Set(endDateJDay);
      ostringstream out;
      d.Write(out);
      ScreenForm->EndDateLabel->Caption = out.str().c_str();
      }
   Application->ProcessMessages();
   }
// ------------------------------------------------------------------
// RespondToEvent
// ------------------------------------------------------------------
void ScreenComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      if (startDateJDay == 0)
         {
         getStartEndDate();
         // Try and do 40 updates per simulation but make sure there is
         // an update every 100 days.  updateInterval units is days.
         // Range is 1 to 100.
         updateInterval = max(min((endDateJDay - startDateJDay) / 40, 100), 1);

         ScreenForm->ProgressBar->Step = 1;
         ScreenForm->ProgressBar->Max = (endDateJDay - startDateJDay) / updateInterval;
         }

      timeType tick;
      variant.unpack(tick);
      currentDate = tick.startday;
      static int interval = 0;
      interval++;
      if (interval == updateInterval || currentDate == endDateJDay)
         {
         interval = 0;
         if (ScreenForm->ProgressBar->Position != ScreenForm->ProgressBar->Max)
            ScreenForm->ProgressBar->StepIt();

         GDate d;
         d.Set(currentDate);
         ostringstream out;
         d.Write(out);
         ScreenForm->CurrentDateLabel->Caption = out.str().c_str();

         Application->ProcessMessages();
         }
      if (!ScreenForm->Visible || ScreenForm->ModalResult == mrOk)
         terminateSimulation();
      }
   else if (eventID == summaryFileWriteID)
      {
      FString moduleName, line;
      variant.unpack(moduleName);
      variant.unpack(line);
      writeLine(moduleName, line);
      }
   else if (eventID == prepareID && !inDiaryState)
      {
      inDiaryState = true;
      ScreenForm->addLine("------- Start of simulation  --------------------------------------------------");
      }
   else if (eventID == externalErrorID)
      {
      bool isFatal;
      FString errorMessage;
      variant.unpack(isFatal);
      variant.unpack(errorMessage);
      string componentName = asString(errorMessage);
      unsigned int posComponentName = componentName.find("Component name: ");
      if (posComponentName != string::npos)
         {
         componentName = componentName.substr(posComponentName + strlen("Component name: "));
         componentName = componentName.erase(componentName.find("\n"));
         }
      else
         componentName = "";
      writeLine(componentName.c_str(), errorMessage);
      if (isFatal)
         ScreenForm->errorsWereEncountered();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     write a line(s) to the summary file.

//  Notes:

//  Changes:
//    dph 22/6/99

// ------------------------------------------------------------------
void ScreenComponent::writeLine(const FString& componentName, const FString& lines)
   {
   static string previousComponentName;
   unsigned indentation = 5;
   string line;
   if (!inDiaryState)
      {
      if (componentName != previousComponentName.c_str())
         {
         ScreenForm->addLine("");
         line = "------- " + asString(componentName) + " Initialisation ";
         line += string(79-24-componentName.length(), '-');
         ScreenForm->addLine(line);
         previousComponentName = asString(componentName);
         indentation = 5;
         }
      }
   else
      {
      // in diary state write date first if current date is different from the
      // last date we wrote.
      static int previousDate;
      if (componentName != previousComponentName.c_str() || previousDate != currentDate)
         {
         ostringstream out;
         if (previousDate == 0)
            previousDate = currentDate;
         GDate date;
         date.Set((unsigned long) currentDate);
         date.Set_write_format("D MMMMMM YYYY");
         date.Write(out);
         out << "(Day of year=" << date.Get_day_of_year() << ")";
         out << ", " << asString(componentName) << ": ";
         ScreenForm->addLine(out.str());
         previousDate = currentDate;
         previousComponentName = asString(componentName);
         indentation = 5;
         }
      }
   // write out the lines.
   unsigned posStart = 0;
   unsigned posCR;
   do
      {
      posCR = lines.find("\n", posStart);
      if (posCR == FString::npos)
         posCR = lines.length();
      line = string(indentation, ' ');
      line += asString(lines.substr(posStart, posCR-posStart));
      ScreenForm->addLine(line);
      posStart = posCR + 1;
      }
   while (posCR < lines.length());
   }

