#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ScreenComponent.h"
#include <ComponentInterface\MessageDataExt.h>
#include <ComponentInterface\ApsimVariant.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimComponentData.h>
#include <ApsimShared\ApsimServiceData.h>
#include <general\math_functions.h>
#include <general\stl_functions.h>
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
   ScreenForm = new TScreenForm(NULL);
   ScreenForm->Show();
   Application->ProcessMessages();
   }
// ------------------------------------------------------------------
// destructor.
// ------------------------------------------------------------------
ScreenComponent::~ScreenComponent(void)
   {
   ScreenForm->CloseButton->Caption = "Close";
   if (ScreenForm->PauseCheckBox->Checked)
      {
      while (ScreenForm->ModalResult != mrOk)
         Application->ProcessMessages();
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

   tickID = addRegistration(respondToEventReg, "tick", "");
   prepareID = addRegistration(respondToEventReg, "prepare", "");
   titleID = addRegistration(getVariableReg, "title", stringDDML);
   startDateID = addRegistration(getVariableReg, "simulation_start_date", doubleDDML);
   endDateID = addRegistration(getVariableReg, "simulation_end_date", doubleDDML);

   string sdmlString(sdml.f_str(), sdml.length());
   ApsimServiceData service(sdmlString);
   screenOutput = Str_i_Eq(service.getProperty("screen_output"), "on");
   if (screenOutput)
      {
      ScreenForm->Height = 450;
      summaryFileWriteID = addRegistration(respondToEventReg, "summaryFileWrite", "");
      }
   }
// ------------------------------------------------------------------
// Get start and end dates
// ------------------------------------------------------------------
void ScreenComponent::getStartEndDate(void)
   {
   string st;
   protocol::Variant* variant;
   bool ok = getVariable(startDateID, variant);
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
         getStartEndDate();

      protocol::ApsimVariant apsimVariant(variant);
      double jday;
      apsimVariant.get("jday", protocol::DTdouble, jday);
      currentDate = jday;
      int percent = (currentDate - startDateJDay) * 100 / (endDateJDay - startDateJDay);
      if (percent / 5.0 == percent / 5)
         {
         ScreenForm->ProgressBar->Position = percent;
         Application->ProcessMessages();
         }
      if (ScreenForm->ModalResult == mrOk)
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
         date.Write(out);

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
      if (posCR == MAXINT)
         posCR = lines.length();
      line = string(indentation, ' ');
      line += asString(lines.substr(posStart, posCR-posStart));
      ScreenForm->addLine(line);
      posStart = posCR + 1;
      }
   while (posCR < lines.length());
   }

