#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SummaryFileComponent.h"
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimServiceData.h>
#include <general\date_class.h>
#include <sstream>
#include <iomanip>
using namespace std;
using namespace protocol;
// ------------------------------------------------------------------
//  Short description:
//     createComponent

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new SummaryFileComponent;
   }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
SummaryFileComponent::SummaryFileComponent(void)
   {
   inDiaryState = false;
   }
// ------------------------------------------------------------------
//  Short description:
//     INIT1 method handler.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void SummaryFileComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   summaryFileWriteID = addRegistration(respondToEventReg, "summaryFileWrite", "");
   tickID = addRegistration(respondToEventReg, "tick", "");
   prepareID = addRegistration(respondToEventReg, "prepare", "");
   externalErrorID = addRegistration(respondToEventReg, "error", "");

   string sdmlString(sdml.f_str(), sdml.length());
   ApsimServiceData service(sdmlString);
   string filename = service.getProperty("filename");
   out.open(filename.c_str());
   if (!out)
      {
      string msg = "Cannot open summary file: " + filename;
      ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   else
      writeBanner();
   }

// ------------------------------------------------------------------
//  Short description:
//    Event handler.

//  Notes:

//  Changes:
//    DPH 23/5/2001

// ------------------------------------------------------------------
void SummaryFileComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      int jday;
      variant.unpack(jday);

      currentDate = jday;
      }
   else if (eventID == prepareID && !inDiaryState)
      {
      inDiaryState = true;
      out << "------- Start of simulation  --------------------------------------------------" << endl;
      }
   else if (eventID == summaryFileWriteID)
      {
      FString moduleName, line;
      variant.unpack(moduleName);
      variant.unpack(line);
      writeLine(moduleName, line);
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
         terminateSimulation();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     write a line(s) to the summary file.

//  Notes:

//  Changes:
//    dph 22/6/99

// ------------------------------------------------------------------
void SummaryFileComponent::writeLine(const FString& componentName, const FString& lines)
   {
   static string previousComponentName;
   unsigned indentation = 5;
   if (!inDiaryState)
      {
      if (componentName != previousComponentName.c_str())
         {
         out << endl;
         out << "------- " << asString(componentName)
             << " Initialisation ";
         out.width(79-24-componentName.length());
         out.fill('-');
         out << '-' << endl;
         out.fill(' ');
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
         if (previousDate == 0)
            previousDate = currentDate;
         GDate date;
         date.Set((unsigned long) currentDate);
         date.Write(out);

         out << ", " << asString(componentName) << ": " << endl;
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
      out << setw(indentation) << ' ';
      out << asString(lines.substr(posStart, posCR-posStart)) << endl;
      posStart = posCR + 1;
      }
   while (posCR < lines.length());
   }

// ------------------------------------------------------------------
//  Short description:
//     write an APSIM banner to summary file.

//  Notes:

//  Changes:
//    dph 22/6/99

// ------------------------------------------------------------------
void SummaryFileComponent::writeBanner(void)
   {
   static const char* Banner = "     ###     ######     #####   #   #     #   \n"
                               "    #   #    #     #   #        #   ##   ##   \n"
                               "   #     #   #     #   #        #   ##   ##   \n"
                               "   #######   ######     #####   #   # # # #   \n"
                               "   #     #   #              #   #   #  #  #   \n"
                               "   #     #   #         #####    #   #  #  #   \n"
                               "                                              \n"
                               "                                              \n"
                               " The Agricultural Production Systems Simulator\n"
                               "             Copyright(c) APSRU               \n\n";
   out << Banner;
   }

