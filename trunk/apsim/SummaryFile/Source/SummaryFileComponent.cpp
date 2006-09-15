#include <sstream>
#include <iomanip>
#include <fstream>

#include <general/date_class.h>
#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimVersion.h>
#include <ComponentInterface/Component.h>
#include "SummaryFileComponent.h"

using namespace std;
using namespace protocol;

static const char* stringType = "<type kind=\"string\"/>";

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

   // do registrations.
   static const char* stringDDML = "<type kind=\"string\"\\>";
   static const char* stringArrayDDML = "<type kind=\"string\" array=\"T\"\\>";
   summaryFileWriteID = addRegistration(RegistrationType::respondToEvent, "summaryFileWrite", "");
   tickID = addRegistration(RegistrationType::respondToEvent, "tick", DDML(timeType()).c_str());
   prepareID = addRegistration(RegistrationType::respondToEvent, "prepare", "");
   externalErrorID = addRegistration(RegistrationType::respondToEvent, "error", "");
   summaryFileID = addRegistration(RegistrationType::respondToGet, "summaryFile", stringDDML);
   titleID = addRegistration(RegistrationType::get, "title", stringDDML);
   componentsID = addRegistration(RegistrationType::get, "components", stringArrayDDML);
   }
// ------------------------------------------------------------------
// do INIT2 stuff.
// ------------------------------------------------------------------
void SummaryFileComponent::doInit2(void)
   {
   // read in and open our file. Extra care taken during init1.
   fileName = componentData->getProperty("parameters", "summaryfile");
   if (fileName == "")
      fileName = calcFileName();

   out.open(fileName.c_str());
   if (!out)
      {
      string msg = "Cannot open summary file: " + fileName;
      throw std::runtime_error(msg);
      }

   writeBanner();

   writeInfo();
   if (!out) terminateSimulation(); // If open() failed earlier, stop now.
   }

// ------------------------------------------------------------------
// Calculate a file name based on simulation title and PM name.
// ------------------------------------------------------------------
string SummaryFileComponent::calcFileName()
   {
   string title, pmName;
   unsigned titleID = addRegistration(RegistrationType::get,
                                      "title",
                                      stringType);
   protocol::Variant* variant;
   if (getVariable(titleID, variant, true))
      variant->unpack(title);

   char buffer[500];
   strcpy(buffer, "\0");
   FString parentName(buffer, sizeof(buffer), CString);
   componentIDToName(parentID, parentName);
   pmName = asString(parentName);

   string fileName = title;
   if (!Str_i_Eq(pmName, "paddock") && !Str_i_Eq(pmName, "masterpm"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += pmName;
      }

   if (!Str_i_Eq(name, "outputfile"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += name;
      }
   fileName += ".sum";
   return fileName;
   }
// ------------------------------------------------------------------
// write all simulation information to summary file.
// ------------------------------------------------------------------
void SummaryFileComponent::writeInfo(void)
   {
   // write out apsim version and simulation file
   string line = "Version                = " + getApsimVersion();
   writeLine("", line.c_str());

   // write out title.
   protocol::Variant* variant;
   bool ok = getVariable(titleID, variant);
   if (ok)
      {
      string title;
      variant->unpack(title);
      line = "Title                  = " + title;
      writeLine("", line.c_str());
      }

   // write out list of components.
   ok = getVariable(componentsID, variant);
   if (ok)
      {
      std::vector<string> components;
      variant->unpack(components);
      for (unsigned comp = 0; comp != components.size(); comp++)
         {
         line = "Component DLL          = " + components[comp];
         writeLine("", line.c_str());
         }
      }

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
      timeType tick;
      variant.unpack(tick);
      currentDate = tick.startday;
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
      ErrorData errorData;
      variant.unpack(errorData);
      string componentName = asString(errorData.errorMessage);
      unsigned int posComponentName = componentName.find("Component name: ");
      if (posComponentName != string::npos)
         {
         componentName = componentName.substr(posComponentName + strlen("Component name: "));
         componentName = componentName.erase(componentName.find("\n"));
         }
      else
         componentName = "";
      writeLine(componentName.c_str(), errorData.errorMessage);
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
   if (!out) return;

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
         date.Set_write_format("D MMMMMM YYYY");
         date.Write(out);
         out << "(Day of year=" << date.Get_day_of_year() << ")";

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
      if (posCR == FString::npos)
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
// ------------------------------------------------------------------
// return one of our variables to caller
// ------------------------------------------------------------------
void SummaryFileComponent::respondToGet(unsigned int& fromID, QueryValueData& queryData)
   {
   if (queryData.ID == summaryFileID)
      sendVariable(queryData, FString(fileName.c_str()));
   }

