//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "ComponentInterfaceGenerator.h"
#include <general\path.h>
#include <general\Macro.h>
#include <general\TreeNodeIterator.h>
#include <general\xml.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDataTypeData.h>
#include <ApsimShared\ApsimComponentData.h>
#include <ApsimShared\ApsimRegistrationData.h>
#include <iostream>
#include <vector>
using namespace std;
//---------------------------------------------------------------------------
// Predicate to filter registrations of a particular type.
//---------------------------------------------------------------------------
class IsOfType
   {
   private:
      const char* type;
   public:
      IsOfType(const char* t) : type(t) { }

      bool operator() (const ApsimRegistrationData& registration)
        {return registration.isOfType(type);}
   };
//---------------------------------------------------------------------------
// Return a list of subscribed events to caller.
//---------------------------------------------------------------------------
void GetSubscribedEvents(ApsimComponentData* component,
                         vector<string>& eventList)
   {
   for_each_if(component->regBegin(), component->regEnd(),
               GetName<ApsimRegistrationData>(eventList),
               IsOfType("respondToEvent"));
   }
//---------------------------------------------------------------------------
// Return a list of published events to caller.
//---------------------------------------------------------------------------
void GetPublishedEvents(ApsimComponentData* component,
                        vector<string>& eventList)
   {
   for_each_if(component->regBegin(), component->regEnd(),
               GetName<ApsimRegistrationData>(eventList),
               IsOfType("event"));
   }
//---------------------------------------------------------------------------
// Return a list of subscribed methods to caller.
//---------------------------------------------------------------------------
void GetSubscribedMethods(ApsimComponentData* component,
                          vector<string>& eventList)
   {
   for_each_if(component->regBegin(), component->regEnd(),
               GetName<ApsimRegistrationData>(eventList),
               IsOfType("respondToMethodCall"));
   }
//---------------------------------------------------------------------------
// Return a list of published methods to caller.
//---------------------------------------------------------------------------
void GetPublishedMethods(ApsimComponentData* component,
                         vector<string>& eventList)
   {
   for_each_if(component->regBegin(), component->regEnd(),
               GetName<ApsimRegistrationData>(eventList),
               IsOfType("methodCall"));
   }

// ------------------------------------------------------------------
//  Short description:
//     generate a component interface from the APSIM interface filename
//     passed in.  The generated output files will go in the current
//     working directory.

//  Notes:
//    The macro substitution file is assumed to reside the the
//    directory with the program executable file.

//  Changes:
//    NIH 13/12/2000

// ------------------------------------------------------------------
void GenerateComponentInterface(const string& interfaceFileName)
   {
   try
      {

      if (!FileExists(interfaceFileName.c_str()))
         throw runtime_error("Cannot find component interface file: " + interfaceFileName);

      ifstream in(interfaceFileName.c_str());
      ostringstream contents;
      contents << in.rdbuf();
      ApsimComponentData* component = new ApsimComponentData(contents.str());

      // Set up macrosubst file object using the derived file name
      XMLDocument xml("Data", XMLDocument::rootName);

      // Loop through all registrations and create a macro value for each.
      for (ApsimComponentData::RegIterator reg = component->regBegin();
                                           reg != component->regEnd();
                                           ++reg)
         {
         string name = reg->getName();
         string type;
         To_lower(name);
         try
            {
            ApsimDataTypeData dataType = component->getDataType(reg->getDataTypeName());
            type = dataType.getName();
            if (!dataType.isStructure())
               type = "null";
            }
         catch (...)
            {
            type = "null";
            }

         string macroName;
         if (reg->isOfType("event"))
            macroName = "pubevent";
         else if (reg->isOfType("respondToEvent"))
            macroName = "subEvent";
         else if (reg->isOfType("methodCall"))
            macroName = "pubmethod";
         else if (reg->isOfType("respondToMethodCall"))
            macroName = "submethod";
         if (macroName != "")
            {
            XMLNode macroNode = xml.documentElement().appendChild(macroName, true);
            macroNode.setAttribute("name", name);
            macroNode.setAttribute("kind", type);
            }
         }

      // read in contents of macro file.
      string macrofile;
      ApsimSettings settings;
      settings.read("APSBuild|ComponentInterfaceMacroFile", macrofile, true);
      if (!FileExists(macrofile.c_str()))
         throw runtime_error("Cannot find component interface macro file: " + macrofile);

      ifstream in2(macrofile.c_str());
      ostringstream macroContents;
      macroContents << in2.rdbuf();

      //  All done - so now write out the output files
      vector<string> filesGenerated;
      Macro macro;
      macro.go(xml.documentElement(), macroContents.str(), filesGenerated);
      }
   catch (const runtime_error& error)
      {
      cerr << error.what() << endl;
      }
   catch (...)
      {
      }
   }
//---------------------------------------------------------------------------
