//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "ComponentInterfaceGenerator.h"
#include <general\path.h>
#include <general\MacroSubstFile.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimComponentData.h>
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
               GetNameFunction<vector<string>, ApsimRegistrationData>(eventList),
               IsOfType("respondToEvent"));
   }
//---------------------------------------------------------------------------
// Return a list of published events to caller.
//---------------------------------------------------------------------------
void GetPublishedEvents(ApsimComponentData* component,
                        vector<string>& eventList)
   {
   for_each_if(component->regBegin(), component->regEnd(),
               GetNameFunction<vector<string>, ApsimRegistrationData>(eventList),
               IsOfType("event"));
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
   // need to change the current working directory.
   AnsiString sourceDir = ExtractFileDir(interfaceFileName.c_str()) + "\\source";
   SetCurrentDir(sourceDir);

   if (!FileExists(interfaceFileName.c_str()))
      throw runtime_error("Cannot find component interface file: " + interfaceFileName);
   ifstream in(interfaceFileName.c_str());
   ostringstream contents;
   contents << in.rdbuf();
   ApsimComponentData* component = new ApsimComponentData(contents.str());

   // Macro substitution file in the same directory as the
   // program executable file.
   string macrofile = getApsimDirectory() + "\\apsbuild\\ComponentInterface.amf";

   // Set up macrosubst file object using the derived file name
   MacroSubstFile* AMF = new MacroSubstFile ();
   try
      {
      AMF->filename = macrofile;

      // set the module name as an attribute of the module macro.
      string moduleName = Path(interfaceFileName).Get_name_without_ext();
      Macro* moduleMacro = AMF->getMacro("module");
      moduleMacro->setAttribute("moduleName", moduleName);

      // Now to set macrovalues to be used in the
      // macrosubstitution file

      // Step One. - Set up temp data holder
      vector<string> temp;

      // Step Two. - Set Subscribed Event Macro Values
      GetSubscribedEvents(component, temp);
      Macro* subMacro = AMF->getMacro("subevent");
      if (subMacro == NULL)
         throw string("Cannot find a 'subevent' macro.");
      for (vector<string>::iterator nameI = temp.begin();
                                    nameI != temp.end();
                                    nameI++)
         {
         MacroValue macroValue;
         macroValue.addAttribute("name", *nameI);
         subMacro->addValue(macroValue);
         }

      // Step Three. - Set Published Event Macro Values
      temp.clear();
      GetPublishedEvents(component, temp);
      Macro* pubevent = AMF->getMacro("pubevent");
      if (pubevent == NULL)
         throw string("Cannot find a 'pubevent' macro.");
      for (vector<string>::iterator nameI = temp.begin();
                                    nameI != temp.end();
                                    nameI++)
         {
         MacroValue macroValue;
         macroValue.addAttribute("name", *nameI);
         pubevent->addValue(macroValue);
         }

      //  All done - so now write out the output files
      vector<string> filesWriten;
      AMF->write(filesWriten);
      }
   catch (...)
      {
      }
   delete AMF;
   }
//---------------------------------------------------------------------------
