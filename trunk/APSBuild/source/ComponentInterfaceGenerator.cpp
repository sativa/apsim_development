//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "ComponentInterfaceGenerator.h"
#include <general\path.h>
#include <general\MacroSubstFile.h>
#include <odl\odlsystem.h>
#include <aps\apsuite.h>
#include <iostream>
#include <list>
using namespace std;
//---------------------------------------------------------------------------
void GetSubscribedEvents(string filename, list<string>& eventlist)
   {
   // Derive the name of the class in the specified file from the
   // file name passed in.
   Path argPath(filename.c_str());

   string className = "APSIM" + argPath.Get_name_without_ext();

   // need to create an ODLSystem, set the include path to the apsuite
   // directory and read contents of our interface file
   ODLSystem system;
   list<string> includeDirectories;
   includeDirectories.push_back(APSDirectories().Get_home() + "\\apsim");
   system.setIncludeDirectories(includeDirectories);
   system.read(filename);

   // Get a list of all subscribed APSIMEvents.
   TypeClass type;
   list<string> subscribedEventNames;
   if (system.get(className + ".external", type))
      {
      type.enumerate(GetNameCallback<list<string>, TypeClass>(subscribedEventNames),
                     "APSIMEvent");
      }

   eventlist = subscribedEventNames;
   }
//---------------------------------------------------------------------------

void GetPublishedEvents(string filename, list<string>& eventlist)
   {
   // Derive the name of the class in the specified file from the
   // file name passed in.
   Path argPath(filename.c_str());

   string className = "APSIM" + argPath.Get_name_without_ext();

   // need to create an ODLSystem, set the include path to the apsuite
   // directory and read contents of our interface file
   ODLSystem system;
   list<string> includeDirectories;
   includeDirectories.push_back(APSDirectories().Get_home() + "\\apsim");
   system.setIncludeDirectories(includeDirectories);
   system.read(filename);

   // Get a list of all published APSIMEvents.
   list<string> publishedEventNames;
   TypeClass type;
   if (system.get(className + ".public", type))
      {
      type.enumerate(GetNameCallback<list<string>, TypeClass>(publishedEventNames),
                     "APSIMEvent");
      }

   eventlist = publishedEventNames;
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
void GenerateComponentInterface(const string& interfaceFile)
   {
   // Macro substitution file in the same directory as the
   // program executable file.
   string macrofile;
   macrofile =  ExtractFilePath(Application->ExeName).c_str();
   macrofile+= "APSIntGen.amf";

   // Set up macrosubst file object using the derived file name
   MacroSubstFile* AMF = new MacroSubstFile ();
   try
      {
      AMF->filename = macrofile;
      // Now to set macrovalues to be used in the
      // macrosubstitution file

      // Step One. - Set up temp data holder
      list<string> temp;

      // Step Two. - Set Subscribed Event Macro Values
      GetSubscribedEvents(interfaceFile,temp);
      Macro* subMacro = AMF->getMacro("subevent");
      if (subMacro == NULL)
         throw string("Cannot find a 'subevent' macro.");
      for (list<string>::iterator nameI = temp.begin();
                                  nameI != temp.end();
                                  nameI++)
         {
         MacroValue macroValue;
         macroValue.addAttribute("name", *nameI);
         subMacro->addValue(macroValue);
         }

      // Step Three. - Set Published Event Macro Values
      temp.clear();
      GetPublishedEvents(interfaceFile,temp);
      Macro* pubevent = AMF->getMacro("pubevent");
      if (pubevent == NULL)
         throw string("Cannot find a 'pubevent' macro.");
      for (list<string>::iterator nameI = temp.begin();
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
