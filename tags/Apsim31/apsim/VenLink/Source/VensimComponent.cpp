//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "VensimComponent.h"
#include <ComponentInterface\MessageDataExt.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimComponentData.h>

#include <general\string_functions.h>
#include <general\stristr.h>
#pragma package(smart_init)
using namespace std;
using namespace protocol;

static const char* MES_Prepare = "prepare";
static const char* MES_Process = "process";
extern "C" int __stdcall vensim_command(const char* command);
extern "C" int __stdcall vensim_start_simulation(int, int, int);
extern "C" int __stdcall vensim_get_val(const char* name, float* value);
extern "C" int __stdcall vensim_continue_simulation(int numIterations);
extern "C" int __stdcall vensim_finish_simulation(void);
extern "C" int __stdcall vensim_check_status(void);
extern "C" int __stdcall vensim_get_varnames(const char* filter,
                                             int vartype,
                                             char* buf,
                                             int buflength);
extern "C" unsigned long __stdcall vensim_get_varoff(const char* varname);
extern "C" int __stdcall vensim_get_data(char *filename,
                                         char *varname,
                                         char *tname,
                                         float *vval,
                                         float *tval,
                                         int maxn) ;
// ------------------------------------------------------------------
// Create an instance of the REPORT module
// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new VensimComponent;
   }
// ------------------------------------------------------------------
// Initialises the Vensim component.
// ------------------------------------------------------------------
VensimComponent::VensimComponent()
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
VensimComponent::~VensimComponent(void)
   {
   vensim_command("GAME>ENDGAME");
   }
// ------------------------------------------------------------------
// Stage 1 initialisation.
// ------------------------------------------------------------------
void VensimComponent::doInit1(const FString& sdml)
   {
   Component::doInit1(sdml);
   processID = addRegistration(respondToEventReg, "prepare", "");
   }
// ------------------------------------------------------------------
// Initialise the VENSIM component.
// ------------------------------------------------------------------
void VensimComponent::doInit2(void)
   {
   Component::doInit2();

   // write copyright notice.
   writeString("Portions Copyright (c) 1987-1999 Ventana Systems, Inc.");

   // get the model filename.
   fileName = componentData->getProperty("parameters", "model_filename");

   // Tell the vensim dll to load the simulation
   string command = "SPECIAL>LOADMODEL|" + fileName;
   int result = vensim_command(command.c_str());
   if (result == 0)
      {
      string msg = "Cannot load VENSIM model: " + fileName;
      error(msg.c_str(), true);
      }
   writeString(string("Model filename: " + fileName).c_str());

   // register all model variables.
   registerModelVariables();

   // Loop through all groups.  Read the model filename from the .parameters
   // group, the model inputs from the .inputs group and the model outputs
   // from the .outputs group.
   writeString("Vensim constants:");
   std::vector<string> names, values;
   componentData->getProperties("constants", names, values);
   for (unsigned g = 0; g != names.size(); g++)
      {
      writeString(string("   " + names[g] + " = " + values[g]).c_str());

      // pass constant to VENSIM.
      Replace_all(names[g], "_", " ");
      string command = "SIMULATE>SETVAL|" + names[g] + " = " + values[g];
      if (vensim_command(command.c_str()) == 0)
         {
         string msg = "Cannot set VENSIM variable value.  Variable=" + names[g];
         error(msg.c_str(), true);
         }
      }

   result = vensim_command("MENU>GAME");
   result = vensim_command("GAME>GAMEINTERVAL|1");
   if (result == 0)
      {
      string msg = "Cannot start VENSIM simulation: " + fileName;
      error(msg.c_str(), true);
      }
   }
// ------------------------------------------------------------------
// Register all model variables.
// ------------------------------------------------------------------
void VensimComponent::registerModelVariables(void)
   {
   static const char* floatDDML = "<type kind=\"single\"/>";

   writeString("VENSIM model variables:");
   std::vector<string> names;
   componentData->getVariables(names);
   for (unsigned variable = 0; variable != names.size(); variable++)
      {
      string nameWithUnderscore = names[variable];
      Replace_all(nameWithUnderscore, " ", "_");
      unsigned id = addRegistration(respondToGetSetReg,
                                    nameWithUnderscore.c_str(),
                                    floatDDML);
      variables.insert(VariableMap::value_type(id, names[variable]));
      writeString(string("   " + names[variable]).c_str());
      }
   }
// ------------------------------------------------------------------
// Look for a process message.
// ------------------------------------------------------------------
void VensimComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == processID)
      doTimestep();
   }
// ------------------------------------------------------------------
// Return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void VensimComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   string name = variables[queryData.ID];
   float value = 0.0;
   if (vensim_get_val(name.c_str(), &value) == 1)
      sendVariable(queryData, value);
   }
// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool VensimComponent::respondToSet(unsigned int& fromID, QuerySetValueData& setValueData)
   {
   string name = variables[setValueData.ID];
   string stringValue;
   setValueData.variant.unpack(stringValue);
   float value = StrToFloat(stringValue.c_str());

   string command = "SIMULATE>SETVAL|" + name + " = " + ftoa(value, 5);
   if (vensim_command(command.c_str()) == 0)
      return false;
   return true;
   }
// ------------------------------------------------------------------
// Perform the next iteration of the VENSIM model.
// ------------------------------------------------------------------
void VensimComponent::doTimestep(void)
   {
   // run VENSIM one more timestep.
   int result = vensim_command("GAME>GAMEON");
   if (result != 1)
      error("VENSIM has prematurely finished its simulation", true);
   }

