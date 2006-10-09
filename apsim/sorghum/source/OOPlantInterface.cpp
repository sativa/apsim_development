//---------------------------------------------------------------------------

#include <general/pch.h>
#ifdef __WIN32__
#include <vcl.h>
#endif

#include <boost/function.hpp>
#pragma hdrstop

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <map>

#include "OOPlant.h"
#include "OOPlantInterface.h"

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <iostream.h>
#include <boost/function.hpp>
#include <boost/bind.hpp>


#include <ComponentInterface/datatypes.h>
#include <ApsimShared/FStringExt.h>



using namespace std;


//-----------------------------------------------------------------------------
// DLL entry point
//-----------------------------------------------------------------------------
#ifdef __WIN32__
int WINAPI DllEntryPoint(HINSTANCE /*hinst*/, unsigned long /*reason*/, void*)
   {
   return 1;
   }
#endif
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }

//-----------------------------------------------------------------------------
// Create an instance of the Plant module
//-----------------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new PlantInterface;
   }
//-----------------------------------------------------------------------------
// Initialises the Plant component.
//-----------------------------------------------------------------------------
PlantInterface::PlantInterface()
   {
   }
//-----------------------------------------------------------------------------
// Destructor
//-----------------------------------------------------------------------------
PlantInterface::~PlantInterface(void)
   {
   delete plant;
   }
//-----------------------------------------------------------------------------
// Stage 1 initialisation.
//-----------------------------------------------------------------------------
void PlantInterface::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);
   plant = new OOPlant(this);
   plant->plantInit();
   }
//-----------------------------------------------------------------------------
// Stage 2 initialisation.
//-----------------------------------------------------------------------------
void PlantInterface::doInit2(void)
   {
   protocol::Component::doInit2();
   }


// Look for a messages we have an interest in.
//-----------------------------------------------------------------------------
//void PlantInterface::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID,
//                                                      protocol::Variant& variant)
//   {
//   plant->doEvent(eventID, variant);
//   }
//-----------------------------------------------------------------------------
//void PlantInterface::respondToMethod(unsigned int& /*fromID*/, unsigned int& eventID,
//                                                      protocol::Variant& variant)
//   {
//   plant->doEvent(eventID, variant);
//   }
//-----------------------------------------------------------------------------
// Return a variable to caller.
//-----------------------------------------------------------------------------
//void PlantInterface::respondToGet(unsigned int& /*fromID*/,
/*                                          protocol::QueryValueData& queryData)
   {
   plant->getVariable(queryData);
   } */
//-----------------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
//-----------------------------------------------------------------------------
bool PlantInterface::respondToSet(unsigned int& /*fromID*/,
                                    protocol::QuerySetValueData& setValueData)
   {
   return (plant->setVariable(setValueData.ID, setValueData));
   }
//-----------------------------------------------------------------------------
// Register a variable to the rest of the system
// returns an identifier for later use..
//-----------------------------------------------------------------------------
/*unsigned int PlantInterface::registerVar(const char *systemName,
                                 string varType,bool isArray,const char *units)
   {
   // Build the xml fragment that describes this variable
   string msg = varType + " array=\"";
   msg += (isArray) ? "T" : "F";
   msg += "\" units=\"(" + string(units) + ")\"/>";

   return (this->addRegistration(protocol::respondToGetReg, systemName, msg.c_str()));
   }   */
//-----------------------------------------------------------------------------
void PlantInterface::warningError (const char *msg)
{
  error(msg, false);
}

void PlantInterface::writeString (const char *line)
{
  protocol::Component::writeString(FString(line));
}
