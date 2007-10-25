#include <general/pch.h>
#ifdef __WIN32__
 #include <vcl.h>
#endif

#include <boost/function.hpp>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/ScienceAPI.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <map>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "Plant.h"

using namespace std;

extern "C" EXPORT void STDCALL getDescriptionInternal(char* initScript, char* description);

#ifdef WIN32
int WINAPI DllEntryPoint(HINSTANCE /*hinst*/, unsigned long /*reason*/, void*)
//=======================================================================================
// DLL entry point
   {
   return 1;
   }
#endif

extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
//=======================================================================================
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.
   {
   strcpy(wrapperDll, "");
   }


extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
//=======================================================================================
// Return component description info.
   {
   getDescriptionInternal(initScript, description);
   }

extern "C" EXPORT void STDCALL getDescriptionLength(char* initScript, int* length)
//=======================================================================================
// Return component description info.
   {
   char* buffer = new char[500000];
   getDescriptionInternal(initScript, buffer);
   *length = strlen(buffer);
   delete [] buffer;
   }


protocol::Component* createComponent(void)
//=======================================================================================
// Create an instance of the Plant module
   {
   return new PlantComponent;
   }

PlantComponent::PlantComponent()
//=======================================================================================
// Initialises the Plant component.
   {
   plant = NULL;
   }

PlantComponent::~PlantComponent(void)
//=======================================================================================
// Destructor
   {
   if (plant) delete plant;
   plant = NULL;
   }

void PlantComponent::doInit1(const FString& sdml)
//=======================================================================================
// Stage 1 initialisation.
   {
   protocol::Component::doInit1(sdml);

   string crop_type;
   scienceAPI().read("crop_type", crop_type);
   if (crop_type == "sorghum")
     throw std::invalid_argument("Sorghum is not in generic plant framework yet..");
   else
     plant = new Plant(this, scienceAPI());

//   if (plant) delete plant; plant= NULL;
   if (plant) plant->doInit1(this);
   }

void PlantComponent::doInit2(void)
//=======================================================================================
// Stage 2 initialisation.
   {
   protocol::Component::doInit2();
   if (plant) plant->doInit2(this);
   }

bool PlantComponent::respondToSet(unsigned int& /*fromID*/, protocol::QuerySetValueData& setValueData)
//=======================================================================================
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
   {
   if (plant) return (plant->respondToSet(/*fromID, */setValueData.ID, setValueData));
   return false;
   }

void PlantComponent::warningError (const char *msg)
//=======================================================================================
   {
   protocol::Component::error(msg, false);
   }

void PlantComponent::writeString (const char *line)
//=======================================================================================
   {
   protocol::Component::writeString(FString(line));
   }