#include "StdPlant.h"

#include "Plant.h"
#include "PlantComponent.h"

using namespace std;

extern "C" EXPORT void STDCALL getDescriptionInternal(char* initScript, char* description);

#ifdef WIN32
#include <windows.h>
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

void PlantComponent::doInit1(const protocol::Init1Data& initData)
//=======================================================================================
// Stage 1 initialisation.
   {
   protocol::Component::doInit1(initData);

//   string crop_type;
//   scienceAPI().read("crop_type", crop_type);
//   if (crop_type == "sorghum")
//     throw std::invalid_argument("Sorghum is not in generic plant framework yet..");
//   else
     plant = new Plant(this, scienceAPI());

   if (plant) plant->onInit1();

   // Cruft for registering events in getComponentDescription - 
   addRegistration(::event, 0, "sowing", "<null/>");
   addRegistration(::event, 0, "harvesting", "<null/>");
   }

void PlantComponent::doInit2(void)
//=======================================================================================
// Stage 2 initialisation.
   {
   protocol::Component::doInit2();
   if (plant) plant->onInit2();
   }

void PlantComponent::warningError (const char *msg)
//=======================================================================================
   {
   protocol::Component::error(msg, false);
   }

void PlantComponent::writeString (const char *line)
//=======================================================================================
   {
   protocol::Component::writeString(line);
   }
