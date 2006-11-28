#pragma hdrstop
#include <stdexcept>
#include <string>

#include <ComponentInterface2/CMPScienceAPI.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <general/platform.h>
#include <general/dll.h>
#include <map>

using namespace std;

struct Bit
   {
   CMPComponentInterface* componentInterface;
   CMPScienceAPI* scienceAPI;
   void* dllHandle;
   unsigned component;

   ~Bit()
      {
      delete componentInterface;
      delete scienceAPI;

      void STDCALL (*deleteComponent)(unsigned component);
      deleteComponent = (void STDCALL(*)(unsigned component)) dllProcAddress(dllHandle, "deleteComponent");
      deleteComponent(component);

      closeDLL(dllHandle);
      }
   };

// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL createInstance
   (const char* dllFileName,
    unsigned int* componentID,
    unsigned int* parentID,
    unsigned int* instanceNumber,
    unsigned int* callbackArg,
    CallbackType* callback)
   {
   Bit* bit = new Bit;

   // create a component interface and a science api that the component
   // will talk to.
   bit->componentInterface = new CMPComponentInterface(callbackArg, callback, *componentID, *parentID);
   bit->scienceAPI = new CMPScienceAPI(*bit->componentInterface);

   // go create an instance of our component by loading the correct dll
   // and calling a createComponent entry point.
   unsigned STDCALL (*createComponent)(ScienceAPI* scienceAPI);
   bit->dllHandle = loadDLL(dllFileName);
   createComponent = (unsigned STDCALL(*)(ScienceAPI* scienceAPI)) dllProcAddress(bit->dllHandle, "createComponent");
   bit->component = createComponent(bit->scienceAPI);

   // The instance number we return to the PM is a pointer to the component
   // object we just created.
   *instanceNumber = (unsigned) bit;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL deleteInstance (unsigned* instanceNumber)
   {
   Bit* bit = (Bit*) *instanceNumber;
   delete bit;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   Bit* bit = (Bit*) *instanceNumber;
   bit->componentInterface->messageToLogic(*message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL getDescriptionInternal(char* initScript,
                                                         char* description)
   {
/*   ApsimComponentData componentData(initScript);
   std::string dllFileName = componentData.getExecutableFileName();
   std::string instanceName = componentData.getName();
   //MessageBox(NULL, instanceName.c_str(), "", MB_OK);

   // create an instance of the module.
   unsigned dummy = 0;
   unsigned instanceNumber = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy, NULL);

   // call init1.
   Message* init1Message = newInit1Message(0, 0, initScript, instanceName.c_str(), true);
   bool processed;
   messageToLogic(&instanceNumber, init1Message, &processed);

   std::string compDescription = ((protocol::Component*) instanceNumber)->getDescription();
   strcpy(description, compDescription.c_str());

   // delete the instance.
   deleteInstance(&instanceNumber);
*/   }

