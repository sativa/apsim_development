#include "Component.h"
#include <ApsimShared\ApsimComponentData.h>

namespace protocol {
// ------------------------------------------------------------------
// The PM is instructing us to create an instance of all our data.
// ------------------------------------------------------------------
extern "C" _export void __stdcall createInstance
   (const char* dllFileName,
    const unsigned int* compID,
    const unsigned int* parentID,
    unsigned int* instanceNumber,
    const unsigned int* callbackArg,
    protocol::CallbackType* callback)
   {
   protocol::Component* component = ::createComponent();
   component->setup(dllFileName, *compID, *parentID, callbackArg, (FARPROC)callback);
   *instanceNumber = (unsigned) component;
   }
// ------------------------------------------------------------------
// The PM is instructing us to delete an instance of our data.
// ------------------------------------------------------------------
extern "C" _export void __stdcall deleteInstance (unsigned* instanceNumber)
   {
   delete (protocol::Component*) *instanceNumber;
   }
// ------------------------------------------------------------------
// All messages to component go through here.
// ------------------------------------------------------------------
extern "C" _export void __stdcall messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   ((protocol::Component*) *instanceNumber)->messageToLogic(message);
   *processed = true; // ???? not sure why we need this.
   }

// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescriptionInternal(char* initScript,
                                                         char* description)
   {
   ApsimComponentData componentData(initScript);
   string dllFileName = componentData.getExecutableFileName();
   string instanceName = componentData.getName();
   //MessageBox(NULL, instanceName.c_str(), "", MB_OK);

   // create an instance of the module.
   unsigned dummy = 0;
   unsigned instanceNumber = 0;
   createInstance(dllFileName.c_str(), &dummy, &dummy, &instanceNumber, &dummy, NULL);

   // call init1.
   Message* init1Message = newInit1Message(0, 0, initScript, instanceName.c_str(), true);
   bool processed;
   messageToLogic(&instanceNumber, init1Message, &processed);

   string compDescription = ((protocol::Component*) instanceNumber)->getDescription();
   strcpy(description, compDescription.c_str());

   // delete the instance.
   deleteInstance(&instanceNumber);
   }


} // namespace protocol