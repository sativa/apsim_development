#include "Component.h"

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

} // namespace protocol