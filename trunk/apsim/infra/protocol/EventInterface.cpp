//---------------------------------------------------------------------------
#include "EventInterface.h"
#include "computation.h"
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//     create an event interface

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export unsigned __stdcall ei_create
   (IComputation** computation)
   {
   return (unsigned) new EventInterface(*computation);
   }
// ------------------------------------------------------------------
//  Short description:
//     destroy an event interface

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_free
   (EventInterface** EI)
   {
   delete *EI;
   }

// ------------------------------------------------------------------
//  Short description:
//     Return the name of the current component.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_getname
   (EventInterface** EI, char* Name, long NameLength)
   {
   if (*EI != NULL)
      FString(Name, NameLength) = (*EI)->getComponentName().c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//    send and action to a component

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_sendaction
   (EventInterface** EI, char* ComponentName, char* Action, char* Data,
    long ComponentNameLength, long ActionLength, long DataLength)
   {
   (*EI)->sendAction (FString(ComponentName, ComponentNameLength),
                      FString(Action, ActionLength),
                      FString(Data, DataLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    return true if component exists.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export int __stdcall ei_existscomponent
   (EventInterface** EI, char* ComponentName, long ComponentNameLength)
   {
   return (*EI)->existsComponent(FString(ComponentName, ComponentNameLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    send an action to all component

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_broadcastaction
   (EventInterface** EI, char* Action, char* Data,
    long ActionLength, long DataLength)
   {
   (*EI)->broadcastAction (FString(Action, ActionLength),
                           FString(Data, DataLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    publish an event.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_publishevent
   (EventInterface** EI, char* EventName, long EventNameLength)
   {
   (*EI)->publishEvent (FString(EventName, EventNameLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    get a variable from another component

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export int __stdcall ei_getvariable
   (EventInterface** EI, char* variableName, long variableNameLength)
   {
   return (*EI)->getVariable (FString(variableName, variableNameLength));
   }
// ------------------------------------------------------------------
//  Short description:
//    set the value of a variable in another component

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export int __stdcall ei_setvariable
   (EventInterface** EI, char* variableName, long variableNameLength)
   {
   return (*EI)->setVariable (FString(variableName, variableNameLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    set a variable in another component.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export int __stdcall ei_sendvariable
   (EventInterface** EI, char* variableName, long variableNameLength)
   {
   return (*EI)->setVariable (FString(variableName, variableNameLength));
   }


// ------------------------------------------------------------------
//  Short description:
//    signal that a message was used.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_setcomponentresponded
   (EventInterface** EI, int* MessageIsUsed)
   {
   (*EI)->setComponentResponded (*MessageIsUsed);
   }

// ------------------------------------------------------------------
//  Short description:
//    signal that a message was used.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export int __stdcall ei_componentresponded
   (EventInterface** EI)
   {
   return (*EI)->componentResponded();
   }

// ------------------------------------------------------------------
//  Short description:
//    signal that a message was used.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_registersubscribedevent
   (EventInterface** EI, const char* eventName, const long eventNameLength)
   {
   (*EI)->registerSubscribedEvent(FString(eventName, eventNameLength));
   }

// ------------------------------------------------------------------
//  Short description:
//    signal that a message was used.

//  Notes:

//  Changes:
//    DPH 21/5/98

// ------------------------------------------------------------------
extern "C" _export void __stdcall ei_changecomponentorder
   (EventInterface** EI, char* FComponents, int* NumElements, long For_st_len)
   {
   vector<string> components;
   FStrings(FComponents, For_st_len, *NumElements, *NumElements).toC(components);
   (*EI)->changeComponentOrder(components);
   }


