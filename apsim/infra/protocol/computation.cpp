//---------------------------------------------------------------------------
#include "computation.h"
#include "Loader.h"

#include <sstream>
#include <general\stl_functions.h>

// GLOBAL Callback function that is called when a line needs to be written
// to the log stream.  Declared in run.cpp
class IMessageCallbackClass
   {
   public:
      virtual void callback(const std::string& to,
                            const PROTOCOLMessage* msg) = 0;
   };
extern IMessageCallbackClass* MessageCallback;
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComputation::PROTOCOLComputation(IComponent* comp,
                                         const string& dllFileName,
                                         const string& sdl)
   : component(comp), ssdl(sdl)
   {
   // load the dll into memory
   PROTOCOLLoader loader;
   loader.loadComponent(dllFileName, dllInfo);
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComputation::~PROTOCOLComputation(void)
   {
   PROTOCOLLoader loader;
   loader.unloadComponent(dllInfo);
   }

// ------------------------------------------------------------------
//  Short description:
//    call the CREATE entry point.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComputation::create(void) const
   {
   if (MessageCallback)
      MessageCallback->callback(component->getName().c_str(),
                                &PROTOCOLMessage("create"));

   const IComputation* computation = this;
   const char* name = component->getName().c_str();
   (*(dllInfo.CREATEProc)) (name,
                            &dllInfo.instanceNo,
                            &computation,
                            ssdl.c_str(),
                            strlen(name),
                            ssdl.length());

   if (MessageCallback)
      MessageCallback->callback("", NULL);
   }
// ------------------------------------------------------------------
//  Short description:
//    call the INIT entry point.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComputation::initialise(void) const
   {
   if (MessageCallback)
      MessageCallback->callback(component->getName().c_str(),
                                &PROTOCOLMessage("init"));

   (*(dllInfo.INITProc)) (&dllInfo.instanceNo);

   if (MessageCallback)
      MessageCallback->callback("", NULL);
   }
// ------------------------------------------------------------------
//  Short description:
//    call the TERMINATE entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComputation::terminate(void) const
   {
   if (MessageCallback)
      MessageCallback->callback(component->getName().c_str(),
                                &PROTOCOLMessage("term"));

   (*(dllInfo.TERMProc)) (&dllInfo.instanceNo);

   if (MessageCallback)
      MessageCallback->callback("", NULL);
   }
// ------------------------------------------------------------------
//  Short description:
//    call the ACTION entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComputation::action(PROTOCOLMessage& anEvent) const
   {
   if (MessageCallback)
      MessageCallback->callback(component->getName().c_str(),
                                &anEvent);

   int Dummy = 0;
   (*(dllInfo.ACTIONProc)) (&dllInfo.instanceNo,
                            anEvent.action.f_str(),
                            &Dummy,
                            (void*) anEvent.data.f_str(),
                            &Dummy,
                            &Dummy,
                            anEvent.action.length(),
                            anEvent.data.length());

   if (MessageCallback)
      MessageCallback->callback("", NULL);
   }

// ------------------------------------------------------------------
//  Short description:
//    call the INEVENT entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLComputation::inEvent(PROTOCOLEvent& anEvent) const
   {
   if (MessageCallback)
      MessageCallback->callback(component->getName().c_str(),
                                &anEvent);

   int Dummy = 0;
   (*(dllInfo.INEVENTProc)) (&dllInfo.instanceNo,
                             anEvent.action.f_str(),
                             &Dummy,
                             (void*) anEvent.data.f_str(),
                             &Dummy,
                             &Dummy,
                             anEvent.action.length());

   if (MessageCallback)
      MessageCallback->callback("", NULL);
   }

