//---------------------------------------------------------------------------
#include "Run.h"
#include "Coordinator.h"

#pragma package(smart_init)

// GLOBAL Callback function that is called when a line needs to be written
// to the log stream
IMessageCallbackClass* MessageCallback;

// DPH - need to remove this - see coordinator::publishevent.
CallbackFunction<int>* TickCallback;

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLRun::PROTOCOLRun(void)
   {
   masterPM = NULL;
   MessageCallback = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     set the configuration for the run.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::setConfiguration(ISimulationConfiguration* config)
   {
   simConfiguration = config;
   }

// ------------------------------------------------------------------
//  Short description:
//     return the name of the simulation

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
string PROTOCOLRun::getName(void)
   {
   if (simConfiguration != NULL)
      return simConfiguration->getSimulationName();
   else
      return "";
   }

// ------------------------------------------------------------------
//  Short description:
//    initialise this run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool PROTOCOLRun::init (void)
   {
   // create a Master PM and initialise it.
   masterPM = new PROTOCOLCoordinator("main",
                                      NULL,
                                      simConfiguration->getISystem("main"));
   masterPM->create();
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//     terminate the run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::term(void)
   {
   if (masterPM != NULL)
      {
      masterPM->term();
      delete masterPM;
      masterPM = NULL;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    start this run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::start(void)
   {
   masterPM->start();
   }
// ------------------------------------------------------------------
//  Short description:
//    finish this run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::finish(void)
   {
   masterPM->finish();
   }

// ------------------------------------------------------------------
//  Short description:
//    set the global log callback function

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::setMessageCallback(IMessageCallbackClass* callback)
   {
   MessageCallback = callback;
   }

// ------------------------------------------------------------------
//  Short description:
//    set the global tick callback function

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLRun::setTickCallback(CallbackFunction<int>* callback)
   {
   TickCallback = callback;
   }

// ------------------------------------------------------------------
//  Short description:
//     return a masterPM component to caller.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
const PROTOCOLRun::Component PROTOCOLRun::getMasterPM(void) const
   {
   return Component(masterPM);
   }

// ------------------------------------------------------------------
//  Short description:
//     'Component' source

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class IComponentCallback : public ConstCallbackFunction<const IComponent*>
   {
   public:
      ConstCallbackFunction<const PROTOCOLRun::Component&>& f;
      IComponentCallback(ConstCallbackFunction<const PROTOCOLRun::Component&>& fun)
         : f(fun) { }
      virtual void callback(const IComponent* icomp) const
         {
         f.callback(PROTOCOLRun::Component(icomp));
         }
   };

void PROTOCOLRun::Component::enumerateComponents
   (ConstCallbackFunction<const PROTOCOLRun::Component&>& f) const
   {
   const ICoordinator* coordinator = dynamic_cast<const ICoordinator*> (comp);
   if (coordinator != NULL)
      coordinator->enumerateComponents (IComponentCallback(f));
   }

string PROTOCOLRun::Component::getName(void) const
   {
   return comp->getName();
   }
string PROTOCOLRun::Component::getFilename(void) const
   {
   return comp->getFilename();
   }
