//---------------------------------------------------------------------------
#ifndef RunH
#define RunH
#include "interfaces.h"
#include "protocolexport.h"
#include <general\stl_functions.h>

class IMessageCallbackClass
   {
   public:
      virtual void callback(const std::string& to,
                    const PROTOCOLMessage* msg) = 0;
   };

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a top-level run.  It is passed a configuration
//     and a simulation and sets the simulation running.  Exceptions
//     are thrown when an error occurs.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLRun : public IRun
   {
   public:
      PROTOCOLRun(void);

      // set the configuration and simulation for the run.
      void setConfiguration(ISimulationConfiguration* simConfiguration);

      // return the name of the run
      virtual std::string getName(void);

      // initialise this run
      virtual bool init (void);

      // terminate this run
      virtual void term (void);

      // start the run
      virtual void start(void);

      // finish the run
      virtual void finish(void);

      // use this component class when traversing the components in the systems.
      class PROTOCOL_EXPORT Component
         {
         public:
            Component(const IComponent* c)
               : comp(c) { }

            std::string getName(void) const;
            std::string getFilename(void) const;
            void enumerateComponents(ConstCallbackFunction<const Component&>& f) const;

         private:
            const IComponent* comp;
         };
      const Component getMasterPM(void) const;

      // set message callback function.  Callback is called just prior to a
      // message begin sent.
      void setMessageCallback(IMessageCallbackClass* MessageCallback);

      // dph - need to remove this - see coordinator::publishevent
      void setTickCallback(CallbackFunction<int>* callback);

   private:
      ISimulationConfiguration* simConfiguration;
      ICoordinator* masterPM;

   };
#endif
