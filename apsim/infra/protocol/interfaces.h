#ifndef InterfacesH
#define InterfacesH

#include "BaseTypes.h"
#include "IConfiguration.h"
#include "Event.h"
#include <general\stl_functions.h>
#include <list>
#include <vector>

// ------------------------------------------------------------------
//  Short description:
//    Interface for a run.  This is the top-level starting point
//    for all simulations.  It is passed a configuration and a
//    simulation, initialises them both and gets the simulation
//    started.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class IRun
   {
   public:
      virtual ~IRun(void) { };

      // return the name of the run
      virtual std::string getName(void) = 0;

      // initialise this run
      virtual bool init (void) = 0;

      virtual void term(void) = 0;

      // start the run
      virtual void start(void) = 0;

      // finish the run
      virtual void finish(void) = 0;
   };


class IComponent;
class IComputation /*: public IExecute */
   {
   public:
      virtual ~IComputation(void) { };
      virtual void init(void) = 0;
      virtual IComponent* getComponent(void) const = 0;

      virtual void idle(void) const = 0;
      virtual void create(void) const = 0;
      virtual void initialise(void) const = 0;
      virtual void terminate(void) const = 0;
      virtual void action(PROTOCOLMessage& Event) const = 0;
      virtual void inEvent(PROTOCOLEvent& Event) const = 0;
      virtual void outEvent(PROTOCOLEvent& Event) const = 0;
      virtual void complete(PROTOCOLMessage& Event) const = 0;
   };

class ICoordinator;
class IComponent /* : public ICompABS */
   {
   public:
      virtual ~IComponent(void) { }
      virtual std::string getName(void) const = 0;
      virtual std::string getFilename(void) const = 0;
      virtual ICoordinator* getCoordinator(void) = 0;
      virtual void init(void) = 0;
      virtual void term(void) = 0;
      virtual const IComputation* getComputation(void) const = 0;
   };

// ------------------------------------------------------------------
//  Short description:
//    This interface provides the functionality to manage a "system".
//    A "system" is a group of components.  It provides methods to
//    locate components and component addresses, names of all components.
//    It provides the functionality to send messages and events between
//    components within the system.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class ICoordinator : virtual public IComponent
   {
   public:
      virtual ~ICoordinator(void) { };
      virtual void init(void) = 0;
      virtual void term(void) = 0;
      virtual void start(void) = 0;
      virtual void finish(void) = 0;

      // return true if component exists in system.
      virtual bool existsComponent(const FString& componentName) = 0;

      virtual bool sendMessage(const FString& toAddress,
                               PROTOCOLMessage& aMsg) const = 0;
      virtual void broadcastMessage(PROTOCOLMessage& aMsg) const = 0;
      virtual void publishEvent(PROTOCOLEvent& anEvent) const = 0;
      virtual bool sendMessageToFirst(PROTOCOLMessage& aMsg) = 0;
      virtual void enumerateComponents
         (ConstCallbackFunction<const IComponent*>& f) const = 0;

      virtual void registerSubscribedEvent(FString& eventName,
                                           FString& componentName) = 0;

      // APSIM kludge - used for getvariable and setvariable - remove!
      bool componentResponded;

      // reorder the component's - used for competition.
      // will eventually be removed.
      virtual void changeComponentOrder(std::vector<std::string>& componentsToChange) = 0;


   };


#endif

