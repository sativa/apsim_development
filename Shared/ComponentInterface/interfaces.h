#ifndef InterfacesH
#define InterfacesH

//#include "IConfiguration.h"

#include <string>

namespace protocol {

class Message;
class IMessageHook
   {
   public:
      virtual void callback(const std::string& toName,
                            const protocol::Message* message) = 0;
   };

// ------------------------------------------------------------------
//  Short description:
//    Interface for a simulation.  This is the top-level starting point
//    for all simulations.

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class ISimulation
   {
   public:
      virtual ~ISimulation(void) { };

      // go execute the simulation
      virtual void go(const std::string& simFilename) = 0;

      // set a hook into the message stream.
      virtual void setMessageHook(IMessageHook* hook) = 0;

   };


// ------------------------------------------------------------------
//  Short description:
//    Interface for a computation.  A computation is looks after
//    all communications with a component executable.  In APSIM
//    terms this is a DLL.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class IComputation
   {
   public:
      virtual ~IComputation(void) { };

      virtual bool isOk(void) const = 0;
      virtual void messageToLogic(const Message* Message) const = 0;
      virtual std::string getExecutable(void) = 0;
   };
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the transport layer for the simulation.  Its sole
//     purpose is to deliver messages to components.  The transport
//     layer has no knowledge of the simulation structure.

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
class ITransport
   {
   public:
      virtual ~ITransport(void) { }
      virtual void deliverMessage(Message* message) = 0;
      virtual void addComponent(unsigned int id,
                                const std::string& name,
                                IComputation* computation) = 0;
      virtual void setMessageHook(IMessageHook* hook) = 0;
   };


} // namespace protocol
#endif

