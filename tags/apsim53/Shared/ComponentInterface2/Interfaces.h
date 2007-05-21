#ifndef InterfacesH
#define InterfacesH

#include <ComponentInterface2/MessageData.h>

#include <stdexcept>
#include <string>
#include <vector>

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


class ArraySpecifier;
class IPackableData
   {
   public:
      std::string ddml;
      virtual ~IPackableData() { }
      virtual unsigned memorySize() = 0;
      virtual void pack(MessageData& messageData) = 0;
      virtual void unpack(MessageData& messageData, const std::string& ddml,
                          ArraySpecifier* arraySpec) = 0;
      virtual void setValue(const std::vector<std::string>& values) = 0;
   };

class IData
   {
   // -----------------------------------------------------------
   // This interface describes the necessary methods for any data
   // that can be transported from one module to another in APSIM
   // -----------------------------------------------------------
   public:
      virtual ~IData() = 0;
      virtual IData* clone() const = 0;
      virtual int asInteger() = 0;
      virtual float asSingle() = 0;
      virtual double asDouble() = 0;
      virtual std::string asString() = 0;
      virtual void asIntegerArray(std::vector<int>& values) = 0;
   };
class IDataRef
   {
   public:
      virtual ~IDataRef() = 0;
      IDataRef* clone();
   };

class IEventFunction
   {

   };


#endif

