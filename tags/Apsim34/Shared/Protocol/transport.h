//---------------------------------------------------------------------------
#ifndef transportH
#define transportH
#include <ComponentInterface\interfaces.h>
#include <vector>

namespace protocol {

class IComputation;
class Message;

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the transport layer for the simulation.  Its sole
//     purpose is to deliver messages to components.  The transport
//     layer has no knowledge of the simulation structure.

//  Notes:
//     The transport layer ALWAYS deletes the message after it has
//     been delivered.

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
class __declspec(dllexport) Transport : public ITransport
   {
   public:
      Transport(void) : messageHook(NULL) {}

      void deliverMessage(Message* message);
      void addComponent(unsigned int id,
                        const std::string& name,
                        IComputation* computation);

      void setMessageHook(IMessageHook* hook) {messageHook = hook;}

      static Transport& getTransport(void); // singleton global transport layer.

   private:
      std::vector<IComputation*> addressBook;
      std::vector<std::string> addressBookNames;
      IMessageHook* messageHook;

   };

void _export setMessageHook(IMessageHook* hook);

} // namespace protocol
#endif