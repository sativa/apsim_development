//---------------------------------------------------------------------------
#ifndef LogComponentH
#define LogComponentH
#include <fstream>
#include <ComponentInterface\interfaces.h>
#include <ComponentInterface\component.h>
#include <map>
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the APSIM LOG module

//  Notes:

//  Changes:
//    DPH 25/7/2001

// ------------------------------------------------------------------
class LogComponent : public protocol::Component,
                     public protocol::IMessageHook
   {
   public:
      LogComponent(void) { };
      ~LogComponent(void) { };
      virtual void doInit1(const FString& sdml);
      virtual void callback(const std::string& toName,
                            const protocol::Message* message);
   private:
      ofstream out;

      typedef std::map<unsigned int, std::string> Registrations;
      struct RegComponent
         {
         Registrations registrations;
         };
      typedef std::map<unsigned int, RegComponent> Components;
      Components components;

      void writeMessage(const std::string& toName,
                        const protocol::Message* message,
                        std::ostream& out);
      void writeMessageData(const protocol::Message* message);
      void writeRegistrationData(const protocol::Message* message);
      void storeRegistration(const protocol::Message* message);
      void writeVariant(const protocol::Variant& variant);

   };

#endif
