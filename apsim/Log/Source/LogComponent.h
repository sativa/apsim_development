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
      LogComponent(void);
      ~LogComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void callback(const std::string& toName,
                            const protocol::Message* message);
   private:
      ofstream out;
      int previousNesting;
      int nesting;

      typedef std::pair<unsigned, unsigned> key;
      typedef std::map<key, std::string> Registrations;
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
      void writeRegistrationData(const protocol::Message* message, RegistrationType kind);
      void storeRegistration(const protocol::Message* message);
      void writeVariant(const protocol::Variant& variant);
      std::string formatType(std::string RegistrationTypeString);

   };

#endif
