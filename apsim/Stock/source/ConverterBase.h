//---------------------------------------------------------------------------
#ifndef ConverterBaseH
#define ConverterBaseH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>

// ------------------------------------------------------------------
class ConverterBase : public protocol::Component
   {
   public:
      ConverterBase(protocol::Component *system);
      ~ConverterBase(void);
      virtual void doInit1(const FString& sdml) = 0;
      virtual void doInit2(void) = 0;
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData) = 0;
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant) = 0;

   private:
      protocol::Component *system;

   };

#endif
