//---------------------------------------------------------------------------
#ifndef ScreenComponentH
#define ScreenComponentH
#include <string>
#include <vector>
#include <ComponentInterface\Component.h>
// ------------------------------------------------------------------
// Screen APSIM component.
// ------------------------------------------------------------------
class ScreenComponent : public protocol::Component
   {
   public:
      ScreenComponent(void);
      ~ScreenComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      unsigned screenOutput;
      unsigned tickID;
      unsigned prepareID;
      unsigned externalErrorID;
      unsigned titleID;
      unsigned summaryFileWriteID;
      unsigned startDateID;
      unsigned endDateID;
      unsigned startDateJDay, endDateJDay;
      unsigned currentDate;
      bool inDiaryState;
      int updateInterval;
      
      void getStartEndDate(void);
      void writeLine(const FString& componentName, const FString& lines);

   };


#endif
