//---------------------------------------------------------------------------
#ifndef SummaryFileComponentH
#define SummaryFileComponentH
#include <fstream>
#include <component.h>
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the APSIM SUMMARY FILE module

//  Notes:

//  Changes:
//    DPH 25/7/2001

// ------------------------------------------------------------------
class SummaryFileComponent : public protocol::Component
   {
   public:
      SummaryFileComponent(void);
      ~SummaryFileComponent(void) { };
      virtual void doInit1(const FString& sdml);
   private:
      ofstream out;
      bool inDiaryState;
      unsigned summaryFileWriteID;
      unsigned tickID;
      unsigned prepareID;
      unsigned externalErrorID;
      int currentDate;
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      void writeLine(const FString& moduleName, const FString& line);
      void writeBanner(void);

   };

#endif
