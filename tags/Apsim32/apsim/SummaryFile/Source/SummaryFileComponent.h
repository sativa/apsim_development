//---------------------------------------------------------------------------
#ifndef SummaryFileComponentH
#define SummaryFileComponentH
#include <fstream>
#include <ComponentInterface\Component.h>
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
      virtual void doInit2(void);
   private:
      ofstream out;
      bool inDiaryState;
      unsigned summaryFileWriteID;
      unsigned summaryFileID;
      unsigned tickID;
      unsigned prepareID;
      unsigned externalErrorID;
      unsigned titleID;
      unsigned componentsID;
      int currentDate;
      string fileName;

      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);

      void writeLine(const FString& moduleName, const FString& line);
      void writeBanner(void);
      void writeInfo(void);

   };

#endif
