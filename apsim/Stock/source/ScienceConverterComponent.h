//---------------------------------------------------------------------------
#ifndef ScienceConverterComponentH
#define ScienceConverterComponentH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
// ------------------------------------------------------------------
// TRACKER component for APSIM.
// eg of parameter file specification:
//    sum(rain)[1jan-31dec]
//    sum(rain)[sow-harvest]
//    sum(rain)[3]
// ------------------------------------------------------------------
class ScienceConverterComponent : public protocol::Component
   {
   public:
      ScienceConverterComponent(void);
      ~ScienceConverterComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      unsigned day_lengthID;
      unsigned dayLengthID;
      unsigned plant2stockID;
      unsigned removeHerbageID;
   };


#endif
