//---------------------------------------------------------------------------
#ifndef PatchInputComponentH
#define PatchInputComponentH
#include <ComponentInterface\Component.h>
#include "StringVariant.h"
#include <map>
#include <InputComponent.h>
class ApsimDataFile;
// ------------------------------------------------------------------
// This module patches an existing input component from data read in
// by this module.
// ------------------------------------------------------------------
class PatchInputComponent : public InputComponent
   {
   public:
      PatchInputComponent(void);
      ~PatchInputComponent(void);

      virtual void doInit1(const FString& sdml);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      unsigned preNewmetID;
      bool patchAllYears;
      typedef std::map<long, unsigned> PatchDates;
      PatchDates patchDates;
      unsigned currentRecord;
      unsigned minYear;
      unsigned maxYear;

      boost::gregorian::date advanceToTodaysPatchData(void);

      // ------------------------------------------------------------------
      // Read all patch dates.
      // ------------------------------------------------------------------
      void readPatchDates(void);


   };
#endif
