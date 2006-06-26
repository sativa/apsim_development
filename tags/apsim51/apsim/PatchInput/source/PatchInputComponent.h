//---------------------------------------------------------------------------
#ifndef PatchInputComponentH
#define PatchInputComponentH

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
      unsigned getDataMethodID;
      unsigned returnDataMethodID;
      std::vector<std::string> patchVariablesLongTerm;
      typedef std::map<unsigned, protocol::newmetType> PatchData;
      PatchData patchData;
      bool haveReadPatchData;
      boost::gregorian::date patchDate;



      boost::gregorian::date advanceToTodaysPatchData(unsigned int fromID);

      // ------------------------------------------------------------------
      // Read all patch dates.
      // ------------------------------------------------------------------
      void readPatchDates(void);

      // ------------------------------------------------------------------
      // Read the same datalong term patch data for the dates in the patch file.
      // ------------------------------------------------------------------
      void readLongTermData(unsigned int fromID);

      // ------------------------------------------------------------------
      // Get matching variables from INPUT for the same dates as specified in our
      // patch data file.
      // ------------------------------------------------------------------
      void getDataFromInput(unsigned int fromID);

      // ------------------------------------------------------------------
      // Do a bunch of setVariables back to INPUT for all patchVariablesLongTerm.
      // ------------------------------------------------------------------
      void setPatchData();

   };
#endif
