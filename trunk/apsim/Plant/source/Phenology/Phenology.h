#ifndef PhenologyH
#define PhenologyH

#include <vector>
#include "CompositePhase.h"
class plantInterface;

// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

// An abstract phenology class.
class Phenology : public plantThing
   {
   private:
      plantInterface& plant;
      std::vector<pPhase*> phases;             // The list of phases that this plant goes through

      typedef std::map<string, compositePhase> string2composite;
      mutable string2composite   composites;                    // Composite phases we know about

      float previousStage, currentStage, dltStage;
      std::string currentStageName;
      int   day_of_year;                                // Todays julian daynumber

      // Parameters
      std::vector<string>   iniSectionList;                  // list of sections to search in ini file

      virtual void setupTTTargets();

      int   das;
      float dlt_tt;
      float dlt_tt_phenol;
      float phase_fraction(float dlt_tt);               // return the fraction through the current phase we are in
      pPhase *getStage(const string &);
      float  stageCode (void);

      void get_stage(protocol::Component *, protocol::QueryValueData &);
      void get_stage_name(protocol::Component *, protocol::QueryValueData &);
      void get_stage_code(protocol::Component *, protocol::QueryValueData &);
      void get_phase_tt(protocol::Component *, protocol::QueryValueData &);
      void get_tt_tot(protocol::Component *, protocol::QueryValueData &);
      void get_days_tot(protocol::Component *, protocol::QueryValueData &);

      pPhase* find(const string& phase_name) const;
      void initialise();
      void clear();
      void onHarvest();
      void onEndCrop();
      void onKillStem();
      void zeroAll(void);

   public:
      Phenology(ScienceAPI& scienceAPI, plantInterface& p);
      ~Phenology();
      virtual void writeSummary();
      virtual void process();
      virtual void update(void);
      virtual void read();
      std::string description();
      bool on_day_of(const string &);

      bool inPhase(const string &) const;
      float ttInPhase(const string &phaseName) const;   //XX should be private
      float TTTargetInPhase(const string &phaseName) const;
      float ttInCurrentPhase(void);               //XX should be private

      float stageNumber(void) {return currentStage;};//XXX a bad thing

      std::string stageName(void);//xxxbad
      string previousStageName(void);//xxxbad

      float doInterpolation(interpolationFunction& f);
      float doLookup(const std::vector<float>& f);

      void onSetPhase(float resetPhase);
      virtual void onSow(protocol::Variant &v);
      virtual void onRemoveBiomass(float removeBiomPheno); // XX arg should be protocol::Variant &v

      virtual float get_dlt_tt(void);       // XX remove when leaf number development is finished
   };

#endif

