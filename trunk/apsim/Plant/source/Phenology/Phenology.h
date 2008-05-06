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
      pPhase *getStage(const string &);

      void get_stage(protocol::Component *, protocol::QueryValueData &);
      void get_stage_name(protocol::Component *, protocol::QueryValueData &);
      void get_stage_code(protocol::Component *, protocol::QueryValueData &);
      void get_phase_tt(protocol::Component *, protocol::QueryValueData &);
      void get_tt_tot(protocol::Component *, protocol::QueryValueData &);
      void get_days_tot(protocol::Component *, protocol::QueryValueData &);

      pPhase* find(const string& phase_name) const;
      void clear();
      void onHarvest();
      void onEndCrop();
      void onKillStem();
      void zeroAll();
      void initialise();
      string stageName();
      void onSetStage(float newStageNumber);

   public:
      Phenology(ScienceAPI& scienceAPI, plantInterface& p);
      ~Phenology();
      virtual void onInit1(protocol::Component *);
      virtual void writeSummary();
      virtual void process();
      virtual void read();
      std::string description();

      bool on_day_of(const string &);
      float fractionInCurrentPhase() const;
      bool inPhase(const string& phaseName) const;
      float ttInPhase(const string &phaseName) const;
      float TTTargetInPhase(const string &phaseName) const;
      float doInterpolation(externalFunction& f);
      float doLookup(const std::vector<float>& f);

      virtual void onSow(protocol::Variant &v);
      virtual void onRemoveBiomass(float removeBiomPheno);

      virtual float get_dlt_tt(void);       // XX remove when leaf number development is finished
   };

#endif

