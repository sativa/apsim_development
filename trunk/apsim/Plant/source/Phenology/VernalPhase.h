#ifndef VernalPhaseH
#define VernalPhaseH

class Environment;
class Output;

class VernalPhase : public pPhase
   // A phenological phase.
   {
      protected:
      interpolationFunction vernal_days;     // relate temperature to vernalisation
      interpolationFunction vernal_tt;       // Growing degree days to complete phase as a function of cum vd
      float dlt_cumvd;
      float cumvd;

      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      VernalPhase(ScienceAPI& scienceAPI, plantInterface* plant, const string& stage_name)
         : pPhase (scienceAPI, plant, stage_name){};
      void updateTTTargets(PlantPhenology &parent);
      void GetOutputs(std::vector <Output*> &Outputs);
      virtual void reset();
      virtual string description();
   };


#endif

