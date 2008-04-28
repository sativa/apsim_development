#ifndef PhotoPhaseH
#define PhotoPhaseH

class Environment;

class PhotoPhase : public pPhase
   {
   protected:
      interpolationFunction stressFunction;
      interpolationFunction photo_tt;       // Growing degree days to complete phase as a function of cum vd
      float photoperiod;
      float twilight;
      string cutoff;

      virtual float stress();

   public:
      void read();
      PhotoPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : pPhase (scienceAPI, p, stage_name) {}
      void updateTTTargets(Phenology &parent);
      virtual string description();
   };


#endif

