#ifndef PhotoPhase_H
#define PhotoPhase_H

class Environment;

class PhotoPhase : public pPhase
   // A phenological phase.
   {
      protected:
      interpolationFunction photo_tt;       // Growing degree days to complete phase as a function of cum vd
      float photoperiod;
      float twilight;
      string cutoff;

      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      PhotoPhase(ScienceAPI& scienceAPI, plantInterface* p, const string& stage_name)
         : pPhase (scienceAPI, p, stage_name) {}
      void updateTTTargets(PlantPhenology &parent);
      virtual string description();
   };


#endif

