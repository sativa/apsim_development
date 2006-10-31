#ifndef PhotoPhase_H
#define PhotoPhase_H

class environment_t;

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
      PhotoPhase(const string& stage_name) : pPhase (stage_name){};
      void updateTTTargets(PlantPhenology &parent, const environment_t &e);
      virtual string description() const;
   };


#endif

