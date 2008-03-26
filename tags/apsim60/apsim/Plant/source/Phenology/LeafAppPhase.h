#ifndef LeafAppPhase_H
#define LeafAppPhase_H

class environment_t;
class Output;

class LeafAppPhase : public pPhase
   // A phenological phase.
   {
      protected:
      float leaf_init_rate;
      float leaf_no_seed;
      float leaf_no_min;
      float leaf_no_max;
      float leaf_no_at_emerg;
      float final_leaf_no;

      interpolationFunction node_app;       // leaf node appearance as a function of leaf node number


      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      LeafAppPhase(ScienceAPI& scienceAPI, const string& stage_name)
         : pPhase (scienceAPI, stage_name){};
      void updateTTTargets(PlantPhenology &parent, const environment_t &e);
      virtual string description();
      virtual void reset();
      void GetOutputs(std::vector <Output*> &Outputs);
   };


#endif

