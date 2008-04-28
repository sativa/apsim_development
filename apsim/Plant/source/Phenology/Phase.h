#ifndef PhaseH
#define PhaseH

#include <string>

class Phenology;
class Environment;
class Output;

// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

/////////////////////////////////////////////////////////////////////////////////////////////
// A phenological phase.
class pPhase
   {
   protected:
     std::string myName;   // The name of the "stage" that the phase starts from.
     float tt,             // Thermal time spent in this phase
           target,         // Target time we want to spend here
           days;           // Number of days spent in this phase.
     bool empty;
     ScienceAPI& scienceAPI;
     plantInterface& plant;

     interpolationFunction y_tt;

     virtual float stress() {return 1.0;}  // no stress.

   public:
     pPhase(ScienceAPI& api, plantInterface& p, const std::string& n);
     virtual ~pPhase() {};

     virtual void process() {};
     virtual void OnSow(float sowing_depth) {};

     virtual void calcPhaseDevelopment(int das, 
                                       float& dlt_tt_phenol, float& phase_devel);

     void  add(float dlt_days, float dlt_tt) {days += dlt_days; tt += dlt_tt;};
     void  setTT(float value)     {tt = value;};
     virtual float TT();
     float getTT(void) const       {return tt;};
     float getTTTarget(void) const {return target;};
     float getDays(void) const     {return days;};
     virtual void  reset(void)             {tt = days = 0.0; empty = true;};
     void  update(void)            {empty = false;};
     bool  isFirstDay(void) const  {return empty == true;};
     bool  isEmpty(void)  {return empty;};
     string name(void) const {return myName;};
     virtual string description(void)  {return "";};
     virtual void read();
     virtual void updateTTTargets(Phenology &parent){};
     virtual void onSow(protocol::ApsimVariant incomingApsimVariant){};
     virtual void setupTTTarget(void){};
     virtual void GetOutputs(std::vector <Output*> &Outputs){};
   };

bool operator == (const pPhase &a, const pPhase &b);


#endif

