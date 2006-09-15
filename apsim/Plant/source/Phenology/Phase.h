#ifndef PLANTPHENOLOGYPHASE_H
#define PLANTPHENOLOGYPHASE_H

#include <string>

class PlantPhenology;
class environment_t;
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
     std::string  myName;  // Usually the name of the "stage" that the phase starts from.
     float tt,             // Thermal time spent in this phase
           target,         // Target time we want to spend here
           days;           // Number of days spent in this phase.
     bool empty;

   public:
     pPhase(const std::string& n) {myName = n; tt = target = days = 0.0; empty = true;};
     pPhase(const char *n) {myName = n; tt = target = days = 0.0; empty = true;};
     virtual ~pPhase() {};

     void  add(float dlt_days)               {days += dlt_days;};
     void  add(float dlt_days, float dlt_tt) {days += dlt_days; tt += dlt_tt;};
     void  add(float dlt_days, float dlt_tt, float *balance_days, float *balance_tt);
     void  setTarget(float value) {target = value;};
     float getTT(void) const       {return tt;};
     float getTTTarget(void) const {return target;};
     float getDays(void) const     {return days;};
     virtual void  reset(void)             {tt = target = days = 0.0; empty = true;};
     void  update(void)            {empty = false;};
     bool  isFirstDay(void) const  {return empty == true;};
     bool  isEmpty(void) const {return empty;};
     string name() const {return myName;}
     virtual string description() const {return "";};
     virtual void readCultivarParameters(protocol::Component *s, const string & cultivar){};
     virtual void readSpeciesParameters (protocol::Component *, std::vector<string> &){};
     virtual void updateTTTargets(PlantPhenology &parent, const environment_t &e){};
     virtual void onSow(protocol::ApsimVariant incomingApsimVariant){};
     virtual void setupTTTarget(void){};
     virtual void GetOutputs(std::vector <Output*> &Outputs){};
   };

bool operator == (const pPhase &a, const pPhase &b);


#endif

