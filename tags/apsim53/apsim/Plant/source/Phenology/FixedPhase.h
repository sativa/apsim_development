#ifndef FIXEDPHASE_H
#define FIXEDPHASE_H

class FixedPhase : public pPhase
// A fixed duration phenological phase.
   {
   protected:

   public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      FixedPhase(ScienceAPI& scienceAPI, const string& stage_name)
         : pPhase (scienceAPI, stage_name){};
      virtual string description() const;
   };


#endif

