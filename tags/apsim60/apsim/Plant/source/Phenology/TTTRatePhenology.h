#ifndef TTTRATEPHENOLOGY_H
#define TTTRATEPHENOLOGY_H

#include "TTTPhenology.h"

class TTTRatePhenology : public TTTPhenology {
 private:

   void get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd);

 public:
   TTTRatePhenology(ScienceAPI& scienceAPI, plantInterface *p)
      : TTTPhenology(scienceAPI, p) {};
   void readSpeciesParameters (protocol::Component *, std::vector<string> &); // read species parameters
   //void process(const environment_t &e, const pheno_stress_t &ps);
   //void setupTTTargets(void);
   void updateTTTargets(const environment_t &e);
   void onInit1(protocol::Component *);
      float TT(const environment_t &e);

 protected:
   interpolationFunction y_tt_post_anthesis;

};

#endif
