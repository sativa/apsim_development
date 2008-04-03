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
   void updateTTTargets();
   void onInit1(protocol::Component *);
      float TT();

 protected:
   interpolationFunction y_tt_post_anthesis;

};

#endif
