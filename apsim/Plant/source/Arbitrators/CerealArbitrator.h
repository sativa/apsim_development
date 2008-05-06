#ifndef CerealArbitratorH
#define CerealArbitratorH

#include "PlantInterface.h"
#include "arbitrator.h"

class cerealArbitrator : public Arbitrator
   {
  private:
   interpolationFunction y_frac_leaf;                // fraction of remaining dm allocated to leaves
   interpolationFunction y_ratio_root_shoot;         // root:shoot ratio of new dm ()
   int   num_stage_no_partition;

  public:
   cerealArbitrator(ScienceAPI& scienceAPI, plantInterface *p)
      : Arbitrator(scienceAPI, p) {};
   ~cerealArbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,vector <plantPart *>& Parts, string FruitName);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };



#endif

