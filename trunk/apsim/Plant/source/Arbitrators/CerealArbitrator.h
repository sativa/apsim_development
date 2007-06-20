#ifndef CEREALARBITRATOR_H
#define CEREALARBITRATOR_H

#include "PlantInterface.h"
#include "Arbitrator.h"

class cerealArbitrator : public Arbitrator
   {
  private:
   float x_stage_no_partition[max_table];
   float y_frac_leaf[max_table];                     // fraction of remaining dm allocated to leaves
   float y_ratio_root_shoot[max_table];              // root:shoot ratio of new dm ()
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

