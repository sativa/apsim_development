#ifndef ArbitratorH
#define ArbitratorH

#include "PlantInterface.h"

class rootPart;
class Arbitrator : public plantThing
   {
   private:
      int n_retrans_option;
      float _DMSupply;
      void doNRetranslocate(vector<plantPart*>& allParts, float g_grain_n_demand);

   protected:
      plantInterface& plant;
      vector <string> PartitionParts;
      vector <string> PartitionRules;

      virtual float ratioRootShoot() = 0;
      virtual float fracDMRemainingInPart(int partNumber) = 0;

   public:
      Arbitrator(ScienceAPI& scienceAPI, plantInterface& p);
      virtual ~Arbitrator(void) {};

      void partitionDM();
      void doDmRetranslocate(plantPart* stemPart, plantPart* leafPart,
                                     plantPart* fruitPart);
      void doNRetranslocate(vector<plantPart*>& allParts,
                                    plantPart* fruitPart);
      void doNPartition(float g_n_fix_pot, float nDemandTotal, float& nFixUptake);
      void doNSenescedRetrans(plantPart* leafPart);


      float DMSupply() {return _DMSupply;}
      float RelativeGrowthRate(void);
      float TotalPotentialGrowthRate(void);
   };

#endif

