#ifndef FruitCohortFNH
#define FruitCohortFNH

#include "Phenology/PlantPhenology.h"
#include "FruitCohort.h"
#include "GrainPartFN.h"
#include "PodPartFN.h"

class FruitCohortFN : public FruitCohort
{
   friend ostream &operator<<(ostream &, const FruitCohortFN &);
public:                                             // member functions
   FruitCohortFN(ScienceAPI& scienceAPI, plantInterface *p, const string &name);

   const FruitCohortFN &operator=(const FruitCohortFN &other);      // Assigment operator

   void process(void);
   void onInit1(protocol::Component *);

   void onPlantEvent(const string &);
   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);


   void doFruitNumber(void);
   float flowerNumber(void);
   void doDmDemand (float dlt_dm_supply_by_veg);
   float getStageNumber(void);
   bool  on_day_of(const string &what);

#if TEST_FruitCohortFN
   virtual ~FruitCohortFN();                          // destructor
#else
   ~FruitCohortFN();
#endif

private:



   /* system interface: */
////   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myGrainParts;
   vector <plantPart *> myVegParts;
   vector<plantPart *> supplyPools;

   fruitPodPartFN  *podPart;
   fruitGrainPart  *grainPart;
   PlantPhenology *fruitPhenology;

   bool  gHasreadconstants;
   ////float dmRetranslocate;
   float gDlt_dm;

   // Fruit cohort specific
      float     fruit_no;
      float     flower_no;
      float     fruit_sdr_daily[366];  //(max_fruit_cohorts,366)
      float     fruit_sdr;
      float     dlt_fruit_no;
      float     dlt_fruit_no_abort;
      float     dlt_dm_fruit_abort;  //(max_fruit_cohorts, max_part)
      float     dlt_dm_green_abort;  //(max_part)

         // fruit cohorts
   interpolationFunction fracPod;
   interpolationFunction start_to_end_grain;
   interpolationFunction sdr_min;
   interpolationFunction rel_grainfill;

   struct {
      float     dm_fruit_max;
      float     potential_fruit_filling_rate;
   } p; // Parameters

   struct {
      int         fruit_no_option;
      float       dm_abort_fract;
      float       fract_dm_fruit_abort_crit;
      float       fruit_phen_end;
      float       tt_flower_to_start_pod;
   } c; // Constants

};

#endif
