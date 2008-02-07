#ifndef FruitCohortFNH
#define FruitCohortFNH

#include "Phenology/PlantPhenology.h"
#include "CompositePart.h"
#include "GrainPart.h"
#include "GrainPartGN.h"
#include "GrainPartHI.h"
#include "PodPart.h"

class FruitCohortFN : public CompositePart
{
   friend ostream &operator<<(ostream &, const FruitCohortFN &);
public:                                             // member functions
   FruitCohortFN(ScienceAPI& scienceAPI, plantInterface *p, const string &name);

   const FruitCohortFN &operator=(const FruitCohortFN &other);      // Assigment operator

   void checkBounds(void);
   void prepare(void);
   void process(void);
   void onInit1(protocol::Component *);

   void get_head_wt(protocol::Component *, protocol::QueryValueData &);
   void get_head_n(protocol::Component *, protocol::QueryValueData &);

   void get_head_p(protocol::Component *, protocol::QueryValueData &qd);

   void doNDemand1(float, float);
   void doNDemand1Pot(float, float);

   float dmGreenGrainTotal(void);

   float nMax(void);
   float pMaxPot(void);
   float pMinPot(void);

   void onPlantEvent(const string &);
   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);


   virtual void display(ostream &os = cout) ;  // display function
   void doDmDemand (float dlt_dm_supply_by_veg);

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

   fruitPodPart  *podPart;
   fruitGrainPart  *grainPart;
   PlantPhenology *fruitPhenology;

   bool  gHasreadconstants;
   ////float dmRetranslocate;
   float gDlt_dm;

   // Fruit cohort specific
      float     fruit_no;
      float     fruit_flower_no;
      float     fruit_sdr_daily[366];  //(max_fruit_cohorts,366)
      float     fruit_sdr;
      float     dlt_fruit_no;
      float     dlt_fruit_no_abort;
      float     dlt_dm_fruit_abort;  //(max_fruit_cohorts, max_part)
      float     dlt_dm_green_abort;  //(max_part)

         // fruit cohorts
   interpolationFunction fruit_sites_per_node;
   interpolationFunction frac_pod;
   interpolationFunction start_to_end_grain;
   interpolationFunction sdr_min;

   struct {
      float    dm_fruit_set_crit;
      float    dm_fruit_set_min;
      float     dm_fruit_max;
      float     potential_fruit_filling_rate;
      float     cutout_fract;
   } p; // Parameters

   interpolationFunction rel_fruit_site;
   struct {
      int         fruit_no_option;
      int         days_assimilate_ave;
      float       dm_abort_fract;
      float       fract_dm_fruit_abort_crit;
      float       fruit_phen_end;
      float       tt_flower_to_start_pod;
   } c; // Constants
};

#endif
