
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef FruitCohort_H
#define FruitCohort_H

#include "CompositePart.h"
#include "GrainPart.h"
#include "PodPart.h"

class FruitCohort : public CompositePart
{
   friend ostream &operator<<(ostream &, const FruitCohort &);
public:												// member functions
   FruitCohort(plantInterface *p, const string &name);

   const FruitCohort &operator=(const FruitCohort &other);		// Assigment operator

   void doInit1();
   void checkBounds(void);
   void doRegistrations(protocol::Component *);
   float availableRetranslocateN(void);

   void get_head_wt(protocol::Component *, protocol::QueryValueData &);
   void get_head_n(protocol::Component *, protocol::QueryValueData &);

   void get_pod_n(protocol::Component *, protocol::QueryValueData &);
   void get_pod_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_head_p(protocol::Component *, protocol::QueryValueData &qd);



   void doNDemand1(float, float);
   void doNDemand1Pot(float, float);

   void doNSenescedRetrans(float navail, float n_demand_tot);   //remove  problem

   float grainNConcPercent(void);       //remove  problem

   float dmGrainTotal(void);
   float dmVegTotal(void);
   float dmGreenGrainTotal(void);
   float dmGreenVegTotal(void);
   float dmSenescedVegTotal(void);
   float dmDeadVegTotal(void);

   float nGrainTotal(void);
   float nVegTotal(void);
   float nGreenGrainTotal(void);
   float nGreenVegTotal(void);
   float nSenescedVegTotal(void);
   float nDeadVegTotal(void);
   float nDemandGrain2(void);          //remove  problem
   float nCapacity(void);
////   void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);

   float nMaxPot(void);
   float nMax(void);
   float nMinPot(void);
   float pGrainTotal(void);
   float pVegTotal(void);
   float pGreenGrainTotal(void);
   float pDeadGrainTotal(void);
   float pGreenVegTotal(void);
   float pSenescedGrainTotal(void);     //remove
   float pSenescedVegTotal(void);
   float pDeadVegTotal(void);
   float pConcGrain(void);              //remove
   float pConcGrainTotal(void);        //remove
   float pMaxPot(void);
   float pMinPot(void);



   virtual void display(ostream &os = cout) const;	// display function
   float calcCover (float canopy_fac);                  // return pod cover   //FIXME  //remove problem
   void doDmDemand (float dlt_dm_supply_by_veg);
   void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);

#if TEST_FruitCohort
   virtual ~FruitCohort();							// destructor
#else
   ~FruitCohort();
#endif

private:



   /* system interface: */
////   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myGrainParts;
   vector <plantPart *> myVegParts;
   vector<plantPart *> supplyPools;

   fruitPodPart  *podPart;
   fruitGrainPart  *grainPart;

   bool  gHasreadconstants;
   float dmRetranslocate;
   float gDlt_dm;

};

#endif
