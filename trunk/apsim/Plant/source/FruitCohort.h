
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef FruitCohort_H
#define FruitCohort_H

////#include "Phenology/PlantPhenology.h"
#include "CompositePart.h"
#include "GrainPart.h"
#include "GrainPartGN.h"
#include "GrainPartHI.h"
#include "PodPart.h"

class FruitCohort : public CompositePart
{
   friend ostream &operator<<(ostream &, const FruitCohort &);
public:                                             // member functions
   FruitCohort(plantInterface *p, const string &name);

   const FruitCohort &operator=(const FruitCohort &other);      // Assigment operator

   void doInit1(protocol::Component *system);
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


//   float grainWaterContent(void) const;
   float dmGrainTotal(void) const;
   float dmGrainWetTotal(void) const;
   float dmVegTotal(void) const;
   float dmGreenGrainTotal(void) const;
   float dmGreenVegTotal(void)const;
   float dmSenescedVegTotal(void)const;
   float dmDeadVegTotal(void)const;

   float nGrainTotal(void)const;
   float nVegTotal(void)const;
   float nGreenGrainTotal(void)const;
   float nGreenVegTotal(void)const;
   float nSenescedVegTotal(void)const;
   float nDeadVegTotal(void)const;
   float nCapacity(void);

   float nMaxPot(void)const;
   float nMax(void)const;
   float nMinPot(void)const;
   float pGrainTotal(void)const;
   float pVegTotal(void)const;
   float pGreenGrainTotal(void)const;
   float pDeadGrainTotal(void)const;
   float pGreenVegTotal(void)const;
   float pSenescedVegTotal(void)const;
   float pDeadVegTotal(void)const;
   float pMaxPot(void)const;
   float pMinPot(void)const;



   virtual void display(ostream &os = cout) const;  // display function
   void doDmDemand (float dlt_dm_supply_by_veg);

#if TEST_FruitCohort
   virtual ~FruitCohort();                          // destructor
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
////   PlantPhenology *fruitPhenology;

   bool  gHasreadconstants;
   float dmRetranslocate;
   float gDlt_dm;

};

#endif
