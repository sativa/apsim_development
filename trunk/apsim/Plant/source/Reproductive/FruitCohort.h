#ifndef FruitCohortH
#define FruitCohortH

#include "CompositePart.h"
#include "GrainPart.h"
#include "GrainPartGN.h"
#include "GrainPartHI.h"
#include "PodPart.h"

class FruitCohort : public CompositePart
{
   friend ostream &operator<<(ostream &, const FruitCohort &);
public:                                             // member functions
   FruitCohort(ScienceAPI& scienceAPI, plantInterface *p, const string &name);

   const FruitCohort &operator=(const FruitCohort &other);      // Assigment operator

   void checkBounds(void);
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

   virtual void display(ostream &os = cout) ;  // display function
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

   bool  gHasreadconstants;
   float gDlt_dm;

};

#endif
