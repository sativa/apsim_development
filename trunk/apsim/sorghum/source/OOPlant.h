#ifndef OOPLANT_H_
#define OOPLANT_H_

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>

#include "OORoots.h"
#include "OOLeaf.h"
#include "OOStem.h"
#include "OORachis.h"
#include "OOGrain.h"

#include "OONitrogen.h"
#include "OOPhosphorus.h"
#include "OOPhenology.h"
#include "OOWater.h"
#include "OOBiomass.h"
//#include "o_GenePhenology.h"

#define setupEvent(s,name,type,address) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   s->addEvent(name, type, fn);\
   }

#define setupGetFunction(s,name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }


//------------------------------------------------------------------------------------------------

typedef bool (OOPlant::*ptr2setFn) (protocol::QuerySetValueData&);
//typedef void (Plant::*ptr2EventFn) (unsigned int &, protocol::Variant&);

//typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
//typedef std::map<unsigned, ptr2EventFn> UInt2EventFnMap;

typedef std::map<unsigned, ptr2setFn>   UInt2SetFnMap;
typedef std::map<unsigned, string>      UInt2StringMap;




typedef enum {aFloat, aInt, aString} aType;
/*typedef struct VAR
   {
   void *ptr;
   int type;
   string name;
   }VarInfo; */

//typedef std::map<unsigned, VarInfo *>   VariableMap;

//------------------------------------------------------------------------------------------------
//---------------- PLANT CLASS

//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.

class OOPlant
   {
   private:
   /* system interface: */
 //  UInt2EventFnMap   IDtoEventFn;   /* events */
   UInt2SetFnMap     IDtoSetFn;     /* setVariable */
 //  VariableMap       vMap;          /* getVariable */


   float stage;

   public:
   OOPlant(PlantInterface *P);
   ~OOPlant();

   PlantInterface *plantInterface;            // for interface calls to system
   // Plant sub-classes
   Roots *roots;
   Leaf *leaf;
   Stem *stem;
   Rachis *rachis;
   Grain *grain;

   Nitrogen *nitrogen;
   Phosphorus *phosphorus;
   Water *water;
   Phenology *phenology;
   Biomass *biomass;

   vector<PlantComponent *> PlantComponents;
   vector<PlantPart *> PlantParts;
   vector<PlantProcess *> PlantProcesses;


   Today  today;                      // holds day,year,rain,temp etc

   int das;


   private:
// Parameters ----------------------------------------------------------
   string cultivar;
   string cropClass;

   string defaultCropClass;
   string cropType;

   float rowSpacing;
   float skipRow;
   float sowingDepth;
   float plantDensity;
   float ftn;                   // fertile tiller number
   float vpd;

   vector<float> rue;
   float radnIntercepted;
   float extinctionCoef;
   float transpEff;

   float tempStress;

   vector<float> transpEffCf;
   float svpFract;


   float ttEmergeLimit;

   //Detachment Parameters
   vector<float> senDetachFrac;
   vector<float> deadDetachFrac;


//  Variables  -----------------------------------------------------
   Status plantStatus;               // plant status - out, dead, alive
   string statusString;
//   int das;
   float dltPlants;
   float frIntcRadn;
   float eo;

   float coverGreen;
   float dltDeadPlants;
   TableFn tempStressTable;

//  Ids  -----------------------------------------------------
//   unsigned int frIntcRadnID;
//   unsigned int no3MinID;

   // sets

   // events.
   unsigned int cropChoppedID;
   unsigned int incorpFomID;
   unsigned int swDepID;

   float radnInt(void);

   void initialize(void);
   void endPlant (void);

   public:
   // ------------------------------------------------------
   void setStatus(Status status);


   void plantInit(void) ;
   void readParams(void);
   void prepare (void);               // do crop preparation
   void process (void);               // do crop processes

   // Plant - System actions   - in PlantActions.cpp
   void doRegistrations(void) ;
  // void doEvent(unsigned int &id, protocol::Variant &v);
   void doPrepare(unsigned &, unsigned &,protocol::Variant &) ;
   void doProcess(unsigned &, unsigned &, protocol::Variant &) ;
   void doTick(unsigned &id, unsigned &, protocol::Variant &v) ;
   void doNewMet(unsigned &, unsigned &, protocol::Variant &v) ;
   void doNewProfile(unsigned &, unsigned &, protocol::Variant &v) ;

   void sowCrop(unsigned &, unsigned &, protocol::Variant &v);
   void doHarvest(unsigned &, unsigned &, protocol::Variant &v) ;
   void doEndCrop(unsigned &, unsigned &, protocol::Variant &v) ;
   void doEndRun(unsigned &, unsigned &, protocol::Variant &v) ;
   void doKillCrop(unsigned &, unsigned &, protocol::Variant &v);

   bool setVariable(unsigned id, protocol::QuerySetValueData& qd);
 //  void getVariable(protocol::QueryValueData& qd);
 //  void mapVar(unsigned id,string name,void *ptr,int dType);

   void getOtherVariables(void);
 //  void startCrop (protocol::Variant &v/*(INPUT) message arguments*/);

   void updateVars(void);
   void death(void);
   void detachment(void);
   void cleanup(void);
   
   float transpEfficiency(void);
   float svp(float temp);

   float getTempStress(void)const{return tempStress;}
   float getTranspEff(void)const{return transpEff;}
   float getSowingDepth(void)const{return sowingDepth;}

   float getRadnInt(void)const{return radnIntercepted;}


   float getPlantDensity(void)const{return plantDensity;}
   float getFtn(void)const{return ftn;}
   void killCrop(void);

   void getPlantStatus(protocol::Component *system, protocol::QueryValueData &qd);

   void   phenologyEvent(int stage);


   };  // Plant

//------------------------------------------------------------------------------------------------



#endif //PLANT_H_

