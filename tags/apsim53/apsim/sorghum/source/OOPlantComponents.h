//---------------------------------------------------------------------------

#ifndef OOPlantComponentsH
#define OOPlantComponentsH


#include <ComponentInterface/Component.h>

#include "OOPlantInterface.h"

#include "Utilities.h"


#include <vector>

class TableFn;

//---------------------------------------------------------------------------

class PlantComponent
   {
   private:

   public:
   PlantInterface *plantInterface;            // for interface calls to system
   OOPlant *plant;

   virtual void initialize(void) = 0;
   virtual void readParams (string cultivar) = 0;
   virtual void updateVars(void) = 0;

   };
//---------------------------------------------------------------------------
class PlantPart : public PlantComponent
   {
   private:

   protected:

   // variables
   float stage;
   int partNo;
   string name;


   // Biomass
   float dmGreen;
   float dltDmGreen;

   float dmSenesced;
   float dltDmSenesced;

   float dmDead;
   float dltDetDmSenesced;
   float dltDetDmDead;

   float dmPlantMin;
   float dmRetranslocate;
   

   // Nitrogen
   float nGreen;
   float dltNGreen;

   float nDemand;
   float dltNRetranslocate;

   float nSenesced;
   float dltNSenesced;
   float nDead;
   float dltDetNSenesced;
   float dltDetNDead;

   float nConc;

   // Phosphorus
   float pGreen;
   float pSenesced;
   float pDead;
   float dltPGreen;
   float dltPSenesced;
   float dltPDetached;

   float pDemand;
   float dltPRetranslocate;

   float pConc;

   // phosphorus  parameters
   TableFn pMaxTable;
   TableFn pMinTable;
   TableFn pSenTable;
   float initialPConc;


   public:
   PlantPart();
   void initialize(void);
   virtual void phenologyEvent(int) = 0;
   virtual float calcNDemand(void) = 0;
   virtual float calcPDemand(void) = 0;

   //Detatchmenet Routines
   virtual void dmDetachment(vector<float>, vector<float>);
   virtual void NDetachment(vector<float>, vector<float>);

   //Getters
   virtual float getNGreen(void){return nGreen;};
   virtual float getNSenesced(void){return nSenesced;};
   virtual float getNDead(void){return nDead;};

   virtual float getDmGreen(void){return dmGreen;};
   virtual float getDmSenesced(void){return dmSenesced;};
   virtual float getDmDead(void){return dmDead;};

   virtual float getDltNGreen(void){return dltNGreen;};
   virtual float getDltDetNSenesced(void){return dltDetNSenesced;};
   virtual float getDltDetNDead(void){return dltDetNDead;};

   virtual float getDltDmGreen(void){return dltDmGreen;};
   virtual float getDltDetDmSenesced(void){return dltDetDmSenesced;};
   virtual float getDltDetDmDead(void){return dltDetDmDead;};

   virtual float getNDemand(void){return nDemand;};
   virtual float getDltDmRetranslocate(void){return dmRetranslocate;};
   virtual float getDltNRetranslocate(void){return dltNRetranslocate;};

   virtual void resetDailyVars(void);

   virtual float pConcMax(void){return pMaxTable.value(stage);}
   virtual float pConcMin(void){return pMinTable.value(stage);}
   virtual float pConcSen(void){return pSenTable.value(stage);}
   virtual float getPGreen(void){return pGreen;};
   virtual float getPSenesced(void){return pSenesced;};
   virtual float getPDead(void){return pDead;};
   virtual float getPDemand(void){return pDemand;};
   virtual float getDltPGreen(void){return dltPGreen;};
   virtual float getDltPRetrans(void){return dltPRetranslocate;};


   virtual void setPRetrans(float P){dltPRetranslocate = P;}


   void partitionN(float N){dltNGreen += N;}
   void partitionP(float P){dltPGreen += P;}
   void calcDltPSenesced(void);
   void calcDltPDetached(void);
   void updateP(void);

   string getName(void){return name;}
   };
//---------------------------------------------------------------------------
class PlantProcess : public PlantComponent
   {
   private:

   public:


   // variables
   };
//---------------------------------------------------------------------------
#endif
