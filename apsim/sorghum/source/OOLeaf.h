//------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------

#ifndef OOLeafH
#define OOLeafH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Leaf : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   float noSeed;
   float noEmergence;
   float initRate;
   float initialTPLA;
   float tplaInflectionRatio,tplaProductionCoef;
   // leaf appearance
   float appearanceRate1;
   float appearanceRate2;
   float noRateChange;
   float minLeafNo,maxLeafNo;

   // leaf dry mass
   float initialDM;
   float translocFrac;
   float leafPartitionRate;
   float dmTotal;

   // leaf area
   float tillerCoef;
   float mainStemCoef;

   float slaMin;
   float slaMax;

   // sencescence
   float splaIntercept;
   float splaSlope;
   float splaProdCoef;
   float senRadnCrit;        // radiation level for onset of light senescence
   float senLightTimeConst;  // delay factor for light senescence
   float senWaterTimeConst;  // delay factor for water senescence
   float senThreshold;       // supply:demand ratio for onset of water senescence
   float frostKill;          // temperature threshold for leaf death

   // nitrogen
   float initialSLN;
   float targetSLN;
   float newLeafSLN;
   float senescedLeafSLN;
   float nTotal;

   // plant
   float density;

//  Variables  -----------------------------------------------------
   // Leaf Number
   vector<float> leafNo;              // leaf number per stage
   vector<float> nodeNo;              // node number per stage

   float finalLeafNo;
   float dltLeafNo;
   float nLeaves;

   // LAI
   float lai;
   float dltPotentialLAI;
   float dltStressedLAI;
   float dltLAI;
   float maxLai;

   float coverGreen;
   // TPLA
   float tplaMax;                     // maximum possible total plant leaf area
   float tplaPot;
   float tpla;
   float spla;

   // senescence
   float sLai;
   float dltSlai;
   float maxLaiPossible;
   float waterStressLaiLoss;
   float dltSlaiAge;
   float dltSlaiLight;
   float dltSlaiWater;
   float dltSlaiFrost;
   float dltDmSenesced;
   vector<float> laiEquilibLight;
   vector<float> laiEquilibWater;
   float dltSlaiN;


   // dead
   float deadLai;

   // SLN
   float SLN;
   float SLN0;


// Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   initialize(void);
   void   initLeafNo(void);
   void   calcFinalLeafNo(void);
   void   calcLeafAppearance(void);
   void   calcTplaMax(void);
   float calcDltPotentialTPLA(void);
   float calcStressedLeafArea(void);
   float calcMaxLaiPossible(void);
   float calcLaiSenescenceAge(void);
   float calcLaiSenescenceLight(void);
   float calcLaiSenescenceWater(void);
   float calcLaiSenescenceFrost(void);
   float calcSLN(void);
   float calcLAI(void);


// public Methods -------------------------------------------------------
   public:
   Leaf(OOPlant *p);
   ~Leaf();

   ///////////////////////////
   float dltNSenesced;
   void areaActual(void);           // plant
   void senesceArea(void);          // plant
   //////////////////////////
   void readParams (string cultivar);
   void updateVars(void);

   void process(void);              // plant
   void calcLeafNo(void);           // plant
   void calcPotentialArea(void);    // plant

   float calcCover(float extinctionCoef,float skipRow); // plant
   float calcEmergFlagTT(void);                           // phenology

   // nitrogen
   float calcNDemand(void);                         // nitrogen
   float calcNewLeafNDemand(void);                  // nitrogen
   float provideN(float requiredN);
   float getSLN(void)const{return SLN;}             // nitrogen
   float laiToday(void)const;                       // nitrogen
//   float getNGreen(void)const{return nGreen;}       // nitrogen
//   float getDltNGreen(void)const{return dltNGreen;} // nitrogen
//   float getNDemand(void)const{return nDemand;}     // nitrogen
   float getDltLAI(void)const{return dltLAI;}       // nitrogen
   float getLAI(void)const{return lai;}             // nitrogen
   float getDltSlai(void)const{return dltSlai;}     // nitrogen
   void   addDltSlai(float add);                    // nitrogen
/* TODO : Fix this */

   // phosphorus
   float calcPDemand(void);

//   float getDmGreen(void)const{return dmGreen;}            // biomass
//   float getDmSenesced(void)const{return dmSenesced;}      // biomass
   void   calcSenescence(void);                             // biomass
   float partitionDM(float dltDM);                        // biomass
   float dmRetransAvailable(void);                         // biomass
   void   dmRetrans(float dltDm){dmRetranslocate = dltDm;} // biomass
   float getLeafNo(void){return nLeaves;}
   void laiDetachment(vector<float> senDetachFrac, vector<float> deadDetachFrac);

   void Summary(void);

   // phenology
   void phenologyEvent(int);

   };  // Leaf

//------------------------------------------------------------------------------------------------
#endif
