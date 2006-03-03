//------------------------------------------------------------------------------------------------


#pragma hdrstop

#include "OOLeaf.h"
#include "OOPlant.h"
using namespace std;
//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
//------ Leaf Constructor
//------------------------------------------------------------------------------------------------
Leaf::Leaf(OOPlant *p)
   {
   plant = p;
   plantInterface = p->plantInterface;
   name = "Leaf";

   doRegistrations();
   initialize();
   }
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
Leaf::~Leaf()
   {
   
   }
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Leaf::doRegistrations(void)
   {  
#define setupGetVar plantInterface->addGettableVar
   setupGetVar("lai", lai, "m2/m2", "Live plant green lai");
   setupGetVar("dlt_lai", dltLAI, "m2/m2", "Leaf area growth rate");
   setupGetVar("slai", sLai, "m2/m2", "Senesced plant lai");
   setupGetVar("dlt_slai", dltSlai, "m2/m2", "Leaf area senescence rate");
   setupGetVar("tpla", tpla, "m2", "Total plant leaf area");
   setupGetVar("spla", spla, "m2", "Senesced plant leaf area");
   setupGetVar("leaf_no", nLeaves, "leaves", "Number of fully expanded leaves within a stage");
   setupGetVar("leaf_wt", dmGreen, "g/m2", "Live leaf dry weight");
   setupGetVar("dleaf_wt", dmSenesced, "g/m2", "Dead leaf dry weight");
   setupGetVar("tleaf_wt", dmSenesced, "g/m2", "Dead leaf dry weight");
   setupGetVar("lai_max", maxLai, "m2/m2", "Maximum lai reached during the growing season");
   setupGetVar("gleaf_n", nGreen, "g/m2", "N in green leaf");
   setupGetVar("dleaf_n", nSenesced, "g/m2", "N in senesced and dead leaf");
   setupGetVar("tleaf_n", nTotal, "g/m2", "Total N in live and dead leaf");
   setupGetVar("sln",SLN, "g(N)/m2(leaf)", "Specific leaf nitrogen");
   setupGetVar("dlt_n_retrans_leaf", dltNRetranslocate, "g/m2", "Nitrogen retranslocated from leaf to the grain");
   setupGetVar("n_conc_leaf", nConc, "%", "N concentration in live leaf");
   setupGetVar("leaf_nd", nDemand, "g/m2", "Today's N demand from leaves");
   setupGetVar("dlt_n_green_leaf", dltNGreen, "g/m2", "Daily N increase in leaves");
   setupGetVar("dlt_leaf_no", dltLeafNo, "lvs/d", "Fraction of oldest leaf expanded");

#undef setupGetVar
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Leaf::initialize(void)
   {
   // leaf area
   lai = 0.0;
   dltSlai = 0.0;
   dltPotentialLAI = 0.0;
   maxLaiPossible = 0.0;
   waterStressLaiLoss = 0.0;
   sLai = 0.0;
   dltSlai = 0.0;
   tplaMax = 0.0;
   tplaPot = 0.0;
   deadLai = 0.0;
   dltSlaiN = 0.0;
   maxLai = 0.0;

   // leaf number
   finalLeafNo = 0.0;
   dltLeafNo = 0.0;
   nLeaves = 0.0;
   leafNo.clear();
   nodeNo.clear();
   for(int i=0;i < nStages;i++)
      {
      leafNo.push_back(0.0);
      nodeNo.push_back(0.0);
      }

   // nitrogen
   SLN = 0.0;
   SLN0 = 0.0;
   senescedLeafSLN = 0.3;
   dltNSenesced = 0.0;

   partNo = 1;

   coverGreen = 0.0;
   coverSen = 0.0;
   coverDead = 0.0;

   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf parameters
//------------------------------------------------------------------------------------------------
void Leaf::readParams (string cultivar)
   {
   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);

   noSeed      = readVar(plantInterface,sections,"leaf_no_seed");
   initRate    = readVar(plantInterface,sections,"leaf_init_rate");
   noEmergence = readVar(plantInterface,sections,"leaf_no_at_emerg");
   minLeafNo   = readVar(plantInterface,sections,"leaf_no_min");
   maxLeafNo   = readVar(plantInterface,sections,"leaf_no_max");

   // leaf appearance rates
   appearanceRate1 = readVar(plantInterface,sections,"leaf_app_rate1");
   appearanceRate2 = readVar(plantInterface,sections,"leaf_app_rate2");
   noRateChange    = readVar(plantInterface,sections,"leaf_no_rate_change");

   // leaf area TPLA
   initialTPLA         = readVar(plantInterface,sections,"initial_tpla");
   tplaInflectionRatio = readVar(plantInterface,sections,"tpla_inflection_ratio");
   tplaProductionCoef  = readVar(plantInterface,sections,"tpla_prod_coef");
   tillerCoef = readVar(plantInterface,sections,"tiller_coef");
   mainStemCoef = readVar(plantInterface,sections,"main_stem_coef");

   // dry matter
   initialDM    = readVar(plantInterface,sections,"dm_leaf_init");
   translocFrac = readVar(plantInterface,sections,"Leaf_trans_frac");
   leafPartitionRate = readVar(plantInterface,sections,"partition_rate_leaf");

   // sla
   slaMin = readVar(plantInterface,sections,"sla_min");
   slaMax = readVar(plantInterface,sections,"sla_max");

   // spla
   splaIntercept = readVar(plantInterface,sections,"spla_intercept");
   splaSlope     = readVar(plantInterface,sections,"spla_slope");
   splaProdCoef  = readVar(plantInterface,sections,"spla_prod_coef");


   // nitrogen
   initialSLN = readVar(plantInterface,sections,"initialLeafSLN");
   targetSLN  = readVar(plantInterface,sections,"targetLeafSLN");
   newLeafSLN  = readVar(plantInterface,sections,"newLeafSLN");


   // senescence
   senRadnCrit  = readVar(plantInterface,sections,"sen_radn_crit");
   senLightTimeConst  = readVar(plantInterface,sections,"sen_light_time_const");
   senThreshold = readVar(plantInterface,sections,"sen_threshold");
   senWaterTimeConst  = readVar(plantInterface,sections,"sen_water_time_const");
   frostKill  = readVar(plantInterface,sections,"frost_kill");

   // phosphorus
   pMaxTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_max_leaf");
   pMinTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_min_leaf");
   pSenTable.read(plantInterface,sections,"x_p_stage_code","y_p_conc_sen_leaf");
   initialPConc = readVar(plantInterface,sections,"p_conc_init_leaf");

   density = plant->getPlantDensity();

   // report
   plantInterface->writeString ("    -------------------------------------------------------");
   char msg[100];
   sprintf(msg, "    tpla_prod_coef           =  %6.2f",tplaProductionCoef);plantInterface->writeString (msg);
   sprintf(msg, "    tpla_inflection_ratio    =  %6.2f",tplaInflectionRatio);plantInterface->writeString (msg);
   sprintf(msg, "    spla_prod_coef           =  %6.2f",splaProdCoef);plantInterface->writeString (msg);
   sprintf(msg, "    spla_intercept           =  %6.2f",splaIntercept);plantInterface->writeString (msg);
   sprintf(msg, "    spla_slope               =  %6.2f",splaSlope);plantInterface->writeString (msg);



   // calculate extinction coef
   TableFn extinction;
   extinction.read(plantInterface,sections,"x_row_spacing","y_extinct_coef");
   extinctionCoef = extinction.value(plant->getRowSpacing());


   }
//------------------------------------------------------------------------------------------------
//----------- update Leaf state variables at the end of the day
//------------------------------------------------------------------------------------------------
void Leaf::updateVars(void)
   {
   int iStage = (int)stage;

   // leaf number
   leafNo[iStage] += dltLeafNo;
   nLeaves = sumVector(leafNo);

   // Dry matter
   dmGreen += dltDmGreen;
   dmGreen += dmRetranslocate;
   dmSenesced += dltDmSenesced;
   dmGreen -= dltDmSenesced;

   nSenesced += dltNSenesced;

   // Nitrogen
   SLN0 = divide(nGreen,lai);
   nGreen += (dltNGreen +  dltNRetranslocate - dltNSenesced);
   dltNSenesced = 0.0;


   // leaf area
   lai += dltLAI;

   sLai += dltSlai;
   lai -= dltSlai;
   tpla = lai / density * 10000;
   spla = sLai / density * 10000;

   dltSlaiN = 0.0;
   dltSlai = 0.0;

   SLN = divide(nGreen,lai);
   nConc = divide(nGreen,dmGreen,0);
   // leaf dm senesced returns N to rest of leaf
   //   nGreen += dltNGreen + dltNSenesced;// + dltNRetranslocate;

   // phenology
   stage = plant->phenology->currentStage();

   nTotal = nGreen + nSenesced;
   dmTotal = dmGreen + dmSenesced;

   dltDmGreen = 0.0;
   dltDmGreen = 0.0;
   dmRetranslocate = 0.0;
   dltNRetranslocate = 0.0;
   dltNGreen = 0.0;

   maxLai = Max(lai,maxLai);

   calcCover();


   }
//------------------------------------------------------------------------------------------------
void Leaf::process(void)
   {
   areaActual();
   senesceArea();
   }
//------------------------------------------------------------------------------------------------
//------- react to a phenology event
//------------------------------------------------------------------------------------------------
void Leaf::phenologyEvent(int iStage)
   {
   switch (iStage)
      {
      case emergence :
         initLeafNo();
         lai = initialTPLA * smm2sm * density;
         tplaPot = initialTPLA;
         dmGreen = initialDM * density;
         SLN = initialSLN;
         nGreen = SLN * lai;
         pGreen = initialPConc * dmGreen;
         break;
      case flowering :  
         //set the minimum weight of leaf; used for translocation to grain and stem
         float dmPlantLeaf = divide (dmGreen, density);
         dmPlantMin = dmPlantLeaf * (1.0 - translocFrac);
         break;

      }
   }
//------------------------------------------------------------------------------------------------
//-------  leaf number
//------------------------------------------------------------------------------------------------
void Leaf::calcLeafNo(void)
   {
   // calculate final leaf number up to initiation
   if(stage >= emergence && stage <= fi)
      {
      calcFinalLeafNo();
      }
   if(stage >= emergence)
      {
      calcLeafAppearance();
      }
   }
//------------------------------------------------------------------------------------------------
//------- potential leaf area
//------------------------------------------------------------------------------------------------
void Leaf::calcPotentialArea(void)
   {
   dltPotentialLAI = 0.0;
   dltStressedLAI = 0.0;

   if(finalLeafNo > 1.0 && stage <= fi)calcTplaMax();

   if(stage >= emergence && stage <= flag)
      {
      float dltPotentialTPLA = calcDltPotentialTPLA();
      tplaPot += dltPotentialTPLA;
      dltPotentialLAI = dltPotentialTPLA * density * smm2sm;
      dltStressedLAI = calcStressedLeafArea();
      }
   }
//------------------------------------------------------------------------------------------------
//-------  limit new leaf area by carbon
//------------------------------------------------------------------------------------------------
void Leaf::areaActual(void)
   {
   if(stage >= endJuv && stage < maturity)
      dltLAI = Min(dltStressedLAI,dltDmGreen * slaMax * smm2sm);
   else dltLAI = dltStressedLAI;
   }

//------------------------------------------------------------------------------------------------
//------- calc senesced leaf area
//------------------------------------------------------------------------------------------------
void Leaf::senesceArea(void)
   {

   maxLaiPossible = lai + sLai;
   if(stage >= fi && stage < flag)
      maxLaiPossible = calcMaxLaiPossible();

   // senesced leaf area due to age
   dltSlaiAge = 0.0;
   if(stage >= fi && stage < harvest)
      dltSlaiAge = calcLaiSenescenceAge();

   if (dltSlaiAge > 0.0 && isEqual(dltSlaiAge, lai))
      plantInterface->writeString ("Age kills all leaves.");


   dltSlai = Max(dltSlai,dltSlaiAge);


  // senesced leaf area due to light (crowding)
   dltSlaiLight = calcLaiSenescenceLight();

   dltSlai = Max(dltSlai,dltSlaiLight);

   // senesced leaf area due to water
   dltSlaiWater = calcLaiSenescenceWater();
   dltSlai = Max(dltSlai,dltSlaiWater);

   // senesced leaf area due to frost
   dltSlaiFrost = calcLaiSenescenceFrost();
   dltSlai = Max(dltSlai,dltSlaiFrost);

   // senesced leaf area due to N
   dltSlai = Max(dltSlai,dltSlaiN);

   }
//------------------------------------------------------------------------------------------------
float Leaf::calcLaiSenescenceFrost(void)
   {
   //  calculate senecence due to frost
   float dltSlaiFrost = 0.0;
   if (plant->today.minT < frostKill)
      dltSlaiFrost = lai;

   return dltSlaiFrost;
   }
   /* TODO : put in messages */
//------------------------------------------------------------------------------------------------
float Leaf::calcLaiSenescenceWater(void)
   {
   /* TODO : Direct translation sort of. needs work */
   float c_extinction_coef = 0.4;
   float dlt_dm_transp = plant->biomass->getDltDMPotTE();

   float effectiveRue = plant->biomass->getEffectiveRue();

   float radnCanopy = divide (plant->getRadnInt(), coverGreen, plant->today.radn);

   float sen_radn_crit = divide (dlt_dm_transp, effectiveRue, radnCanopy);
   float intc_crit = divide (sen_radn_crit, radnCanopy, 1.0);

//            ! needs rework for row spacing
   float laiEquilibWaterToday;
   if (intc_crit < 1.0)
         laiEquilibWaterToday = -log (1.0 - intc_crit) / c_extinction_coef;
   else
      laiEquilibWaterToday = lai;

   laiEquilibWater.push_back(laiEquilibWaterToday);

   float avLaiEquilibWater = 0.0;int nRecs = laiEquilibWater.size();
   // average of the last 10 days of laiEquilibWater
   int start = ( nRecs > 10 ? nRecs - 9 : 1);
   for(int i = start;i <= nRecs;i++)
      avLaiEquilibWater += laiEquilibWater[i-1];
   avLaiEquilibWater /= Min(10,nRecs);


   float dltSlaiWater = 0.0;
   if(plant->water->getSdRatio() < senThreshold)
      dltSlaiWater = Max(0.0,divide((lai - avLaiEquilibWater) , senWaterTimeConst,0.0));

   dltSlaiWater = Min(lai,dltSlaiWater);

   return dltSlaiWater;
   }
//------------------------------------------------------------------------------------------------
//------- NITROGEN
//------------------------------------------------------------------------------------------------
float Leaf::calcNDemand(void)
   {
   // LEAF N demand (g/m2) to keep SLN = targetLeafSLN before flag, or sustain SLN after flag.

//   float laiToday = Max(0.0, lai + dltLAI - dltSlai);
   float laiToday = calcLAI();
   float nRequired;
   if(stage < flag)
      nRequired = laiToday * targetSLN;
   else
      nRequired = laiToday * Min(SLN0,targetSLN);

   nDemand = Max(nRequired - nGreen,0.0);
   return nDemand;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcNewLeafNDemand(void)
   {
   // New leaf demand is SLN = newLeafSLN (1.0)
   return dltLAI * newLeafSLN;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcLAI(void)
   {
   float laiToday = Max(0.0, lai + dltLAI - dltSlai);
   return laiToday;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcSLN(void)
   {
   float laiToday = calcLAI();
   float nGreenToday = nGreen + dltNGreen - dltNRetranslocate;
   float slnToday = divide(nGreenToday,laiToday);
   return slnToday;
   }
//------------------------------------------------------------------------------------------------
float Leaf::provideN(float requiredN)
   {
   // calculate the N available for translocation to other plant parts
   // N could be required for structural Stem/Rachis N, new leaf N or grain N
   // Canopy N is made available by dilution until SLN = 1.0 then by
   // dilution, reducing delta lai and senescence
   // dltSLN/day = 0.0525*SLN(anthesis) - 0.0212 (EVO 21/5/05)


   float laiToday = calcLAI();
   float slnToday = calcSLN();

   // total N available by dilution
   float dilutionN = divide(17,plant->phenology->getDltTT()) * (0.04 * slnToday - 0.016) * laiToday;
   dilutionN = Max(dilutionN,0.0);

   //if SLN is > 1.0 then provide this N
   if(slnToday > 1.0)
      {
      float nProvided = Min(dilutionN,requiredN);
      dltNRetranslocate -= nProvided;
      return nProvided;
      }

   // else provide 1/3 by dilution, 1/3 by reducing the dltLAI and 1/3 by senescence
   // dltLAI first   : if dltLAi > 0 then decrease dltLAI by enough to provide 1/3 requiredN
   if(dltLAI > 0)
      {
      float n = dltLAI * newLeafSLN;
      float laiN = min(n,requiredN/3);
      dltLAI = (n - laiN) / newLeafSLN;
      requiredN -= laiN;
      }
   // by dilution
   float nProvided = Min(dilutionN,requiredN/2);
   dltNRetranslocate -= nProvided;
   requiredN -= nProvided;

   // recalc the SLN after this N has been removed
   laiToday = calcLAI();
   slnToday = calcSLN();
   float maxN = divide(17,plant->phenology->getDltTT()) * (0.04 * slnToday - 0.016) * laiToday;

   requiredN = Min(requiredN,maxN);

   // 1/3 by senescence


   float senescenceLAI = Max(divide(requiredN/2,(slnToday-senescedLeafSLN)),0.0);

   float newN = Max(senescenceLAI * (slnToday-senescedLeafSLN),0.0);
   dltNRetranslocate -= newN;
   nProvided += newN;
   dltSlaiN += senescenceLAI;
   dltSlai = Max(dltSlai,dltSlaiN);

   dltNSenesced += senescenceLAI * senescedLeafSLN;

   return nProvided;
   }
//------------------------------------------------------------------------------------------------

void Leaf::initLeafNo(void)
   {
   leafNo[emergence] = noEmergence;
   nodeNo[emergence] = noEmergence;
   }
//------------------------------------------------------------------------------------------------
// estimate the final leaf no from an approximated thermal time
//  emergence to floral initiation.
void Leaf::calcFinalLeafNo(void)
   {
   float ttFi = plant->phenology->sumTTtarget(emergence,fi);
   finalLeafNo = bound(divide(ttFi,initRate) + noSeed,minLeafNo,maxLeafNo);
   }
//------------------------------------------------------------------------------------------------
void Leaf::calcLeafAppearance(void)
   {
   dltLeafNo = 0.0;
   float remainingLeaves = finalLeafNo - nLeaves;
   if(remainingLeaves <= 0.0)
      {
      return;
      }
   // Peter's 2 stage version used here, modified to apply to last few leaves before flag
   // i.e. c_leaf_no_rate_change is leaf number from the top down (e.g. 4)
   float leafAppRate;
   if (remainingLeaves <= noRateChange)
      {
      leafAppRate = appearanceRate2;
      }
   else leafAppRate = appearanceRate1;

   // if leaves are still growing, the cumulative number of phyllochrons or fully expanded
   // leaves is calculated from thermal time for the day.
   dltLeafNo = bound(divide(plant->phenology->getDltTT(),leafAppRate),0.0,remainingLeaves);
   }
//------------------------------------------------------------------------------------------------
//-----------  Leaf area
//------------------------------------------------------------------------------------------------
void Leaf::calcTplaMax(void)
   {
   tplaMax = (pow(plant->getFtn() + 1.0, tillerCoef) * pow(finalLeafNo,mainStemCoef)) * scm2smm;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcDltPotentialTPLA(void)
   {
   // need to ramp the dltPotTPLA for the first 60Cd because the function evaluates to approx 2000
   // when ttElapsed = 0
   float ttTPLAPhase = plant->phenology->sumTTtarget(emergence,flag);
   float ttElapsed   = plant->phenology->sumTTtotal (emergence,flag);
   float tplaInflection = ttTPLAPhase * tplaInflectionRatio;
   float tplaToday;

   if(ttElapsed < 80)
      {
      float exponent = tplaProductionCoef * (80 - tplaInflection);
      float tpla80 = divide(tplaMax,1.0 + exp(-1 * exponent));
      tplaToday = divide(ttElapsed,80) * tpla80;
      }
   else
      {
      float exponent = tplaProductionCoef * (ttElapsed - tplaInflection);
      tplaToday = divide(tplaMax,1.0 + exp(-1 * exponent));
      }
   tplaToday = Max(tplaToday,tplaPot);
   return tplaToday - tplaPot;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcStressedLeafArea()
   {
   return dltPotentialLAI * Min(plant->water->getExpansionStress(),
                                    plant->nitrogen->getExpansionStress());
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcMaxLaiPossible(void)
   {
   waterStressLaiLoss += (dltPotentialLAI - dltStressedLAI);
   return lai + sLai - waterStressLaiLoss;
   }
//------------------------------------------------------------------------------------------------
//  Return the lai that would senesce on the current day from natural ageing
float Leaf::calcLaiSenescenceAge(void)
   {
   float ttSinceEmergence = plant->phenology->sumTTtotal (emergence,harvest);
   float splaInflection = splaIntercept + (splaSlope * finalLeafNo);

   float sLaiToday = divide(maxLaiPossible,
      (1.0 + exp(-1 * splaProdCoef * (ttSinceEmergence - splaInflection))));

   return Max(sLaiToday - sLai,0.0);
   }
//------------------------------------------------------------------------------------------------
//  Return the lai that would senesce on the current day from light (crowding)
float Leaf::calcLaiSenescenceLight(void)
   {
   //    senRadnCrit
   float critTransmission = divide(senRadnCrit,plant->today.radn);
   /* TODO : Direct translation - needs cleanup */
   float c_extinction_coef = 0.4;
//            ! needs rework for row spacing
   float laiEqlbLightToday;
   if (critTransmission > 0.0)laiEqlbLightToday = -log (critTransmission)/c_extinction_coef;
   else laiEqlbLightToday = lai;
   laiEquilibLight.push_back(laiEqlbLightToday);

   float avLaiEquilibLight = 0.0;int nRecs = laiEquilibLight.size();
   // average of the last 10 days of laiEqlbLightToday
   int start = ( nRecs > 10 ? nRecs - 9 : 1);
   for(int i = start;i <= nRecs;i++)
      avLaiEquilibLight += laiEquilibLight[i-1];
   avLaiEquilibLight /= Min(10,nRecs);

   float radnTransmitted = plant->today.radn - plant->getRadnInt();
   float dltSlaiLight = 0.0;
   if (radnTransmitted < senRadnCrit)
      dltSlaiLight = Max(0.0,divide (lai - avLaiEquilibLight, senLightTimeConst , 0.0));
   dltSlaiLight = Min(dltSlaiLight,lai);
   return dltSlaiLight;
   }
//------------------------------------------------------------------------------------------------
void Leaf::calcCover()
   {
   float skipRow = plant->getSkipRow();
   coverGreen = divide(1.0 - exp(-extinctionCoef * lai * skipRow), skipRow,0.0);
   coverSen = divide(1.0 - exp(-extinctionCoef * sLai * skipRow), skipRow,0.0);
   coverDead = divide(1.0 - exp(-extinctionCoef * deadLai * skipRow), skipRow,0.0);
   coverTot = 1.0 - (1.0 - coverGreen) * (1 - coverSen) * (1 - coverDead);
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcEmergFlagTT(void)
   {
   // estimate emergence to flag using leaf appearance rates
   float nLeavesAtChange = bound(finalLeafNo - noRateChange,noEmergence,finalLeafNo);
   return (nLeavesAtChange - noEmergence) * appearanceRate1 +
          (finalLeafNo - nLeavesAtChange) * appearanceRate2;
   }
//------------------------------------------------------------------------------------------------
float Leaf::laiToday(void)const
   {
   return Max(0.0, lai + dltLAI - dltSlai);
   }
//------------------------------------------------------------------------------------------------
void Leaf::addDltSlai(float add)
   {
   dltSlai += add;
   dltSlai = Min(sLai, lai + dltLAI);
   }
//------------------------------------------------------------------------------------------------
void Leaf::calcSenescence(void)
   {
   // Derives seneseced plant dry matter (g/m^2) for the day
   // calculate scenesced N
   float laiToday = lai + dltLAI;

   float dmGreenLeafToday = dmGreen + dltDmGreen + dmRetranslocate;               // -ve
   float slaToday = divide(laiToday,dmGreenLeafToday);

   dltDmSenesced = divide(dltSlai,slaToday);


   float slnToday = divide(nGreen,laiToday);
   dltNSenesced  += dltSlai * Max((slnToday - senescedLeafSLN),0.0);

   }
//------------------------------------------------------------------------------------------------
float Leaf::partitionDM(float dltDM)
   {
   float dltDmLeafMax = divide (dltStressedLAI,slaMin * smm2sm);

   float leafPartitionCoef = 1.0 / (1.0 + leafPartitionRate * pow(nLeaves,2.0));
   // limit the delta leaf area to maximum  using sla
   dltDmGreen = Min(leafPartitionCoef * dltDM,dltDmLeafMax);
   return dltDmGreen;
   }
//------------------------------------------------------------------------------------------------
float Leaf::dmRetransAvailable(void)
   {
   // calculate dry matter available for translocation to grain
   float leafWt = dmGreen + dltDmGreen;
   float leafWtAvail = leafWt - dmPlantMin * density;
   return Max(leafWtAvail,0.0);
   }
//------------------------------------------------------------------------------------------------
//------- Calculate detachment of lai (based upon fractional decay rates)
//------------------------------------------------------------------------------------------------
void Leaf::laiDetachment(vector<float> senDetachFrac, vector<float> deadDetachFrac)
   {

   //These are change within the call but are not used anywhere
   float sLaiDetachedDelta;
   float dLaiDetachedDelta;

   //Do Calculations
   calcPartFractionDelta (partNo, senDetachFrac, sLai, sLaiDetachedDelta);
   calcPartFractionDelta (partNo, deadDetachFrac, deadLai, dLaiDetachedDelta);

    }

//------------------------------------------------------------------------------------------------
float Leaf::calcPDemand(void)
   {
   // Leaf P demand

   float rel_growth_rate = divide(plant->biomass->getDltDMPotRUE(),
         plant->biomass->getAboveGroundBiomass(),0.0);

   float deficit = pConcMax() * dmGreen * (1.0 + rel_growth_rate) - pGreen;

   pDemand = Max(deficit,0.0);
   return pDemand;
   }
//------------------------------------------------------------------------------------------------
void Leaf::Summary(void)
   {
   summaryLine(plantInterface,"maximum lai           = %.3f \t number of leaves        = %.3f",
            maxLai,nLeaves);
   }






