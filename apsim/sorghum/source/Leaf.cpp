//------------------------------------------------------------------------------------------------
#include "Plant.h"
#include "Leaf.h"

//------------------------------------------------------------------------------------------------
//------ Leaf Constructor
//------------------------------------------------------------------------------------------------
Leaf::Leaf(ScienceAPI &api, Plant *p) : PlantPart(api)
   {
   plant = p;
   name = "Leaf";
   partNo = 1;

   initialize();
   doRegistrations();
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
   scienceAPI.expose("LAI",            "m2/m2", "Live plant green LAI",            false, lai);
   scienceAPI.expose("DeltaLAI",       "m2/m2", "Leaf area growth rate",           false, dltLAI);
   scienceAPI.expose("SLAI",           "m2/m2", "Senesced plant LAI",              false, sLai);
   scienceAPI.expose("DeltaSLAI",      "m2/m2", "Leaf area senescence rate",       false, dltSlai);
   scienceAPI.expose("TPLA",           "m2",    "Total plant leaf area",           false, tpla);
   scienceAPI.expose("SPLA",           "m2",    "Senesced plant leaf area",        false, spla);
   scienceAPI.expose("GPLA",           "m2",    "Green plant leaf area",           false, spla);
   scienceAPI.expose("LeafNo",         "leaves","Number of fully expanded leaves", false, nLeaves);
   scienceAPI.expose("LeafGreenWt",    "g/m^2", "Live leaf dry weight",            false, dmGreen);
   scienceAPI.expose("LeafSenescedWt", "g/m^2", "Senesced leaf dry weight",        false, dmSenesced);
   scienceAPI.expose("MaxLAI",         "m2/m2", "Maximum LAI reached",             false, maxLai);
   scienceAPI.expose("LeafGreenN",     "g/m^2", "N in green leaf",                 false, nGreen);
   scienceAPI.expose("LeafSenescedN",  "g/m^2", "N in senesced leaf",              false, nSenesced);
   scienceAPI.expose("SLN",            "g(N)/m2","Specific Leaf Nitrogen",         false, SLN);
   scienceAPI.expose("LeafGreenNConc", "%",     "Live leaf N concentration",       false, nConc);
   scienceAPI.expose("LeafNDemand",    "g/m^2", "Today's N demand from leaves",    false, nDemand);
   scienceAPI.expose("DeltaLeafGreenN","g/m^2", "Daily N increase in leaves",      false, dltNGreen);
   scienceAPI.expose("DeltaLeafNo",    "lvs/d", "Fraction of oldest leaf expanded",false, dltLeafNo);
   scienceAPI.expose("ExtinctionCoef", "()",    "Light Extinction coefficient",    false, extinctionCoef);
   scienceAPI.expose("LeafGreenP",     "g/m^2" ,"P in live leaf",                  false, pGreen);
   }
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Leaf::initialize(void)
   {
   // leaf area
   tpla = 0.0;
   spla = 0.0;
   gpla = 0.0;
   lai  = 0.0;
   sLai = 0.0;
   dltSlai  = 0.0;
   tplaMax  = 0.0;
   tplaPot  = 0.0;
   dltSlaiN = 0.0;
   maxLai   = 0.0;
   dltPotentialLAI    = 0.0;
   maxLaiPossible     = 0.0;
   waterStressLaiLoss = 0.0;

   // leaf number
   finalLeafNo = 0.0;
   dltLeafNo   = 0.0;
   nLeaves     = 0.0;

   leafNo.assign (nStages,0.0);
   nodeNo.assign (nStages,0.0);

   // nitrogen
   SLN  = 0.0;
   SLN0 = 0.0;
   dltNSenesced = 0.0;

   coverGreen = 0.0;
   coverSen   = 0.0;
   coverTot   = 0.0;
   PlantPart::initialize();
   }
//------------------------------------------------------------------------------------------------
//-----------  read leaf parameters
//------------------------------------------------------------------------------------------------
void Leaf::readParams (void)
   {
   scienceAPI.read("leaf_no_seed"    , "", false, noSeed);
   scienceAPI.read("leaf_init_rate"  , "", false, initRate);
   scienceAPI.read("leaf_no_at_emerg", "", false, noEmergence);
   scienceAPI.read("leaf_no_min"     , "", false, minLeafNo);
   scienceAPI.read("leaf_no_max"     , "", false, maxLeafNo);

   // leaf appearance rates
   scienceAPI.read("leaf_app_rate1"     ,"", false, appearanceRate1);
   scienceAPI.read("leaf_app_rate2"     ,"", false, appearanceRate2);
   scienceAPI.read("leaf_no_rate_change","", false, noRateChange);

   // leaf area TPLA
   scienceAPI.read("initial_tpla"         ,"", false, initialTPLA);
   scienceAPI.read("tpla_inflection_ratio","", false, tplaInflectionRatio);
   scienceAPI.read("tpla_prod_coef"       ,"", false, tplaProductionCoef);
   scienceAPI.read("tiller_coef"          ,"", false, tillerCoef);
   scienceAPI.read("main_stem_coef"       ,"", false, mainStemCoef);

   // dry matter
   scienceAPI.read("dm_leaf_init"       , "", false, initialDM);
   scienceAPI.read("Leaf_trans_frac"    , "", false, translocFrac);
   scienceAPI.read("partition_rate_leaf", "", false, leafPartitionRate);

   // sla
   scienceAPI.read("sla_min", "", false, slaMin);
   scienceAPI.read("sla_max", "", false, slaMax);

   // nitrogen
   scienceAPI.read("initialLeafSLN", "", false, initialSLN);
   scienceAPI.read("targetLeafSLN" , "", false, targetSLN);
   scienceAPI.read("newLeafSLN"    , "", false, newLeafSLN);
   scienceAPI.read("leafDilnNSlope", "", false, dilnNSlope);
   scienceAPI.read("leafdilnNInt"  , "", false, dilnNInt);
   scienceAPI.read("senescedLeafSLN","", false, senescedLeafSLN);

   // senescence
   scienceAPI.read("sen_radn_crit"       , "", false, senRadnCrit);
   scienceAPI.read("sen_light_time_const", "", false, senLightTimeConst);
   scienceAPI.read("sen_threshold"       , "", false, senThreshold);
   scienceAPI.read("sen_water_time_const", "", false, senWaterTimeConst);
   scienceAPI.read("frost_kill"          , "", false, frostKill);

   // phosphorus
   pMaxTable.read(scienceAPI, "x_p_stage_code","y_p_conc_max_leaf");
   pMinTable.read(scienceAPI, "x_p_stage_code","y_p_conc_min_leaf");
   pSenTable.read(scienceAPI, "x_p_stage_code","y_p_conc_sen_leaf");
   scienceAPI.read("p_conc_init_leaf", "", false, initialPConc);

   density = plant->getPlantDensity();

   // report
   char msg[120];
   scienceAPI.write("    -------------------------------------------------------\n");
   sprintf(msg, "    tpla_prod_coef           =  %6.2f\n",tplaProductionCoef); scienceAPI.write(msg);
   sprintf(msg, "    tpla_inflection_ratio    =  %6.2f\n",tplaInflectionRatio); scienceAPI.write(msg);



   // calculate extinction coef
   TableFn extinction;
   extinction.read(scienceAPI,"x_row_spacing","y_extinct_coef");
   extinctionCoef = extinction.value(plant->getRowSpacing() / 1000.0);
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
   if(lai < 0.001)lai = 0.0;
   lai += dltLAI;

   sLai += dltSlai;
   lai -= dltSlai;
   gpla = lai / density * 10000;
   spla = sLai / density * 10000;
   tpla = gpla + spla;

   SLN = divide(nGreen,lai,0);
   nConc = divide(nGreen,dmGreen,0);

   // phenology
   stage = plant->phenology->currentStage();

   dmTotal = dmGreen + dmSenesced;
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
   ExternalMassFlowType EMF;
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
         EMF.PoolClass = "crop";
         EMF.FlowType = "gain";
         EMF.DM = 0.0;
         EMF.N  = nGreen * gm2kg/sm2ha;
         EMF.P  = pGreen * gm2kg/sm2ha;
         EMF.C = 0.0; // ?????
         EMF.SW = 0.0;
         scienceAPI.publish("ExternalMassFlow", EMF);
         break;
      case flag :                   // limit target sln to sln at flag
         targetSLN = SLN;
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
   dltStressedLAI  = 0.0;

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
//   if (dltLAI < 0.001)dltLAI = 0.0;
   }
//------------------------------------------------------------------------------------------------
//------- calc senesced leaf area
//------------------------------------------------------------------------------------------------
void Leaf::senesceArea(void)
   {
   dltSlaiN = 0.0;
   dltSlai = 0.0;

   // NOTE: age senescence removed when new nitrogen routines added GMcL 1/08
   maxLaiPossible = lai + sLai;
   if(stage >= fi && stage < flag)
      maxLaiPossible = calcMaxLaiPossible();

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
   float dlt_dm_transp = plant->biomass->getDltDMPotTE();

   float effectiveRue = plant->biomass->getEffectiveRue();

   float radnCanopy = divide (plant->getRadnInt(), coverGreen, plant->today.radn);

   float sen_radn_crit = divide (dlt_dm_transp, effectiveRue, radnCanopy);
   float intc_crit = divide (sen_radn_crit, radnCanopy, 1.0);

//            ! needs rework for row spacing
   float laiEquilibWaterToday;
   if (intc_crit < 1.0)
         laiEquilibWaterToday = -log (1.0 - intc_crit) / extinctionCoef;
   else
      laiEquilibWaterToday = lai;

   // average of the last 10 days of laiEquilibWater
   laiEquilibWater.push_back(laiEquilibWaterToday);
   float avLaiEquilibWater = movingAvgVector(laiEquilibWater,10);

   // calculate a 5 day moving average of the supply demand ratio
   avSD.push_back(plant->water->getSdRatio());

   float dltSlaiWater = 0.0;
   if(movingAvgVector(avSD,5) < senThreshold)
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
   // at flag, targetSLN is set to current SLN

   float laiToday = calcLAI();
   float nRequired = laiToday * targetSLN;

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


   float laiToday = calcLAI();
   float slnToday = calcSLN();

   // total N available by dilution
   float dilutionN = plant->phenology->getDltTT()
      * (dilnNSlope * slnToday + dilnNInt) * laiToday;
   dilutionN = Max(dilutionN,0.0);

   // pre anthesis, get N from dilution, reduction in dltLai and senesence
   if(stage <= flowering)
      {
      float nProvided = Min(dilutionN,requiredN/2.0);
      dltNRetranslocate -= nProvided;
      requiredN -= nProvided;
      if(requiredN <= 0.0001)
         return nProvided;

      // not sufficient N from dilution - take from decreasing dltLai and senescence
      if(dltLAI > 0)
         {
         float n = dltLAI * newLeafSLN;
         float laiN = min(n,requiredN/2);
         dltLAI = (n - laiN) / newLeafSLN;
         requiredN -= laiN;
         nProvided += laiN;
         }
      // recalc the SLN after this N has been removed
      laiToday = calcLAI();
      slnToday = calcSLN();
      float maxN = plant->phenology->getDltTT()
      * (dilnNSlope * slnToday + dilnNInt) * laiToday;
      requiredN = Min(requiredN,maxN);

      float senescenceLAI = Max(divide(requiredN,(slnToday-senescedLeafSLN)),0.0);

      float newN = Max(senescenceLAI * (slnToday-senescedLeafSLN),0.0);
      dltNRetranslocate -= newN;
      nProvided += newN;
      dltSlaiN += senescenceLAI;
      dltSlai = Max(dltSlai,dltSlaiN);

      dltNSenesced += senescenceLAI * senescedLeafSLN;

      return nProvided;
      }
   else        // post anthesis
      {
      // if sln > 1, dilution then senescence
      if(slnToday > 1.0)
         {
         float nProvided = Min(dilutionN,requiredN);
         dltNRetranslocate -= nProvided;
         requiredN -= nProvided;
         if(requiredN <= 0.0001)
            return nProvided;

         // rest from senescence
         // recalc the SLN after this N has been removed
         laiToday = calcLAI();
         slnToday = calcSLN();
         float maxN = plant->phenology->getDltTT()
                     * (dilnNSlope * slnToday + dilnNInt) * laiToday;
         requiredN = Min(requiredN,maxN);

         float senescenceLAI = Max(divide(requiredN,(slnToday-senescedLeafSLN)),0.0);

         float newN = Max(senescenceLAI * (slnToday-senescedLeafSLN),0.0);
         dltNRetranslocate -= newN;
         nProvided += newN;
         dltSlaiN += senescenceLAI;
         dltSlai = Max(dltSlai,dltSlaiN);

         dltNSenesced += senescenceLAI * senescedLeafSLN;

         return nProvided;
         }
      else     // sln < 1.0
         {
         // half from dilution and half from senescence
         // dilution
         float nProvided = Min(dilutionN,requiredN/2.0);
         dltNRetranslocate -= nProvided;
         requiredN -= nProvided;

         // senescence
         // recalc the SLN after this N has been removed
         laiToday = calcLAI();
         slnToday = calcSLN();
         float maxN = plant->phenology->getDltTT()
                  * (dilnNSlope * slnToday + dilnNInt) * laiToday;
         requiredN = Min(requiredN,maxN);

         float senescenceLAI = Max(divide(requiredN,(slnToday-senescedLeafSLN)),0.0);

         float newN = Max(senescenceLAI * (slnToday-senescedLeafSLN),0.0);
         dltNRetranslocate -= newN;
         nProvided += newN;
         dltSlaiN += senescenceLAI;
         dltSlai = Max(dltSlai,dltSlaiN);

         dltNSenesced += senescenceLAI * senescedLeafSLN;

         return nProvided;
         }
      }
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
   tplaMax = (pow(plant->getFtn() + 1.0F, tillerCoef) * pow(finalLeafNo,mainStemCoef)) * scm2smm;
   }
//------------------------------------------------------------------------------------------------
float Leaf::calcDltPotentialTPLA(void)
   {
   // need to ramp the dltPotTPLA for the first 80Cd because the function evaluates to approx 2000
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
//  Return the lai that would senesce on the current day from light (crowding)
float Leaf::calcLaiSenescenceLight(void)
   {
   //    senRadnCrit
   float critTransmission = divide(senRadnCrit,plant->today.radn);
   /* TODO : Direct translation - needs cleanup */
//            ! needs rework for row spacing
   float laiEqlbLightToday;
   if (critTransmission > 0.0)laiEqlbLightToday = -log (critTransmission)/extinctionCoef;
   else laiEqlbLightToday = lai;

   // average of the last 10 days of laiEquilibLight
   laiEquilibLight.push_back(laiEqlbLightToday);
   float avLaiEquilibLight = movingAvgVector(laiEquilibLight,10);

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
   coverGreen = Max(0.0,divide(1.0 - exp(-extinctionCoef * lai * skipRow), skipRow,0.0));
   coverSen = Max(0.0,divide(1.0 - exp(-extinctionCoef * sLai * skipRow), skipRow,0.0));
   coverTot = 1.0 - (1.0 - coverGreen) * (1 - coverSen);
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

   float dmGreenLeafToday = dmGreen + dltDmGreen + dmRetranslocate;   // -ve
   float slaToday = divide(laiToday,dmGreenLeafToday);

   dltDmSenesced = divide(dltSlai,slaToday);


   float slnToday = divide(nGreen,laiToday);
   dltNSenesced  += dltSlai * Max((slnToday - senescedLeafSLN),0.0);

   }
//------------------------------------------------------------------------------------------------
float Leaf::partitionDM(float dltDM)
   {
   float dltDmLeafMax = divide (dltStressedLAI,slaMin * smm2sm);

   float leafPartitionCoef = 1.0 / (1.0 + leafPartitionRate * pow(nLeaves,2.0F));
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
void Leaf::laiDetachment(vector<float> senDetachFrac)
   {
   //These are change within the call but are not used anywhere
   float sLaiDetachedDelta;
   float dLaiDetachedDelta;

   //Do Calculations
   calcPartFractionDelta (partNo, senDetachFrac, sLai, sLaiDetachedDelta);
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
   char msg[120];
   sprintf(msg,"Maximum LAI           = %.1f \t\t Number of leaves        = %.1f\n",
            maxLai,nLeaves);
   scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
