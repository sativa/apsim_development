#include "PlantHerbage.h"
#include "Conversion.h"

#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML \
   "<type  kind=\"single\"/>"
#define stringTypeDDML \
   "<type  kind=\"string\"/>"

      const int  GREEN = 0 ;
      const int  SENESCED = 1 ;
      const int  DEAD = 2 ;

//      enum plantPart {ROOT, LEAF, STEM, POD, MEAL, OIL};
 //      indices of plant part names
      const int  ROOT = 0 ;
      const int  LEAF = 1 ;
      const int  STEM = 2 ;
      const int  POD  = 3 ;
      const int  MEAL = 4 ; // excludes oil component
      const int  OIL  = 5 ; // seed oil

// number of plant parts
// const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
PlantHerbage::PlantHerbage(protocol::Component *s) : HerbageBase(s)
   {
////      system = s;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
PlantHerbage::~PlantHerbage(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PlantHerbage::doInit1(const FString& sdml)
   {
//   protocol::Component::doInit1(sdml);

//   dmFeedOnOfferID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
//   dmFeedRemovedID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

   }

// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PlantHerbage::doInit2(void)
   {
      readParameters (); // Read constants
      readHerbageModuleParameters ();
      doRunTimeReg ();
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
      cropMatureStageNo = 1000;
   }

void PlantHerbage::doDigestibility(void)
{
      getVariables();

////      calcDmdDecline();

      if (dmVeg.total() > 0.0)
      {

// distribute herbage


      calcDmdDistribution(dmdFractionVeg);

      if (cDebug == "on")
      {
         for (int pool = 0; pool < cNumDmdPoolsVeg; pool++)
         {
            ostringstream msgFraction;
            msgFraction << endl << "Plant Herbage dmd distribution, pool " << (pool+1) << ":-" << endl;
            msgFraction << dmdFractionVeg[pool];
            system->writeString (msgFraction.str().c_str());
         }

         for (int pool = 0; pool < cNumDmdPoolsSeed; pool++)
         {
            ostringstream msgFraction;
            msgFraction << endl << "Plant Seed dmd distribution, pool " << (pool+1) << ":-" << endl;
            msgFraction << dmdFractionSeed[pool];
            system->writeString (msgFraction.str().c_str());
         }
      }
   }
}

// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void PlantHerbage::doRunTimeReg(void)
   {

   dmGreenID = system->addRegistration(RegistrationType::get, "dm_green", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   nGreenID = system->addRegistration(RegistrationType::get, "n_green", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   pGreenID = system->addRegistration(RegistrationType::get, "p_green", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab

   dmSenescedID = system->addRegistration(RegistrationType::get, "dm_senesced", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   nSenescedID = system->addRegistration(RegistrationType::get, "n_senesced", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   pSenescedID = system->addRegistration(RegistrationType::get, "p_senesced", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab

   dmDeadID = system->addRegistration(RegistrationType::get, "dm_dead", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   nDeadID = system->addRegistration(RegistrationType::get, "n_dead", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab
   pDeadID = system->addRegistration(RegistrationType::get, "p_dead", singleArrayTypeDDML,"", herbageModuleName().c_str());   // parameter crop name=lablab

   tramplingID = system->addRegistration(RegistrationType::get, "trampling", singleTypeDDML,"", "");
   heightID = system->addRegistration(RegistrationType::get, "height", singleTypeDDML,"", herbageModuleName().c_str());
   stageID = system->addRegistration(RegistrationType::get, "stage", singleTypeDDML,"", herbageModuleName().c_str());
   stageNameID = system->addRegistration(RegistrationType::get, "stage_name", stringTypeDDML,"", herbageModuleName().c_str());

    removeCropBiomassID = system->addRegistration(RegistrationType::event, "remove_crop_biomass", DDML(protocol::removeCropDmType()).c_str(),"", herbageModuleName().c_str());
    detachRateID = system->addRegistration(RegistrationType::event, "detach_crop_biomass_rate", "","", herbageModuleName().c_str());
  }


// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PlantHerbage::doGrazed(protocol::remove_herbageType &grazed)
{
      protocol::removeCropDmType crop;

      doDmdPoolsToHerbageParts(grazed, crop);

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal +=  crop.dm[pool].dlt[part];
         }
      }

      if (cDebug == "on")
      {
         ostringstream msg;
         msg << endl << "Remove herbage plant parts:-" << endl;

         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               msg << "   dm " << crop.dm[pool].pool << " " << crop.dm[pool].part[part] << " = " << crop.dm[pool].dlt[part] << " (g/m2)" << endl;
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << ends;

         system->writeString (msg.str().c_str());

      }

      if (dmTotal > 1.0e-6)
      {
         system->publish (removeCropBiomassID, crop);
      }

      if (trampling() > 1.0e-6)
      {
         ostringstream msg;
         msg << endl << "Detach herbage plant parts:-" << endl;

         float detachRate = trampling() * c.specificDetachRate;
         msg << "   Detach fraction = " << detachRate << " ()" << endl << ends;
         system->writeString (msg.str().c_str());

         system->publish (detachRateID, detachRate);
      }

}

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PlantHerbage::doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop)
{
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++)
      {
         PlantPool poolDm = dmVeg * dmdFractionVeg[pool];
         float dmTot = poolDm.total();
         partFractionVeg[pool] = poolDm / dmTot;
      }

      for (int pool = 0; pool < numDmdPoolsSeed(); pool++)
      {
         SeedPool poolDm = dmSeed * dmdFractionSeed[pool];
         float dmTot = poolDm.total();
         partFractionSeed[pool] = poolDm / dmTot;
      }

      protocol::dmType dm;
      crop.dm.erase(crop.dm.begin(), crop.dm.end());
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "green";
      dm.part.push_back("leaf");
      float dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("pod");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].green.pod;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("meal");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].green.meal;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("oil");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].green.oil;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "senesced";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("pod");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].senesced.pod;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("meal");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].senesced.meal;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("oil");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].senesced.oil;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "dead";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].dead.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].dead.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("pod");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmPart += grazed.herbage[pool]*partFractionVeg[pool].dead.pod;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("meal");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].dead.meal;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("oil");
      dmPart = 0.0;
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmPart += grazed.seed[pool]*partFractionSeed[pool].dead.oil;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());
}

void PlantHerbage::getParts(PlantPartType &parts, SeedPartType &seedParts, unsigned partsID)
{
      protocol::Variant* variant;
      bool ok = system->getVariable(partsID, variant, true);
      if (ok)
      {
         vector <float> partsArray;
         bool ok = variant->unpack(partsArray);
         if (ok && partsArray.size() >= 2)
         {
            parts.stem = partsArray[STEM]*g2kg/sm2ha;
            parts.leaf = partsArray[LEAF]*g2kg/sm2ha;
            parts.pod = partsArray[POD]*g2kg/sm2ha;
            seedParts.meal = partsArray[MEAL]*g2kg/sm2ha;
            seedParts.oil = partsArray[OIL]*g2kg/sm2ha;
         }
         else
         {
            throw std::runtime_error("Couldn't unpack partsArray");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable partsID");
      }
}

void PlantHerbage::getPGreen(PlantPartType &pGreen, PlantPool &dm, SeedPartType &pGreenSeed, SeedPool &dmSeed)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pGreenID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] || P[LEAF] || P[POD] || P[MEAL] || P[OIL] > 0.0)
               {
                  pGreen.stem = P[STEM]*g2kg/sm2ha;
                  pGreen.leaf = P[LEAF]*g2kg/sm2ha;
                  pGreen.pod = P[POD]*g2kg/sm2ha;
                  pGreenSeed.meal = P[MEAL]*g2kg/sm2ha;
                  pGreenSeed.oil = P[OIL]*g2kg/sm2ha;
               }
               else
               {
                  pGreen.stem = c.pConcGreenStemDefault * dm.green.stem;  // parameter for default P contents
                  pGreen.leaf = c.pConcGreenLeafDefault * dm.green.leaf;
                  pGreen.pod = c.pConcGreenPodDefault * dm.green.pod;
                  pGreenSeed.meal = c.pConcGreenMealDefault * dmSeed.green.meal;
                  pGreenSeed.oil = c.pConcGreenOilDefault * dmSeed.green.oil;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pGreen");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pGreenID");
         }
}

void PlantHerbage::getPSenesced(PlantPartType &pSenesced, PlantPool &dm, SeedPartType &pSenescedSeed, SeedPool &dmSeed)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pSenescedID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] || P[LEAF] || P[POD] || P[MEAL] || P[OIL] > 0.0)
               {
                  pSenesced.stem = P[STEM]*g2kg/sm2ha;
                  pSenesced.leaf = P[LEAF]*g2kg/sm2ha;
                  pSenesced.pod = P[POD]*g2kg/sm2ha;
                  pSenescedSeed.meal = P[MEAL]*g2kg/sm2ha;
                  pSenescedSeed.oil = P[OIL]*g2kg/sm2ha;
               }
               else
               {
                  pSenesced.stem = c.pConcSenescedStemDefault * dm.senesced.stem;  // parameter for default P contents
                  pSenesced.leaf = c.pConcSenescedLeafDefault * dm.senesced.leaf;
                  pSenesced.pod = c.pConcSenescedPodDefault * dm.senesced.pod;
                  pSenescedSeed.meal = c.pConcSenescedMealDefault * dmSeed.senesced.meal;
                  pSenescedSeed.oil = c.pConcSenescedOilDefault * dmSeed.senesced.oil;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pSenesced");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pSenescedID");
         }
}

void PlantHerbage::getPDead(PlantPartType &pDead, PlantPool &dm, SeedPartType &pDeadSeed, SeedPool &dmSeed)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pDeadID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] || P[LEAF] || P[POD] || P[MEAL] || P[OIL] > 0.0)
               {
                  pDead.stem = P[STEM]*g2kg/sm2ha;
                  pDead.leaf = P[LEAF]*g2kg/sm2ha;
                  pDead.pod = P[POD]*g2kg/sm2ha;
                  pDeadSeed.meal = P[MEAL]*g2kg/sm2ha;
                  pDeadSeed.oil = P[OIL]*g2kg/sm2ha;
               }
               else
               {
                  pDead.stem = c.pConcDeadStemDefault * dm.dead.stem;  // parameter for default P contents
                  pDead.leaf = c.pConcDeadLeafDefault * dm.dead.leaf;
                  pDead.pod = c.pConcDeadPodDefault * dm.dead.pod;
                  pDeadSeed.meal = c.pConcDeadMealDefault * dmSeed.dead.meal;
                  pDeadSeed.oil = c.pConcDeadOilDefault * dmSeed.dead.oil;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pDead");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pDeadID");
         }
}

void PlantHerbage::getTrampling(void)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(tramplingID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(eTrampling);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack trampling");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable tramplingID");
         }
}

void PlantHerbage::getHeight(float &height)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(heightID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(height);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack height");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable heightID");
         }
}

void PlantHerbage::getStage()
{
      protocol::Variant* variant;
         bool ok = system->getVariable(stageID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(cropStageNo);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack cropStageNo");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable stageID");
         }

         ok = system->getVariable(stageNameID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(cropStageName);
            if (ok)
            {  // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack cropStageName");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable stageNameID");
         }
         if (cropStageName == "maturity")
         {
            cropMatureStageNo = cropStageNo;
         }
         else
         {  // leave as is
         }
}

void PlantHerbage::getVariables(void)
{
         // Get dm GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
      PlantPartType dmGreen;                              // get type names: standing,lying or green, senesced, dead
      SeedPartType dmGreenSeed;                              // get type names: standing,lying or green, senesced, dead
      getParts(dmGreen, dmGreenSeed, dmGreenID);                       // get element: dm, N, P, ash alk
                                                          // get dm deltas?
         // Get dm SENESCED
      PlantPartType dmSenesced;
      SeedPartType dmSenescedSeed;
      getParts(dmSenesced, dmSenescedSeed, dmSenescedID);

         // Get dm DEAD
      PlantPartType dmDead;
      SeedPartType dmDeadSeed;
      getParts(dmDead, dmDeadSeed, dmDeadID);

      dmVeg.setValue(dmGreen, dmSenesced, dmDead);
      dmSeed.setValue(dmGreenSeed, dmSenescedSeed, dmDeadSeed);

      if ((dmVeg.total() + dmSeed.total()) > 0.0)
      {
            // Get N GREEN
         PlantPartType nGreen;
         SeedPartType nGreenSeed;
         getParts(nGreen, nGreenSeed, nGreenID);

         // Get N SENESCED
         PlantPartType nSenesced;
         SeedPartType nSenescedSeed;
         getParts(nSenesced, nSenescedSeed, nSenescedID);

         // Get N DEAD
         PlantPartType nDead;
         SeedPartType nDeadSeed;
         getParts(nDead, nDeadSeed, nDeadID);

         NVeg.setValue(nGreen, nSenesced, nDead);
         NSeed.setValue(nGreenSeed, nSenescedSeed, nDeadSeed);

         // Get P GREEN
         PlantPartType pGreen;
         SeedPartType pGreenSeed;
         getPGreen(pGreen, dmVeg, pGreenSeed, dmSeed);

         // Get P SENESCED
         PlantPartType pSenesced;
         SeedPartType pSenescedSeed;
         getPSenesced(pSenesced, dmVeg, pSenescedSeed, dmSeed);

         // Get P DEAD
         PlantPartType pDead;
         SeedPartType pDeadSeed;
         getPDead(pDead, dmVeg, pDeadSeed, dmSeed);

         PVeg.setValue(pGreen, pSenesced, pDead);
         PSeed.setValue(pGreenSeed, pSenescedSeed, pDeadSeed);

        // Get HEIGHT
//         PlantPool  heightRatio;

         getHeight(height);
        // Get plant stage and seed maturity
         getStage();
         getTrampling();
      }

}


void PlantHerbage::calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin)
{

// get GREEN Leaf, stem, pod dmd classes
      dmdClass (dmdMaxVeg.green.leaf, dmdMinVeg.green.leaf, dmdClassMaxVeg.green.leaf, dmdClassMinVeg.green.leaf);
      dmdClass (dmdMaxVeg.green.stem, dmdMinVeg.green.stem, dmdClassMaxVeg.green.stem, dmdClassMinVeg.green.stem);
      dmdClass (dmdMaxVeg.green.pod, dmdMinVeg.green.pod, dmdClassMaxVeg.green.pod, dmdClassMinVeg.green.pod);

// get SENESCED Leaf, stem, pod dmd classes
      dmdClass (dmdMaxVeg.senesced.leaf, dmdMinVeg.senesced.leaf, dmdClassMaxVeg.senesced.leaf, dmdClassMinVeg.senesced.leaf);
      dmdClass (dmdMaxVeg.senesced.stem, dmdMinVeg.senesced.stem, dmdClassMaxVeg.senesced.stem, dmdClassMinVeg.senesced.stem);
      dmdClass (dmdMaxVeg.senesced.pod, dmdMinVeg.senesced.pod, dmdClassMaxVeg.senesced.pod, dmdClassMinVeg.senesced.pod);

// get DEAD Leaf, stem, pod dmd classes
      dmdClass (dmdMaxVeg.dead.leaf, dmdMinVeg.dead.leaf, dmdClassMaxVeg.dead.leaf, dmdClassMinVeg.dead.leaf);
      dmdClass (dmdMaxVeg.dead.stem, dmdMinVeg.dead.stem, dmdClassMaxVeg.dead.stem, dmdClassMinVeg.dead.stem);
      dmdClass (dmdMaxVeg.dead.pod, dmdMinVeg.dead.pod, dmdClassMaxVeg.dead.pod, dmdClassMinVeg.dead.pod);
}

void PlantHerbage::calcDmdDistribution(PlantPool dmdFraction[])
{
////      PlantPool dmdAvgVeg = dmdMaxVeg - dQVeg;

      float fraction[maxDmdPoolsVeg];
      float fractionSeed[maxDmdPoolsSeed];

// get GREEN Leaf dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.leaf, dmdMaxVeg.green.leaf, dmdMinVeg.green.leaf, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.leaf = fraction[pool];

// get GREEN stem dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.stem, dmdMaxVeg.green.stem, dmdMinVeg.green.stem, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.stem = fraction[pool];

// get GREEN pod dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.green.pod, dmdMaxVeg.green.pod, dmdMinVeg.green.pod, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].green.pod = fraction[pool];

// get GREEN meal dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.green.meal, dmdRipeSeed.green.meal, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].green.meal = fractionSeed[pool];

// get GREEN oil dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.green.oil, dmdRipeSeed.green.oil, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].green.oil = fractionSeed[pool];

// get SENESCED Leaf dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.leaf, dmdMaxVeg.senesced.leaf, dmdMinVeg.senesced.leaf, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.leaf = fraction[pool];

// get SENESCED stem dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.stem, dmdMaxVeg.senesced.stem, dmdMinVeg.senesced.stem, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.stem = fraction[pool];

// get SENESCED pod dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.senesced.pod, dmdMaxVeg.senesced.pod, dmdMinVeg.senesced.pod, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].senesced.pod = fraction[pool];

// get SENESCED meal dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.senesced.meal, dmdRipeSeed.senesced.meal, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].senesced.meal = fractionSeed[pool];

// get SENESCED oil dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.senesced.oil, dmdRipeSeed.senesced.oil, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].senesced.oil = fractionSeed[pool];

// get DEAD Leaf dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.dead.leaf, dmdMaxVeg.dead.leaf, dmdMinVeg.dead.leaf, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].dead.leaf = fraction[pool];

// get DEAD Stem dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.dead.stem, dmdMaxVeg.dead.stem, dmdMinVeg.dead.stem, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].dead.stem = fraction[pool];

// get DEAD Pod dmd fractions
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) fraction[pool] = 0.0;
      proportion (dmdAvgVeg.dead.pod, dmdMaxVeg.dead.pod, dmdMinVeg.dead.pod, fraction);
      for (int pool = 0; pool < numDmdPoolsVeg(); pool++) dmdFraction[pool].dead.pod = fraction[pool];

// get DEAD meal dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.dead.meal, dmdRipeSeed.dead.meal, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].dead.meal = fractionSeed[pool];

// get DEAD oil dmd fractions
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) fractionSeed[pool] = 0.0;
      proportion (dmdUnripeSeed.dead.oil, dmdRipeSeed.dead.oil, fractionSeed);
      for (int pool = 0; pool < numDmdPoolsSeed(); pool++) dmdFractionSeed[pool].dead.oil = fractionSeed[pool];
}


   // REST
float PlantHerbage::trampling ( void )
{
      return eTrampling;
}

float PlantHerbage::dmTotalVeg ( void )
{
      return dmVeg.total();
}

float PlantHerbage::dmTotalSeed ( void )
{
      return dmSeed.total();
}

float PlantHerbage::dmTotVeg ( int pool )
{
      PlantPool poolDmVeg = dmVeg * dmdFractionVeg[pool];
      float dmTot = poolDmVeg.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float PlantHerbage::dmTotSeed ( int pool )
{
      SeedPool poolDmSeed = dmSeed * dmdFractionSeed[pool];
      float dmTot = poolDmSeed.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float PlantHerbage::cpConcVeg ( int pool )
{
        PlantPool poolNVeg = NVeg * dmdFractionVeg[pool];
        float nTot = poolNVeg.total();
        float nConc = divide (nTot, dmTotVeg(pool), 0.0);
      return nConc * c.cpNRatio;
}

float PlantHerbage::cpConcSeed ( int pool )
{
        SeedPool poolNSeed = NSeed * dmdFractionSeed[pool];
        float nTot = poolNSeed.total();
        float nConc = divide (nTot, dmTotSeed(pool), 0.0);
      return nConc * c.cpNRatio;
}

float PlantHerbage::pConcVeg ( int pool )
{
      PlantPool NPRatioVeg(c.NPRatioGreenLeafDefault,  c.NPRatioGreenStemDefault,  c.NPRatioGreenPodDefault,  c.NPRatioSenescedLeafDefault,  c.NPRatioSenescedStemDefault,  c.NPRatioSenescedPodDefault,  c.NPRatioDeadLeafDefault,  c.NPRatioDeadStemDefault,  c.NPRatioDeadPodDefault);
//      poolP = P * dmdFractionVeg[pool];
      PlantPool poolPVeg = NVeg/NPRatioVeg * dmdFractionVeg[pool];
      float pTot = poolPVeg.total();
      float pConc = divide (pTot, dmTotVeg(pool), 0.0);
      return pConc;
}

float PlantHerbage::pConcSeed ( int pool )
{
      SeedPool NPRatioSeed(c.NPRatioGreenMealDefault,  c.NPRatioGreenOilDefault,  c.NPRatioSenescedMealDefault,  c.NPRatioSenescedOilDefault,  c.NPRatioDeadMealDefault,  c.NPRatioDeadOilDefault);
//      poolP = P * dmdFractionSeed[pool];
      SeedPool poolPSeed = NSeed/NPRatioSeed * dmdFractionSeed[pool];
      float pTot = poolPSeed.total();
      float pConc = divide (pTot, dmTotSeed(pool), 0.0);
      return pConc;
}

float PlantHerbage::ashAlkVeg ( int pool )
{
      PlantPool partAshAlkVeg(c.AshAlkGreenLeafDefault, c.AshAlkGreenStemDefault, c.AshAlkGreenPodDefault, c.AshAlkSenescedLeafDefault, c.AshAlkSenescedStemDefault, c.AshAlkSenescedPodDefault, c.AshAlkDeadLeafDefault, c.AshAlkDeadStemDefault, c.AshAlkDeadPodDefault);
      partAshAlkVeg = partAshAlkVeg*cmol2mol;
      PlantPool poolAAVeg    = partAshAlkVeg * dmVeg * dmdFractionVeg[pool];  // ash alk to be got from lablab
      float aaTot = poolAAVeg.total();
      float ashAlk = divide (aaTot, dmTotVeg(pool), 0.0);
      return ashAlk;
}

float PlantHerbage::ashAlkSeed ( int pool )
{
      SeedPool partAshAlkSeed(c.AshAlkGreenMealDefault, c.AshAlkGreenOilDefault, c.AshAlkSenescedMealDefault, c.AshAlkSenescedOilDefault, c.AshAlkDeadMealDefault, c.AshAlkDeadOilDefault);
      partAshAlkSeed = partAshAlkSeed*cmol2mol;
      SeedPool poolAASeed    = partAshAlkSeed * dmSeed * dmdFractionSeed[pool];  // ash alk to be got from lablab
      float aaTot = poolAASeed.total();
      float ashAlk = divide (aaTot, dmTotSeed(pool), 0.0);
      return ashAlk;
}

float PlantHerbage::sConcVeg ( int pool )
{
      PlantPool NSRatioVeg(c.NSRatioGreenLeafDefault, c.NSRatioGreenStemDefault, c.NSRatioGreenPodDefault, c.NSRatioSenescedLeafDefault, c.NSRatioSenescedStemDefault, c.NSRatioSenescedPodDefault, c.NSRatioDeadLeafDefault, c.NSRatioDeadStemDefault, c.NSRatioDeadPodDefault);
      PlantPool poolSVeg = NVeg/NSRatioVeg * dmdFractionVeg[pool];
      float sTot = poolSVeg.total();
      float sConc = divide (sTot, dmTotVeg(pool), 0.0);
      return sConc;
}

float PlantHerbage::sConcSeed ( int pool )
{
      SeedPool NSRatioSeed(c.NSRatioGreenMealDefault, c.NSRatioGreenOilDefault, c.NSRatioSenescedMealDefault, c.NSRatioSenescedOilDefault, c.NSRatioDeadMealDefault, c.NSRatioDeadOilDefault);
      SeedPool poolSSeed = NSeed/NSRatioSeed * dmdFractionSeed[pool];
      float sTot = poolSSeed.total();
      float sConc = divide (sTot, dmTotSeed(pool), 0.0);
      return sConc;
}

float PlantHerbage::proportionGreen ( void )
{
         float dm_green = dmVeg.green.pod + dmVeg.green.stem + dmVeg.green.leaf;
         float dm_dead = dmVeg.senesced.pod + dmVeg.senesced.stem + dmVeg.senesced.leaf + dmVeg.dead.pod + dmVeg.dead.stem + dmVeg.dead.leaf;
         float dm_total = dm_green + dm_dead;
         return divide (dm_green, dm_total, 0.0);
}

float PlantHerbage::proportionLegume ( void )
{
   return c.proportionLegume;       //FIXME - calc legume content
}

float PlantHerbage::selectionFactor ( void )
{
   return 0.0; // ??
}

//===========================================================================
void PlantHerbage::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValueVeg[0];
   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1];
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;
   const float roundingMargin = 1.0e-2;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdAvg > dmdMax + errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility > Maximum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Maximum      = " <<  dmdMax << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdAvg < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility < Minimum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Minimum      = " <<  dmdMin << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin > dmdAvg + errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Average digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax < dmdAvg - errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility < Average digestibility:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }


   float x = (dmdAvg - dmdMin) / (dmdMax - dmdMin);
   int startDmd = (MAXDMD - dmdMax)*10.0 + errorMargin;
   int endDmd = (MAXDMD - dmdMin)*10.0 + errorMargin;
   int numPools = (endDmd - startDmd) + 1;

   switch (numPools)
   {
      case 1:
         {
            dmdFraction[startDmd] = 1.0;
         }
         break;
      case 2:
         {
            dmdFraction[startDmd] = x;
            dmdFraction[startDmd+1] = 1.0-x;
         }
         break;
      case 3:
         {
            dmdFraction[startDmd] = pow(x, 2);
            dmdFraction[startDmd+1] = 2.0 * x * (1.0-x);
            dmdFraction[startDmd+2] = pow(1.0-x, 2);
         }
         break;
      case 4:
         {
            dmdFraction[startDmd] = pow(x, 3);
            dmdFraction[startDmd+1] = 3.0 * pow(x, 2) * (1.0-x);
            dmdFraction[startDmd+2] = 3.0 * x * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = pow(1.0-x, 3);
         }
         break;
      case 5:
         {
            dmdFraction[startDmd] = pow(x, 4);
            dmdFraction[startDmd+1] = 4.0 * pow(x, 3) * (1.0-x);
            dmdFraction[startDmd+2] = 6.0 * pow(x, 2) * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = 4.0 * x * pow(1.0-x,3);
            dmdFraction[startDmd+4] = pow(1.0-x, 4);
         }
         break;
      default:
         throw std::runtime_error("Too many digestibility classes");

   }
   for (int pool = 0; pool < numPools; pool ++)
   {
      if (dmdFraction[startDmd + pool] < roundingMargin)
      {
         dmdFraction[startDmd+pool+1] += dmdFraction[startDmd+pool];
         dmdFraction[startDmd+pool] = 0.0;
      }
   }
}

//===========================================================================
void PlantHerbage::proportion (float dmdUnripe, float dmdRipe, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

//   const float MAXDMD = cDmdValueSeed[0];
//   const float MINDMD = cDmdValueSeed[cNumDmdPoolsSeed-1];
   const float MAXDMD = 0.8;
   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdRipe < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Ripe digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdRipe << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdUnripe > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Unripe digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdUnripe << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   int Unripe = 0;
   int ripe = 1;

   if (ripeSeed())
   {
      dmdFraction[Unripe] = 0.0;
      dmdFraction[ripe] = 1.0;
   }
   else
   {
      dmdFraction[Unripe] = 1.0;
      dmdFraction[ripe] = 0.0;
   }
}

//===========================================================================
void PlantHerbage::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValueVeg[0];
   const float MINDMD = cDmdValueVeg[cNumDmdPoolsVeg-1];
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdMin < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   if (dmdMax > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   if (dmdMax < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Maximum      = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMax = dmdClassNum;
      if (fabs(dmdMax - cDmdValueVeg[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPoolsVeg; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMin = dmdClassNum;
      if (fabs(dmdMin - cDmdValueVeg[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }
}

   // REST
float PlantHerbage::hHeight ( void )
{
      return height;
}

float PlantHerbage::bD ( void )
{
//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
      float bd = divide(dmVeg.total() *kg2g/ha2sm, hHeight()*mm2m, 0.0);
      return bd;
}

float PlantHerbage::heightRatioVeg ( int pool )
{
      return  divide(100.0, 0.03*bD(), 0.0);
}

float PlantHerbage::heightRatioSeed ( int pool )
{
      return  1.0;
}

float PlantHerbage::dmdValueVeg ( int pool )
{
      return cDmdValueVeg[pool];
}

float PlantHerbage::protDgVeg ( int pool )
{
      return cDmdValueVeg[pool] + 0.1;
}

int PlantHerbage::numDmdPoolsVeg ( void )
{
   return cNumDmdPoolsVeg; // ??
}

float PlantHerbage::dmdValueSeed ( int pool )
{
      return cDmdValueSeed[pool];
}

float PlantHerbage::protDgSeed ( int pool )
{
      return cDmdValueSeed[pool] + 0.1;
}

int PlantHerbage::numDmdPoolsSeed ( void )
{
   return cNumDmdPoolsSeed; // ??
}

string PlantHerbage::herbageModuleName(void)
   {
      return cHerbageModuleName;
   }

string PlantHerbage::debug(void)
   {
      return cDebug;
   }

int PlantHerbage::seedClass(int pool)
{
      return c.seedClass[pool];
}

bool PlantHerbage::ripeSeed(void)
{
      return (seedMaturity() == 1);
}

int PlantHerbage::seedMaturity(void)
{
   if (cropMatureStageNo < 1000)     // maturity has been reached
      return 1;    // seed is ripe
   else
      return 0;    // seed is unripe
}

void PlantHerbage::readParameters ( void )
{

//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - conversion object reading parameters");

    cHerbageModuleName = system->readParameter (section_name, "herbage_module_name");
    cDebug = system->readParameter (section_name, "debug");
    system->readParameter (section_name, "dmdValue", cDmdValueVeg, cNumDmdPoolsVeg, 0.0, 1.0);

      ostringstream msg;
      msg << "Herbage module name = " << cHerbageModuleName << endl
          << "Debug = " << cDebug << ends;
      system->writeString (msg.str().c_str());
}

void PlantHerbage::readHerbageModuleParameters ( void )
{
//- Implementation Section ----------------------------------
      ostringstream msg;
      msg << " - reading  herbage parameters for module '" << herbageModuleName().c_str() << "'" << endl << ends;
      system->writeString (msg.str().c_str());

    system->readParameter (herbageModuleName().c_str(), "specific_detach_rate", c.specificDetachRate, 0.0, 1.0);

    system->readParameter (herbageModuleName().c_str(), "p_conc_green_leaf_default", c.pConcGreenLeafDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_green_stem_default", c.pConcGreenStemDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_green_pod_default", c.pConcGreenPodDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_green_meal_default", c.pConcGreenMealDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_green_oil_default", c.pConcGreenOilDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_senesced_leaf_default", c.pConcSenescedLeafDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_senesced_stem_default", c.pConcSenescedStemDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_senesced_pod_default", c.pConcSenescedPodDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_senesced_meal_default", c.pConcSenescedMealDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_senesced_oil_default", c.pConcSenescedOilDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_dead_leaf_default", c.pConcDeadLeafDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_dead_stem_default", c.pConcDeadStemDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_dead_pod_default", c.pConcDeadPodDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_dead_meal_default", c.pConcDeadMealDefault, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "p_conc_dead_oil_default", c.pConcDeadOilDefault, 0.0, 1.0);

    system->readParameter (herbageModuleName().c_str(), "ash_alk_green_leaf_default", c.AshAlkGreenLeafDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_green_stem_default", c.AshAlkGreenStemDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_green_pod_default", c.AshAlkGreenPodDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_green_meal_default", c.AshAlkGreenMealDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_green_oil_default", c.AshAlkGreenOilDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_senesced_leaf_default", c.AshAlkSenescedLeafDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_senesced_stem_default", c.AshAlkSenescedStemDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_senesced_pod_default", c.AshAlkSenescedPodDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_senesced_meal_default", c.AshAlkSenescedMealDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_senesced_oil_default", c.AshAlkSenescedOilDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_dead_leaf_default", c.AshAlkDeadLeafDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_dead_stem_default", c.AshAlkDeadStemDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_dead_pod_default", c.AshAlkDeadPodDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_dead_meal_default", c.AshAlkDeadMealDefault, 0.0, 500.0);
    system->readParameter (herbageModuleName().c_str(), "ash_alk_dead_oil_default", c.AshAlkDeadOilDefault, 0.0, 500.0);

    system->readParameter (herbageModuleName().c_str(), "ns_ratio_green_leaf_default", c.NSRatioGreenLeafDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_green_stem_default", c.NSRatioGreenStemDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_green_pod_default", c.NSRatioGreenPodDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_green_meal_default", c.NSRatioGreenMealDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_green_oil_default", c.NSRatioGreenOilDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_senesced_leaf_default", c.NSRatioSenescedLeafDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_senesced_stem_default", c.NSRatioSenescedStemDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_senesced_pod_default", c.NSRatioSenescedPodDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_senesced_meal_default", c.NSRatioSenescedMealDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_senesced_oil_default", c.NSRatioSenescedOilDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_dead_leaf_default", c.NSRatioDeadLeafDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_dead_stem_default", c.NSRatioDeadStemDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_dead_pod_default", c.NSRatioDeadPodDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_dead_meal_default", c.NSRatioDeadMealDefault, 0.0, 30.0);
    system->readParameter (herbageModuleName().c_str(), "ns_ratio_dead_oil_default", c.NSRatioDeadOilDefault, 0.0, 30.0);

    system->readParameter (herbageModuleName().c_str(), "np_ratio_green_leaf_default", c.NPRatioGreenLeafDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_green_stem_default", c.NPRatioGreenStemDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_green_pod_default", c.NPRatioGreenPodDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_green_meal_default", c.NPRatioGreenMealDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_green_oil_default", c.NPRatioGreenOilDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_senesced_leaf_default", c.NPRatioSenescedLeafDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_senesced_stem_default", c.NPRatioSenescedStemDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_senesced_pod_default", c.NPRatioSenescedPodDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_senesced_meal_default", c.NPRatioSenescedMealDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_senesced_oil_default", c.NPRatioSenescedOilDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_dead_leaf_default", c.NPRatioDeadLeafDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_dead_stem_default", c.NPRatioDeadStemDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_dead_pod_default", c.NPRatioDeadPodDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_dead_meal_default", c.NPRatioDeadMealDefault, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "np_ratio_dead_oil_default", c.NPRatioDeadOilDefault, 0.0, 10.0);

    int numClasses = 3;
    system->readParameter (herbageModuleName().c_str(), "dmd_green_leaf", c.dmdGreenLeaf, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_green_stem", c.dmdGreenStem, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_green_pod", c.dmdGreenPod, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_senesced_leaf", c.dmdSenescedLeaf, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_senesced_stem", c.dmdSenescedStem, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_senesced_pod", c.dmdSenescedPod, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_dead_leaf", c.dmdDeadLeaf, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_dead_stem", c.dmdDeadStem, numClasses, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "dmd_dead_pod", c.dmdDeadPod, numClasses, 0.0, 1.0);

    int numSeedClasses = 2;
    system->readParameter (herbageModuleName().c_str(), "dmd_seed", cDmdValueSeed, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_green_meal", c.dmdGreenMeal, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_green_oil", c.dmdGreenOil, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_senesced_meal", c.dmdSenescedMeal, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_senesced_oil", c.dmdSenescedOil, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_dead_meal", c.dmdDeadMeal, numSeedClasses, 0.0, 1.0);
//    system->readParameter (herbageModuleName().c_str(), "dmd_dead_oil", c.dmdDeadOil, numSeedClasses, 0.0, 1.0);

   for (int pool = 0; pool < numSeedClasses; pool ++)
   {
       c.dmdGreenMeal[pool] = cDmdValueSeed[pool];
       c.dmdGreenOil[pool] = cDmdValueSeed[pool];
       c.dmdSenescedMeal[pool] = cDmdValueSeed[pool];
       c.dmdSenescedOil[pool] = cDmdValueSeed[pool];
       c.dmdDeadMeal[pool] = cDmdValueSeed[pool];
       c.dmdDeadOil[pool] = cDmdValueSeed[pool];
   }

    system->readParameter (herbageModuleName().c_str(), "cp_n_ratio", c.cpNRatio, 0.0, 10.0);
    system->readParameter (herbageModuleName().c_str(), "proportion_legume", c.proportionLegume, 0.0, 1.0);
    system->readParameter (herbageModuleName().c_str(), "seed_class", c.seedClass, numSeedClasses, 0.0, 6.0);
    cNumDmdPoolsSeed = numSeedClasses;

   const int MAX = 0;
   const int AVG = 1;
   const int MIN = 2;
         //plant pools  GL    GS   SL    SS   DL   DS
//      PlantPool dmdMax(c.dmdGreenLeaf, c.dmdMaxGreenStem, c.dmdMaxSenescedLeaf, c.dmdMaxSenescedStem, c.dmdMaxDeadLeaf, c.dmdMaxDeadStem);
   dmdMaxVeg.setValue(c.dmdGreenLeaf[MAX], c.dmdGreenStem[MAX], c.dmdGreenPod[MAX], c.dmdSenescedLeaf[MAX], c.dmdSenescedStem[MAX], c.dmdSenescedPod[MAX], c.dmdDeadLeaf[MAX], c.dmdDeadStem[MAX], c.dmdDeadPod[MAX]);
   dmdAvgVeg.setValue(c.dmdGreenLeaf[AVG], c.dmdGreenStem[AVG], c.dmdGreenPod[AVG], c.dmdSenescedLeaf[AVG], c.dmdSenescedStem[AVG], c.dmdSenescedPod[AVG], c.dmdDeadLeaf[AVG], c.dmdDeadStem[AVG], c.dmdDeadPod[AVG]);
   dmdMinVeg.setValue(c.dmdGreenLeaf[MIN], c.dmdGreenStem[MIN], c.dmdGreenPod[MIN], c.dmdSenescedLeaf[MIN], c.dmdSenescedStem[MIN], c.dmdSenescedPod[MIN], c.dmdDeadLeaf[MIN], c.dmdDeadStem[MIN], c.dmdDeadPod[MIN]);

   const int UNRIPE = 0;
   const int RIPE = 1;
   dmdUnripeSeed.setValue(c.dmdGreenMeal[UNRIPE], c.dmdGreenStem[UNRIPE], c.dmdSenescedMeal[UNRIPE], c.dmdSenescedOil[UNRIPE], c.dmdDeadMeal[UNRIPE], c.dmdDeadOil[UNRIPE]);
   dmdRipeSeed.setValue(c.dmdGreenMeal[RIPE], c.dmdGreenStem[RIPE], c.dmdSenescedMeal[RIPE], c.dmdSenescedOil[RIPE], c.dmdDeadMeal[RIPE], c.dmdDeadOil[RIPE]);

   calcDmdClass(dmdClassMaxVeg, dmdClassMinVeg);
}

//===========================================================================
float PlantHerbage::divide (float dividend, float divisor, float default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }



