#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <string>
#include <strstream>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>
#include "PlantHerbage.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"

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

      const float kg2g = 1000.0 ;
      const float ha2sm = 10000.0 ;
      const float g2kg = 1.0/kg2g ;
      const float sm2ha = 1.0/ha2sm ;
      const float cmol2mol = 1.0/100.0 ;
      const float mm2m = 1.0/1000.0;

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
PlantHerbage::PlantHerbage(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
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
   }
// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void PlantHerbage::doRunTimeReg(void)
   {
   dmGreenID = system->addRegistration(RegistrationType::get, "dm_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenRetransDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_green_retrans", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nGreenID = system->addRegistration(RegistrationType::get, "n_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pGreenID = system->addRegistration(RegistrationType::get, "p_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmSenescedID = system->addRegistration(RegistrationType::get, "dm_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDetachedDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_detached", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nSenescedID = system->addRegistration(RegistrationType::get, "n_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pSenescedID = system->addRegistration(RegistrationType::get, "p_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmDeadID = system->addRegistration(RegistrationType::get, "dm_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenDeadDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_green_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDeadDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_senesced_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmDeadDetachedDeltaID = system->addRegistration(RegistrationType::get, "dlt_dm_dead_detached", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nDeadID = system->addRegistration(RegistrationType::get, "n_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pDeadID = system->addRegistration(RegistrationType::get, "p_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   heightID = system->addRegistration(RegistrationType::get, "height", singleTypeDDML,"", c.herbageModuleName.c_str());
   thermalTimeID = system->addRegistration(RegistrationType::get, "tt_tot()", singleTypeDDML,"", c.herbageModuleName.c_str());
   thermalTimeBGID = system->addRegistration(RegistrationType::get, "tt_tot(1-2)", singleArrayTypeDDML,"", c.herbageModuleName.c_str());

   removeCropBiomassID = system->addRegistration(RegistrationType::event, "remove_crop_biomass", removeCropDmTypeDDML,"", c.herbageModuleName.c_str());
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PlantHerbage::doGrazed(protocol::remove_herbageType grazed)
{
      for (int pool = 0; pool < c.numDmdPools; pool++)
      {
         PlantPool poolDm = dm * dmdFraction[pool];
         float dmTot = poolDm.total();
         partFraction[pool] = poolDm / dmTot;
      }

      protocol::removeCropDmType crop;
      protocol::dmType dm;
      crop.dm.erase(crop.dm.begin(), crop.dm.end());
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "green";
      dm.part.push_back("leaf");
      float dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].green.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].green.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "senesced";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].senesced.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].senesced.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "dead";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].dead.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].dead.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal +=  crop.dm[pool].dlt[part];
         }
      }

      if (c.debug == "on")
      {
         ostrstream msg;
         msg << endl << "Remove herbage plant parts:-" << endl;

         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               msg << "   dm " << crop.dm[pool].pool << " " << crop.dm[pool].part[part] << " = " << crop.dm[pool].dlt[part] << " (g/m2)" << endl;
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

         system->writeString (msg.str());

      }

      if (dmTotal > 1.0e-6)
      {
         publish (removeCropBiomassID, crop);
      }

}

void PlantHerbage::getParts(PlantPartType &parts, unsigned partsID)
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


void PlantHerbage::getPGreen(PlantPartType &pGreen, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pGreenID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pGreen.stem = P[STEM]*g2kg/sm2ha;
                  pGreen.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pGreen.stem = c.pConcGreenStemDefault * dm.green.stem;  // parameter for default P contents
                  pGreen.leaf = c.pConcGreenStemDefault * dm.green.leaf;
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

void PlantHerbage::getPSenesced(PlantPartType &pSenesced, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pSenescedID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pSenesced.stem = P[STEM]*g2kg/sm2ha;
                  pSenesced.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pSenesced.stem = c.pConcSenescedStemDefault * dm.senesced.stem;  // parameter for default P contents
                  pSenesced.leaf = c.pConcSenescedLeafDefault * dm.senesced.leaf;
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

void PlantHerbage::getPDead(PlantPartType &pDead, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(pDeadID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pDead.stem = P[STEM]*g2kg/sm2ha;
                  pDead.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pDead.stem = c.pConcDeadStemDefault * dm.dead.stem;  // parameter for default P contents
                  pDead.leaf = c.pConcDeadLeafDefault * dm.dead.leaf;
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

void PlantHerbage::getThermalTime(float &thermalTime)
{
      protocol::Variant* variant;
         bool ok = system->getVariable(thermalTimeID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(thermalTime);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack thermalTime");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable thermalTimeID");
         }

         ok = system->getVariable(thermalTimeBGID, variant, true);
         if (ok)
         {
            vector<float> thermalTimeBG;
            bool ok = variant->unpack(thermalTimeBG);
            if (ok)
            {
               for (int stage = 0; stage < thermalTimeBG.size(); stage++)
               {
                  thermalTime -= thermalTimeBG[stage];
               }
               thermalTime = max(0, thermalTime);
            }
            else
            {
               throw std::runtime_error("Couldn't unpack thermalTimeBG");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable thermalTimeBGID");
         }
}

void PlantHerbage::getVariables(PlantPool &dm, PlantPool &N, PlantPool &P, float &height, float &thermalTime)
{
         // Get dm GREEN                                  // get part names: crop type (residue) or plant parts (leaf, stem etc).
      PlantPartType dmGreen;                              // get type names: standing,lying or green, senesced, dead
      getParts(dmGreen, dmGreenID);                       // get element: dm, N, P, ash alk
                                                          // get dm deltas?
         // Get dm SENESCED
      PlantPartType dmSenesced;
      getParts(dmSenesced, dmSenescedID);

         // Get dm DEAD
      PlantPartType dmDead;
      getParts(dmDead, dmDeadID);

      dm.setValue(dmGreen, dmSenesced, dmDead);

      if (dm.total() > 0.0)
      {
            // Get delta dm GREEN
         PlantPartType  dmGreenDelta;
         getParts(dmGreenDelta, dmGreenDeltaID);

//         dmdPoolDm[?]

            // Get delta dm GREEN retrans
         PlantPartType dmGreenRetransDelta;
         getParts(dmGreenRetransDelta, dmGreenRetransDeltaID);

            // Get dm SENESCED
         PlantPartType dmSenescedDelta;
         getParts(dmSenescedDelta, dmSenescedDeltaID);

         PlantPartType dmSenescedDetachedDelta;
         getParts(dmSenescedDetachedDelta, dmSenescedDetachedDeltaID);

            // Get dm DEAD
         PlantPartType dmGreenDeadDelta;
         getParts(dmGreenDeadDelta, dmGreenDeadDeltaID);

         PlantPartType dmSenescedDeadDelta;
         getParts(dmSenescedDeadDelta, dmSenescedDeadDeltaID);

         PlantPartType dmDeadDetachedDelta;
         getParts(dmDeadDetachedDelta, dmDeadDetachedDeltaID);


//         int dmdClass = dmdClassMax.green.
//
//         dmdPoolDm[?].addGreen(dmGreenDelta);
//         dmdPoolDm[?].addGreen(dmGreenRetransDelta);
//         dmdPoolDm[?].removeGreen(dmSenescedDelta);
//         dmdPoolDm[?].removeGreen(dmGreenDeadDelta);
//
//         dmdPoolDm[?].addSenesced(dmSenescedDelta);
//         dmdPoolDm[?].removeSenesced(dmSenescedDetachedDelta);
//         dmdPoolDm[?].removeSenesced(dmSenescedDeadDelta);
//
//         dmdPoolDm[?].addDead(dmGreenDeadDelta);
//         dmdPoolDm[?].addDead(dmSenescedDeadDelta);
//         dmdPoolDm[?].removeDead(dmDeadDetachedDelta);

            // Get N GREEN
         PlantPartType nGreen;
         getParts(nGreen, nGreenID);

         // Get N SENESCED
         PlantPartType nSenesced;
         getParts(nSenesced, nSenescedID);

         // Get N DEAD
         PlantPartType nDead;
         getParts(nDead, nDeadID);

         N.setValue(nGreen, nSenesced, nDead);

         // Get P GREEN
         PlantPartType pGreen;
         getPGreen(pGreen, dm);

         // Get P SENESCED
         PlantPartType pSenesced;
         getPSenesced(pSenesced, dm);

         // Get P DEAD
         PlantPartType pDead;
         getPDead(pDead, dm);

         P.setValue(pGreen, pSenesced, pDead);

        // Get HEIGHT
//         PlantPool  heightRatio;

         getHeight(height);
        // Get Thermal Time
         getThermalTime(thermalTime);
      }

}


void PlantHerbage::calcDmdDecline(const float &thermalTime, PlantPool &dQ)
{

   const float ADJ = 1.1;
   const float TTCorrection = 20.0;

   dQ = dmdMax - dmdAvg;
//   dQ = (dmdAvg - dmdMin) * (KQ5 * thermalTime);
//   dQ.green.leaf = exp(-KQ5*thermalTime*max(0.0,1.0-thermalTime/KQ4)*ADJ) * (dmdAvg.green.leaf - dmdMin.green.leaf)*ADJ + dmdMin.green.leaf;
//   dQ.green.stem = exp(-KQ5*thermalTime*ADJ) * (dmdAvg.green.stem - dmdMin.green.stem)*ADJ + dmdMin.green.stem;
   dQ.green.leaf = max(0.0, (1.0-exp(-c.KQ5Leaf*(thermalTime-c.KQ4-TTCorrection))*ADJ)) * (dmdMax.green.leaf - dmdMin.green.leaf);
   dQ.green.stem = max(0.0, (1.0-exp(-c.KQ5Stem*(thermalTime-TTCorrection))*ADJ)) * (dmdMax.green.stem - dmdMin.green.stem);
//   float grlf = min(100.0, -c.KQ5Leaf*(thermalTime-c.KQ4-TTCorrection));
//   float grst = min(100.0, -c.KQ5Stem*(thermalTime-TTCorrection));
//   dQ.green.leaf = max(0.0, (1.0-exp(grlf)*ADJ)) * (dmdMax.green.leaf - dmdMin.green.leaf);
//   dQ.green.stem = max(0.0, (1.0-exp(grst)*ADJ)) * (dmdMax.green.stem - dmdMin.green.stem);

}

void PlantHerbage::calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin)
{

// get GREEN Leaf & stem dmd classes
      dmdClass (dmdMax.green.leaf, dmdMin.green.leaf, dmdClassMax.green.leaf, dmdClassMin.green.leaf);
      dmdClass (dmdMax.green.stem, dmdMin.green.stem, dmdClassMax.green.stem, dmdClassMin.green.stem);

// get SENESCED Leaf & dmd classes
      dmdClass (dmdMax.senesced.leaf, dmdMin.senesced.leaf, dmdClassMax.senesced.leaf, dmdClassMin.senesced.leaf);
      dmdClass (dmdMax.senesced.stem, dmdMin.senesced.stem, dmdClassMax.senesced.stem, dmdClassMin.senesced.stem);

// get DEAD Leaf & stem dmd classes
      dmdClass (dmdMax.dead.leaf, dmdMin.dead.leaf, dmdClassMax.dead.leaf, dmdClassMin.dead.leaf);
      dmdClass (dmdMax.dead.stem, dmdMin.dead.stem, dmdClassMax.dead.stem, dmdClassMin.dead.stem);
}

void PlantHerbage::calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ)
{
      PlantPool dmdDeclined = dmdMax - dQ;

      float fraction[maxDmdPools];

// get GREEN Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.green.leaf, dmdMax.green.leaf, dmdMin.green.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.leaf = fraction[pool];

// get GREEN stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.green.stem, dmdMax.green.stem, dmdMin.green.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.stem = fraction[pool];

// get SENESCED Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.senesced.leaf, dmdMax.senesced.leaf, dmdMin.senesced.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.leaf = fraction[pool];

// get SENESCED stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.senesced.stem, dmdMax.senesced.stem, dmdMin.senesced.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.stem = fraction[pool];

// get DEAD Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.dead.leaf, dmdMax.dead.leaf, dmdMin.dead.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.leaf = fraction[pool];

// get DEAD Stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.dead.stem, dmdMax.dead.stem, dmdMin.dead.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.stem = fraction[pool];
}

void PlantHerbage::calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ)
{
      PlantPool dmdDeclined = dmdMax - dQ;

      float fraction[maxDmdPools];

// get GREEN Leaf dmd fractions
      dmdFraction[0].green.leaf = 1.0;
      c.dmdValue[0] = dmdDeclined.green.leaf;

// get GREEN stem dmd fractions
      dmdFraction[1].green.stem = 1.0;
      c.dmdValue[1] = dmdDeclined.green.stem;

// get SENESCED Leaf dmd fractions
      dmdFraction[2].senesced.leaf = 1.0;
      c.dmdValue[2] = dmdDeclined.senesced.leaf;

// get SENESCED stem dmd fractions
      dmdFraction[3].senesced.stem = 1.0;
      c.dmdValue[3] = dmdDeclined.senesced.stem;

// get DEAD Leaf dmd fractions
      dmdFraction[4].dead.leaf = 1.0;
      c.dmdValue[4] = dmdDeclined.dead.leaf;

// get DEAD Stem dmd fractions
      dmdFraction[5].dead.stem = 1.0;
      c.dmdValue[5] = dmdDeclined.dead.stem;
}


//===========================================================================
void PlantHerbage::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = c.dmdValue[0];
   const float MINDMD = c.dmdValue[c.numDmdPools-1];
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;
   const float roundingMargin = 1.0e-2;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdAvg > dmdMax + errorMargin)
   {  ostrstream msg;
      msg << endl << "Average digestibility > Maximum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Maximum      = " <<  dmdMax << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdAvg < dmdMin - errorMargin)
   {  ostrstream msg;
      msg << endl << "Average digestibility < Minimum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Minimum      = " <<  dmdMin << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMin > dmdAvg + errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility > Average digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMin < MINDMD - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMax > MAXDMD + errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMax < dmdAvg - errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility < Average digestibility:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str());
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
void PlantHerbage::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = c.dmdValue[0];
   const float MINDMD = c.dmdValue[c.numDmdPools-1];
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdMin < MINDMD - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   if (dmdMax > MAXDMD + errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   if (dmdMax < dmdMin - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Maximum      = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   for (int dmdClassNum = 0; dmdClassNum < c.numDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMax = dmdClassNum;
      if (abs(dmdMax - c.dmdValue[dmdClassNum]) < errorMargin)
      {
         exit;
      }
   }

   for (int dmdClassNum = 0; dmdClassNum < c.numDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMin = dmdClassNum;
      if (abs(dmdMin - c.dmdValue[dmdClassNum]) < errorMargin)
      {
         exit;
      }
   }
}

void PlantHerbage::doDigestibility(void)
{
      getVariables(dm, N, P, height, thermalTime);

      calcDmdDecline(thermalTime, dQ);

      if (dm.total() > 0.0)
      {

// distribute herbage


////      calcDmdDistribution(dmdFraction, dQ);
      calcDmdDistributionB(dmdFraction, dQ);

      for (int pool = 0; pool < c.numDmdPools; pool++)
      {
         if (c.debug == "on")
         {
            ostrstream msgFraction;
            msgFraction << endl << "Herbage dmd distribution, pool " << pool+1 << ":-" << endl;
            msgFraction << dmdFraction[pool] << ends;
            system->writeString (msgFraction.str());
         }


      } // end of Pools loop
   }
}
   // REST
float PlantHerbage::dmTotal ( void )
{
      return dm.total();
}

float PlantHerbage::dmTot ( int pool )
{
      PlantPool poolDm = dm * dmdFraction[pool];
      float dmTot = poolDm.total();
//         if (dmTot < 0.5) dmTot = 0.0;
      return dmTot;
}

float PlantHerbage::cpConc ( int pool )
{
        PlantPool poolN = N * dmdFraction[pool];
        float nTot = poolN.total();
        float nConc = divide (nTot, dmTot(pool), 0.0);
      return nConc * c.cpNRatio;
}

float PlantHerbage::pConc ( int pool )
{
      PlantPool NPRatio(c.NPRatioGreenLeafDefault,  c.NPRatioGreenStemDefault,  c.NPRatioSenescedLeafDefault,  c.NPRatioSenescedStemDefault,  c.NPRatioDeadStemDefault,  c.NPRatioDeadStemDefault);
//      poolP = P * dmdFraction[pool];
      PlantPool poolP = N/NPRatio * dmdFraction[pool];
      float pTot = poolP.total();
      float pConc = divide (pTot, dmTot(pool), 0.0);
      return pConc;
}

float PlantHerbage::ashAlk ( int pool )
{
      PlantPool partAshAlk(c.AshAlkGreenLeafDefault, c.AshAlkGreenStemDefault, c.AshAlkSenescedLeafDefault, c.AshAlkSenescedStemDefault, c.AshAlkDeadLeafDefault, c.AshAlkDeadStemDefault);
      partAshAlk = partAshAlk*cmol2mol;
      PlantPool poolAA    = partAshAlk * dm * dmdFraction[pool];  // ash alk to be got from lablab
      float aaTot = poolAA.total();
      float ashAlk = divide (aaTot, dmTot(pool), 0.0);
      return ashAlk;
}

float PlantHerbage::sConc ( int pool )
{
      PlantPool NSRatio(c.NSRatioGreenLeafDefault, c.NSRatioGreenStemDefault, c.NSRatioSenescedLeafDefault, c.NSRatioSenescedStemDefault, c.NSRatioDeadLeafDefault, c.NSRatioDeadStemDefault);
      PlantPool poolS = N/NSRatio * dmdFraction[pool];
      float sTot = poolS.total();
      float sConc = divide (sTot, dmTot(pool), 0.0);
      return sConc;
}

float PlantHerbage::hHeight ( void )
{
      return height;
}

float PlantHerbage::bD ( void )
{
//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
      float bd = divide(dm.total() *kg2g/ha2sm, hHeight()*mm2m, 0.0);
      return bd;
}

float PlantHerbage::heightRatio ( int pool )
{
//         if (pool==4) herbage.height_ratio = heightRatio.senesced.leaf + heightRatio.senesced.stem;   //   herbage.height_ratio = 0.0006;
//         herbage.height_ratio = heightRatio.green.leaf;   //   herbage.height_ratio = 0.0006;

      return  divide(100.0, 0.03*bD(), 0.0);
}

float PlantHerbage::dmdValue ( int pool )
{
      return c.dmdValue[pool];
}

float PlantHerbage::protDg ( int pool )
{
      return c.dmdValue[pool] + 0.1;
}

float PlantHerbage::proportionGreen ( void )
{
         float dm_green = dm.green.stem + dm.green.leaf;
         float dm_dead = dm.senesced.stem + dm.senesced.leaf + dm.dead.stem + dm.dead.leaf;
         float dm_total = dm_green + dm_dead;
         return divide (dm_green, dm_total, 0.0);
}

float PlantHerbage::proportionLegume ( void )
{
   return 1.0;       //FIXME - calc legume content
}

float PlantHerbage::selectionFactor ( void )
{
   return 0.0; // ??
}

int PlantHerbage::numDmdPools ( void )
{
   return c.numDmdPools; // ??
}

void PlantHerbage::readParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------

    system->writeString (" - reading parameters");

    c.herbageModuleName = system->readParameter (section_name, "herbage_module_name");
    c.debug = system->readParameter (section_name, "debug");

      ostrstream msg;
      msg << "Herbage module name = " << c.herbageModuleName << endl
          << "Debug = " << c.debug << ends;
      system->writeString (msg.str());
}

void PlantHerbage::readHerbageModuleParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readHerbageModuleParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------
      ostrstream msg;
      msg << " - reading  herbage parameters for module '" << c.herbageModuleName.c_str() << "'" << endl << ends;
      system->writeString (msg.str());

    system->readParameter (section_name, "dmdValue", c.dmdValue, c.numDmdPools, 0.0, 1.0);

    system->readParameter (section_name, "p_conc_green_stem_default", c.pConcGreenStemDefault, 0.0, 1.0);
    system->readParameter (section_name, "p_conc_green_leaf_default", c.pConcGreenLeafDefault, 0.0, 1.0);
    system->readParameter (section_name, "p_conc_senesced_stem_default", c.pConcSenescedStemDefault, 0.0, 1.0);
    system->readParameter (section_name, "p_conc_senesced_leaf_default", c.pConcSenescedLeafDefault, 0.0, 1.0);
    system->readParameter (section_name, "p_conc_dead_stem_default", c.pConcDeadStemDefault, 0.0, 1.0);
    system->readParameter (section_name, "p_conc_dead_leaf_default", c.pConcDeadLeafDefault, 0.0, 1.0);

    system->readParameter (section_name, "ash_alk_green_stem_default", c.AshAlkGreenStemDefault, 0.0, 500.0);
    system->readParameter (section_name, "ash_alk_green_leaf_default", c.AshAlkGreenLeafDefault, 0.0, 500.0);
    system->readParameter (section_name, "ash_alk_senesced_stem_default", c.AshAlkSenescedStemDefault, 0.0, 500.0);
    system->readParameter (section_name, "ash_alk_senesced_leaf_default", c.AshAlkSenescedLeafDefault, 0.0, 500.0);
    system->readParameter (section_name, "ash_alk_dead_stem_default", c.AshAlkDeadStemDefault, 0.0, 500.0);
    system->readParameter (section_name, "ash_alk_dead_leaf_default", c.AshAlkDeadLeafDefault, 0.0, 500.0);

    system->readParameter (section_name, "ns_ratio_green_stem_default", c.NSRatioGreenStemDefault, 0.0, 30.0);
    system->readParameter (section_name, "ns_ratio_green_leaf_default", c.NSRatioGreenLeafDefault, 0.0, 30.0);
    system->readParameter (section_name, "ns_ratio_senesced_stem_default", c.NSRatioSenescedStemDefault, 0.0, 30.0);
    system->readParameter (section_name, "ns_ratio_senesced_leaf_default", c.NSRatioSenescedLeafDefault, 0.0, 30.0);
    system->readParameter (section_name, "ns_ratio_dead_stem_default", c.NSRatioDeadStemDefault, 0.0, 30.0);
    system->readParameter (section_name, "ns_ratio_dead_leaf_default", c.NSRatioDeadLeafDefault, 0.0, 30.0);

    system->readParameter (section_name, "np_ratio_green_stem_default", c.NPRatioGreenStemDefault, 0.0, 10.0);
    system->readParameter (section_name, "np_ratio_green_leaf_default", c.NPRatioGreenLeafDefault, 0.0, 10.0);
    system->readParameter (section_name, "np_ratio_senesced_stem_default", c.NPRatioSenescedStemDefault, 0.0, 10.0);
    system->readParameter (section_name, "np_ratio_senesced_leaf_default", c.NPRatioSenescedLeafDefault, 0.0, 10.0);
    system->readParameter (section_name, "np_ratio_dead_stem_default", c.NPRatioDeadStemDefault, 0.0, 10.0);
    system->readParameter (section_name, "np_ratio_dead_leaf_default", c.NPRatioDeadLeafDefault, 0.0, 10.0);

    int numClasses = 3;
    system->readParameter (section_name, "dmd_green_leaf", c.dmdGreenLeaf, numClasses, 0.0, 1.0);
    system->readParameter (section_name, "dmd_green_stem", c.dmdGreenStem, numClasses, 0.0, 1.0);
    system->readParameter (section_name, "dmd_senesced_leaf", c.dmdSenescedLeaf, numClasses, 0.0, 1.0);
    system->readParameter (section_name, "dmd_senesced_stem", c.dmdSenescedStem, numClasses, 0.0, 1.0);
    system->readParameter (section_name, "dmd_dead_leaf", c.dmdDeadLeaf, numClasses, 0.0, 1.0);
    system->readParameter (section_name, "dmd_dead_stem", c.dmdDeadStem, numClasses, 0.0, 1.0);

    system->readParameter (section_name, "KQ5Leaf", c.KQ5Leaf, 0.0, 1.0);
    system->readParameter (section_name, "KQ5Stem", c.KQ5Stem, 0.0, 1.0);
    system->readParameter (section_name, "KQ4", c.KQ4, 0.0, 1000.0);

    system->readParameter (section_name, "cp_n_ratio", c.cpNRatio, 0.0, 10.0);

   const int MAX = 0;
   const int AVG = 1;
   const int MIN = 2;
         //plant pools  GL    GS   SL    SS   DL   DS
//      PlantPool dmdMax(c.dmdGreenLeaf, c.dmdMaxGreenStem, c.dmdMaxSenescedLeaf, c.dmdMaxSenescedStem, c.dmdMaxDeadLeaf, c.dmdMaxDeadStem);
   dmdMax.setValue(c.dmdGreenLeaf[MAX], c.dmdGreenStem[MAX], c.dmdSenescedLeaf[MAX], c.dmdSenescedStem[MAX], c.dmdDeadLeaf[MAX], c.dmdDeadStem[MAX]);
   dmdAvg.setValue(c.dmdGreenLeaf[AVG], c.dmdGreenStem[AVG], c.dmdSenescedLeaf[AVG], c.dmdSenescedStem[AVG], c.dmdDeadLeaf[AVG], c.dmdDeadStem[AVG]);
   dmdMin.setValue(c.dmdGreenLeaf[MIN], c.dmdGreenStem[MIN], c.dmdSenescedLeaf[MIN], c.dmdSenescedStem[MIN], c.dmdDeadLeaf[MIN], c.dmdDeadStem[MIN]);

   calcDmdClass(dmdClassMax, dmdClassMin);
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



