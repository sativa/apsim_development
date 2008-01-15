#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "FruitCohortFN.h"

using namespace std;

// ##############################################################
// DPH: Most of this file can be removed by relying on
// composite part to do the looping over parts for dmGreenVeg,
// dmSenescedVeg etc. We would need to put in lines in the grain
// parts to return 0.0 for the dmGreenVeg, dmSenescedVeg etc.
// ##############################################################

//  initialise data members.
FruitCohortFN::FruitCohortFN(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(scienceAPI, p, name)
{
    string phenologyModel;
    scienceAPI.readOptional("phenology_model", phenologyModel);
    fruitPhenology = constructPhenology(scienceAPI, plant, phenologyModel);
}

// destructor
FruitCohortFN::~FruitCohortFN()
   // ====================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      delete (*part);
      delete fruitPhenology;
}

void FruitCohortFN::onInit1(protocol::Component *system)
   //===========================================================================
   {
   zeroAllGlobals(); zeroDeltas();

    fruitPhenology->onInit1(system);

   string grainType;
   scienceAPI.read("grain_part_type", grainType);
   if (grainType == "harvest_index")
      grainPart = new fruitGrainPartHI(scienceAPI, plant, "grain");
   else if (grainType == "grain_no")
      grainPart = new fruitGrainPartGN(scienceAPI, plant, "grain");
   else
     throw std::runtime_error("Unknown grain_part_type '" + grainType + "'");

   podPart = new fruitPodPart(scienceAPI, plant, grainPart, "pod");

   add(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);

   add(grainPart);
   myGrainParts.push_back(grainPart);

   // call into base class.
   CompositePart::onInit1(system);

   // register some other things.
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&FruitCohortFN::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "head_n", protocol::DTsingle, false,&FruitCohortFN::get_head_n, "g/m^2", "N in heads");
   setupGetFunction(system, "head_p", protocol::DTsingle, false, &FruitCohortFN::get_head_p, "g/m^2","P in head");
   }

void FruitCohortFN::checkBounds (void)
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->checkBounds();
}

ostream &operator<<(ostream &output, const FruitCohortFN /*&pool*/)
{
   //   output << "FruitCohortFN:" << endl;
   //   output << "   Green cover:    " << pool.coverPod.green << endl;
   //   output << "   Senesced cover: " << pool.coverPod.sen << endl;
   //   output << "   Dead cover:     " << pool.coverPod.dead << endl;
   //   output << endl;
   //   output << "   Green shell:    " << pool.green.shell << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced shell: " << pool.senesced.shell << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead shell:     " << pool.dead.shell << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}


// Assigment operator
//  assign data members of object
const FruitCohortFN &FruitCohortFN::operator=(const FruitCohortFN &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for FruitCohortFN");
}

void FruitCohortFN::prepare (void)
   {
      fruitPhenology->prepare (*(plant->getEnvironment()));
      CompositePart::prepare();
   }

void FruitCohortFN::process (void)
   {
      fruitPhenology->process (*(plant->getEnvironment()), plant->getPhotoStress(), plant->getFaswSeed(), plant->getPeswSeed());
      CompositePart::process();
   }

float FruitCohortFN::nMax(void)
   //===========================================================================
{
   float nMax = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMax += (*part)->nMax();
   return nMax;
}

float FruitCohortFN::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float FruitCohortFN::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void FruitCohortFN::get_head_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float headWt = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      headWt += (*part)->Green.DM();

   system->sendVariable(qd, headWt);
}

void FruitCohortFN::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Grain.N() + Vegetative.N());  // Why not VegetativeTotal ????
}

void FruitCohortFN::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   systemInterface->sendVariable(qd, Grain.P() + Vegetative.P());  // Why not VegetativeTotal?????
}


void FruitCohortFN::display(ostream &os)
{
   //   os << "FruitCohortFN:" << endl;
   //   os << "Green cover:    " << coverPod.green << endl;
   //   os << "Senesced cover: " << coverPod.sen << endl;
   //   os << "Dead cover:     " << coverPod.dead << endl;
   //   os << "Green shell: " << green.shell << endl;
   //   os << "Green meal: " << green.meal << endl;
   //   os << "Senesced shell: " << senesced.shell << endl;
   //   os << "Senesced meal: " << senesced.meal << endl;
   //   os << "Dead shell: " << dead.shell << endl;
   //   os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}

void FruitCohortFN::doDmDemand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   doProcessBioDemand();
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts
   podPart->doDmDemand(dlt_dm_veg_supply);
}


void FruitCohortFN::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);
}

void FruitCohortFN::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                               , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);
}

void FruitCohortFN::onPlantEvent(const string &event)
//=======================================================================================
   {
      fruitPhenology->onPlantEvent(event);
      CompositePart::onPlantEvent(event);
   }

void FruitCohortFN::zeroAllGlobals(void)
   //===========================================================================
{
      fruitPhenology->zeroAllGlobals();
      CompositePart::zeroAllGlobals();
}

void FruitCohortFN::zeroDeltas(void)
   //===========================================================================
{
      fruitPhenology->zeroDeltas();
      CompositePart::zeroDeltas();
}

void FruitCohortFN::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
      fruitPhenology->readCultivarParameters(system, cultivar);

      scienceAPI.read("potential_fruit_filling_rate", p.potential_fruit_filling_rate, 0.0f, 1.0f);  //'(g/fruit/degday)'
      frac_pod.read(scienceAPI, "x_fruit_stage_no_partition", "-", 0.0, 20.0
                              , "y_fruit_frac_pod", "-", 0.0, 2.0);

      sdr_min.read(scienceAPI, "x_stage_sdr_min", "-", 1.0, 100.0
                             , "y_sdr_min", "-", 0.0, 2.0);

      scienceAPI.read("dm_fruit_max", p.dm_fruit_max, 0.0f, 100.0f);
      scienceAPI.read("potential_fruit_filling_rate", p.potential_fruit_filling_rate, 0.0f, 1.0f); // '(g/fruit/degday)'

      scienceAPI.read("cutout_fract", p.cutout_fract, 0.0f, 1.0f);  // '(0-1)'

      fruit_sites_per_node.read(scienceAPI, "x_node_no_fruit_sites", "sites/node", 0.0, 100.0
                             , "y_fruit_sites_per_node", "sites/node", 0.0, 100.0);

      scienceAPI.read("dm_fruit_set_min", p.dm_fruit_set_min, 0.0f, 10.0f);
   //   scienceAPI.read("dm_fruit_set_crit", p.dm_fruit_set_crit, 0.0f, 10.0f);

      scienceAPI.read("days_assimilate_ave", c.days_assimilate_ave, 0, 100);
// Temporarily remove
//      scienceAPI.read("dm_abort_fract", c.dm_abort_fract, 0.0f, 1.0f);
//      scienceAPI.read("fract_dm_fruit_abort_crit", c.fract_dm_fruit_abort_crit, 0.0f, 1.0f);
//      scienceAPI.read("fruit_phen_end", c.fruit_phen_end, 0.0f, 1.0f);
//      scienceAPI.read("tt_flower_to_start_pod", c.tt_flower_to_start_pod, 0.0f, 1000.0f);
//      rel_fruit_site.read(scienceAPI, "x_temp_fruit_site", "oC", 0.0, 50.0
//                                    , "y_rel_fruit_site", "-", 0.0, 1.0);

   CompositePart::readCultivarParameters(system, cultivar);
}

void FruitCohortFN::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
      fruitPhenology->readConstants(system, section);
      CompositePart::readConstants(system, section);
}

void FruitCohortFN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
      fruitPhenology->readSpeciesParameters(system, sections);
      CompositePart::readSpeciesParameters(system, sections);
}

