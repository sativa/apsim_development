#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "CompositePart.h"
#include "CompositePool.h"

using namespace std;

//  initialise data members.
CompositePart::CompositePart(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : plantPart(scienceAPI, p, name,
               *new CompositePool(*p, scienceAPI, "Green", name),
               *new CompositePool(*p, scienceAPI, "Senesced", name))
   {
   }

// destructor
CompositePart::~CompositePart()
{
}

ostream &operator<<(ostream &output, const CompositePart /*&pool*/)
{
   //   output << "CompositePart:" << endl;
   output << endl;
   return output;
}


// Assigment operator
//  assign data members of object
const CompositePart &CompositePart::operator=(const CompositePart &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for CompositePart");
}

string CompositePart::addPartToVar(const string& variableName)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified variable name.
   // --------------------------------------------------------------------------
   string LcaseName = myName;
   To_lower(LcaseName);
   if (myName != "")
      return variableName + "_" + LcaseName;
   else
      return variableName;
   }

string CompositePart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (myName != "")
      return description + myName;
   else
      return description + " plant";
   }
void CompositePart::onInit1(protocol::Component *system)
   //===========================================================================
   {
   plantPart::onInit1(system);


   vector <plantPart *>::iterator part;
   if (myName == "")  // If you don't have this then we get a TopsSWDemand - not needed.
      {
      scienceAPI.exposeFunction("sw_demand", "mm",  "Demand for soil water", FloatFunction(&CompositePart::SWDemand));
      scienceAPI.exposeFunction("sw_demand_te", "mm",  "TE Demand for soil water", FloatFunction(&CompositePart::SWDemandTE));
      scienceAPI.exposeFunction("dlt_dm", "g/m^2",  "Actual above_ground dry matter production", FloatFunction(&CompositePart::dltDm));
      scienceAPI.exposeFunction("dlt_dm_pot_rue", "g/m^2",  "Potential above_ground dry matter production via photosynthesis", FloatFunction(&CompositePart::dltDmPotRue));
      scienceAPI.exposeFunction("cover_green", "",  "Green cover", FloatFunction(&CompositePart::coverGreen));
      }
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onInit1(system);


   }

void CompositePart::add(plantPart* part)
   //===========================================================================
   {
   myParts.push_back(part);

   // Add this part's pools to green and senesced composite pools
   CompositePool& GreenPool = (CompositePool&) Green;
   CompositePool& SenescedPool = (CompositePool&) Senesced;
   CompositePool& GrainPool = (CompositePool&) Grain;
   CompositePool& GrainTotalPool = (CompositePool&) GrainTotal;
   GreenPool.AddPool(part->Green);
   SenescedPool.AddPool(part->Senesced);

   Vegetative.AddPool(part->Vegetative);
   VegetativeTotal.AddPool(part->VegetativeTotal);

   GrainPool.AddPool(part->Grain);
   GrainTotalPool.AddPool(part->GrainTotal);
   }

float CompositePart::nConcCrit()
//=======================================================================================
   {
   float sum = 0.0;
   float count = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->nConcCrit();
      count++;
   return sum/count;
   }

float CompositePart::nConcMin()
//=======================================================================================
   {
   float sum = 0.0;
   float count = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->nConcMin();
      count++;
   return sum/count;
   }

float CompositePart::dltNGreen(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNGreen();
   return sum;
}


float CompositePart::dltPGreen(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPGreen();
   return sum;
}

float CompositePart::dltDmSenesced(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltDmSenesced();
   return sum;
}


float CompositePart::dltNSenesced(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenesced();
   return sum;
}


float CompositePart::dltPSenesced(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPSenesced();
   return sum;
}

float CompositePart::dltNDetached(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNDetached();
   return sum;
}


float CompositePart::dltPDetached(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPDetached();
   return sum;
}


float CompositePart::n_conc_crit(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_crit();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}


float CompositePart::n_conc_min(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_min();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}


float CompositePart::dltNRetrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNRetrans();
   return sum;
}


float CompositePart::dltNSenescedRetrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedRetrans();
   return sum;
}


float CompositePart::dltNSenescedTrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedTrans();
   return sum;
}


float CompositePart::dltDmGreenRetrans(void)
   //===========================================================================
{
   float sum = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltDmGreenRetrans();
   return sum;
}



float CompositePart::dmGreenDemand(void)
   //===========================================================================
{
   float dmGreenDemand = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dmGreenDemand += (*part)->dmGreenDemand();
   return dmGreenDemand;
}

float CompositePart::grainWaterContent(void)
   //===========================================================================
{
   float total = 0.0;
   float count = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      total += (*part)->grainWaterContent();
      count +=1.0;
   }
//   return divide (total, count, 0.0);
   return total;                       //FIXME
}

float CompositePart::grainWt(void)
   //===========================================================================
{
   float grainWtTotal = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      grainWtTotal += (*part)->grainWt();
   return grainWtTotal;
}

float CompositePart::dmGrainWetTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGrainWetTotal();
   return dmTotal;
}

float CompositePart::dltDmDetached(void)
   //===========================================================================
{
   float dlt_dm_detached = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dlt_dm_detached += (*part)->dltDmDetached();
   return dlt_dm_detached;
}

float CompositePart::nMax(void)
   //===========================================================================
{
   float result = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nMax();                                      //FIXME Is this a conc?
   return result;
}

float CompositePart::nDemandGrain2(void)
   //===========================================================================
{
   float n_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_demand += (*part)->nDemandGrain2();
   return n_demand;
}

float CompositePart::soilNDemand(void)
   //============================================================================
{
   float SoilNDemand = 0.0;
   for (vector <plantPart * >::iterator part = myParts.begin(); part != myParts.end(); part++)
      SoilNDemand += (*part)->soilNDemand();
   return SoilNDemand;
}

float CompositePart::nDemand(void)
   //============================================================================
{
   float n_demand = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      n_demand += (*part)->nDemand();
   return n_demand;
}

float CompositePart::nCapacity(void)
   //============================================================================
{
   float NCapacity = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      NCapacity += (*part)->nCapacity();
   return NCapacity;
}

void CompositePart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   plantPart::doNPartition(nSupply, n_demand_sum, n_capacity_sum);   //FIXME need to remove this sometime

   n_demand_sum = nDemand();
   n_capacity_sum = nCapacity();

   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNPartition(Growth.N(), n_demand_sum, n_capacity_sum);

   float dlt_n_green_sum = dltNGreen();
   if (!reals_are_equal(dlt_n_green_sum - Growth.N(), 0.0))
      {
      string msg = myName + " dlt_n_green mass balance is off: dlt_n_green_sum ="
                  + ftoa(dlt_n_green_sum, ".6")
                  + " vs nSupply ="
                  + ftoa(Growth.N(), ".6");
      plant->warningError(msg.c_str());
      }
}


float CompositePart::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();                                      //FIXME Is this a conc?
   return pMaxPot;
}

float CompositePart::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void CompositePart::get_p_demand(vector<float> &p_demand)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_demand(p_demand);
}

void CompositePart::get_name(vector<string> &name)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_name(name);
}

void CompositePart::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_retrans(dlt_p_retrans);
}

void CompositePart::get_dm_plant_min(vector<float> &dm_min)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_plant_min(dm_min);
}

void CompositePart::get_dm_green(vector<float> &dm_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_green(dm_green);
}

void CompositePart::get_dm_senesced(vector<float> &dm_senesced)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_senesced(dm_senesced);
}

void CompositePart::get_dlt_dm_green(vector<float> &dlt_dm_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green(dlt_dm_green);
}

void CompositePart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);
}

void CompositePart::get_dlt_dm_detached(vector<float> &dlt_dm_detached)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_detached(dlt_dm_detached);
}

void CompositePart::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_senesced(dlt_dm_senesced);
}

void CompositePart::get_n_demanded(vector<float> &n_demand)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_demanded(n_demand);
}

void CompositePart::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Numer
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doGrainNumber();
}

void CompositePart::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->readCultivarParameters(system, cultivar);
}

void CompositePart::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{
   // report
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->writeCultivarInfo(system);
}

void CompositePart::onDayOf(const string &stage)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onDayOf(stage);
}

void CompositePart::morphology(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->morphology();
}

void CompositePart::zeroAllGlobals(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroAllGlobals();
}

void CompositePart::zeroDeltas(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDeltas();
}

void CompositePart::zeroDltDmGreen(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDltDmGreen();
}

void CompositePart::zeroDltNSenescedTrans(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDltNSenescedTrans();
}

float CompositePart::dltDmGreenRemoved(void)
//=======================================================================================
   {
   float DMGreenRemoved = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      DMGreenRemoved +=(*part)->dltDmGreenRemoved();
   return DMGreenRemoved;
   }

float CompositePart::dltDmSenescedRemoved(void)
//=======================================================================================
   {
   float DMSenescedRemoved = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      DMSenescedRemoved +=(*part)->dltDmSenescedRemoved();
   return DMSenescedRemoved;
   }

float CompositePart::dltDmRemoved(void)
//=======================================================================================
   {
   float DMRemoved = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      DMRemoved +=(*part)->dltDmRemoved();
   return DMRemoved;
   }

float CompositePart::dltNRemoved(void)
//=======================================================================================
   {
   float NRemoved = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      NRemoved +=(*part)->dltNRemoved();
   return NRemoved;
   }

void CompositePart::doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doRemoveBiomass(dmRemoved, c_remove_biomass_report);
}

void CompositePart::removeBiomass(void)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->removeBiomass();
}

void CompositePart::removeBiomass2(float chop_fr)
//=======================================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->removeBiomass2(chop_fr);
}

void CompositePart::onHarvest(float cutting_height, float remove_fr,
                           vector<string> &dm_type,
                           vector<float> &dlt_crop_dm,
                           vector<float> &dlt_dm_n,
                           vector<float> &dlt_dm_p,
                           vector<float> &fraction_to_residue)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onHarvest(cutting_height, remove_fr,
                         dm_type,
                         dlt_crop_dm,
                         dlt_dm_n,
                         dlt_dm_p,
                         fraction_to_residue);
}

void CompositePart::onEmergence()
//=======================================================================================
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onEmergence();
   }

void CompositePart::onPlantEvent(const string &event)
//=======================================================================================
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onPlantEvent(event);
   }

void CompositePart::onKillStem(void)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onKillStem();
}

void CompositePart::onEndCrop(vector<string> &dm_type,
                           vector<float> &dlt_crop_dm,
                           vector<float> &dlt_dm_n,
                           vector<float> &dlt_dm_p,
                           vector<float> &fraction_to_residue)
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->onEndCrop(dm_type,
                         dlt_crop_dm,
                         dlt_dm_n,
                         dlt_dm_p,
                         fraction_to_residue);
}


void CompositePart::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->readConstants(system, section);
}

void CompositePart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->readSpeciesParameters(system, sections);
}

float CompositePart::dltDmGreen(void)
   //===========================================================================
{
   float dltDmGreen = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dltDmGreen +=(*part)->dltDmGreen();
   return dltDmGreen;
}

float CompositePart::dltDmUptake(void)
   //===========================================================================
{
   float dltDmUptake= 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      dltDmUptake+=(*part)->dltDmUptake();
   return dltDmUptake;
}


float CompositePart::dltDmRetranslocateSupply(float /* demand_differential*/)
   //===========================================================================
{
   float dlt_dm_green_retrans = 0.0;
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      dlt_dm_green_retrans += (*part)->dltDmGreenRetrans();
      }
   return dlt_dm_green_retrans;
}

void CompositePart::doNSenescedRetrans(float navail, float n_demand_tot)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNSenescedRetrans(navail, n_demand_tot);
}

void CompositePart::collectDetachedForResidue(vector<string> &part_name
                                           , vector<float> &dm_residue
                                           , vector<float> &dm_n
                                           , vector<float> &dm_p
                                           , vector<float> &fraction_to_residue)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->collectDetachedForResidue(part_name
                                         , dm_residue
                                         , dm_n
                                         , dm_p
                                         , fraction_to_residue);
}


void CompositePart::update(void)
   //===========================================================================
{
   // Update
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->update();
}

void CompositePart::doNConccentrationLimits(float modifier)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNConccentrationLimits(modifier);
}

// Query
float CompositePart::coverTotal(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverTotal());
   return cover;
}

float CompositePart::coverGreen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverGreen());
   return cover;
}

float CompositePart::coverSen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      cover = add_covers (cover, (*part)->coverSen());
   return cover;
}

//float CompositePart::total()
//{
//
//  return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void CompositePart::display(ostream &os)
{
   //   os << "CompositePart:" << endl;
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


void CompositePart::doCover(PlantSpatial &spatial)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doCover(spatial);
}
void CompositePart::prepare (void)
   //===========================================================================
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->prepare ();
   }

void CompositePart::process(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->process();
}

void CompositePart::doProcessBioDemand(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doProcessBioDemand();
}

float CompositePart::grainNo(void)
   //===========================================================================
{
   float grainNo = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      grainNo += (*part)->grainNo();
   return grainNo;
}

float CompositePart::nDemandGrain(void)
   //===========================================================================
{
   float nDemandGrain = 0.0;
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      nDemandGrain += (*part)->nDemandGrain();
   return nDemandGrain;
}

float CompositePart::transpirationEfficiency(void)
   //===========================================================================
{
   float transpEff = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      transpEff += (*part)->transpirationEfficiency();   //FIXME - the sum is not the correct result
   return transpEff;
}

float CompositePart::dltDmPotRue(void)
   //===========================================================================
{
   float dltDmPotRue = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmPotRue += (*part)->dltDmPotRue();
   return dltDmPotRue;
}

float CompositePart::dltDm(void)
   //===========================================================================
{
   float dltDm = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDm += (*part)->dltDm();
   return dltDm;
}

float CompositePart::dltDmGrainDemand(void)
   //===========================================================================
{
   float dltDmDemand = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmDemand += (*part)->dltDmGrainDemand();
   return dltDmDemand;
}

void CompositePart::calcDlt_pod_area (void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->calcDlt_pod_area();
}

float CompositePart::dltDmRetranslocate(void)
   //===========================================================================
{
   float dlt_dm_green_retrans = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dlt_dm_green_retrans += (*part)->dltDmRetranslocate();
   return dlt_dm_green_retrans;
}

float CompositePart::dltDmGreenRetransUptake(void)
   //===========================================================================
{
   float dltDmUptake = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmUptake += (*part)->dltDmGreenRetransUptake();
   return dltDmUptake;
}

float CompositePart::interceptRadiationGreen (float radiation)
   //===========================================================================
{
   float interceptRadiation = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      interceptRadiation += (*part)->interceptRadiationGreen (radiation);         //FIXME - divey up radiation
   return interceptRadiation;
}

float CompositePart::interceptRadiationTotal (float radiation)
   //===========================================================================
{
   float interceptRadiation = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      interceptRadiation += (*part)->interceptRadiationTotal (radiation);         //FIXME - divey up radiation
   return interceptRadiation;
}

void CompositePart::doDmPotRUE (void )
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmPotRUE ();
}

void CompositePart::doSWDemand(float SWDemandMaxFactor)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSWDemand(SWDemandMaxFactor);
}

float CompositePart::SWDemand(void)
   //===========================================================================
{
   float SWDemand = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      SWDemand += (*part)->SWDemand();
   return SWDemand;
}

float CompositePart::SWDemandTE(void)
   //===========================================================================
{
   float SWDemandTE = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      SWDemandTE += (*part)->SWDemandTE();
   return SWDemandTE;
}

void CompositePart::doBioActual (void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doBioActual();
}


void CompositePart::doNDemandGrain(float g_nfact_grain_conc      //   (INPUT)
                                 , float g_swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemandGrain(g_nfact_grain_conc
                                        , g_swdef_expansion);
   }

void CompositePart::doDmDemand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmDemand(dlt_dm_veg_supply);                                //FIXME - divey up dlt_dm_veg_supply? Only for HI approach
}

float CompositePart::dmDemandDifferential(void)
   //===========================================================================
{
   float dm_demand_differential = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dm_demand_differential += (*part)->dmDemandDifferential();
   return dm_demand_differential;
}

float CompositePart::giveDmGreen(float dmSupplied)
//=======================================================================================
// Arbritator has given us some DM to distribute amongst individual parts
   {
   float dmDemand = dmGreenDemand();
   float uptake = 0.0;
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      float partFrac =  divide((*part)->dmGreenDemand(), dmDemand, 0.0);
      uptake += (*part)->giveDmGreen (dmSupplied * partFrac);
      }

   // do mass balance check
   if (!reals_are_equal(uptake, dmSupplied, 1.0E-4))
       {
       string msg = myName + " giveDmGreen mass balance is off:\n"
                   + "uptake = " + ftoa(uptake, ".6")
                   + " vs "
                   + "supplied = " + ftoa(dmSupplied, ".6") +"\n";
       for (vector<plantPart *>::iterator part = myParts.begin();
            part != myParts.end();
            part++)
         msg += (*part)->name() + "=" + ftoa((*part)->dltDmGreen(), ".6") +"\n";

       plant->warningError(msg.c_str());
       }

   return uptake;
   }


void CompositePart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   float dm_demand_differential = dmDemandDifferential ();
   float dlt_dm_green_retrans = DMAvail * divide (dm_demand_differential, DMDemandDifferentialTotal, 0.0);

   // get available carbohydrate from local supply pools
   float demand_differential = dm_demand_differential - dlt_dm_green_retrans;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
      {
      float dlt_dm_retrans_part = (*part)->dltDmRetranslocateSupply(demand_differential);
      demand_differential = demand_differential - dlt_dm_retrans_part;
      }

   float dlt_dm_green_retrans_tot = dlt_dm_green_retrans + (-dltDmRetranslocate());

   // now distribute the assimilate to plant parts
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
       {
       (*part)->doDmRetranslocate (dlt_dm_green_retrans_tot, dm_demand_differential);
       }
   // do mass balance check

   if (!reals_are_equal(dltDmGreenRetransUptake (), dlt_dm_green_retrans, 1.0E-4))  // XX this is probably too much slop - try doubles XX
      {
      string msg = myName + " dlt_dm_green_retrans_tot mass balance is off: "
                   + ftoa(dltDmGreenRetransUptake (), ".6")
                   + " vs "
                   + ftoa(dlt_dm_green_retrans, ".6");
      plant->warningError(msg.c_str());
      }
   }

void CompositePart::doSenescence (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSenescence(sen_fr);
}


void CompositePart::doDmMin (void)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmMin();
}

float CompositePart::availableRetranslocateN(void)
   //============================================================================
{
   float nAvail = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)     //FIXME later - parts need to know if they hold a supply pool
      nAvail += (*part)->availableRetranslocateN();

   return nAvail;
}

float CompositePart::nDemandDifferential(void)
   //===========================================================================
{
   float n_demand_differential = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      n_demand_differential += (*part)->nDemandDifferential();
   return n_demand_differential;
}

void CompositePart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
{
    plantPart::doNFixRetranslocate(NFix, NDemandDifferentialTotal);
   float n_demand_differential = nDemandDifferential();

        // now distribute the n fixed to plant parts

   NFix = NFix * divide (n_demand_differential, NDemandDifferentialTotal, 0.0);
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)      //FIXME later
       (*part)->doNFixRetranslocate (NFix, n_demand_differential);
}

void CompositePart::doNRetranslocate( float N_supply, float g_grain_n_demand)
   //============================================================================
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.
{

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   for (vector <plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->doNRetranslocate(N_supply, g_grain_n_demand);           //FIXME - divy up?
}

void CompositePart::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);                     //FIXME - divy up?
}

void CompositePart::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                               , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);                  //FIXME - divy up?
}

void CompositePart::doNDemand2(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDemand2(dlt_dm, dlt_dm_pot_rue);                     //FIXME - divy up?
}


void CompositePart::doSoilNDemand(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doSoilNDemand();
}


void CompositePart::doNSenescence(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNSenescence();
}

void CompositePart::Detachment(void)
   //============================================================================
{
   float DetachingDM = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      DetachingDM += (*part)->dltDmDetached();
      (*part)->Detachment();
      }
   Detaching = Biomass(DetachingDM, 0, 0);
}


void CompositePart::doPDemand(void)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPDemand();
      }
}

void CompositePart::doPSenescence(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPSenescence();
}

float CompositePart::pDemand(void)
   //============================================================================
{
   float p_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_demand += (*part)->pDemand();
   return p_demand;
}

float CompositePart::pRetransSupply(void)
   //============================================================================
{
   float p_retrans_supply = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_retrans_supply += (*part)->pRetransSupply();
   return p_retrans_supply;
}

float CompositePart::pRetransDemand(void)
   //============================================================================
{
   float p_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_retrans_demand += (*part)->pRetransDemand();
   return p_retrans_demand;
}

float CompositePart::dmRetransSupply(void)
   //============================================================================
{
   float dm_retrans_supply = 0.0;
   vector <plantPart *>::const_iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_retrans_supply += (*part)->dmRetransSupply();
   return dm_retrans_supply;
}

float CompositePart::dmRetransDemand(void)
//============================================================================
{
   float dm_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_retrans_demand += (*part)->dmRetransDemand();
   return dm_retrans_demand;
}

float CompositePart::nRetransSupply(void)
   //============================================================================
{
   float n_retrans_supply = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_retrans_supply += (*part)->nRetransSupply();
   return n_retrans_supply;
}

float CompositePart::dltNRetransOut(void)
   //============================================================================
{
   float dlt_n_retrans = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt_n_retrans += (*part)->dltNRetransOut();

   return dlt_n_retrans;
}

float CompositePart::nRetransDemand(void)
   //============================================================================
{
   float n_retrans_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_retrans_demand += (*part)->nRetransDemand();
   return n_retrans_demand;
}

void CompositePart::doPPartition(float p_uptake, float total_p_demand)
   //============================================================================
{
   float myP = p_uptake * divide(pDemand(), total_p_demand,  0.0); // Amount of P for this composite part
   for (vector <plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPPartition(myP, pDemand());
      }
}

void CompositePart::doPRetranslocate(float total_p_supply, float total_p_demand)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPRetranslocate(total_p_supply, total_p_demand);    //FIXME - divy up?
}


float CompositePart::dmGreenStressDeterminant(void)
   //============================================================================
{
   float dm_green = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dm_green +=  (*part)->dmGreenStressDeterminant();
   return dm_green;
}

float CompositePart::pGreenStressDeterminant(void)
   //============================================================================
{
   float p_green = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_green +=  (*part)->pGreenStressDeterminant();
   return p_green;
}

float CompositePart::pMaxPotStressDeterminant(void)
   //============================================================================
{
   float p_max_pot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_max_pot +=  (*part)->pMaxPotStressDeterminant();
   return p_max_pot;
}

float CompositePart::pMinPotStressDeterminant(void)
//============================================================================
{
   float p_min_pot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_min_pot +=  (*part)->pMinPotStressDeterminant();
   return p_min_pot;
}


bool CompositePart::isYieldPart(void)
//============================================================================
// True if at least one of our parts is a dm sink
   {
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isYieldPart()) return true;
   return false;
   }

bool CompositePart::isRetransPart(void)
// True if at least one of our parts supplies retranslocate
   {
   for (vector <plantPart * >::const_iterator part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isRetransPart()) return true;
   return false;
   }