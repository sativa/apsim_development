
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "CompositePart.h"


using namespace std;

// default constructor
// 	initialise data members.
CompositePart::CompositePart(plantInterface *p, const string &name) : plantPart(p, name)
{
}

// destructor
CompositePart::~CompositePart()
{
}

ostream &operator<<(ostream &output, const CompositePart /*&pool*/)
{
   //	output << "CompositePart:" << endl;
   output << endl;
   return output;
}

// copy constructor
//	copy data members of object
//CompositePart::CompositePart(const CompositePart &CompositePart)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for CompositePart");
//}


// Assigment operator
//	assign data members of object
const CompositePart &CompositePart::operator=(const CompositePart &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for CompositePart");
}

void CompositePart::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doRegistrations(system);

   string varName1, varName2, varName3, varName4, varName5, varName6;
   string varName7, varName8, varName9, VarName;
   string desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8, desc9;


   varName1 = "dm_green_" + c.name;
   desc1 = "Weight of " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_DMGreen, "g/m^2", desc1.c_str());

   varName2 = "n_green_" + c.name;
   desc2 = "N in " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_NGreen, "g/m^2", desc2.c_str());

   varName3 = "p_green_" + c.name;
   desc3 = "P in " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_PGreen, "g/m^2", desc3.c_str());

   varName1 = c.name + "_wt";
   desc1 = "Weight of " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_DMGreen, "g/m^2", desc1.c_str());

   varName2 = c.name + "_n";
   desc2 = "N in " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_NGreen, "g/m^2", desc2.c_str());

   varName3 = c.name + "_p";
   desc3 = "P in " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_PGreen, "g/m^2", desc3.c_str());

   varName1 = "dm_dead_" + c.name;
   desc1 = "Weight of dead " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_DMDead, "g/m^2", desc1.c_str());

   varName2 = "n_dead_" + c.name;
   desc2 = "N in dead " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_NDead, "g/m^2", desc2.c_str());

   varName3 = "p_dead_" + c.name;
   desc3 = "P in dead " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_PDead, "g/m^2", desc3.c_str());

   varName1 = "dead" + c.name + "_wt";
   desc1 = "Weight of dead " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_DMDead, "g/m^2", desc1.c_str());

   varName2 = "dead" + c.name + "_n";
   desc2 = "N in dead " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_NDead, "g/m^2", desc2.c_str());

   varName3 = "dead" + c.name + "_p";
   desc3 = "P in dead " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_PDead, "g/m^2", desc3.c_str());

   varName1 = "dm_senesced_" + c.name;
   desc1 = "Weight of senesced " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_DMSenesced, "g/m^2", desc1.c_str());

   varName2 = "n_senesced_" + c.name;
   desc2 = "N in senesced " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_NSenesced, "g/m^2", desc2.c_str());

   varName3 = "p_senesced_" + c.name;
   desc3 = "P in senesced " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_PSen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_green_" + c.name;
   desc1 = "Delta Weight of " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_dm_green, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_green_" + c.name;
   desc2 = "Delta N in " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_green, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_green_" + c.name;
   desc3 = "Delta P in " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_p_green, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_dead_" + c.name;
   desc1 = "Delta Weight of dead " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_dm_dead, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_dead_" + c.name;
   desc2 = "Delta N in dead " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_dead, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_dead_" + c.name;
   desc3 = "Delta P in dead " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_p_dead, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_senesced_" + c.name;
   desc1 = "Delta Weight of senesced " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_dm_senesced, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_senesced_" + c.name;
   desc2 = "Delta N in senesced " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_senesced, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_senesced_" + c.name;
   desc3 = "Delta P in senesced " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_p_sen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_detached_" + c.name;
   desc1 = "Delta Weight of detached " + c.name;
   setupGetFunction(system, varName1.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_dm_detached, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_detached_" + c.name;
   desc2 = "Delta N in detached " + c.name;
   setupGetFunction(system, varName2.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_detached, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_detached_" + c.name;
   desc3 = "Delta P in detached " + c.name;
   setupGetFunction(system, varName3.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_p_det, "g/m^2", desc3.c_str());

   varName4 =  "n_conc_" + c.name;
   desc4 = "N concentration in " + c.name;
   setupGetFunction(system, varName4.c_str(), protocol::DTsingle, false, &CompositePart::get_n_conc, "%", desc4.c_str());

   varName5 = "p_conc_" + c.name;
   desc5 = "P concentration in " + c.name;
   setupGetFunction(system, varName5.c_str(), protocol::DTsingle, false, &CompositePart::get_p_conc, "%", desc5.c_str());

   varName6 = "n_conc_crit_" + c.name;
   desc6 = "critical N content in " + c.name;
   setupGetFunction(system, varName6.c_str(), protocol::DTsingle, false, &CompositePart::get_n_conc_crit, "%", desc6.c_str());

   varName7 = "n_conc_min_" + c.name;
   desc7 = "minimum N content in " + c.name;
   setupGetFunction(system, varName7.c_str(), protocol::DTsingle, false, &CompositePart::get_n_conc_min,"%", desc7.c_str());

   varName8 = "n_demand_" + c.name;
   desc8 = "N demand of " + c.name;
   setupGetFunction(system, varName8.c_str(), protocol::DTsingle, false, &CompositePart::get_NDemand, "g/m^2", desc8.c_str());

   varName9 = "dlt_n_retrans_" + c.name;
   desc9 = "N retranslocated to/from " + c.name;
   setupGetFunction(system, varName9.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_retrans_" + c.name;
   desc9 = "N retranslocated to/from senesced " + c.name;
   setupGetFunction(system, varName9.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_senesced_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_trans_" + c.name;
   desc9 = "N translocated to/from senesced " + c.name;
   setupGetFunction(system, varName9.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_n_senesced_trans, "g/m^2", desc9.c_str());

   varName9 = "dlt_dm_retrans_" + c.name;
   desc9 = "DM retranslocated to/from " + c.name;
   setupGetFunction(system, varName9.c_str(), protocol::DTsingle, false, &CompositePart::get_Dlt_dm_green_retrans, "g/m^2", desc9.c_str());

}

void CompositePart::get_DMGreen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreen());
   }

void CompositePart::get_NGreen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, nGreen());
   }

void CompositePart::get_PGreen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, pGreen());
   }

void CompositePart::get_DMDead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmDead());
   }

void CompositePart::get_NDead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, nDead());
   }

void CompositePart::get_PDead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, pDead());
   }

void CompositePart::get_DMSenesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmSenesced());
   }

void CompositePart::get_NSenesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, nSenesced());
   }

void CompositePart::get_PSen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, pSenesced());
   }

void CompositePart::get_Dlt_dm_green(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltDmGreen());
   }

void CompositePart::get_Dlt_n_green(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNGreen());
   }

void CompositePart::get_Dlt_p_green(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltPGreen());
   }

void CompositePart::get_Dlt_dm_dead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltDmDead());
   }

void CompositePart::get_Dlt_n_dead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNDead());
   }

void CompositePart::get_Dlt_p_dead(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltPDead());
   }

void CompositePart::get_Dlt_dm_senesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltDmSenesced());
   }

void CompositePart::get_Dlt_n_senesced(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNSenesced());
   }

void CompositePart::get_Dlt_p_sen(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltPSenesced());
   }

void CompositePart::get_Dlt_dm_detached(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltDmDetached());
   }

void CompositePart::get_Dlt_n_detached(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNDetached());
   }

void CompositePart::get_Dlt_p_det(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltPDetached());
   }

void CompositePart::get_n_conc(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, nConcPercent());
   }

void CompositePart::get_p_conc(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, pConcPercent());          //FIXME this is inconsistent with get_n_conc
   }

void CompositePart::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, n_conc_crit());
   }

void CompositePart::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, n_conc_min());
   }

void CompositePart::get_NDemand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, nDemand());
   }

void CompositePart::get_Dlt_n_retrans(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNRetrans());
   }

void CompositePart::get_Dlt_n_senesced_retrans(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNSenescedRetrans());
   }

void CompositePart::get_Dlt_n_senesced_trans(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltNSenescedTrans());
   }

void CompositePart::get_Dlt_dm_green_retrans(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dltDmGreenRetrans());
   }


float CompositePart::dltNGreen(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNGreen();
   return sum;
}


float CompositePart::dltPGreen(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPGreen();
   return sum;
}


float CompositePart::dltDmDead(void)
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltDmDead();
   return sum;
}


float CompositePart::dltNDead(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNDead();
   return sum;
}


float CompositePart::dltPDead(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPDead();
   return sum;
}


float CompositePart::dltDmSenesced(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltDmSenesced();
   return sum;
}


float CompositePart::dltNSenesced(void)
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenesced();
   return sum;
}


float CompositePart::dltPSenesced(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPSenesced();
   return sum;
}

float CompositePart::dltNDetached(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNDetached();
   return sum;
}


float CompositePart::dltPDetached(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltPDetached();
   return sum;
}


float CompositePart::nConc(void)                                                 //FIXME
   //===========================================================================
{
   float sum = 0.0;
   float dmSum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      sum += (*part)->nGreen();
      dmSum += (*part)->dmGreen();
   }
   return divide (sum , dmSum , 0.0);
}

float CompositePart::nConcPercent(void)                                                 //FIXME
   //===========================================================================
{
   return nConc() * fract2pcnt;
}


float CompositePart::pConc(void)                                                  //FIXME
   //===========================================================================
{
   float sum = 0.0;
   float dmSum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      sum += (*part)->pGreen();
      dmSum += (*part)->dmGreen();
   }
   return divide (sum , dmSum , 0.0);
}

float CompositePart::pConcPercent(void)                                                  //FIXME
   //===========================================================================
{
   return pConc() * fract2pcnt;
}


float CompositePart::n_conc_crit(void)                                             //FIXME
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_crit();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}


float CompositePart::n_conc_min(void)                                              //FIXME
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->n_conc_min();
   return divide (sum , myParts.size() , 0.0);           //unweighted mean
}


float CompositePart::dltNRetrans(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNRetrans();
   return sum;
}


float CompositePart::dltNSenescedRetrans(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedRetrans();
   return sum;
}


float CompositePart::dltNSenescedTrans(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltNSenescedTrans();
   return sum;
}


float CompositePart::dltDmGreenRetrans(void) 
   //===========================================================================
{
   float sum = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      sum += (*part)->dltDmGreenRetrans();
   return sum;
}


float CompositePart::dmTotal(void) 
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float CompositePart::dmGreenDemand(void)  
   //===========================================================================
{
   float dmGreenDemand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmGreenDemand += (*part)->dmGreenDemand();
   return dmGreenDemand;
}

float CompositePart::grainWt(void)
   //===========================================================================
{
   float grainWtTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      grainWtTotal += (*part)->grainWt();
   return grainWtTotal;
}

float CompositePart::dmGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGrainTotal();
   return dmTotal;
}

float CompositePart::dmVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmVegTotal();
   return dmTotal;
}

float CompositePart::dmGreenGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGreenGrainTotal();
   return dmTotal;
}

float CompositePart::dmGreenVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGreenVegTotal();
   return dmTotal;
}

float CompositePart::dmSenescedVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmSenescedVegTotal();
   return dmTotal;
}

float CompositePart::dltDmDetached(void) 
   //===========================================================================
{
   float dlt_dm_detached = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt_dm_detached += (*part)->dltDmDetached();
   return dlt_dm_detached;
}

float CompositePart::dmSenesced(void) 
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->dmSenesced();
   return result;
}

float CompositePart::dmDeadVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmDead();
   return dmTotal;
}

float CompositePart::dmDead(void) 
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->dmDead();
   return result;
}

float CompositePart::nTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float CompositePart::nGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nGrainTotal();
   return nTotal;
}

float CompositePart::nVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nVegTotal();
   return nTotal;
}

float CompositePart::nGreenGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nGreenGrainTotal();
   return nTotal;
}

float CompositePart::nGreenVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nGreenVegTotal();
   return nTotal;
}

float CompositePart::nGreen(void) 
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nGreen();
   return result;
}

float CompositePart::nSenescedVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nSenescedVegTotal();
   return nTotal;
}

float CompositePart::nSenesced(void)
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nSenesced();
   return result;
}

float CompositePart::nDeadVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nDeadVegTotal();
   return nTotal;
}

float CompositePart::nDead(void)
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nDead();
   return result;
}

float CompositePart::nMaxPot(void)
   //===========================================================================
{
   float nMaxPot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();                                //FIXME Is this a conc?
   return nMaxPot;
}

float CompositePart::nMax(void)
   //===========================================================================
{
   float result = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      result += (*part)->nMax();                                      //FIXME Is this a conc?
   return result;
}

float CompositePart::nMinPot(void)
   //===========================================================================
{
   float nMinPot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nMinPot += (*part)->nMinPot();
   return nMinPot;                                                 //FIXME Is this a conc?
}

float CompositePart::nConcGrain(void)
   //===========================================================================
{
   float nGreen = 0.0;
   float dmGreen = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      nGreen += (*part)->nGreenGrainTotal();
      dmGreen += (*part)->dmGreenGrainTotal();
   }
   return divide (nGreen , dmGreen , 0.0) * fract2pcnt;
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
   SoilNDemand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      SoilNDemand += (*part)->soilNDemand();
   return SoilNDemand;
}

float CompositePart::nDemand(void)
   //============================================================================
{
   float n_demand = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_demand += (*part)->nDemand();
   return n_demand;
}

float CompositePart::nCapacity(void)
   //============================================================================
{
   NCapacity = 0.0;
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
      (*part)->doNPartition(dlt.n_green, n_demand_sum, n_capacity_sum);

   float dlt_n_green_sum = dltNGreen();
   if (!reals_are_equal(dlt_n_green_sum - dlt.n_green, 0.0))
      {
      string msg = c.name + " dlt_n_green mass balance is off: dlt_n_green_sum ="
                  + ftoa(dlt_n_green_sum, ".6")
                  + " vs nSupply ="
                  + ftoa(dlt.n_green, ".6");
      plant->warningError(msg.c_str());
      }
}

float CompositePart::pTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float CompositePart::pGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGrainTotal();
   return pTotal;
}

float CompositePart::pVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pVegTotal();
   return pTotal;
}

float CompositePart::pGreenGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreenGrainTotal();
   return pTotal;
}

float CompositePart::pDeadGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDeadGrainTotal();
   return pTotal;
}

float CompositePart::pGreenVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreenVegTotal();
   return pTotal;
}

float CompositePart::pGreen(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float CompositePart::pSenescedGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenescedGrainTotal();
   return pTotal;
}

float CompositePart::pSenescedVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenescedVegTotal();
   return pTotal;
}

float CompositePart::pSenesced(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

float CompositePart::pDeadVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDeadVegTotal();
   return pTotal;
}

float CompositePart::pDead(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float CompositePart::pConcGrain(void)
   //===========================================================================
{
   float pGreen = 0.0;
   float dmGreen = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      pGreen += (*part)->pGreenGrainTotal();
      dmGreen += (*part)->dmGreenGrainTotal();
   }
   return divide (pGreen , dmGreen , 0.0) * fract2pcnt;
}

float CompositePart::pConcGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   float dmTotal = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      pTotal += (*part)->pGrainTotal();
      dmTotal += (*part)->dmGrainTotal();
   }
   return divide (pTotal , dmTotal , 0.0) * fract2pcnt;
}

float CompositePart::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();                                      //FIXME Is this a conc?
   return pMaxPot;
}

float CompositePart::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)           //FIXME Is this a conc?
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

void CompositePart::get_dlt_p_green(vector<float> &dlt_p_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_green(dlt_p_green);
}

void CompositePart::get_p_green(vector<float> &p_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_green(p_green);
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

void CompositePart::get_dm_dead(vector<float> &dm_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_dead(dm_dead);
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

void CompositePart::get_dlt_dm_dead_detached(vector<float> &dlt_dm_dead_detached)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);
}

void CompositePart::get_dlt_dm_green_dead(vector<float> &dlt_dm_green_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_dead(dlt_dm_green_dead);
}

void CompositePart::get_dlt_dm_senesced_dead(vector<float> &dlt_dm_senesced_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);
}

void CompositePart::get_n_demanded(vector<float> &n_demand)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_demanded(n_demand);
}

void CompositePart::get_n_green(vector<float> &n_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_green(n_green);
}

void CompositePart::get_n_senesced(vector<float> &n_senesced)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_senesced(n_senesced);
}

void CompositePart::get_n_dead(vector<float> &n_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_dead(n_dead);
}

void CompositePart::get_dlt_n_green(vector<float> &n_green)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_green(n_green);
}

void CompositePart::get_dlt_n_dead(vector<float> &n_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_dead(n_dead);
}

void CompositePart::get_dlt_n_retrans(vector<float> &n_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_retrans(n_retrans);
}

void CompositePart::get_dlt_n_senesced(vector<float> &n_senesced)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced(n_senesced);
}

void CompositePart::get_dlt_n_senesced_dead(vector<float> &dlt_n_senesced_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);
}

void CompositePart::get_dlt_n_senesced_retrans(vector<float> &n_senesced_retrans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_retrans(n_senesced_retrans);
}

void CompositePart::get_dlt_n_senesced_trans(vector<float> &n_senesced_trans)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_trans(n_senesced_trans);
}

void CompositePart::get_dlt_n_detached(vector<float> &n_detached)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_detached(n_detached);
}

void CompositePart::get_dlt_n_dead_detached(vector<float> &n_dead_detached)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_dead_detached(n_dead_detached);
}

void CompositePart::get_p_dead(vector<float> &p_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_dead(p_dead);
}

void CompositePart::get_p_sen(vector<float> &p_sen)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_sen(p_sen);
}

void CompositePart::get_dlt_p_detached(vector<float> &dlt_p_detached)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_detached(dlt_p_detached);
}

void CompositePart::get_dlt_p_dead(vector<float> &dlt_p_dead)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_dead(dlt_p_dead);
}

void CompositePart::get_dlt_p_sen(vector<float> &dlt_p_sen)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_sen(dlt_p_sen);
}

void CompositePart::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Numer
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doGrainNumber();
}

void CompositePart::doTick(protocol::timeType &tick)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doTick(tick);
}

// Field a NewMet event
void CompositePart::doNewMet(protocol::newmetType &newmet)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNewMet(newmet);
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


void CompositePart::doInit1 ()
   // ====================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doInit1 ();
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

float CompositePart::dmGreen(void) 
   //===========================================================================
{
   float DMGreen = 0.0;
   vector <plantPart * >::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      DMGreen +=(*part)->dmGreen();
   return DMGreen;
}

float CompositePart::dltDmGreen(void) 
   //===========================================================================
{
   float dltDmGreen = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dltDmGreen +=(*part)->dltDmGreen();
   return dltDmGreen;
}


float CompositePart::dltDmRetranslocateSupply(float demand_differential) 
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

void CompositePart::collectDeadDetachedForResidue(vector<string> &part_name
                                               , vector<float> &dm_dead_detached
                                               , vector<float> &n_dead_detached
                                               , vector<float> &p_dead_detached
                                               , vector<float> &fraction_to_residue)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->collectDetachedForResidue(part_name
                                         , dm_dead_detached
                                         , n_dead_detached
                                         , p_dead_detached
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
      add_covers (cover, (*part)->coverTotal());
   return cover;
}

float CompositePart::coverGreen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      add_covers (cover, (*part)->coverGreen());
   return cover;
}

float CompositePart::coverDead(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      add_covers (cover, (*part)->coverDead());
   return cover;
}

float CompositePart::coverSen(void)
   //===========================================================================
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      add_covers (cover, (*part)->coverSen());
   return cover;
}

//float CompositePart::total()
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void CompositePart::display(ostream &os) const
{
   //	os << "CompositePart:" << endl;
   //	os << "Green cover:    " << coverPod.green << endl;
   //	os << "Senesced cover: " << coverPod.sen << endl;
   //	os << "Dead cover:     " << coverPod.dead << endl;
   //	os << "Green shell: " << green.shell << endl;
   //	os << "Green meal: " << green.meal << endl;
   //	os << "Senesced shell: " << senesced.shell << endl;
   //	os << "Senesced meal: " << senesced.meal << endl;
   //	os << "Dead shell: " << dead.shell << endl;
   //	os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


float CompositePart::calcCover (float canopy_fac)
{
   float cover = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      add_covers (cover, (*part)->calcCover(canopy_fac));
   return cover;
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
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      grainNo += (*part)->grainNo();
   return grainNo;
}

float CompositePart::nDemandGrain(void)
   //===========================================================================
{
   float nDemandGrain = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      nDemandGrain += (*part)->nDemandGrain();
   return nDemandGrain;
}

float CompositePart::dltDmPotTe(void)
   //===========================================================================
{
   float dltDmPotTe = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmPotTe += (*part)->dltDmPotTe();
   return dltDmPotTe;
}

float CompositePart::dltDmPotRue(void)
   //===========================================================================
{
   float dltDmPotRue = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmPotRue += (*part)->dltDmPotRue();
   return dltDmPotRue;
}

float CompositePart::grainNConcPercent(void)
   //===========================================================================
{
   float nGreen = 0.0;
   float dmGreen = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
   {
      nGreen += (*part)->nGreenGrainTotal();
      dmGreen += (*part)->dmGreenGrainTotal();
   }
   return divide (nGreen , dmGreen , 0.0) * fract2pcnt;
}

float CompositePart::dltDmGrainDemand(void) 
   //===========================================================================
{
   float dltDmDemand = 0.0;
   vector <plantPart *>::iterator part;
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
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dlt_dm_green_retrans += (*part)->dltDmRetranslocate();
   return dlt_dm_green_retrans;
}

float CompositePart::dltDmGreenRetransUptake(void) 
   //===========================================================================
{
   float dltDmUptake = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      dltDmUptake += (*part)->dltDmGreenRetransUptake();
   return dltDmUptake;
}

float CompositePart::interceptRadiation (float radiation)
   //===========================================================================
{
   float interceptRadiation = 0.0;
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      interceptRadiation += (*part)->interceptRadiation (radiation);         //FIXME - divey up radiation
   return interceptRadiation;
}

void CompositePart::doDmPotRUE (double  radn_int_pod )
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmPotRUE(radn_int_pod);                                     //FIXME divey up radiation intercepted
}

void CompositePart::doTECO2(void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doTECO2();
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

void CompositePart::doDmPotTE (void)
   //===========================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmPotTE();
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
   vector <plantPart *>::iterator part;
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
       string msg = c.name + " giveDmGreen mass balance is off:\n"
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
      string msg = c.name + " dlt_dm_green_retrans_tot mass balance is off: "
                   + ftoa(dltDmGreenRetransUptake (), ".6")
                   + " vs "
                   + ftoa(dlt_dm_green_retrans, ".6");
      plant->warningError(msg.c_str());
      }
   }

void CompositePart::doSenescence1 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSenescence1(sen_fr);
}

void CompositePart::doSenescence2 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doSenescence2(sen_fr);
}

void CompositePart::doDmMin (void)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doDmMin();
}

void CompositePart::doNInit (void)
   //============================================================================
{
   //       Initialise plant nitrogen.
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->doNInit();
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
////    plantPart::doNFixRetranslocate(NFix, NDemandDifferentialTotal);
////    float n_demand_differential = 0.0;
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

void CompositePart::doDmDetachment(void)
   //============================================================================
{
   dlt.dm_detached = 0.0;
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dlt.dm_detached += (*part)->dltDmDetached();
      (*part)->doDmDetachment();
      }
}

void CompositePart::doNDetachment(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNDetachment();
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
   vector <plantPart *>::iterator part;
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

void CompositePart::doPDetachment(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPDetachment();
}

void CompositePart::doPInit(void)
   //============================================================================
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doPInit();
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
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isYieldPart()) return true;
   return false;
   }

bool CompositePart::isRetransPart(void)
// True if at least one of our parts supplies retranslocate
   {
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      if ((*part)->isRetransPart()) return true;
   return false;
   }
