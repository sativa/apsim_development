
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_PlantFruit NO					// build unit test?
#include "PlantFruit.h"


using namespace std;

void push_routine (const char *) {};
void pop_routine (const char *) {};

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
PlantFruit::PlantFruit(plantInterface *p, const string &name) : CompositePart(p, name)
{
}

// destructor
PlantFruit::~PlantFruit()
{
   vector <plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      delete (*part);
}

ostream &operator<<(ostream &output, const PlantFruit /*&pool*/)
{
   //	output << "PlantFruit:" << endl;
   //	output << "   Green cover:    " << pool.coverPod.green << endl;
   //	output << "   Senesced cover: " << pool.coverPod.sen << endl;
   //	output << "   Dead cover:     " << pool.coverPod.dead << endl;
   //	output << endl;
   //	output << "   Green shell:    " << pool.green.shell << endl;
   //	output << "   Green meal:    " << pool.green.meal << endl;
   //	output << "   Senesced shell: " << pool.senesced.shell << endl;
   //	output << "   Senesced meal: " << pool.senesced.meal << endl;
   //	output << "   Dead shell:     " << pool.dead.shell << endl;
   //	output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// copy constructor
//	copy data members of object
//PlantFruit::PlantFruit(const PlantFruit &PlantFruit)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for plantFruit");
//}


// Assigment operator
//	assign data members of object
const PlantFruit &PlantFruit::operator=(const PlantFruit &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for plantFruit");
}

void PlantFruit::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   CompositePart::doRegistrations(system);

//   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
//   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&PlantFruit::get_head_wt, "g/m^2", "Weight of heads");
//   setupGetFunction(system, "head_n", protocol::DTsingle, false,&PlantFruit::get_head_n, "g/m^2", "N in heads");
//   setupGetFunction(system, "head_p", protocol::DTsingle, false, &PlantFruit::get_head_p, "g/m^2","P in head");

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doRegistrations(system);
}

void PlantFruit::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   plantPart::doNPartition(nSupply, n_demand_sum, n_capacity_sum);             //FIXME need to do this differently
   vector<plantPart *>::iterator part;

   n_demand_sum = nDemand();
   n_capacity_sum = nCapacity();

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNPartition(dlt.n_green, n_demand_sum, n_capacity_sum);

   float dlt_n_green_sum = dltNGreen();
   if (!reals_are_equal(dlt_n_green_sum - dlt.n_green, 0.0))
      {
      string msg = c.name + " dlt_n_green mass balance is off: dlt_n_green_sum ="
         + ftoa(dlt_n_green_sum, ".6")
         + " vs nSupply ="
         + ftoa(nSupply, ".6");
      plant->warningError(msg.c_str());
      }
}

void PlantFruit::doInit1 ()
   // ====================================================================
{  
   plantPart *c1 = new FruitCohort(plant, "cohort1");
   myParts.push_back(c1);
   for (vector<plantPart *>::iterator myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->doInit1();
}

void PlantFruit::doNSenescedRetrans(float navail, float n_demand_tot)              //remove  problem
   //===========================================================================   //remove
{                                                                                  //remove
   dlt.n_senesced_retrans = 0.0;                                                   //remove
   vector<plantPart *>::iterator myPart;                                           //remove
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)               //remove
      {                                                                            //remove
      (*myPart)->doNSenescedRetrans(navail, n_demand_tot);                         //remove
      dlt.n_senesced_retrans +=(*myPart)->dltNSenescedRetrans();                   //remove
      }                                                                            //remove
}                                                                                  //remove

void PlantFruit::display(ostream &os) const
{
   //	os << "PlantFruit:" << endl;
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

void PlantFruit::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
{
    plantPart::doNFixRetranslocate(NFix, NDemandDifferentialTotal);                        // FIXME - this needs to be done another way

    float n_demand_differential = nDemandDifferential ();

        // now distribute the n fixed to plant parts

    NFix = NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0);
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
       (*part)->doNFixRetranslocate (NFix, n_demand_differential);
}

void PlantFruit::doNRetranslocate( float N_supply, float g_grain_n_demand)
   //============================================================================
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.
{

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNRetranslocate(N_supply, g_grain_n_demand);

   dlt.n_retrans = 0.0;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt.n_retrans += (*part)->dlt.n_retrans;
}

