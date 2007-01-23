
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "PlantFruit.h"


using namespace std;

void push_routine (const char *) {};
void pop_routine (const char *) {};

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
PlantFruit::PlantFruit()
{}

// 	initialise data members.
PlantFruit::PlantFruit(plantInterface *p, const string &name) : CompositePart(p, name)
{
   cohortNum = 0;
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
PlantFruit::PlantFruit(const PlantFruit &/* PlantFruit*/)
//===========================================================================
{
	throw std::invalid_argument("Copy constructor NI for plantFruit");
}


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

void PlantFruit::doInit1(protocol::Component *system)
   // ====================================================================
{
   addNewCohort(system);
}

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

void PlantFruit::addNewCohort (protocol::Component *system)
   // ====================================================================
{
   cohortNum ++;
   ostringstream cohortName;
   cohortName << "cohort" << cohortNum;
   myParts.push_back(new FruitCohort(plant, cohortName.str()));

   vector<plantPart *>::iterator myPart = & myParts.back();
   (*myPart)->doInit1(system);
}

void PlantFruit::doNewCohort (protocol::Component *system)
   // ====================================================================
{
   // insert rules to make new cohort here
   addNewCohort(system);
}

