#include "StdPlant.h"

#include "PlantFruit.h"
#include "FloretPart.h"

using namespace std;


inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
//  initialise data members.
PlantFruit::PlantFruit(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(scienceAPI, p, name)
{
   cohortNum = 0;
}

// destructor
PlantFruit::~PlantFruit()
{
}

ostream &operator<<(ostream &output, const PlantFruit /*&pool*/)
{
   //   output << "PlantFruit:" << endl;
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
const PlantFruit &PlantFruit::operator=(const PlantFruit &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for plantFruit");
}

void PlantFruit::prepare (void)
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->prepare ();
   }

void PlantFruit::process (void)
   {
   vector <plantPart *>::iterator part;
   for (part =  myParts.begin(); part != myParts.end(); part++)
      (*part)->process ();

   }

void PlantFruit::onInit1(protocol::Component *system)
   //===========================================================================
   {
   CompositePart::onInit1(system);
   addNewCohort(system);
   }

void PlantFruit::display(ostream &os)
{
   //   os << "PlantFruit:" << endl;
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


void PlantFruit::addNewCohort (protocol::Component *system)
   // ====================================================================
{
   cohortNum ++;
   ostringstream cohortName;
   cohortName << "cohort" << cohortNum;

   string fruitType;
   scienceAPI.readOptional("fruit_part_type", fruitType);
   if (fruitType == "fruit_no")
       add(new FruitCohortFN(scienceAPI, plant, cohortName.str()));
   else
       add(new FruitCohort(scienceAPI, plant, cohortName.str()));

   plantPart *myPart = myParts.back();
   myPart->onInit1(system);
}

void PlantFruit::doNewCohort (protocol::Component *system)
   // ====================================================================
{
   bool newCohort = false;
   // insert rules to make new cohort here

   if (newCohort)
      addNewCohort(system);
}

