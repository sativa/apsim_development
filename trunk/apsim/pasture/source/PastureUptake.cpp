#include "PastureUptake.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureUptake::PastureUptake(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureUptake::PastureUptake(protocol::Component *s, string uptakeNam, string deltaNam, string unitNam)
   {
      system = s;
      deltaName = deltaNam;
      uptakeName = uptakeNam;
      unitName = unitNam;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PastureUptake::~PastureUptake(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PastureUptake::doInit1(const protocol::Init1Data& initData)
//===========================================================================
   {
      // get
   elementUptakeID = system->addRegistration(::get, -1, uptakeName.c_str(), singleArrayTypeDDML);

      // set
   dltElementID = system->addRegistration(::set, -1, deltaName.c_str(), singleArrayTypeDDML);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PastureUptake::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   }

// ------------------------------------------------------------------
void PastureUptake::doUptake(void)
//===========================================================================
{
      protocol::Variant *variantGet;
      std::vector <float> elementUptake;

      bool ok = system->getVariable(elementUptakeID, &variantGet, true);
      if (ok)
      {
         bool ok = variantGet->unpack(elementUptake);
         if (ok && elementUptake.size() >= 1)
         { // ok
         }
         else
            throw std::runtime_error("Couldn't unpack " + uptakeName);
      }
      else
         throw std::runtime_error("Couldn't get variable " + uptakeName);

      float dltElement[max_layer];
      fill_real_array (dltElement, 0.0, max_layer);

      ostringstream msg;
      if (cDebug == "on")
         msg << endl <<  uptakeName + ":-" << endl;

      int numLayers = elementUptake.size();
      float uptakeTotal = 0.0;

      for (int layer = 0; layer < numLayers; layer++)
      {
         dltElement[layer] = -elementUptake[layer];
         uptakeTotal +=  elementUptake[layer];

         if (cDebug == "on")
            msg << "   layer " << layer+1 << " = " << elementUptake[layer]  << " " << unitName << endl;
      }

   if (cDebug == "on")
   {
      msg << "   " << uptakeName << " total = " << uptakeTotal << " " << unitName << endl << ends;
      system->writeString (msg.str().c_str());
   }

   std::vector<float> dltElementValues(dltElement, dltElement+numLayers);
   system->setVariable(dltElementID, dltElementValues);
}



void PastureUptake::readParameters ( void )
//===========================================================================
   {
    const char*  section_name = "parameters" ;
    cDebug = system->readParameter (section_name, "debug");
   }


//===========================================================================
void PastureUptake::fill_real_array (float *var  //(OUTPUT) array to set
                                       , float value //(IN) scalar value to set array to
                                       , int limit)   //(IN) number of elements
//===========================================================================

/*Purpose
 *   sets real array var to value up to level limit
 */

   {
   for (int indx = 0; indx < limit; indx++)
      var[indx] = value;
   }



