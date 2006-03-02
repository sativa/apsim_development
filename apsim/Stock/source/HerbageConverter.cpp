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
#include "HerbageConverter.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"

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
HerbageConverter::HerbageConverter(protocol::Component *s) : ConverterBase(s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
HerbageConverter::~HerbageConverter(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void HerbageConverter::doInit1(const FString& sdml)
   {
   tramplingID = system->addRegistration(RegistrationType::get, "trampling", singleTypeDDML);
   plant2stockID = system->addRegistration(RegistrationType::respondToGet, "plant2stock", plant2stockTypeDDML);
   removeHerbageID = system->addRegistration(RegistrationType::respondToEvent, "remove_herbage", remove_herbageTypeDDML);

   dmFeedOnOfferID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
   dmFeedRemovedID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

     herbage_model = system->readParameter ("constants", "herbage_model");

    if (herbage_model == "")
       throw std::invalid_argument("The parameter 'herbage_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (herbage_model == "plant")
       conversion = new PlantHerbage(system);
//    else if (scratch == "surfaceom")
//       herbage = new ResidueHerbage();
//    else if (scratch == "other")
//       herbage = new NonHerbage();
    else
       throw std::invalid_argument("Unknown herbage model '" + herbage_model + "'");

    conversion->doInit1(sdml);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void HerbageConverter::doInit2(void)
   {
         ostrstream msg;
         msg << "Herbage model:- " << herbage_model << endl << ends;
         system->writeString (msg.str());

      plant2StockSent = false;
      readParameters (); // Read constants
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
    conversion->doInit2();
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void HerbageConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID == removeHerbageID && plant2StockSent == true)
   {
      variant.unpack(grazed);
      if (c.debug == "on")
      {
         ostrstream msg1;
         msg1 << endl << "Remove herbage dmd pools:-" << endl;
         float dmTotal1 = 0.0;

         for (unsigned int pool = 0; pool < feed.herbage.size(); pool++)
         {
            msg1 << "   dm pool " << pool+1 << " (" << feed.herbage[pool].dmd << ") = " << grazed.herbage[pool] << " (kg/ha)" << endl;
            dmTotal1 +=  grazed.herbage[pool];
         }

         msg1 << endl << "   dm total = " << dmTotal1 << " (kg/ha)" << endl << ends;

         system->writeString (msg1.str());
      }

      for (unsigned int dmdPool = 0; dmdPool < grazed.herbage.size(); dmdPool++)
      {
         if (grazed.herbage[dmdPool] > feed.herbage[dmdPool].dm)
         {
            ostrstream msg;
            msg << "Attempting to remove more herbage from dmd pool " << dmdPool+1 << " (dmd " << feed.herbage[dmdPool].dmd << ")" << " than available:-" << endl;
            msg << "Removing " << grazed.herbage[dmdPool] << " (kg/ha) from " << feed.herbage[dmdPool].dm << " (kg/ha) available." << ends;
            throw std::runtime_error (msg.str());
         }
      }

      conversion->doGrazed(grazed);

   }
   else
   {   // Don't respond to any other events.
   }
}
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void HerbageConverter::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
{
   if (queryData.ID == dmFeedOnOfferID) sendFeedOnOffer(queryData);

   //dm feed removed
   else if (queryData.ID == dmFeedRemovedID) sendFeedRemoved(queryData);

   // plant2stock
   else if (queryData.ID == plant2stockID)  sendPlant2Stock(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void HerbageConverter::sendFeedOnOffer(protocol::QueryValueData& queryData)
{
      float dmFeedOnOffer[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = feed.herbage.size();
      if (num_parts > 0)
      {
         for (unsigned i = 0; i != num_parts; i++)
         {
            dmFeedOnOffer[i] = feed.herbage[i].dm;
         }
      }
      else
      {
         num_parts = 6;
      }
      system->sendVariable(queryData, vector <float> (dmFeedOnOffer, dmFeedOnOffer+num_parts-1));
}

void HerbageConverter::sendFeedRemoved(protocol::QueryValueData& queryData)
{
      float dmFeedRemoved[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      int num_parts = grazed.herbage.size();
      if (num_parts > 0)
      {
         for (unsigned i = 0; i != num_parts; i++)
         {
            dmFeedRemoved[i] = grazed.herbage[i];
         }
      }
      else
      {
         num_parts = 6;
      }
      system->sendVariable(queryData, vector <float> (dmFeedRemoved, dmFeedRemoved+num_parts-1));
}

void HerbageConverter::sendPlant2Stock(protocol::QueryValueData& queryData)
{
      conversion->doDigestibility();
//      conversion->getVariables(dm, N, P, height, thermalTime);

      protocol::herbageType herbage;

      if (conversion->dmTotal() > 0.0)
      {

// Now PREPARE herbage
      feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());

// distribute herbage


      for (int pool = 0; pool < conversion->numDmdPools(); pool++)
      {
         herbage.dm = conversion->dmTot(pool);
         herbage.dmd = conversion->dmdValue(pool);        // kg/ha  //fixme
         herbage.cp_conc = conversion->cpConc(pool);   // (kg/ha) - parameter cpNRatio = 6.25
         herbage.p_conc = conversion->pConc(pool);
         herbage.s_conc = conversion->sConc(pool);      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha
         herbage.prot_dg = conversion->protDg(pool);     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha
         herbage.ash_alk = conversion->ashAlk(pool);      // herbage.ash_alk = 2.0;    // mol/kg
         herbage.height_ratio = conversion->heightRatio(pool);

         feed.herbage.push_back(herbage);

         if (herbage.dm > 0.0)
         {
            if (c.debug == "on")
            {
               ostrstream msg;
               msg << endl << "Herbage on offer, pool " << pool+1 << ":-" << endl
                   << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
                   << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
                   << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
                   << "   p_conc       = " <<              herbage.p_conc <<  " (kg/kg)" << endl
                   << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
                   << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg)" << endl
                   << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
                   << "   bd           = " <<              conversion->bD() << " (g/m^3)" << endl
                   << "   dm total     = " <<              conversion->dmTotal() *kg2g/ha2sm << " (g/m2)" << endl
                   << "   height       = " <<              conversion->hHeight()*mm2m << " (m)" << endl
                   << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
               system->writeString (msg.str());
            }
         }
      } // end of Pools loop
   // REST
         feed.propn_green = conversion->proportionGreen();
         feed.legume = conversion->proportionLegume();
         feed.select_factor = conversion->selectionFactor(); // ??

         plant2StockSent = true;
      }
      else
      {
         // No dry matter so Clear herbage
         feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());
         herbage.dm = 0.0;         // kg/ha
         herbage.dmd = 0.0;        // kg/ha
         herbage.cp_conc = 0.0;    // (kg/ha)
         herbage.p_conc = 0.0;
         herbage.s_conc = 0.0;      //  kg/ha
         herbage.prot_dg = 0.0;     //  kg/ha
         herbage.ash_alk = 0.0;      //  mol/kg
         herbage.height_ratio = 0.0; //

         feed.herbage.push_back(herbage);

   // REST
         feed.propn_green = 0.0;
         feed.legume = 0.0;
         feed.select_factor = 0.0;
      }
      // Now SEND feed off
      system->sendVariable(queryData, feed);
}


void HerbageConverter::readParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------

    system->writeString (" - herbage converter reading parameters");

    c.debug = system->readParameter (section_name, "debug");

      ostrstream msg;
      msg << "Debug = " << c.debug << ends;
      system->writeString (msg.str());
}


//===========================================================================
float HerbageConverter::divide (float dividend, float divisor, float default_value)
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





