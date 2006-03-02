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
#include "ScienceConverterComponent.h"


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
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new ScienceConverterComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScienceConverterComponent::ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ScienceConverterComponent::~ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   day_lengthID = addRegistration(RegistrationType::get, "day_length", singleTypeDDML);
   tramplingID = addRegistration(RegistrationType::get, "trampling", singleTypeDDML);
   ureaID = addRegistration(RegistrationType::get, "urea", singleArrayTypeDDML);
   dltUreaID = addRegistration(RegistrationType::set, "dlt_urea", singleArrayTypeDDML);
   labilePID = addRegistration(RegistrationType::get, "labile_p", singleArrayTypeDDML);
   dltLabilePID = addRegistration(RegistrationType::set, "dlt_labile_p", singleArrayTypeDDML);

   dayLengthID = addRegistration(RegistrationType::respondToGet, "dayLength", singleTypeDDML);
   plant2stockID = addRegistration(RegistrationType::respondToGet, "plant2stock", plant2stockTypeDDML);
   removeHerbageID = addRegistration(RegistrationType::respondToEvent, "remove_herbage", remove_herbageTypeDDML);
   addExcretaID = addRegistration(RegistrationType::respondToEvent, "add_excreta", add_excretaTypeDDML);

   dmFeedOnOfferID = addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
   dmFeedRemovedID = addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

   stockBuyID = addRegistration(RegistrationType::respondToEvent, "buystock", stringTypeDDML);
   buyID = addRegistration(RegistrationType::event, "buy", buystockTypeDDML);

   stockSellID = addRegistration(RegistrationType::respondToEvent, "sellstock", stringTypeDDML);
   sellID = addRegistration(RegistrationType::event, "sell", sellstockTypeDDML);

   addManureID = addRegistration(RegistrationType::event, "add_surfaceom", "", "", "");

     string scratch = readParameter ("constants", "conversion_model");
    if (scratch == "")
       throw std::invalid_argument("The parameter 'conversion_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (scratch == "plant")
       conversion = new PlantHerbage(this);
//    else if (scratch == "surfaceom")
//       herbage = new ResidueHerbage();
//    else if (scratch == "other")
//       herbage = new NonHerbage();
    else
       throw std::invalid_argument("Unknown conversion model '" + scratch + "'");

    conversion->doInit1(sdml);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit2(void)
   {
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
void ScienceConverterComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
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

         writeString (msg1.str());
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
   else if (eventID == addExcretaID && plant2StockSent == true)
   {
      variant.unpack(excreted);
      ostrstream msg1;
      if (c.debug == "on")
      {
         msg1 << endl << "Excretion:-" << endl;

         msg1 << "   faeces om " <<  " (weight) =  " << excreted.faeces_om.weight << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (n) =       " << excreted.faeces_om.n      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (p) =       " << excreted.faeces_om.p      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (s) =       " << excreted.faeces_om.s      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (ash_alk) = " << excreted.faeces_om.ash_alk << " (mol/ha)" << endl;

         msg1 << "   faeces inorg " <<  " (n) = " << excreted.faeces_inorg.n << " (kg/ha)" << endl;
         msg1 << "   faeces inorg " <<  " (p) = " << excreted.faeces_inorg.p << " (kg/ha)" << endl;
         msg1 << "   faeces inorg " <<  " (s) = " << excreted.faeces_inorg.s << " (kg/ha)" << endl;

         msg1 << "   urine " <<  " (volume) =  " << excreted.urine.volume  << " (m3/ha)" << endl;
         msg1 << "   urine " <<  " (urea) =    " << excreted.urine.urea    << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (pox) =     " << excreted.urine.pox     << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (so4) =     " << excreted.urine.so4     << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (ash_alk) = " << excreted.urine.ash_alk << " (mol/ha)" << endl;

         msg1 << ends;

         writeString (msg1.str());
      }

      const string omName = "manure";
      const string omType = "manure";
      sendAddSurfaceOMEvent (omName, omType, excreted.faeces_om);
      addUrine (excreted.urine);
    }

   else if (eventID == stockBuyID)
   {
      stockBuy(variant);
   }
   else if (eventID == stockSellID)
   {
      stockSell(variant);
   }
   else
   {   // Don't respond to any other events.
   }
}
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::stockBuy (protocol::Variant &v/*(INPUT) message variant*/)
{
    std::string  valuestr;
    int      value4;
    double   value;
    protocol::buystockType buystock;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         buystock.number = value4;

         ostrstream msg;
         msg << "Buy stock :-" << endl
             << "   number = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.number = 0;
    }

    if (incomingApsimVariant.get("sex", protocol::DTstring, false, valuestr) == true)
    {
         buystock.sex = valuestr;

         ostrstream msg;
         msg << "   sex = " << valuestr << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.sex = "";
    }

    if (incomingApsimVariant.get("age", protocol::DTdouble, false, value) == true)
    {
         buystock.age = value;

         ostrstream msg;
         msg << "   age = "  << value << " (months)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.age = 0.0;
    }

   if (incomingApsimVariant.get("weight", protocol::DTdouble, false, value) == true)
    {
         buystock.weight = value;

         ostrstream msg;
         msg << "   weight = "  << value << " (kg)" << ends;
         writeString (msg.str());

    }
    else
    {
         buystock.weight = 0.0;
    }

    if (incomingApsimVariant.get("fleece_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.fleece_wt = value;

         ostrstream msg;
         msg << "   fleece_wt = "  << value << " (kg)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.fleece_wt = 0.0;
    }

    if (incomingApsimVariant.get("pregnant", protocol::DTint4, false, value4) == true)
    {
         buystock.pregnant = value4;
    }
    else
    {
         buystock.pregnant = 0;
    }

    if (incomingApsimVariant.get("lactating", protocol::DTint4, false, value4) == true)
   {
         buystock.lactating = value4;
    }
    else
    {
         buystock.lactating = 0;
    }

    if (incomingApsimVariant.get("no_young", protocol::DTint4, false, value4) == true)
    {
         buystock.no_young = value4;
    }
    else
    {
         buystock.no_young = 0;
    }

    if (incomingApsimVariant.get("young_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.young_wt = value;
    }
    else
    {
         buystock.young_wt = 0.0;
    }

    if (incomingApsimVariant.get("young_fleece_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.young_fleece_wt = value;
    }
    else
    {
         buystock.young_fleece_wt = 0.0;
    }

    publish (buyID, buystock);
}

void ScienceConverterComponent::stockSell (protocol::Variant &v/*(INPUT) message variant*/)
{
    int      value4;
    double   value;
    protocol::sellstockType sellstock;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         sellstock.number = value4;

         ostrstream msg;
         msg << "Sell stock :-" << endl
             << "   number = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         sellstock.number = 0;
    }

    if (incomingApsimVariant.get("group", protocol::DTint4, false, value4) == true)
    {
         sellstock.group = value4;

         ostrstream msg;
         msg << " Group = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         sellstock.group = 0;
    }

    publish (sellID, sellstock);
}

void ScienceConverterComponent::sendAddSurfaceOMEvent (const string& omName, const string& omType, protocol::faeces_omType faecesOM)
{
    protocol::ApsimVariant outgoingApsimVariant(this);
    outgoingApsimVariant.store("name", protocol::DTstring, false, FString(omName.c_str()));
    outgoingApsimVariant.store("type", protocol::DTstring, false, FString(omType.c_str()));

    outgoingApsimVariant.store("mass", protocol::DTdouble, false, faecesOM.weight);
    outgoingApsimVariant.store("n", protocol::DTdouble, false, faecesOM.n);
    outgoingApsimVariant.store("p", protocol::DTdouble, false, faecesOM.p);
    outgoingApsimVariant.store("s", protocol::DTdouble, false, faecesOM.s);
    outgoingApsimVariant.store("ash_alk", protocol::DTdouble, false, faecesOM.ash_alk);

    publish (addManureID, outgoingApsimVariant);
    return;
}

void ScienceConverterComponent::addUrine (protocol::urineType urine)
{
    std::vector<float> values;               // Scratch area
    float urea[max_layer];                     // soil Urea change (kg/ha)
    float labileP[max_layer];                     // soil Urea change (kg/ha)
    int   layer;                                  // soil layer no.
    int   num_layers;                             // number of layers

    getVariable (ureaID, values, 0.0, 1000.0, true);
    num_layers = values.size();

    for (layer = 0; layer != num_layers; layer++) {urea[layer] = 0.0;}
    urea[0] = urine.urea;
    protocol::vector<float> ureaValues(urea, urea+num_layers);
    setVariable (dltUreaID, ureaValues);

    values.clear();
    if (getVariable (labilePID, values, 0.0, 1000.0, true))
    {
       num_layers = values.size();

       for (layer = 0; layer != num_layers; layer++) {labileP[layer] = 0.0;}
       labileP[0] = urine.pox;
       protocol::vector<float> labilePValues(labileP, labileP+num_layers);
       setVariable (dltLabilePID, labilePValues);
    }

    return;
}

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
{
   // Daylength
   if (queryData.ID == dayLengthID) daylengthRelay(queryData);

   // dm Feed on offer
   else if (queryData.ID == dmFeedOnOfferID) sendFeedOnOffer(queryData);

   //dm feed removed
   else if (queryData.ID == dmFeedRemovedID) sendFeedRemoved(queryData);

   // plant2stock
   else if (queryData.ID == plant2stockID)  sendPlant2Stock(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void ScienceConverterComponent::daylengthRelay (protocol::QueryValueData& queryData)
{
      protocol::Variant* variant;
      bool ok = getVariable(day_lengthID, variant, true);
      if (ok)
      {
         float dayLength;
         bool ok = variant->unpack(dayLength);  // what happens if this is not ok?
         sendVariable(queryData, dayLength);
      }
      else
      {   // didn't get the day_length ID ok. Do nothing about it.
      }
}

void ScienceConverterComponent::sendFeedOnOffer(protocol::QueryValueData& queryData)
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
      sendVariable(queryData, vector <float> (dmFeedOnOffer, dmFeedOnOffer+num_parts-1));
}

void ScienceConverterComponent::sendFeedRemoved(protocol::QueryValueData& queryData)
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
      sendVariable(queryData, vector <float> (dmFeedRemoved, dmFeedRemoved+num_parts-1));
}

void ScienceConverterComponent::sendPlant2Stock(protocol::QueryValueData& queryData)
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
               writeString (msg.str());
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
      sendVariable(queryData, feed);
}


void ScienceConverterComponent::readParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------

    writeString (" - reading parameters");

    c.herbageModuleName = readParameter (section_name, "herbage_module_name");
    c.debug = readParameter (section_name, "debug");

      ostrstream msg;
      msg << "Herbage module name = " << c.herbageModuleName << endl
          << "Debug = " << c.debug << ends;
      writeString (msg.str());
}


//===========================================================================
float ScienceConverterComponent::divide (float dividend, float divisor, float default_value)
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





