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
#include "PastureConverter.h"


#pragma package(smart_init)
using namespace std;


#define doubleArrayTypeDDML "<type  array=\"T\" kind=\"double\"/>"
#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"

      const float kg2g = 1000.0 ;
      const float ha2sm = 10000.0 ;
      const float g2kg = 1.0/kg2g ;
      const float sm2ha = 1.0/ha2sm ;
      const float cmol2mol = 1.0/100.0 ;
      const float mm2m = 1.0/1000.0;

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
//===========================================================================
   {
   return new PastureConverter;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureConverter::PastureConverter(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PastureConverter::~PastureConverter(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit1(const FString& sdml)
//===========================================================================
   {
   protocol::Component::doInit1(sdml);
   sandID = addRegistration(RegistrationType::respondToGet, "sand", doubleArrayTypeDDML);
   vpdID = addRegistration(RegistrationType::respondToGet, "vpd", singleTypeDDML);
   maxtID = addRegistration(RegistrationType::get, "maxt", singleTypeDDML);
   mintID = addRegistration(RegistrationType::get, "mint", singleTypeDDML);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PastureConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void PastureConverter::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
//===========================================================================
{
     // sand_layer
   if (queryData.ID == sandID) sendSand(queryData);
   else if (queryData.ID == vpdID) sendVPD(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void PastureConverter::readParameters ( void )
//===========================================================================
   {
//   const char*  my_name = "readParameters" ;
   const char*  section_name = "parameters" ;

   writeString (" - reading parameters");

    cDebug = readParameter (section_name, "debug");
    readParameter (section_name, "sand", pSandLayer, numLayers, 0.0, 1.0);
    readParameter (section_name,"svp_fract", cSVPFract, 0.0, 1.0);

   ostringstream msg;
   msg << "sand (kg/kg) = ";
   for (int layer = 0; layer < numLayers; layer++)
      msg << pSandLayer[layer] << " ";
   msg << endl << ends;
   writeString (msg.str().c_str());
   }

void PastureConverter::sendSand (protocol::QueryValueData& queryData)
//===========================================================================
{
   vector <double> sandLayers;
   for (int layer = 0; layer != numLayers; layer++)
      sandLayers.push_back(pSandLayer[layer]);

      ostringstream msg;
      msg << "send sand (kg/kg) = ";
      for (int layer = 0; layer < numLayers; layer++)
         msg << pSandLayer[layer] << " ";
      msg << endl << ends;
      writeString (msg.str().c_str());

   sendVariable(queryData, sandLayers);
}

void PastureConverter::sendVPD (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantMaxT;
      bool okMaxt = getVariable(maxtID, variantMaxT, true);
      if (okMaxt)
      {
         protocol::Variant* variantMinT;
         bool ok = getVariable(mintID, variantMinT, true);
         if (ok)
         {
         float maxt;
         bool ok = variantMaxT->unpack(maxt);  // what happens if this is not ok?
         float mint;
         ok = variantMinT->unpack(mint);  // what happens if this is not ok?
         double VPD = vpd(cSVPFract, maxt, mint);

         sendVariable(queryData, VPD);
         }
      }
      else
      {   // didn't get the day_length ID ok. Do nothing about it.
      }
}

float PastureConverter::vpd(float cSVPFract, float maxt, float mint) //(INPUT)
//==========================================================================
{
      float vpd = max (cSVPFract * ( svp(maxt) - svp(mint)), 0.01);
      return vpd;
}

//==========================================================================
float PastureConverter::svp(float temp) //(INPUT)  fraction of distance between svp at mi
//==========================================================================
/*  Purpose
*
*  Mission Statement
*    function to get saturation vapour pressure for a given temperature in oC (kpa)
*
*  Changes
*       21/5/2003 ad converted to BC++
*
*/
   {
      const double ES0 = 6.1078;            // Teten coefficients -SATURATION VAPOR PRESSURE (MB) OVER WATER AT 0C
      const double TC_B = 17.269388;        // Teten coefficients
      const double TC_C = 237.3;            // Teten coefficients
      const float mb2kpa = 100.0/1000.0;    // convert pressure mbar to kpa 1000 mbar = 100 kpa

   float val = ES0 * exp(TC_B * temp / (TC_C + temp)) * mb2kpa;
   return val;
   }

//===========================================================================
float PastureConverter::divide (float dividend, float divisor, float default_value)
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





