#include "HerbageBase.h"
#include "Conversion.h"

using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
HerbageBase::HerbageBase(protocol::Component *s)
   {
      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
HerbageBase::~HerbageBase(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void HerbageBase::doInit1(const FString& sdml)
   {
//   protocol::Component::doInit1(sdml);

//   dmFeedOnOfferID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
//   dmFeedRemovedID = system->addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

  }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void HerbageBase::doInit2(void)
   {
      readParameters (); // Read constants
      readHerbageModuleParameters ();
      doRunTimeReg ();
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
   }
// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void HerbageBase::doRunTimeReg(void)
   {
   removeCropBiomassID = system->addRegistration(RegistrationType::event, "remove_crop_biomass", removeCropDmTypeDDML,"", cHerbageModuleName.c_str());
   }

   void HerbageBase::doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop){};

string HerbageBase::herbageModuleName(void)
   {
      return cHerbageModuleName;
   }

string HerbageBase::debug(void)
   {
      return cDebug;
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void HerbageBase::doGrazed(protocol::remove_herbageType &grazed)
{
      protocol::removeCropDmType crop;

      doDmdPoolsToHerbageParts(grazed, crop);

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal +=  crop.dm[pool].dlt[part];
         }
      }

      if (cDebug == "on")
      {
         ostringstream msg;
         msg << endl << "Remove herbage plant parts:-" << endl;

         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               msg << "   dm " << crop.dm[pool].pool << " " << crop.dm[pool].part[part] << " = " << crop.dm[pool].dlt[part] << " (g/m2)" << endl;
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

         system->writeString (msg.str().c_str());

      }

      if (dmTotal > 1.0e-6)
      {
         system->publish (removeCropBiomassID, crop);
      }

}

//===========================================================================
void HerbageBase::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValue[0];
   const float MINDMD = cDmdValue[cNumDmdPools-1];
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;
   const float roundingMargin = 1.0e-2;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdAvg > dmdMax + errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility > Maximum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Maximum      = " <<  dmdMax << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdAvg < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Average digestibility < Minimum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Minimum      = " <<  dmdMin << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin > dmdAvg + errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Average digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMin < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }
   if (dmdMax < dmdAvg - errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility < Average digestibility:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }


   float x = (dmdAvg - dmdMin) / (dmdMax - dmdMin);
   int startDmd = (MAXDMD - dmdMax)*10.0 + errorMargin;
   int endDmd = (MAXDMD - dmdMin)*10.0 + errorMargin;
   int numPools = (endDmd - startDmd) + 1;

   switch (numPools)
   {
      case 1:
         {
            dmdFraction[startDmd] = 1.0;
         }
         break;
      case 2:
         {
            dmdFraction[startDmd] = x;
            dmdFraction[startDmd+1] = 1.0-x;
         }
         break;
      case 3:
         {
            dmdFraction[startDmd] = pow(x, 2);
            dmdFraction[startDmd+1] = 2.0 * x * (1.0-x);
            dmdFraction[startDmd+2] = pow(1.0-x, 2);
         }
         break;
      case 4:
         {
            dmdFraction[startDmd] = pow(x, 3);
            dmdFraction[startDmd+1] = 3.0 * pow(x, 2) * (1.0-x);
            dmdFraction[startDmd+2] = 3.0 * x * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = pow(1.0-x, 3);
         }
         break;
      case 5:
         {
            dmdFraction[startDmd] = pow(x, 4);
            dmdFraction[startDmd+1] = 4.0 * pow(x, 3) * (1.0-x);
            dmdFraction[startDmd+2] = 6.0 * pow(x, 2) * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = 4.0 * x * pow(1.0-x,3);
            dmdFraction[startDmd+4] = pow(1.0-x, 4);
         }
         break;
      default:
         throw std::runtime_error("Too many digestibility classes");

   }
   for (int pool = 0; pool < numPools; pool ++)
   {
      if (dmdFraction[startDmd + pool] < roundingMargin)
      {
         dmdFraction[startDmd+pool+1] += dmdFraction[startDmd+pool];
         dmdFraction[startDmd+pool] = 0.0;
      }
   }
}

//===========================================================================
void HerbageBase::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = cDmdValue[0];
   const float MINDMD = cDmdValue[cNumDmdPools-1];
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdMin < MINDMD - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   if (dmdMax > MAXDMD + errorMargin)
   {  ostringstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   if (dmdMax < dmdMin - errorMargin)
   {  ostringstream msg;
      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Maximum      = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str().c_str());
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMax = dmdClassNum;
      if (fabs(dmdMax - cDmdValue[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }

   for (int dmdClassNum = 0; dmdClassNum < cNumDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMin = dmdClassNum;
      if (fabs(dmdMin - cDmdValue[dmdClassNum]) < errorMargin)
      {
         break;
      }
   }
}

   // REST
float HerbageBase::dmTotal ( void )
{
      return dm.total();
}

float HerbageBase::hHeight ( void )
{
      return height;
}

float HerbageBase::bD ( void )
{
//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
      float bd = divide(dm.total() *kg2g/ha2sm, hHeight()*mm2m, 0.0);
      return bd;
}

float HerbageBase::heightRatio ( int pool )
{
      return  divide(100.0, 0.03*bD(), 0.0);
}

float HerbageBase::dmdValue ( int pool )
{
      return cDmdValue[pool];
}

float HerbageBase::protDg ( int pool )
{
      return cDmdValue[pool] + 0.1;
}

int HerbageBase::numDmdPools ( void )
{
   return cNumDmdPools; // ??
}

void HerbageBase::readParameters ( void )
{

//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - conversion object reading parameters");

    cHerbageModuleName = system->readParameter (section_name, "herbage_module_name");
    cDebug = system->readParameter (section_name, "debug");
    system->readParameter (section_name, "dmdValue", cDmdValue, cNumDmdPools, 0.0, 1.0);

      ostringstream msg;
      msg << "Herbage module name = " << cHerbageModuleName << endl
          << "Debug = " << cDebug << ends;
      system->writeString (msg.str().c_str());
}

//===========================================================================
float HerbageBase::divide (float dividend, float divisor, float default_value)
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



