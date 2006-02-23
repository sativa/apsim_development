//------------------------------------------------------------------------------------------------
#pragma hdrstop

//#include "OOPlantInterface.h"
#include "OOPlantComponents.h"

#include "Utilities.h"

//------------------------------------------------------------------------------------------------

string ftoa(float Float, char *fmtspec)
   {
   char fbuf[80], buf[80];
   sprintf(fbuf, "%%%sf", fmtspec);
   sprintf(buf, fbuf, Float);
   return(string(buf));
   }
//------------------------------------------------------------------------------------------------

//std::string itoa(int value, int width=5);

inline bool char2any(const char *str, int &value)
   {
   return (sscanf(str, "%d", &value) == 1);
   }
inline bool char2any(const char *str, float &value)
   {
   return (sscanf(str, "%f", &value) == 1);
   }
inline bool string2any(const string str, int &value)
   {
   return (sscanf(str.c_str(), "%d", &value) == 1);
   }
inline bool string2any(const string str, float &value)
   {
   return (sscanf(str.c_str(), "%f", &value) == 1);
   }
inline std::string any2string(float value)
   {
   return(ftoa(value,"10.2"));
   }
inline std::string any2string(int value)
   {
   return(itoa(value));
   }

//------------------------------------------------------------------------------------------------

// Conversion from a Julian date to a Gregorian calendar date.
// Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
//    Communications of the ACM, Vol. 11, No. 10 (October, 1968).

//------------------------------------------------------------------------------------------------
void aDate::convertJulian(float jDay)
   {
   julian = int(jDay);
   if (jDay > 0.0) JulianToCalendar(jDay,day,month,year);
   else day = month = year = 0;

   doy = julian - CalendarToJulian(1,1,year) + 1;
   }
//------------------------------------------------------------------------------------------------
void JulianToCalendar(float jDay,int &day,int &month,int &year)
   {
   float work = jDay + 68569.0;
   float work0 = int(4.0 * work / 146097.0);
   work = work - int((146097.0 * work0 + 3.0) / 4.0);
   float yy = int(4000.0 * (work + 1.0) / 1461001.0);

   work = work - int(1461.0 * yy / 4.0) + 31.0;
   float mm = int(80.0 * work / 2447.0);
   float dayd = work - int(2447.0 * mm / 80.0);

   work = int(mm / 11.0);
   float monthd = mm + 2.0 - 12.0 * work;
   float yeard = 100.0 * (work0 - 49.0) + yy + work;

   day = int(dayd + 0.5);
   month = int(monthd + 0.5);
   year = int(yeard + 0.5);
   }
//------------------------------------------------------------------------------------------------

int CalendarToJulian(int day,int month,int year)
   {
   if (year > 1582)
      {
      // Fliegel calculations
      float quotnt = int ((month - 14.0)/12.0);
      return day - 32075.0
         + int(1461.0* (year + 4800.0 + quotnt) /4.0)
         + int(367.0* (month - 2.0 - quotnt*12.0) /12.0)
         - int(3.0 * int((year + 4900.0 + quotnt) /100.0) /4.0);
      }
   else return 0;
   }
//------------------------------------------------------------------------------------------------
//------ sum up a vector of float
//------------------------------------------------------------------------------------------------
float sumVector(vector<float> vec)
   {
   float vecSum = 0.0;
   for(unsigned i=0;i < vec.size();i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
float sumVector(vector<float> vec, int index)
   {
   float vecSum = 0.0;
   for(int i=0;i < index;i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
float sumVector(vector<float> vec, int from, int to)
   {
   float vecSum = 0.0;
   for(int i=from;i < to;i++)vecSum += vec[i];
   return vecSum;
   }
//------------------------------------------------------------------------------------------------
//---  Checks if a variable lies outside lower and upper bounds.
//------------------------------------------------------------------------------------------------
void checkRange(PlantInterface *p,float value, float lower, float upper,string vName)
   {
   char msg[80];
   if(lower > upper)
      {
      sprintf(msg,
         "%s: Lower bound (%f) exceeds upper bound (%f)\n        Variable is not checked",
         vName.c_str(),lower, upper);
      p->warningError(msg);
      }
   else if (value > upper)                   //is the value too big?
      {
      sprintf(msg,
         "%s = %f\n        exceeds upper limit of %f",vName.c_str(),value,upper);
      p->warningError(msg);
      }
   else if (value  < lower)                  //is the value too small?
      {
      sprintf(msg,
         "%s = %f\n        less than lower limit of %f",vName.c_str(), value, lower);
      p->warningError(msg);
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   read from sdml   -----------------------------------
//------------------------------------------------------------------------------------------------
bool readArray(PlantInterface *P, string section,string variable,string units,
                                    vector<string> &values,bool optional)
   {
   values.clear();
   string valueString;
   valueString.clear();
   valueString = P->getProperty(section, variable);
   if (valueString.length() == 0)
      {
      if (!optional)
         {
         string msg = "Cannot find an array\n Parameter name = " + string(variable);
         //P->error(msg.c_str(),fatal);
         throw std::runtime_error(msg.c_str());
         }
      return false;
      }
   Split_string(valueString, " ", values);
   return true;
   }
//------------------------------------------------------------------------------------------------
bool readArray(PlantInterface *P,string section,string variable,string units,
      vector<float> &values,float lower,float upper,bool optional)
   {


   char msg[256];
   std::string valueString = P->readParameter(section, variable);

   vector<std::string> splitValues;
   Split_string(valueString, " ", splitValues);

   int numValues = splitValues.size();
   values.clear();
   float temp;
   for (int v = 0; v < numValues; v++)
      {
      if (string2any(splitValues[v], temp) != 1)
         {
         if (!optional)
            {
            strcpy(msg, "Cannot read a value from string\n"
                        "Parameter name = ");
            strcat(msg, variable.c_str());
            strcat(msg, "\n");
            strcat(msg, "value = '");
            strncat(msg, splitValues[v].c_str(), 20);
            strcat(msg, "'");
            //fatal_error(&err_user, msg, strlen(msg));
            throw std::runtime_error(msg);
            }
         return false;
         }
         else values.push_back(temp);
      }

   for (int v = 0; v < numValues; v++)
      {
      //are the upper and lower bounds valid?
      if (lower > upper)
         {
         strcpy(msg, "Parameter ");
         strcat(msg, variable.c_str());
         strcpy(msg, "\nLower bound (");
         strcat(msg, any2string(lower).c_str());
         strcat(msg, ") exceeds upper bound (");
         strcat(msg, any2string(upper).c_str());
         strcat(msg, "Variable is not checked");
         //warning_error (&err_user, msg);
         P->warningError(msg);
         }
      //is the value too big?
      else if (values[v] > upper)    //XX wrong. Should be relative tolerance.
         {
         strcpy(msg, variable.c_str());
         strcat(msg, " = ");
         strncat(msg, splitValues[v].c_str(), 20);
         strcat(msg, "\n        exceeds upper limit of ");
         strcat(msg, any2string(upper).c_str());
         //warning_error (&err_user, msg);
         P->warningError(msg);
         }
      //is the value too small?
      else if (values[v]  < lower)
         {
         strcpy(msg, variable.c_str());
         strcat(msg, " = ");
         strncat(msg, splitValues[v].c_str(), 20);
         strcat(msg, "\n        is less than lower limit of ");
         strcat(msg, any2string(lower).c_str());
         //warning_error (&err_user, msg);
         P->warningError(msg);
         }
      }
      return true;
   }
//------------------------------------------------------------------------------------------------
bool readArray(PlantInterface *P,vector<string> sections,string variable,
                                                      vector<float> &values)
   {
   char msg[256];
   std::string valueString;
   for (vector<string>::const_iterator i = sections.begin();
                                       i != sections.end();i++)
      {
      valueString = P->readParameter(*i, variable);
      if(!valueString.empty())break;
      }
   if(valueString.empty())
      {
      return false;
      //fatal_error(&err_user, msg, strlen(msg));
      }

   vector<std::string> splitValues;
   Split_string(valueString, " ", splitValues);

   int numValues = splitValues.size();
   values.clear();
   float temp;
   for (int v = 0; v < numValues; v++)
      if (string2any(splitValues[v], temp) == 1)
         values.push_back(temp);

   return true;
   }
//------------------------------------------------------------------------------------------------
bool readArray(PlantInterface *P,vector<string> sections,string variable,
                                                      vector<int> &values)
   {
   char msg[256];
   std::string valueString;
   for (vector<string>::const_iterator i = sections.begin();
                                       i != sections.end();i++)
      {
      valueString = P->readParameter(*i, variable);
      if(!valueString.empty())break;
      }
   if(valueString.empty())
      {
      return false;
      //fatal_error(&err_user, msg, strlen(msg));
      }

   vector<std::string> splitValues;
   Split_string(valueString, " ", splitValues);

   int numValues = splitValues.size();
   values.clear();
   int temp;
   for (int v = 0; v < numValues; v++)
      if (string2any(splitValues[v], temp) == 1)
         values.push_back(temp);

   return true;
   }
//------------------------------------------------------------------------------------------------
bool readVar(PlantInterface *P,string section,string variable,string units,
                                              string &valueString,bool optional)
   {
   valueString = P->readParameter(section,variable);
   if (valueString.length() == 0)
      {
      if (!optional)
         {
         string msg = "Cannot find a variable\n Parameter name = "
                                             + string(variable);
         //P->error(msg.c_str(),fatal);
         throw std::runtime_error(msg.c_str());
         }
      return false;
      }
   return true;
   }
//------------------------------------------------------------------------------------------------
float readVar(PlantInterface *P,vector<string> sections,string variable)
   {
   float temp;
   readVar(P,sections, variable, temp,false);
   return temp;
   }
//------------------------------------------------------------------------------------------------
bool readVar(PlantInterface *P,vector<string> sections,string variable,
                                                  float &value, bool optional)
   {
   value = 0.0;
   string valueString;
   for (vector<string>::const_iterator i = sections.begin();
                                       i != sections.end();i++)
      {
      valueString = P->readParameter(*i, variable);
      if(!valueString.empty())break;
      }
   if(valueString.empty())
      {
      if (!optional)
         {
         string msg = "Cannot find a variable\n Parameter name = "
                                             + string(variable);
         //P->error(msg.c_str(),fatal);
         throw std::runtime_error(msg.c_str());
         }
      return false;
      }
   char2any(valueString.c_str(), value);
   return true;
   }
 /* TODO : Needs looking at! */
//XX Needs to be replaced with try/catch of EOverflow/EUnderflow etc..    XXXXXXXXXXXXXXXXXXXXXXX
//===========================================================================
float divide (float dividend, float divisor, float default_value)
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
   float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   float nought = 0.0;

   //Local Varialbes
   float quotient;

   //Implementation
   if(isEqual(dividend, 0.0))      //multiplying by 0
      {
      quotient = 0.0;
      }
   else if(isEqual(divisor, 0.0))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < 1.0)            //possible overflow
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
   else if(fabs(divisor) > 1.0)             //possible underflow
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
//------------------------------------------------------------------------------------------------
//-------- function to get index number from a vector
// --       index is the number where Sigma(i=0,index){items[i]} > value
//------------------------------------------------------------------------------------------------
int findIndex(float value, vector<float> items)
   {
   unsigned index;
   float accum = 0.0;
   for(index =0;index < items.size();index++)
      {
      accum += items[index];
      if(accum > value)break;
      }
   return index == items.size() ? index - 1 : index;
   }
//------------------------------------------------------------------------------------------------
//-------- function to copy a protocol::vector to a vector
//------------------------------------------------------------------------------------------------
void convertVector(protocol::vector<float> &temp,vector<float> &newVect)
   {
   newVect.clear();
   for(unsigned i=0;i < temp.size();i++)
      {
      newVect.push_back(temp[i]);
      }
   temp.empty();
   }
//------------------------------------------------------------------------------------------------
//-------- function to copy a vector to a vector
//------------------------------------------------------------------------------------------------
void fillVector(vector<float> &temp,vector<float> &newVect)
   {
   newVect.clear();
   for(unsigned i=0;i < temp.size();i++)
      {
      newVect.push_back(temp[i]);
      }
   temp.empty();
   }
//------------------------------------------------------------------------------------------------

float layerProportion(vector<float> dLayer,float rootDepth,int rootLayer)
   {
   float layerTop = sumVector(dLayer, rootLayer);
   float layerBottom = sumVector(dLayer, rootLayer+1);

   return divide(rootDepth - layerTop,layerBottom - layerTop);
   }
//------------------------------------------------------------------------------------------------

float bound(float value,float lower, float upper)
   {
   if(value < lower)return lower;
   else return Min(value,upper);
   }
//------------------------------------------------------------------------------------------------


//------------------------------------------------------------------------------------------------
//----------- Table Function constructor
//------------------------------------------------------------------------------------------------
TableFn::TableFn(PlantInterface *P, vector<string> sections, string xName,string yName)
   {
   readArray(P,sections,xName,x);
   readArray(P,sections,yName,y);
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function read
//------------------------------------------------------------------------------------------------
void TableFn::read(PlantInterface *P, vector<string> sections, string xName,string yName)
   {
   bool ok = readArray(P,sections,xName,x);
   ok = readArray(P,sections,yName,y);
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function constructor
//------------------------------------------------------------------------------------------------
TableFn::TableFn(vector<float> xVec,vector<float> yVec)
   {
   x = xVec;
   y = yVec;
   }
//------------------------------------------------------------------------------------------------
//----------- Table Function load
//------------------------------------------------------------------------------------------------
void TableFn::load(vector<float> xVec,vector<float> yVec)
   {
   x = xVec;
   y = yVec;
   }
//------------------------------------------------------------------------------------------------
//------------ Return a y value from a table function
//------------------------------------------------------------------------------------------------
float TableFn::value(float v)
   {
   // find which sector of the function that v falls in
   unsigned sector;
   if(x.size() == 0)return 0;             // /* TODO :  */fix: needs work so that this is never called
   for(sector = 0;sector < x.size();sector++)
      if(v < x[sector] || isEqual(v,x[sector]))break;

   if(sector == 0)return y[0];
   if(sector == x.size())return y[y.size()-1];
   if(isEqual(v,x[sector]))return y[sector]; // prevent roundoff errors
   // y = mx + c
   float slope = isEqual(x[sector],x[sector-1]) ? 0 :
                        (y[sector]-y[sector-1])/(x[sector]-x[sector-1]);

   return y[sector-1] + slope * (v - x[sector - 1]);
   }

/* TODO : Needs Work */
//==========================================================================
void accumulate (float value,             //  (INPUT) value to add to array
                 vector<float> &array,            //  (INPUT/OUTPUT) array to split
                 float p_index,           //  (INPUT) current p_index no
                 float dlt_index)         //  (INPUT) increment in p_index no
//==========================================================================
/* Purpose
*     Accumulates a value in an array, at the specified index.
*     If the increment in index value changes to a new index, the value
*     is distributed proportionately between the two indices of the array.
*
*  Mission Statement
*      Accumulate %1 (in array %2)
*
* Changes
*       090994 jngh specified and programmed
*       090795 jngh corrected aportioning of value
*       250996 jngh changed so it always adds instead of reset at changeover
*                    to new phase
*                    corrected to take account of special case when p_index
*                    is integral no.
*       210898 igh  added checking to make sure index > 0
*
* Calls
*/
   {
   // Local Variables
   int current_index;           // current index number ()
   float fract_in_old;           // fraction of value in last index
   float index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   float portion_in_new;         // portion of value in next index
   float portion_in_old;         // portion of value in last index

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
      index_devel = p_index - floor(p_index) + dlt_index;
      if (index_devel >= 1.0)
         {
         // now we need to divvy
         new_index = (int) (p_index + Min(1.0, dlt_index));
         if (isEqual(fmod(p_index,1.0),0.0))
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * (value + array[current_index])-
                                 array[current_index];
            }
         else
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         array[current_index] = array[current_index] + portion_in_old;
         array[new_index] = array[new_index] + portion_in_new;
         }
      else
         {
         array[current_index] = array[current_index] + value;
         }
      }
   else
      {
     throw std::runtime_error("accumulate index < 0!!");
      }
   }


/* TODO : Check all this */
float dayLength (int doy, float latitude, float twilight)
   {
   const float  aeqnox = 82.25 ;//  average day number of autumnal equinox
   const float  pi = 3.14159265359 ;
   const float  dg2rdn =  (2.0*pi) /360.0 ; // convert degrees to radians
   const float  decsol = 23.45116 * dg2rdn ; // amplitude of declination of sun
                                            //   - declination of sun at solstices.
   // cm says here that the maximum declination is 23.45116 or 23 degrees 27 minutes.
   // I have seen else_where that it should be 23 degrees 26 minutes 30 seconds - 23.44167
   const float  dy2rdn =  (2.0*pi) /365.25 ; // convert days to radians
   const float  rdn2hr = 24.0/(2.0*pi)  ; // convert radians to hours

   //+ Local Variables
   float alt;    // twilight altitude limited to max/min sun altitudes end of twilight
                  //   - altitude of sun. (radians)
   float altmn;  // altitude of sun at midnight
   float altmx;  // altitude of sun at midday
   float clcd;   // cos of latitude * cos of declination
   float coshra; // cos of hour angle - angle between the sun and the meridian.
   float dec;    // declination of sun in radians - this is the angular distance at solar
                  //   noon between the sun and the equator.
   float hrangl; // hour angle - angle between the sun and the meridian (radians).
   float hrlt;   // day_length in hours
   float latrn;  // latitude in radians
   float slsd;   // sin of latitude * sin of declination
   float sun_alt;// angular distance between sunset and end of twilight - altitude of sun. (radians)
   // Twilight is defined as the interval between sunrise or sunset and the time when the
   // true centre of the sun is 6 degrees below the horizon.
   // Sunrise or sunset is defined as when the true centre of the sun is 50' below the horizon.

   //- Implementation Section ----------------------------------

   sun_alt = twilight * dg2rdn;

   // calculate daylangth in hours by getting the solar declination (radians) from the day of year,
   // then using the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* ((float)doy - aeqnox));

   // get the max and min altitude of sun for today and limit the twilight altitude between these.

   if (isEqual(fabs(latitude), 90.0))
      {
      //coshra = sign (1.0, -dec) * sign (1.0, lat); XXsign???
      }
   else
      {
      latrn = latitude*dg2rdn;
      slsd = sin(latrn)*sin(dec);
      clcd = cos(latrn)*cos(dec);

      altmn = asin(Min(Max(slsd - clcd, -1.0), 1.0));
      altmx = asin(Min(Max(slsd + clcd, -1.0), 1.0));
      alt = Min(Max(sun_alt, altmn), altmx);

      // get cos of the hour angle
      coshra = (sin (alt) - slsd) /clcd;
      coshra = Min(Max(coshra, -1.0), 1.0);
      }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;
   }
//------------------------------------------------------------------------------------------------
//------- Calculate change in %3 based on fractional decay rates.
//------------------------------------------------------------------------------------------------
void calcPoolFractionDelta (int numParts, vector<float> fraction, vector<float> pool,
        vector<float> &dltPool)
   {
   for(int i = 0; i < numParts; i++)
      {
      dltPool[i] = pool[i] * fraction[i];
      }
   }
//------------------------------------------------------------------------------------------------
//------- Calculate change in a particular plant pool
//------------------------------------------------------------------------------------------------
void calcPartFractionDelta (int partNo, vector<float> fraction, float part,
      float &dltPart)
   {
   dltPart = part * fraction[partNo];
   }

//------------------------------------------------------------------------------------------------

void summaryLine(PlantInterface *p,char *format,float arg0, float arg1)
   {
   char line[200];
   sprintf(line,format,arg0,arg1);p->writeString(line);
   }

