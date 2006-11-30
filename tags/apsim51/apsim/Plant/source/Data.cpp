#include <stdio.h>
#include <math.h>
#include <values.h>
#include <limits>
#include <vector>
#include <string>
#include <stdexcept>
#include <sstream>
#include <iomanip>
#include <ComponentInterface/Component.h>
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "PlantLibrary.h"
using namespace std;

externalFunction::externalFunction() {};
externalFunction::~externalFunction() {};


void externalFunction::read(protocol::Component *P, const string &section,
                     const char *xname, const char * xunits, float x0, float x1,
                     const char *yname, const char * yunits, float y0, float y1)
      {
      vector<string> t;
      t.push_back(section);
      search(P, t, xname, xunits, x0, x1, yname, yunits, y0, y1);
      }

void externalFunction::search(protocol::Component *P, vector<string> &sections,
                       const char *xname, const char * xunits, float x0, float x1,
                       const char *yname, const char * yunits, float y0, float y1)
      {
      xName = string(xname); yName = string(yname);
      xUnits = string(xunits); yUnits = string(yunits);
      }

void lookupFunction::search(protocol::Component *P, vector<string> &sections,
                            const char *xname, const char *xunits, float x0, float x1,
                            const char *yname, const char *yunits, float y0, float y1)
   {
   externalFunction::search(P, sections, xname, xunits, x0, x1, yname, yunits, y0, y1);
   x.clear();   y.clear();

   P->readParameter(sections,xname, x, x0, x1);
   P->readParameter(sections,yname, y, y0, y1);

   if (x.size() != y.size())
   	throw std::runtime_error(string("Mismatched vector size in ") + xname + " and " + yname);

   if (x.size() <= 0)
   	throw std::runtime_error(string("Zero length vectors in") + xname + " and " + yname);
   }
// Linear Interpolation function setup
void interpolationFunction::search(protocol::Component *P, vector<string> &sections,
                                   const char *xname, const char *xunits, float x0, float x1,
                                   const char *yname, const char *yunits, float y0, float y1)
   {
   externalFunction::search(P, sections, xname, xunits, x0, x1, yname, yunits, y0, y1);
   x.clear();   y.clear();

   P->readParameter(sections,xname, x, x0, x1, true);
   P->readParameter(sections,yname, y, y0, y1, true);
   }

std::string externalFunction::description(void)
   {
   return string("");
   }

std::string lookupFunction::description(void)
   {
   int pad;
   ostringstream text;
   pad = max(0, 27 - (int)xName.size());
   text << "   " << xName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != x.size(); i++) {
   	text << " " << setw(7) << x[i] << setw(-1);
   }
   text << " (" << xUnits << ")\n";

   pad = max(0, 27 - (int)yName.size());
   text << "   " << yName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != y.size(); i++) {
   	text << " " << setw(7) << y[i] << setw(-1);
   }
   text << " (" << yUnits << ")\n";
   return text.str();
   }

std::string interpolationFunction::description(void)
   {
   int pad;
   ostringstream text;
   pad = max(0, 27 - (int)xName.size());
   text << "   " << xName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != x.size(); i++) {
   	text << " " << setw(7) << x[i] << setw(-1);
   }
   text << " (" << xUnits << ")\n";

   pad = max(0, 27 - (int)yName.size());
   text << "   " << yName << setw(pad) << " " << setw(-1) << "=";
   for (unsigned int i = 0; i != y.size(); i++) {
   	text << " " << setw(7) << y[i] << setw(-1);
   }
   text << " (" << yUnits << ")\n";

   return text.str();
   }

// Return a y value from a linear interpolation function
float interpolationFunction::value(float v)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to interpolationFunction:") + xName + " and " + yName);

   if (x.size() != y.size())
       throw std::runtime_error(string("Mismatched vector size in ") + xName + " and " + yName);

   // find which sector of the function that v falls in
   unsigned sector;
   for(sector = 0;sector < x.size();sector++)
      if(v < x[sector] || isEqual(v,x[sector]))break;

   if(sector == 0) return y[0];
   if(sector == x.size())return y[y.size()-1];
   if(isEqual(v,x[sector]))return y[sector]; // prevent roundoff errors
   // y = mx + c
   float slope =  divide(y[sector]-y[sector-1],
                         x[sector]-x[sector-1],
                         y[sector]);

   return y[sector-1] + slope * (v - x[sector - 1]);
   }


// Return an integral for a linear interpolation function
float interpolationFunction::integral(float v1, float v2)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to interpolationFunction:") + xName + " and " + yName);

   if (x.size() != y.size())
       throw std::runtime_error(string("Mismatched vector size in ") + xName + " and " + yName);

   // find which sector of the function that v1 falls in
   unsigned sector1;
   for(sector1 = 0;sector1 < x.size();sector1++)
      if(v1 < x[sector1] || isEqual(v1,x[sector1]))break;
   // find which sector of the function that v2 falls in
   unsigned sector2;
   for(sector2 = 0;sector2 < x.size();sector2++)
      if(v2 < x[sector2] || isEqual(v2,x[sector2]))break;

   float I = 0; // Integral value
   unsigned s;
   for (s = sector1;s <= sector2; s++)
      {
      float xstart;
      if (s>0)
         xstart = max(x[s-1],v1);
      else
         xstart = v1;

      float xend;
      if (s>x.size()-1)
         xend = v2;
      else
         xend = min(x[s],v2);

      I = I + (xend-xstart)*(value(xend)+value(xstart))/2.0;
      }
   return I;
   }


// Return a y value via table lookup
float lookupFunction::value(float v)
   {
   if (x.size() == 0 || y.size() == 0)
       throw std::runtime_error(string("Uninitialised call to lookupFunction:") + xName + " and " + yName);

   // find which sector of the table that v falls in
   unsigned sector;
   for(sector = 0; sector < x.size() && v >= x[sector]; sector++) /*nothing*/ ;

   if(sector == 0) return y[0];
   return y[sector-1];
   }


// XX needs to be renamed to "index_of_real..."
int count_of_real_vals (float *array,  // (INPUT) array to be searched
                        int limit)     // (INPUT) number if elements in array
/* Purpose
*     find the index of the last positive value in a real array,
*     up to a specified limit.
*
*  Definition
*     Returns the index of the last positive element of the
*     "limit" elements of "array".  Returns zero if there are
*     no positive elements in "array".
*/
   {
   // check each layer
   for(int indx = limit - 1; indx >= 0; indx--)
      {
      if (array[indx] > 0.0)
         {
         return (indx);
         }
      }
   return 0;
   }

//=========================================================================
int position_in_real_array(float Number,      //(INPUT) Number to search for
                           float *Array,      //(INPUT) Array to search
                           int Array_size)    //(INPUT) Number of elements in array
//========================================================================
/* Purpose
*       returns the index number of the first occurrence of specified value
*
*  Definition
*     Returns the index of the first element of of the
*     "array_size" elements of "array" that compares equal to
*     "number".  Returns -1 if there are none.
*
*  Mission Statement
*      position of %1 in array %2
*
* Changes
*       070994 jngh specified and programmed
*/
   {
   for(int index = 0; index < Array_size; index++)
      {
      if (reals_are_equal(Number, Array[index]))
         {
         return index;
         }
      }
   return -1;
   }

//===========================================================================
extern "C" float _stdcall _export minval (float *arr, int *size)
//===========================================================================

/*Purpose
 *   Returns the smallest value in the array
 *Definition
 *   Returns the smallest value in the array 'arr'.
 *Parameters
 *   arr:     array of floats
 *   size:    size of array
 */

   {
   //Local Variables
   int i;
   int j;
   float temp;
   //Bubblesort array
    for(i = (*size - 1); i >= 0; i--)
      {
      for(j = 0; j < i; j++)
         {
         if (arr[j] > arr[j + 1])
            {
            temp = arr[j];
            arr[j] = arr[j + 1];
            arr[j + 1] = temp;
            }
         }
      }
    return arr[0];
    }
//===========================================================================
extern "C" float _stdcall _export min_c (float *first, float *second)
//===========================================================================

/*Purpose
 *   Returns the largest value of a pair
 *Parameters
 *   first:     the first number
 *   second:    the first number
 */

   {
   //Implementation section
   if(*first < *second)
      {
      return *first;
      }
   else
      {
      return *second;
      }
   }
//===========================================================================
extern "C" float _stdcall _export mod_c (float *first, float *second)
//===========================================================================

/*Purpose
 *   Returns the value returned by thr Fortram function mod
 *Parameters
 *   first:     the first number
 *   second:    the first number
 */

   {
   //Implementation section
   return (*first - int(*first / *second) * *second);
   }
//===========================================================================
extern "C" float _stdcall _export max_c (float *first, float *second)
//===========================================================================

/*Purpose
 *   Returns the largest value of a pair
 *Parameters
 *   first:     the first number
 *   second:    the first number
 */
   {
   //Implementation section
   if(*first > *second)
      {
      return *first;
      }
   else
      {
      return *second;
      }
   }
//===========================================================================
extern "C" float _stdcall _export dim_c (float *first,  float *second)
//===========================================================================

/*Purpose
 *   Returns the difference of the first number and the second number.
 *Definition
 *    Returns the difference of the first number and the second number.
 *    if the result is negative it returns 0.0
 *Parameters
 *   arr:     array of floats
 *   size:    size of array
 */

   {
   //Local Variables
   float diff;
   //Implementation Section
   diff = *first - *second;
   if (diff <= 0.0)
      {
      return 0.0;
      }
   else
      {
      return diff;
      }
   }

//XX Needs to be replaced with try/catch of EOverflow/EUnderflow etc..    XXXXXXXXXXXXXXXXXXXXXXX
//===========================================================================
double divide (double dividend, double divisor, double default_value)
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
   double LARGEST = 1.0e30;    //largest acceptable no. for quotient
   double SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   double nought = 0.0;

   //Local Varialbes
   double quotient;

   //Implementation
   if(reals_are_equal(divisor, 0.0))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(reals_are_equal(dividend, 0.0))      //multiplying by 0
      {
      quotient = 0.0;
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

//===========================================================================
float l_bound (float var, float lower)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is greater than or equal to
 *   the lower bound, "lower".  Otherwise returns "lower".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *Calls
 *
 */

   {
   if (var < lower) return lower;
   return var;
   }

//===========================================================================
float u_bound (float var, float upper)
//===========================================================================

/*Definition
 *   Returns "var" providing that it is less than or equal to the
 *   upper bound, "upper".  Otherwise returns "upper".
 *Parameters
 *   var:   variable to be constrained
 *   upper: upper limit of variable
 *Calls
 *
 */

   {
   if (var > upper) return upper;
   return var;
   }

//===========================================================================
float sum_real_array (float *var,  // INPUT array to be summed
                      int nelem)   // number of elements
//===========================================================================

/*Definition
 *   Returns sum of all "limit" elements of array "var"[0:limit].
 */

   {
   float total = 0.0; // summary result

   for (int i = 0; i < nelem; i++)
      {
      total = total + var[i];
      }
   return total;
   }

//===========================================================================
void bound_check_real_var (plantInterface *i,
                           float value,       // (IN) Value to be checked
                           float lower,       // (IN) Lower bound
                           float upper,       // (IN) Upper bound
                           const char *vname) // (IN) Name of variable
//===========================================================================

/*Purpose
 *   Checks if a variable lies outside lower and upper bounds.
 *   Reports an err if it does.
 */

   {
   char  msg[80];
   numeric_limits<float> mathInfo;
   float epsilon =  mathInfo.epsilon();

   //are the upper and lower bounds valid?
   if ((lower - epsilon) > (upper + epsilon))
      {
      sprintf(msg,
         "Lower bound (%f) exceeds upper bound (%f)\n        Variable is not checked",
         lower, upper);
      i->warningError (msg);
      }
   //is the value too big?
   else if (value > (upper + epsilon))    //XX wrong. Needs to be relative tolerance.
      {
      sprintf(msg,
         "%s = %f\n        exceeds upper limit of %f",vname,value,upper);
      i->warningError (msg);
      }
   //is the value too small?
   else if (value  < (lower - epsilon))
      {
      sprintf(msg,
         "%s = %f\n        less than lower limit of %f",vname, value, lower);
      i->warningError (msg);
      }
   }

//===========================================================================
void bound_check_integer_var (plantInterface *i,
                              int value, int lower, int upper, const char *vname)
//===========================================================================

/*Purpose
 *   Checks if a variable lies outside lower and upper bounds.
 *   Reports an err if it does.
 *Definition
 *   This subroutine will issue a warning message using the
 *   name of "value", "vname", if "value" is greater than "upper" or
 *   less than "lower".  If  "lower" is greater than "upper", then a
 *   warning message will be flagged to that effect.
 *Parameters
 *   value: value to be validated
 *   lower: lower limit of value
 *   upper: upper limit of value
 *   vname: variable name to be validated
 *Calls
 *   bound_check_real_var
 */

   {
   //Local variables
   float lowerf;   //float version of lower limit
   float upperf;   //float version of upper limit
   float valuef;   //float version of value

   //Implementation
   lowerf = float(lower);
   upperf = float(upper);
   valuef = float(value);

   bound_check_real_var(i, valuef, lowerf, upperf, vname);
   }

//===========================================================================
float sum_part_of_real(float *array,     // array to be summed
                       int start,        // index for starting element
                       int stop,         // index for stopping element
                       int size_of)      //  size of array
//===========================================================================

/*Definition
 *   For the purposes of this function, "array" is treated as a
 *   circular array so that "array"(0) is said to follow
 *   "array"("size_of-1").  This function returns the sum of the
 *   elements of "array" from "array"("start") up to, and
 *   including "array"("stop").
 * NB index origin 0!!
 */

   {
   //Local variables
   int index;     // array index
   float sum;     // sum of array
   //Implementation
   if (start < 0 || start >= size_of) {
      throw std::invalid_argument("Start in sum_part_of_real()");
   }
   if (stop < 0 || stop >= size_of) {
      throw std::invalid_argument("Stop in sum_part_of_real()");
   }

   sum = array[start];
   index = start;

   while(index != stop)
      {
      if(index == size_of)   //circular array
         {
         index = 1;
         }
      else
         {
         index ++;
         }
      sum = sum + array[index];
      }
    return sum;
   }

//===========================================================================
int get_cumulative_index_real(float cum_sum,   //sum_of to be found
                              const float *array,    //array to be searched
                              int size_of)     // size of the array
//===========================================================================

/*Purpose
 *   Find the first element of an array where a given value
 *   is contained with the cumulative sum_of of the elements.
 *   If sum_of is not reached by the end of the array, then it
 *   is ok to set it to the last element. This will take
 *   account of the case of the number of levels being 0.
 *Definition
 *   Returns ndx where ndx is the smallest value in the range
 *   1.."size_of" such that the sum of "array"(j), j=1..ndx is
 *   greater than or equal to "cum_sum".  If there is no such
 *   value of ndx, then "size_of" will be returned.
 */

   {
   //Local variables
   float progressive_sum = 0.0; //cumulative sum_of
   int indx;                    //index count_of_real_vals

   for(indx = 0; indx < size_of; indx++)
      {
      progressive_sum = progressive_sum + array[indx];
      if(progressive_sum >= cum_sum)
         {
         break;
         }
      }
   if (indx==size_of) return (indx - 1); // last element in array
   return indx;                          // index of
   }


//===========================================================================
float bound(float var, float lower, float upper)
//===========================================================================

/*Definition
 *   Returns "lower", if "var" is less than "lower".  Returns "upper"
 *   if "var" is greater than "upper".  Otherwise returns "var".  A
 *   warning error is flagged if "lower" is greater than "upper".
 *Parameters
 *   var:   variable to be constrained
 *   lower: lower limit of variable
 *   upper: upper limit of variable
 *Calls
 *   l_bound
 *   u_bound
 *   warningError
 */

   {
   //Local variables
   float result;           // result for return
   float high;             //temporary variable constrained
                           //to upper limit of variable

   numeric_limits<float> mathInfo;
   float epsilon =  mathInfo.epsilon();

   //check that lower & upper bounds are valid
//   if (lower > upper)
   if ((lower - epsilon) > (upper + epsilon))
      {
      // bounds are invalid
      char msg[80];
      sprintf(msg,
            "Lower bound %f is > upper bound %f\n",
            lower, upper);
      throw std::invalid_argument(msg);
      }
   // constrain variable
   high = u_bound (var, upper);
   result = l_bound (high, lower);
   return result;
   }

//===========================================================================
float sum_between (int start,            //(INPUT)   initial level to begin sum_of
                   int finish,           //(INPUT)   final level to be summed to (but not including)
                   const float *array)   //(INPUT)   array to be summed
//===========================================================================

/*Purpose
 *   Returns sum_of of part of real array from start to finish
 *Definition
 *   Returns sum of array(j), j="start" .. "finish".
 */

   {
   float tot = 0.0;
   int level = start;
   do {
      tot = tot + array[level];
      level++;
      } while (level < finish);
   return tot;
   }


//===========================================================================
void fill_real_array (float *var,  //(OUTPUT) array to set
                      float value, //(IN) scalar value to set array to
                      int limit)   //(IN) number of elements
//===========================================================================

/*Purpose
 *   sets real array var to value up to level limit
 */

   {
   //Implementation Section ----------------------------------

   for (int indx = 0; indx < limit; indx++)
      {
      var[indx] = value;
      }
   }
//===========================================================================
void fill_integer_array (int *var,  //(OUTPUT) array to set
                      int value, //(IN) scalar value to set array to
                      int limit)   //(IN) number of elements
//===========================================================================

/*Purpose
 *   sets int array var to value up to level limit
 */

   {
   //Implementation Section ----------------------------------

   for (int indx = 0; indx < limit; indx++)
      {
      var[indx] = value;
      }
   }

#ifdef ANACHRONISTIC_FORTRAN_MATH
//===========================================================================
double error_margin(double value)
//===========================================================================

/*Definition
 *   Returns a tolerance for the comparison of real numbers
 *   having a magnitude of "value".
 *Parameters
 *   value: value for which margin is required
 *Calls
 *
 */
   {
   //Local Variables
   double margin_value;   //margin for precision err
   numeric_limits<double> doubleInfo;

   //Implementation
   margin_value = fabs(value) * doubleInfo.epsilon();

   if (margin_value == 0)
      {
      margin_value = MINFLOAT;
      }
   return (margin_value * pow(10.0,2));
   }
//---------------------------------------------------------------------------
#endif
// ===========================================================
void add_real_array (const float *amount, float *store, int count) {
// ===========================================================


//+ Sub-Program Arguments
//float amount(*);// (INPUT) amount to be added
//int   dimen;// (INPUT) no. of elements to be added
//float store(*);// (INPUT/OUTPUT) array to be added to

//+ Purpose
//     add contents of each element of an array to each element of another
//     array.

//+  Definition
//     This subroutine adds each of the "dimen" elements of "amount" to its
//     corresponding element of "store".

//+  Mission Statement
//     Add array %1 to %2

//+ Changes
//     270591 specified and programmed jngh

//+ Calls

//- Implementation Section ----------------------------------

   for (int indx = 0; indx < count; indx++) {
       store[indx] = store[indx] + amount[indx];
   }
}

//     ===========================================================
void subtract_real_array (const float *amount,// (INPUT) amount to be removed
                          float *store,// (INPUT/OUTPUT) array to be removed
                          int    dimen)// (INPUT) number of elements to be removed  from
{
//+ Purpose
//     remove contents of each element of an array from each element of
//     another array.

//+  Definition
//     This subroutine subtracts each of the "dimen" elements of
//     "amount" from its corresponding element of "store".

//+  Mission Statement
//     Subtract array %1 from %2

//+ Changes
//     270591 specified and programmed jngh
      for (int indx  = 0; indx < dimen; indx++) {
         store[indx] = store[indx] - amount[indx];
      }
}


// ================================================================
void bound_check_real_array (plantInterface *i,
                             float *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             float  lower_bound,// (INPUT) lower bound of values
                             float  upper_bound,// (INPUT) upper bound of values
                             const char *array_name)// (INPUT) key string of array

{
//+ Purpose
//     check bounds of values in an array

//+  Definition
//     This subroutine will issue a warning message using the
//     name of "array", "name", for each element of "array" that is
//     greater than  ("upper" + 2 * error_margin("upper")) or less
//     than ("lower" - 2 *error_margin("lower")).  If
//     ("lower" - 2 *error_margin("lower")) is greater than "upper",
//     then a warning message will be flagged to that effect "size"
//     times.

//+ Assumptions
//      each element has same bounds.

//+  Mission Statement
//      Check that all %1 lies between %2 and %3

//+ Changes
//     010794 PdeV.      initial coding
//     19/10/94 DPH Changed name from bndchk_array

//+ Calls

  //- Implementation Section ----------------------------------
  for (int indx = 0; indx < array_size; indx++)
     {
      bound_check_real_var (i, array[indx], lower_bound,
                            upper_bound, array_name);
     }
}

// ================================================================
void bound_check_integer_array (plantInterface *i,
                             int *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             int  lower_bound,// (INPUT) lower bound of values
                             int  upper_bound,// (INPUT) upper bound of values
                             const char *array_name)// (INPUT) key string of array

{
//+ Purpose
//     check bounds of values in an array

//+  Definition
//     This subroutine will issue a warning message using the
//     name of "array", "name", for each element of "array" that is
//     greater than  ("upper" + 2 * error_margin("upper")) or less
//     than ("lower" - 2 *error_margin("lower")).  If
//     ("lower" - 2 *error_margin("lower")) is greater than "upper",
//     then a warning message will be flagged to that effect "size"
//     times.

//+ Assumptions
//      each element has same bounds.

//+  Mission Statement
//      Check that all %1 lies between %2 and %3

//+ Changes
//     010794 PdeV.      initial coding
//     19/10/94 DPH Changed name from bndchk_array

//+ Calls

  //- Implementation Section ----------------------------------
  for (int indx = 0; indx < array_size; indx++)
     {
      bound_check_integer_var (i, array[indx], lower_bound,
                               upper_bound, array_name);
     }
}


