//---------------------------------------------------------------------------
#include <stdlib.h>
#include "math_functions.h"
#include <math.h>
#include <assert.h>
#include <numeric>
#include <values.h>
#include <strstream>
#include <iomanip>
using namespace std;

// ------------------------------------------------------------------
//  Short description:
//       Linearly interpolates a value y for a given value x and a given
//       set of xy co-ordinates.
//       When x lies outside the x range_of, y is set to the boundary condition.
//       Returns true for Did_interpolate if interpolation was necessary.

//  Notes:
//       XY pairs are ordered by x in ascending order.

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double linear_interp_real (double x,
                           const vector<double>& x_cord,
                           const vector<double>& y_cord,
                           bool& Did_interpolate)
   {
   // find where x lies in the x cord

   if (x_cord.size() == 0 || y_cord.size() == 0 || x_cord.size() != y_cord.size())
       throw std::runtime_error("Sizes are wrong in linear_interp_real.");
   for (unsigned int indx = 0; indx < x_cord.size(); indx++)
      {
      if (x <= x_cord[indx])
         {
         // check to see if x is exactly equal to x_cord(indx).
         // if so then dont calculate y.  This was added to
         // remove roundoff error.  (DPH)

         if (x == x_cord[indx])
            {
            Did_interpolate = false;
            return y_cord[indx];
            }

         // found position
         if (indx == 0)
            {
            Did_interpolate = true;
            return y_cord[indx];
            }

         else
            {
            // interpolate - y = mx+c
            if (x_cord[indx] - x_cord[indx-1] == 0)
               {
               Did_interpolate = true;
               return y_cord[indx-1];
               }

            else
               {
               Did_interpolate = true;
               return ( (y_cord[indx] - y_cord[indx-1]) /
                        (x_cord[indx] - x_cord[indx-1])
                       * (x - x_cord[indx-1])
                       + y_cord[indx-1]);
               }
            }
         }
      else if (indx == x_cord.size()-1)
         {
         Did_interpolate = true;
         return y_cord[indx];
         }
      }
//   assert (false);
   return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates a set of probability values from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
void Calculate_prob_dist(vector<double>& X,
                         bool Prob_exceed,
                         vector<double>& Prob_values)
   {
   // sort our values.
   std::sort(X.begin(), X.end());

   // create another vector for interpolation.
   for (unsigned int x = 1; x <= X.size(); x++)
      Prob_values.push_back( (x-0.5)/X.size()*100 );

   if (Prob_exceed)
      std::reverse(Prob_values.begin(), Prob_values.end());
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates an average

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_mean(vector<double>& X)
   {
   if (X.size() > 0)
      return std::accumulate (X.begin(), X.end(), 0.0) / X.size();
   else
      return 0.0;
   }

// ------------------------------------------------------------------
//  Short description:
//       Calculates a percentile from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_percentile(vector<double>& X,
                            bool Prob_exceed,
                            int Percentile)
   {
   Percentile = min(Percentile, 100);
   Percentile = max(Percentile, 0);

   // if user wants a prob exceedence then we need to flip the percentile around.
   if (Prob_exceed)
      Percentile = 50 - (Percentile - 50); 

   vector<double> Prob_values;
   Calculate_prob_dist(X, false, Prob_values);

   bool Did_interpolate;
   return linear_interp_real (Percentile, Prob_values, X, Did_interpolate);
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate a frequency distribution from the specified values.
//      Return the frequency labels and numebrs.

//  Changes:
//    DPH 15/7/98

//  Usage(JW 13/8/04):
//  Returns occurance count of elements in Values in the range of
//  [Start_values, End_values].
//
//  Input:  Values, Start_values, End_values, Label_precision
//  Output: Frequency_labels, Frequency_numbers
//
//  Example:
//  Calculate_freq_dist( [1.5 1.6 3 7],  /*Values*/
//                                   1,  /*Start_values*/
//                                   2,  /*End_values*/
//                                  fl,  /*Frequency_labels*/
//                                  fn,  /*Frequency_numbers*/
//                                   2   /*Label_precision*/
//  will result in output:
//         fl = "(1.00 to 2.00)"
//         fn = 2
//

// ------------------------------------------------------------------
void Calculate_freq_dist(vector<double>& Values,
                         vector<double>& Start_values,
                         vector<double>& End_values,
                         vector<string>& Frequency_labels,
                         vector<double>& Frequency_numbers,
                         int Label_precision)
   {
   Frequency_labels.erase (Frequency_labels.begin(), Frequency_labels.end());
   Frequency_numbers.erase (Frequency_numbers.begin(), Frequency_numbers.end());

   // loop through all frequency intervals.
   for (unsigned int interval = 0; interval < Start_values.size(); interval++)
      {
      double Start_of_interval = Start_values[interval];
      double End_of_interval = End_values[interval]; 

      range<double> Range(Start_of_interval, End_of_interval);
      int count = 0;
      std::count_if (Values.begin(), Values.end(), Range, count);
      Frequency_numbers.push_back (count);

      // create a label for this interval.
      ostrstream out;
      out.setf(std::ios::fixed, std::ios::floatfield);
      out << '(';
      out << std::setprecision(Label_precision) << Start_of_interval << " to " << std::setprecision(Label_precision) << End_of_interval;
      out << ')' << std::ends;
      Frequency_labels.push_back (out.str());
      delete out.str();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    round a number up or down to nearest number depending on the
//    magnitude of the number passed in.

//  Notes:
//    eg.  if Value = 0.1   Rounds down to  0 Rounds up to 1
//    eg.  if Value = 369   Rounds down to 300 Rounds up to 400
//    eg.  if Value = 1234  Rounds down to 1000 Rounds up to 2000
//    eg.  if Value = 12345 Rounds down to 10000 Rounds up to 20000


//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
void Round_using_magnitude (double& Value, bool Round_up)
   {
   if (fabs(Value) > 0)
      {
      int Magnitude = log10(fabs(Value));
      int Nearest = pow(10, Magnitude);
      Round_to_nearest (Value, Nearest, Round_up);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    round a number up or down to nearest specified number

//  Notes:
//    eg.  if Value = 369 and Nearest = 200 Rounds down to 200 Rounds up to 400

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
void Round_to_nearest (double& Value, double Nearest, bool Round_up)
   {
   Value = floor(Value / Nearest);
   if (Round_up)
      Value++;
   Value *= Nearest;
   }
