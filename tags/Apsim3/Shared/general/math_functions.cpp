//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

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
//    cycle through a number series from 1 to Max_number.
//    returns the next number in the sequence.
//    eg.  if current_number = 1   and max_number = 4 returns 1
//         if current_number = 2   and max_number = 4 returns 2
//         if current_number = 3   and max_number = 4 returns 3
//         if current_number = 4   and max_number = 4 returns 4
//         if current_number = 5   and max_number = 4 returns 1

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
int Cycle (int Current_number, int Max_number)
   {
   // current_number = 1, max_number = 4, m = 4
   // current_number = 2, max_number = 4, m = 4
   // current_number = 3, max_number = 4, m = 4
   // current_number = 4, max_number = 4, m = 4
   // current_number = 5, max_number = 4, m = 8
   int m = ((Current_number-1) / Max_number + 1) * Max_number;

   return Max_number - (m - Current_number);
   }

// ------------------------------------------------------------------
//  Short description:
//    Calculate regression stats.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
void Calc_regression_stats (double X[], double Y[], int Num_points,
                            Regr_stats& stats)
   {
   float SumX = 0;
   float SumY = 0;
   float SumXY = 0;
   float SumX2 = 0;
   float SumY2 = 0;
//   float SumXYdiff = 0;
   float SumXYdiff2 = 0;
//   float XYdiff = 0;
//   float SumXYDiffPer = 0;
   float CSSX, CSSXY;
   float Xbar, Ybar;
   float TSS, TSSM;
   float REGSS, REGSSM;
   float RESIDSS, RESIDSSM;
   float S2;
//   float MeanAbsError, MeanAbsPerError;

   stats.m = 0.0;
   stats.c = 0.0;
   stats.SEslope = 0.0;
   stats.SEcoeff = 0.0;
   stats.R2 = 0.0;
   stats.ADJR2 = 0.0;
   stats.R2YX = 0.0;
   stats.VarRatio = 0.0;
   stats.RMSD = 0.0;

   if (Num_points > 1)
      {

      for (int Point = 0;
           Point < Num_points;
           Point++)
         {
         SumX = SumX + X[Point];
         SumX2 = SumX2 + X[Point] * X[Point];       // SS for X
         SumY = SumY + Y[Point];
         SumY2 = SumY2 + Y[Point] * Y[Point];       // SS for y
         SumXY = SumXY + X[Point] * Y[Point];       // SS for products
//         XYdiff = fabs(Y[Point] - X[Point]);        // calculate XY diff
//         SumXYdiff = SumXYdiff + XYdiff;
//         SumXYdiff2 = SumXYdiff2 + XYdiff * XYdiff; // SS for XY differences
//         if (Y[Point] != 0.0)
//            SumXYDiffPer = SumXYDiffPer + XYdiff * 100.0 / Y[Point];
                                                    // Sum of XYdiff adj. for Y
//         else
//            SumXYDiffPer = -999;                    // Cannot calculate if Y[i]=0
         }
      Xbar = SumX / Num_points;
      Ybar = SumY / Num_points;

      CSSXY = SumXY - SumX * SumY / Num_points;     // Corrected SS for products
      CSSX = SumX2 - SumX * SumX / Num_points;      // Corrected SS for X
      stats.m = CSSXY / CSSX;                             // Calculate slope
      stats.c = Ybar - stats.m * Xbar;                          // Calculate intercept

      TSS = SumY2 - SumY * SumY / Num_points;       // Corrected SS for Y = Sum((y-ybar)^2)
      TSSM = TSS / (Num_points - 1);                // Total mean SS
      REGSS = stats.m * CSSXY;                            // SS due to regression = Sum((yest-ybar)^2)
      REGSSM = REGSS;                               // Regression mean SS
      RESIDSS = TSS - REGSS;                        // SS about the regression = Sum((y-yest)^2)

      if (Num_points > 2)                           // MUST HAVE MORE THAN TWO POINTS FOR REG
         RESIDSSM = RESIDSS / (Num_points - 2);     // Residual mean SS, variance of residual
      else
         RESIDSSM = 0.0;

      stats.RMSD = sqrt(RESIDSSM);                        // Root mean square deviation
      stats.VarRatio= REGSSM / RESIDSSM;                  // Variance ratio - for F test (1,n-2)
      stats.R2 = 1.0 - (RESIDSS / TSS);                   // Unadjusted R2 calculated from SS
      stats.ADJR2 = 1.0 - (RESIDSSM / TSSM);              // Adjusted R2 calculated from mean SS
      if (stats.ADJR2 < 0.0)
         stats.ADJR2 = 0.0;
      S2 = RESIDSSM;                                // Resid. MSS is estimate of variance
                                                    // about the regression
      stats.SEslope = sqrt(S2) / sqrt(CSSX);              // Standard errors estimated from S2 & CSSX
      stats.SEcoeff = sqrt(S2) * sqrt(SumX2 / (Num_points * CSSX));

      // Statistical parameters of Butler, Mayer and Silburn

      stats.R2YX = 1.0 - (SumXYdiff2 / TSS);              // If you are on the 1:1 line then R2YX=1

      // If R2YX is -ve then the 1:1 line is a worse fit than the line y=ybar

//      MeanAbsError = SumXYdiff / Num_points;
//      MeanAbsPerError = SumXYDiffPer / Num_points;  // very dangerous when y is low
                                                    // could use MeanAbsError over mean
      }
   }

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

//  Notes:

//  Changes:
//    DPH 15/7/98

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
//    eg.  if Value = 0.1   Rounds down to
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

