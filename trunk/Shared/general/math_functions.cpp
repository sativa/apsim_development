#include <general\math_functions.h>
#include <math.h>

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


