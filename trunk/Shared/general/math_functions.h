#if !defined (MATH_FUNCTIONS_H)
#define MATH_FUNCTIONS_H

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
int Cycle (int Current_number, int Max_number);


struct Regr_stats
   {
   float m;
   float c;
   float SEslope;
   float SEcoeff;
   float R2;
   float ADJR2;
   float R2YX;
   float VarRatio;
   float RMSD;
   };

// ------------------------------------------------------------------
//  Short description:
//     calculate a regression from the specified data.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Calc_regression_stats (double X[], double Y[], int Num_points,
                            Regr_stats& stats);

#endif


