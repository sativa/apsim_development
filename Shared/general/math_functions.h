#if !defined (MATH_FUNCTIONS_H)
#define MATH_FUNCTIONS_H

#include <general\myvector.h>
#include <algorith>
#include <functional>
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

// ------------------------------------------------------------------
//  Short description:
//     function that takes 2 containers of numbers and multiplies
//     both together and then returns the sum.

//  Notes:
//     eg:   x1*y1 + x2*y2 + x3*y3 ...

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container >
double multiply_accumulate (container& container1, container& container2)
   {
   double Total = 0;

   container::iterator iterator1 = container1.begin();
   container::iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      Total += *iterator1 * *iterator2;
      iterator1++;
      iterator2++;
      }

   return Total;
   }

// ------------------------------------------------------------------
//  Short description:
//     container3 = container1 + container2

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container >
container add (container& container1, container& container2)
   {
   container return_container;

   container::iterator iterator1 = container1.begin();
   container::iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      return_container.push_back ( *iterator1 + *iterator2 );
      iterator1++;
      iterator2++;
      }
   return return_container;
   }

// ------------------------------------------------------------------
//  Short description:
//     container3 = container1 - container2

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container >
container subtract (container& container1, container& container2)
   {
   container return_container;

   container::iterator iterator1 = container1.begin();
   container::iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      return_container.push_back ( *iterator1 - *iterator2 );
      iterator1++;
      iterator2++;
      }
   return return_container;
   }

// ------------------------------------------------------------------
//  Short description:
//     container3 = container1 * container2

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container >
container multiply (container& container1, container& container2)
   {
   container return_container;

   container::iterator iterator1 = container1.begin();
   container::iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      return_container.push_back ( *iterator1 * *iterator2 );
      iterator1++;
      iterator2++;
      }
   return return_container;
   }

// ------------------------------------------------------------------
//  Short description:
//     container3 = container1 / container2

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container >
container devide (container& container1, container& container2)
   {
   container return_container;

   container::iterator iterator1 = container1.begin();
   container::iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      return_container.push_back ( *iterator1 / *iterator2 );
      iterator1++;
      iterator2++;
      }
   return return_container;
   }

// ------------------------------------------------------------------
//  Short description:
//     container1 += value

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type, class value_type >
void add_value (container_type& container1, value_type value)
   {
   container_type::iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      *iterator1 += value;
      ++iterator1;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     container1 -= value

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type, class value_type >
void subtract_value (container_type& container1, value_type value)
   {
   container_type::iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      *iterator1 -= value;
      ++iterator1;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     container1 *= value

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type, class value_type >
void multiply_value (container_type& container1, value_type value)
   {
   container_type::iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      *iterator1 *= value;
      ++iterator1;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     container1 /= value

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type, class value_type >
void devide_value (container_type& container1, value_type value)
   {
   container_type::iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      *iterator1 /= value;
      ++iterator1;
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
                           vector<double>& x_cord,
                           vector<double>& y_cord,
                           bool& Did_interpolate);

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
                         vector<double>& Prob_values);

// ------------------------------------------------------------------
//  Short description:
//       Calculates an average

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_mean(vector<double>& X);

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
                            int Percentile);


// ------------------------------------------------------------------
//  Short description:
//     STL predicate to return true if value is in a specified range.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
template <class T>
struct range : std::unary_function<T, bool>
   {
   double start, end;
   range (double Start, double End) {start = Start; end = End;}
   bool operator() (const T& x) const
     { return (x >= start && x < end); }
   };

// ------------------------------------------------------------------
//  Short description:
//      calculate a frequency distribution from the specified values.
//      Return the frequency labels and numebrs.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void Calculate_freq_dist(vector<double>& Values,
                         double Starting_frequency,
                         double Ending_frequency,
                         double Interval_size,
                         vector<string>& Frequency_labels,
                         vector<double>& Frequency_numbers);

#endif


