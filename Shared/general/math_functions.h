#ifndef Math_functionsH
#define Math_functionsH

#include <vector>
#include <string>
#include <algorith>
#include <functional>
#include <boost\lexical_cast.hpp>
#include <string_functions.h>  // ftoa()

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
template < class c1, class c2 >
c1 multiply (const c1& container1, const c2& container2)
   {
   if (container1.size() != container2.size())
      throw runtime_error("Cannot multiply 2 containers of numbers. Different number of values in each container.");

   c1 return_container;

   c1::const_iterator iterator1 = container1.begin();
   c2::const_iterator iterator2 = container2.begin();
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
template < class c1, class c2 >
c1 divide (const c1& container1, const c2& container2)
   {
   if (container1.size() != container2.size())
      throw runtime_error("Cannot divide 2 containers of numbers. Different number of values in each container.");
   c1 return_container;

   c1::const_iterator iterator1 = container1.begin();
   c2::const_iterator iterator2 = container2.begin();
   while (iterator1 != container1.end() &&
          iterator2 != container2.end())
      {
      if ( (*iterator2) != 0)
         return_container.push_back ( *iterator1 / *iterator2 );
      else
         return_container.push_back (0);
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
container_type multiply_value (const container_type& container1, value_type value)
   {
   container_type values;
   container_type::const_iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      values.push_back(*iterator1 * value);
      ++iterator1;
      }
   return values;
   }

// ------------------------------------------------------------------
//  Short description:
//     container1 /= value

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class container_type, class value_type >
container_type divide_value (const container_type& container1, value_type value)
   {
   container_type values;
   container_type::const_iterator iterator1 = container1.begin();
   while (iterator1 != container1.end())
      {
      values.push_back(*iterator1 / value);
      ++iterator1;
      }
   return values;
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
                           const std::vector<double>& x_cord,
                           const std::vector<double>& y_cord,
                           bool& Did_interpolate);

// ------------------------------------------------------------------
//  Short description:
//       Calculates a set of probability values from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
void Calculate_prob_dist(std::vector<double>& X,
                         bool Prob_exceed,
                         std::vector<double>& Prob_values);

// ------------------------------------------------------------------
//  Short description:
//       Calculates an average

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_mean(std::vector<double>& X);

// ------------------------------------------------------------------
//  Short description:
//       Calculates a percentile from a given set
//       of XValues.

//  Notes:

//  Changes:
//    DPH 16/1/95

// ------------------------------------------------------------------
double Calculate_percentile(std::vector<double>& X,
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
void Calculate_freq_dist(std::vector<double>& Values,
                         std::vector<double>& Start_values,
                         std::vector<double>& End_values,
                         std::vector<std::string>& Frequency_labels,
                         std::vector<double>& Frequency_numbers,
                         int Label_precision);

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
void Round_using_magnitude (double& Value, bool Round_up);

// ------------------------------------------------------------------
//  Short description:
//    round a number up or down to nearest specified number

//  Notes:
//    eg.  if Value = 369 and Nearest = 200 Rounds down to 200 Rounds up to 400

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
void Round_to_nearest (double& Value, double Nearest, bool Round_up);

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
template <class container1, class container2>
void StringContainerToDoubleContainer (const container1& StringContainer,
                                       container2& DoubleContainer)
   {
   DoubleContainer.erase(DoubleContainer.begin(), DoubleContainer.end());
   for (container1::const_iterator i = StringContainer.begin();
                                   i != StringContainer.end();
                                   i++)
      DoubleContainer.push_back ( atof( (*i).c_str() ));
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
template <class container1, class container2>
void StringContainerToIntegerContainer (const container1& StringContainer,
                                        container2& IntegerContainer)
   {
   IntegerContainer.erase(IntegerContainer.begin(), IntegerContainer.end());
   for (container1::const_iterator i = StringContainer.begin();
                                   i != StringContainer.end();
                                   i++)
      IntegerContainer.push_back ( atoi( (*i).c_str() ));
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a container of doubles to a container of strings.

//  Changes:
//    DPH 6/8/98

// ------------------------------------------------------------------
template <class container1, class container2>
void DoubleContainerToStringContainer (const container1& DoubleContainer,
                                       container2& StringContainer)
   {
   StringContainer.erase(StringContainer.begin(), StringContainer.end());
   for (container1::const_iterator i = DoubleContainer.begin();
                                   i != DoubleContainer.end();
                                   i++)
      StringContainer.push_back (ftoa(*i, 3));
   }
// ------------------------------------------------------------------
//  Short description:
//    convert a container of integers to a container of strings.

//  Changes:
//    DPH 15/8/2001
// ------------------------------------------------------------------
template <class container1, class container2>
void IntegerContainerToStringContainer (const container1& IntegerContainer,
                                        container2& StringContainer)
   {
   StringContainer.erase(StringContainer.begin(), StringContainer.end());
   for (container1::const_iterator i = IntegerContainer.begin();
                                   i != IntegerContainer.end();
                                   i++)
      {
      char buffer[100];
      itoa(*i, buffer, 10);
      StringContainer.push_back(buffer);
      }
   }

typedef std::vector<double> OptimParams;
typedef double __fastcall (__closure *TOptimEvent)(const OptimParams& params);

//---------------------------------------------------------------------------
// Return true if value is missing.
//---------------------------------------------------------------------------
template <class T>
T missingValue()
   {
   return boost::lexical_cast<T>(999999);
   }
//---------------------------------------------------------------------------
// Return true if value is missing.
//---------------------------------------------------------------------------
template <class T>
bool isMissingValue(const T& value)
   {
   return (boost::lexical_cast<T>(value) == missingValue<T>());
   }


#endif


