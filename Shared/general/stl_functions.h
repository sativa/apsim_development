//---------------------------------------------------------------------------
#ifndef STL functionsH
#define STL functionsH

#include <functional>
#include <general\string_functions.h>
// ------------------------------------------------------------------
//  Short description:
//    locates an object in a container and returns the numerical
//    location within that container.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class InputIterator, class T>
int location_number (InputIterator first, InputIterator last, const T& value)
   {
   int index = 0;
   while (first != last && value != *first)
      {
      ++first;
      index++;
      }
   if (first == last)
      return -1;
   else
      return index;
   }


// ------------------------------------------------------------------
//  Short description:
//    Find an item in stl container.  This function is a replacement for the
//    find function in algorithm.h.  The released function cannot handle
//    containers of pointers properly.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class InputIterator, class T>
InputIterator Pfind (InputIterator first, InputIterator last, const T& value)
{
    while (first != last && *(*first) != *value)
        ++first;
    return first;
}

// ------------------------------------------------------------------
//  Short description:
//    Destroys all elements in a container.  Works with
//    containers of pointers.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT, class T>
void Destroy_pointers (CT& container, const T )
   {
   while (!container.empty())
      {
		T Ptr = container.back();
      container.pop_back();
		delete Ptr;
		}
   }

template <class CT, class T>
class string2double_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      string2double_and_store (CT& c) : container(c) {}
      void operator() (T& arg)
         {container.push_back (atof(arg.c_str()));}
   };

template <class CT, class T>
class double2string_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      double2string_and_store (CT& c) : container(c) {}
      void operator() (T& arg)
         {container.push_back (ftoa(arg, 5));}
   };

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void String2double (CT1& source, CT2& dest)
   {
   dest.erase(dest.begin(), dest.end());
   string2double_and_store<CT2, string> convert(dest);
   std::for_each(source.begin(), source.end(), convert);
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void Double2string (CT1& source, CT2& dest)
   {
   dest.erase(dest.begin(), dest.end());
   double2string_and_store<CT2, double> convert(dest);
   std::for_each(source.begin(), source.end(), convert);
   }

#endif
