//---------------------------------------------------------------------------

#ifndef ArraySpecifierH
#define ArraySpecifierH

#include "Type.h"
#include "Protocolvector.h"
namespace protocol {

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

//---------------------------------------------------------------------------
// This class encapsulates the array indexing and summing support in all
// APSIM components.  It allows:
//    name() - return sum of array
//    name(n) - return nth element of array
//    name(2-5) - return sum of elements 2 to 5 of array.
//    name(2:4) - return elements 2 to 4 of array.
//---------------------------------------------------------------------------
class ArraySpecifier
   {
   public:
      ArraySpecifier(unsigned num);
      ArraySpecifier(unsigned number1, unsigned number2, char separator);

      //---------------------------------------------------------------------------
      // Calculate the required stats (eg. sum array, return specific elements)
      // and return in same array as passed in.  Returns true if all ok.
      //---------------------------------------------------------------------------
      template <class T>
      bool calcStats(T& obj)
         {
         return false;
         }
      bool calcStats(vector<double>& v)
         {
         bool ok = convertArray(v);
         if (!ok)
            v.empty();
         return ok;
         }
      bool calcStats(vector<float>& v)
         {
         bool ok = convertArray(v);
         if (!ok)
            v.empty();
         return ok;
         }
      bool calcStats(vector<int>& v)
         {
         bool ok = convertArray(v);
         if (!ok)
            v.empty();
         return ok;
         }
      #ifdef MessageDataExt
      bool calcStats(std::vector<std::string>& v)
         {
         vector<double> values;
         for (unsigned i = 0; i != v.size(); ++i)
            values.push_back(atof(v[i].c_str()));

         bool ok = convertArray(values);
         if (!ok)
            v.empty();
         else
            {
            v.erase(v.begin(), v.end());
            static char arraySpecBuffer[100];
            for (unsigned i = 0; i != values.size(); ++i)
               {
               sprintf(arraySpecBuffer, "%10.3f", values[i]);
               v.push_back(arraySpecBuffer);
               }
            }
         return ok;
         }
      #endif

      //---------------------------------------------------------------------------
      // Create an array specifier if necessary.  The caller assumes ownership
      // of the returned arraySpecifier and should delete it accordingly.
      // NB This routine will also change name and registeredType if it creates
      // an array specifier.  Name will have the array specification eg. (2-5)
      // removed and registeredType will become and array e.g. array="T"
      //---------------------------------------------------------------------------
      static ArraySpecifier* ArraySpecifier::create(char* name, char* type);

   private:
      unsigned number1;
      unsigned number2;
      bool doSum;

      //---------------------------------------------------------------------------
      // Convert the specified array of numbers to the required element(s).
      // returns true if all ok.
      //---------------------------------------------------------------------------
      template <class T>
      bool ArraySpecifier::convertArray(vector<T>& values)
         {
         unsigned start = number1;
         unsigned end = number2;
         if (doSum)
            {
            if (start == 0)
               end = values.size() - 1;
            T sum = 0;
            for (unsigned i = start; i <= end; ++i)
               sum += values[i];
            values.empty();
            values.push_back(sum);
            }
         else
            {
            unsigned index = 0;
            for (unsigned i = start; i <= end; ++i)
               values[index++] = values[i];

            while (values.size() > end-start+1)
               values.erase(values.size()-1);
            }
         return true;
         }

   };



} // namespace protocol

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
