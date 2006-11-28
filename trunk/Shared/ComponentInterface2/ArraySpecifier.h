//---------------------------------------------------------------------------
#ifndef ArraySpecifierH
#define ArraySpecifierH

#include <string>
#include <vector>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl
//---------------------------------------------------------------------------
// This class encapsulates the array indexing and summing support in all
// APSIM components.  Array spec can be:
//    name() - return sum of array (for backwards compatibility)
//    sum(name) - return sum of array
//    name(n) - return nth element of array
//    name(2-5) - return elements 2 to 5 of array.
//    name(2:4) - return elements 2 to 4 of array.
//    sum(name(2-5)) - sum elements 2 to 5.
//---------------------------------------------------------------------------
class ArraySpecifier
   {
   public:
      //---------------------------------------------------------------------------
      // Create an array specifier if necessary.  The caller assumes ownership
      // of the returned arraySpecifier and should delete it accordingly.
      // Returns NULL if no array specified found.
      //---------------------------------------------------------------------------
      static ArraySpecifier* create(const std::string& name);

      //----------------------------------------------------------------------
      // Process the specified array and get rid of the values that don't
      // match the array specified passed into the constructor.
      //----------------------------------------------------------------------
      template <class T>
      void processArray(std::vector<T>& values)
         {
         unsigned start = startIndex;
         unsigned end = endIndex;
         if (start == -1)
            start = 0;
         else
            start--;   // 1 based to zero based indexing
         if (end == -1)
            end = values.size()-1;
         else
            end--;     // 1 base to zero based indexing.


         unsigned index = 0;
         for (unsigned i = start; i <= end; i++)
            {
            if (i >= values.size())
               throw std::runtime_error("Invalid array index.");
            values[index++] = values[i];
            }

         values.erase(values.begin()+index, values.end());

         if (functionName == "sum")
            {
            T sum = 0;
            for (unsigned i = 0; i != values.size(); i++)
               sum += values[i];
            values.erase(values.begin(), values.end());
            values.push_back(sum);
            }
         }

      std::string variableName() {return varName;}
      void adornVariableName(std::string& varName);

   private:
      unsigned startIndex;
      unsigned endIndex;
      std::string functionName;
      std::string varName;
      bool oldStyleSum;

      void parseFunctionName(const std::string& functionName);
   };




// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
