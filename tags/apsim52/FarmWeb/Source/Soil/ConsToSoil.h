//---------------------------------------------------------------------------
#include <soil\soil.h>
#include <ApsimShared\apsimControlFile.h>
#include <vector>
#include <fstream>
#include <general\string_functions.h>
#include <stdio.h>
#include <boost\lexical_cast.hpp>

#ifndef ConsToSoilH
#define ConsToSoilH
#include <soil\soils.h>
class __declspec(dllexport) ConsToSoil
   {
   private:
      void stringToInts(string& s
                       ,const char* separators
                       , vector<unsigned int>& d);
      void stringToDoubles(string& s
                          ,const char* separators
                          , vector<double>& d);
      void convertParameter(Soil& XMLFile
                           ,ApsimControlFile& conFile
                           ,const char* moduleName
                           , const char* parameterName
                           , void* setfunc(double));
      vector <string> missingParameters;
      void importSection(IniFile& par, string& sectionName, Soils& soils);

      bool getParameterValue(IniFile& par,
                             const string& sectionName,
                             const string& parameterName,
                             string& value);
      template <class T>
      bool getParameterValue(IniFile& par,
                             const string& sectionName,
                             const string& parameterName,
                             T& value)
         {
         string st;
         if (getParameterValue(par, sectionName, parameterName, st))
            {
            try
               {
               value = boost::lexical_cast<T>(st);
               return true;
               }
            catch (const exception& err)
               {
               missingParameters.push_back("Invalid value: " + st +
                                           " for parameter: " + parameterName +
                                           " in section: " + sectionName);
               return false;
               }
            }
         return false;
         }

      bool getParameterValues(IniFile& par,
                              const string& sectionName,
                              const string& parameterName,
                              vector<string>& values);
      template <class T>
      bool getParameterValues(IniFile& par,
                              const string& sectionName,
                              const string& parameterName,
                              vector<T>& values)
         {
         vector<string> stringValues;
         values.erase(values.begin(), values.end());
         if (getParameterValues(par, sectionName, parameterName, stringValues))
            {
            for (unsigned i = 0; i != stringValues.size(); i++)
               {
               try
                  {
                  T value = boost::lexical_cast<T>(stringValues[i]);
                  values.push_back(value);
                  }
               catch (const exception& err)
                  {
                  missingParameters.push_back("Invalid value: " + stringValues[i] +
                                              " for parameter: " + parameterName +
                                              " in section: " + sectionName);
                  return false;
                  }
               }
            return true;
            }
         return false;
         }
      void displayMessages(string messages);

   public:
      void importFromFile(string& conpath, Soils& soils);
   };
//---------------------------------------------------------------------------
#endif
