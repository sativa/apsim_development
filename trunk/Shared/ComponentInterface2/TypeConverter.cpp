#pragma hdrstop

#include "TypeConverter.h"

using namespace std;

// --------------------------------------------------
// conversions from vector<int> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<int>& source, std::string& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<int> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   dest = "";
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      dest += itoa(values[i]);
      }
   }

TypeConverter::TypeConverter(const std::vector<int>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<int> values = source;
   dest.erase(dest.begin(), dest.end());
   if (arraySpecifier) arraySpecifier->processArray(values);
   for (unsigned i = 0; i != values.size(); i++)
      {
      dest.push_back(itoa(values[i]));
      }
   }

// --------------------------------------------------
// conversions from vector<float> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<float>& source, std::string& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<float> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   char st[100];
   dest = "";
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      sprintf(st, "%f", values[i]);
      dest += st;
      }
   }

TypeConverter::TypeConverter(const std::vector<float>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<float> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   char st[100];
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      sprintf(st, "%f", values[i]);
      dest.push_back(st);
      }
   }

// --------------------------------------------------
// conversions from vector<double> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<double>& source, std::string& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<double> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   char st[100];
   dest = "";
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      sprintf(st, "%f", values[i]);
      dest += st;
      }
   }

TypeConverter::TypeConverter(const std::vector<double>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<double> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   char st[100];
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      sprintf(st, "%f", values[i]);
      dest.push_back(st);
      }
   }

// --------------------------------------------------
// conversions from vector<double> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::string& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<std::string> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      dest += values[i];
      }
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<bool>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<std::string> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      dest.push_back(atoi(values[i].c_str()));
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<int>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<std::string> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      dest.push_back(atoi(values[i].c_str()));
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<float>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<std::string> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      dest.push_back(atof(values[i].c_str()));
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<double>& dest, ArraySpecifier* arraySpecifier)
   {
   std::vector<std::string> values = source;
   if (arraySpecifier) arraySpecifier->processArray(values);
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      dest.push_back(atof(values[i].c_str()));
   }

