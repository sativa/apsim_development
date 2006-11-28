#ifndef TypeConverterH
#define TypeConverterH
#include <vector>
#include <string>
#include <stdexcept>
#include <stdio>
#include <stdlib>
#include <general/string_functions.h>
#include <ComponentInterface2/ArraySpecifier.h>

class TypeConverter
   {
   private:
      template <class T>
      void processArraySpec(ArraySpecifier& arraySpec, std::vector<T>& values)
         {
         if (arraySpec.length() != 0)
            {
            ArraySpecifier arrayS(arraySpecifier);
            arrayS.processArray(values);
            }
         };

   public:
      // ------------------------------------------------
      // conversions from bool to other data type.
      // ------------------------------------------------
      TypeConverter(bool source, bool& dest)
         {dest = source;}
      TypeConverter(bool source, int& dest)
         {dest = source;}
      TypeConverter(bool source, float& dest)
         {dest = source;}
      TypeConverter(bool source, double& dest)
         {dest = source;}
      TypeConverter(bool source, std::string& dest)
         {dest = itoa(source);}
      TypeConverter(bool source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(itoa(source));
         }

      // ------------------------------------------------
      // conversions from int to other data type.
      // ------------------------------------------------
      TypeConverter(int source, bool& dest)
         {dest = source;}
      TypeConverter(int source, int& dest)
         {dest = source;}
      TypeConverter(int source, float& dest)
         {dest = source;}
      TypeConverter(int source, double& dest)
         {dest = source;}
      TypeConverter(int source, std::string& dest)
         {dest = itoa(source);}
      TypeConverter(int source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(itoa(source));
         }

      // ------------------------------------------------
      // conversions from float to other data type.
      // ------------------------------------------------
      TypeConverter(float source, bool& dest)
         {dest = source;}
      TypeConverter(float source, int& dest)
         {dest = source;}
      TypeConverter(float source, float& dest)
         {dest = source;}
      TypeConverter(float source, double& dest)
         {dest = source;}
      TypeConverter(float source, std::string& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest = st;
         }
      TypeConverter(float source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<std::string>& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest.erase(dest.begin(), dest.end());
         dest.push_back(st);
         }

      // ------------------------------------------------
      // conversions from double to other data type.
      // ------------------------------------------------
      TypeConverter(double source, bool& dest)
         {dest = source;}
      TypeConverter(double source, int& dest)
         {dest = source;}
      TypeConverter(double source, float& dest)
         {dest = source;}
      TypeConverter(double source, double& dest)
         {dest = source;}
      TypeConverter(double source, std::string& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest = st;
         }
      TypeConverter(double source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(double source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(double source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(double source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(double source, std::vector<std::string>& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest.erase(dest.begin(), dest.end());
         dest.push_back(st);
         }

      // ------------------------------------------------
      // conversions from string to other data type.
      // ------------------------------------------------
      TypeConverter(const std::string& source, bool& dest)
         {dest = atoi(source.c_str());}
      TypeConverter(const std::string& source, int& dest)
         {dest = atoi(source.c_str());}
      TypeConverter(const std::string& source, float& dest)
         {dest = atof(source.c_str());}
      TypeConverter(const std::string& source, double& dest)
         {dest = atof(source.c_str());}
      TypeConverter(const std::string& source, std::string& dest)
         {dest = source;}
      TypeConverter(const std::string& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(atoi(source.c_str()));
         }
      TypeConverter(const std::string& source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(atoi(source.c_str()));
         }
      TypeConverter(const std::string& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(atof(source.c_str()));
         }
      TypeConverter(const std::string& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(atof(source.c_str()));
         }
      TypeConverter(const std::string& source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }

      // ------------------------------------------------
      // conversions from vector<int> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<int>& source, bool& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<int> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to boolean");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, int& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<int> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to integer");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, float& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<int> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to single");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, double& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<int> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, std::string& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<int>& source, std::vector<bool>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<int>& source, std::vector<int>& dest, ArraySpecifier* arraySpecifier)
         {
         dest = source;
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<int>& source, std::vector<float>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<int>& source, std::vector<double>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<int>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier);


      // ------------------------------------------------
      // conversions from vector<float> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<float>& source, bool dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<float> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to boolean");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, int& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<float> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to integer");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, float& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<float> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to single");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, double& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<float> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, std::string& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<float>& source, std::vector<bool>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<float>& source, std::vector<int>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<float>& source, std::vector<float>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<float>& source, std::vector<double>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<float>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier);

      // ------------------------------------------------
      // conversions from vector<double> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<double>& source, bool& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<double> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to boolean");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<double>& source, int& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<double> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to integer");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<double>& source, float& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<double> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to single");
         else
            dest = values[0];
         }

      TypeConverter(const std::vector<double>& source, double& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<double> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<double>& source, std::string& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<double>& source, std::vector<bool>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<double>& source, std::vector<int>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<double>& source, std::vector<float>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<double>& source, std::vector<double>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }
      TypeConverter(const std::vector<double>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier);

      // ------------------------------------------------
      // conversions from vector<string> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<std::string>& source, bool& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<std::string> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to boolean");
         else
            dest = atoi(values[0].c_str());
         }
      TypeConverter(const std::vector<std::string>& source, int& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<std::string> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to integer");
         else
            dest = atoi(values[0].c_str());
         }
      TypeConverter(const std::vector<std::string>& source, float& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<std::string> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to single");
         else
            dest = atof(values[0].c_str());
         }
      TypeConverter(const std::vector<std::string>& source, double& dest, ArraySpecifier* arraySpecifier)
         {
         std::vector<std::string> values = source;
         if (arraySpecifier) arraySpecifier->processArray(values);
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to double");
         else
            dest = atof(values[0].c_str());
         }
      TypeConverter(const std::vector<std::string>& source, std::string& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<std::string>& source, std::vector<bool>& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<std::string>& source, std::vector<int>& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<std::string>& source, std::vector<float>& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<std::string>& source, std::vector<double>& dest, ArraySpecifier* arraySpecifier);
      TypeConverter(const std::vector<std::string>& source, std::vector<std::string>& dest, ArraySpecifier* arraySpecifier)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         if (arraySpecifier) arraySpecifier->processArray(dest);
         }

   };

#endif
