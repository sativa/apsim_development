//---------------------------------------------------------------------------
#pragma hdrstop

#include "ScienceAPI.h"
#include "Component.h"
#include "DataTypes.h"
#include "RegistrationType.h"

#include <general/string_functions.h>

template <class T>
bool StringConverter(protocol::Component* component, const string& name,
                     const string& from, T& to, double lower, double upper)
   {
   // 1. Convert it
   try {to = boost::lexical_cast<T> (from); }
   catch(boost::bad_lexical_cast &e)
      {
      string msg = string("Problem converting variable to ") +
                   typeid(T).name() + " type.\n"
                   "Parameter name = " + name + "\n"
                   "Value          = '" + from + "'";
      throw runtime_error(msg);
      }

   // 2. Check bounds
   if (to < lower || to > upper)
      {
      string msg = "Bound check warning while reading parameter.\n"
                   "Variable  : " + name + "\n"
                   "Condition : " + ftoa(lower, 2) + " <= " +
                   from + " <= " + ftoa(upper, 2);
      component->error(msg.c_str(), false);
      return false;
      }

   return true;
   }
template <class T>
bool StringConverter(protocol::Component* component, const string& name,
                     const string& from, vector<T>& to, double lower, double upper)
   {
   to.erase(to.begin(), to.end());

   // 1. Convert it
   std::vector <string> strings;
   splitIntoValues (from, " ", strings);
   for (unsigned i = 0; i != strings.size(); i++)
     {
     T value;
     try {value = boost::lexical_cast<T> (strings[i]); }
     catch(boost::bad_lexical_cast &e)
        {
        string msg = string("Problem converting variable to vector<") +
                     typeid(T).name() + "> type.\n"
                     "Parameter name = " + name + "\n"
                     "Value          = '" + strings[i] + "'";
        throw runtime_error(msg);
        }
     to.push_back(value);

     // 2. Check bounds
     if (value < lower || value > upper)
        {
        string msg = "Bound check warning while reading parameter.\n"
                     "Variable  : " + name + "(" + itoa(i+1) + ")\n"
                     "Condition : " + ftoa(lower, 2) + " <= " +
                     strings[i] + " <= " + ftoa(upper, 2);
        component->error(msg.c_str(), false);
        }
     }

   return (to.size() > 0);
   }



ScienceAPI::ScienceAPI(protocol::Component* c)
   {
   component = c;
   }

bool ScienceAPI::read(const std::string& name, int& data, int lower, int upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, float& data, float lower, float upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, double& data, double lower, double upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::string& data)
   {
   if (!readOptional(name, data))
      throw runtime_error("Cannot find a value for parameter: " + name);

   return (data != "");
   }



string ScienceAPI::readFromSection(const std::string& section, const std::string& name)
   {
   // -------------------------------------------------
   // Read the specified parameter name from the
   // specified section name. This method takes
   // "derived_from" into account and uses recursion
   // to find parameters in base sections.
   // -------------------------------------------------
   string value = component->getProperty(section, name);
   if (value == "")
      {
      string baseSection = component->getProperty(section, "derived_from");
      if (baseSection != "")
         value = readFromSection(baseSection, name);
      }
   return value;
   }

bool ScienceAPI::read(const std::string& name, std::vector<int>& data, int lower, int upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::vector<float>& data, float lower, float upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }
bool ScienceAPI::read(const std::string& name, float data[], int& numVals, float lower, float upper)
   {
   vector<float> values;
   if (read(name, values, lower, upper))
      {
      numVals = values.size();
      for (int i = 0; i != numVals; i++)
         data[i] = values[i];
      return true;
      }
   else
      return false;
   }
bool ScienceAPI::read(const std::string& name, std::vector<double>& data, float lower, float upper)
   {
   string valueAsString;
   if (read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::vector<std::string>& data)
   {
   string valueAsString;
   if (read(name, valueAsString))
      {
      splitIntoValues (valueAsString, " ", data);
      return true;
      }
   else
      return false;
   }

// -------------------------------------------------------------
// Optional methods.
// -------------------------------------------------------------
bool ScienceAPI::readOptional(const std::string& name, int& data, int lower, int upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, float& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, double& data, double lower, double upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::string& data)
   {
   data = "";

   if (data == "" && currentClass1 != "")
      data = readFromSection(currentClass1, name);
   if (data == "" && currentClass2 != "")
      data = readFromSection(currentClass2, name);
   if (data == "")
      data = readFromSection("parameters", name);

   // remove any Units specifier "(..)":
   splitOffBracketedValue(data, '(', ')');

   // And any whitespace...
   stripLeadingTrailing(data, " \t");

   return (data != "");
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<int>& data, int lower, int upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<float>& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }
bool ScienceAPI::readOptional(const std::string& name, float data[], int& numVals, float lower, float upper)
   {
   vector<float> values;
   if (readOptional(name, values, lower, upper))
      {
      numVals = values.size();
      for (int i = 0; i != numVals; i++)
         data[i] = values[i];
      return true;
      }
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<double>& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<std::string>& data)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      {
      splitIntoValues (valueAsString, " ", data);
      return true;
      }
   else
      return false;
   }


string addUnitsToDDML(const string& ddml, const string& units)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (pos != string::npos)
      returnString = returnString.substr(0, pos) + " unit=\"" + units + "\"/>";
   return returnString;
   }
   
// -------------------------------------------------------------
// GET methods.
// -------------------------------------------------------------
bool ScienceAPI::get(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper)
   {
   if (!getOptional(name, units, data, lower, upper))
      {   
      string st = "The module " + string(component->getName()) + " has asked for the value of the variable " + name;
      st += ".\nIt received no responses.";
      throw runtime_error(st);
      }
     return true;
   }
bool ScienceAPI::getOptional(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper)
   {
   string ddml = protocol::DDML(vector<float>());
   addUnitsToDDML(ddml, units);
   unsigned id = component->addRegistration(RegistrationType::get, name.c_str(), ddml.c_str(), "", "");
   return component->getVariable(id, data, lower, upper, true);      
   }

// -------------------------------------------------------------
// SET methods.
// -------------------------------------------------------------
void ScienceAPI::set(const std::string& name, const std::string& units, std::vector<float>& data)
   {
   string ddml = protocol::DDML(vector<float>());
   addUnitsToDDML(ddml, units);
   unsigned id = component->addRegistration(RegistrationType::set, name.c_str(), ddml.c_str(), "", "");
   bool ok =  component->setVariable(id, data);      
   if (!ok)
      throw runtime_error("Cannot set the value of variable: " + name);
   }
