//---------------------------------------------------------------------------

#ifndef ScenarioH
#define ScenarioH

#include <TAPSTable.h>
#include <vector>
#include <string>
#include "factor.h"
// ------------------------------------------------------------------
// This class encapsulates a single scenario.  A Scenario has
// a name and 1 or more factors.
// ------------------------------------------------------------------
class Scenario
   {
   public:
      Scenario(void) { }
      Scenario(const std::string& state);
      Scenario(const std::string& name, const std::vector<Factor>& factors);

      // addition operator
      Scenario& operator+ (const Scenario& rhs)
         {
         copy(rhs.factors.begin(), rhs.factors.end(),
              back_inserter(factors));
         return *this;
         }
      bool operator==(const std::string& rhsName) const
         {
         return (strcmpi(name.c_str(), rhsName.c_str()) == 0);
         }

      // get and set name.
      void setName(const std::string& new_name) {name = new_name;}
      std::string getName(void) const {return name;}

      // return a list of all factor names.
      void getFactorNames(std::vector<std::string>& factorNames) const;

      // return a factor value for a given factor name
      std::string getFactorValue(const std::string& factorName) const;

      // set a factor value.  Return false if factor not found.
      bool setFactorValue(const std::string& factorName,
                          const std::string& factorValue);

      // Return a state string to caller.
      std::string getState(void) const;

   private:
      typedef std::vector<Factor> FactorContainer;
      std::string name;
      FactorContainer factors;

      void setState(const std::string& state);
   };

//---------------------------------------------------------------------------
#endif
