//---------------------------------------------------------------------------

#ifndef ScenarioH
#define ScenarioH

#include <TAPSTable.h>
#include <vector>
#include <string>
#include "factor.h"
#include "TValueSelectionForm.h"
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a single scenario.  A Scenario has
//      a name and 1 or more factors.

//  Changes:
//    DPH 4/4/01 - modified slightly from Dene Hughes.

// ------------------------------------------------------------------
class Scenario {
   public:
      Scenario(const std::string& name, const std::vector<Factor>& factors);
      Scenario(const Scenario& from);

      ~Scenario();

      Scenario& operator=(const Scenario& rhs);

      // get and set name.
      void setName(const std::string& new_name);
      std::string getName(void) const;

      // return a copy of all factors.
      void getFactors(std::vector<Factor>& copyOfFactors) const;

      // return a list of all factor names.
      void getFactorNames(std::vector<std::string>& factorNames) const;

      // return attributes for a given factor
      void getFactorAttributes(const std::string& factorName,
                               std::string&       factorValue,
                               const Graphics::TBitmap*& factorBitmap) const;

      // set a factor value.  Return false if factor not found.
      bool setFactorValue(const std::string& factor_name,
                          const std::string& factor_value);

      // return a list of factor values for the specified factor.
      void getFactorValues(const std::string& factor_name,
                           std::vector<std::string>& values);

      TValueSelectionForm*  getUIForm(const std::string& factor_name, TComponent* Owner);

      Scenario createScenarioForAddIn(const AddInBase* addIn);

   private:
      typedef std::vector<Factor> FactorContainer;
      std::string name;
      FactorContainer factors;

      void makeUsValid(const std::string factor_name);
};

//---------------------------------------------------------------------------
#endif
