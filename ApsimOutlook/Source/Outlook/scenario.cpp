//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Scenario.h"
#include "AddIn.h"
#include <general\stl_functions.h>

//---------------------------------------------------------------------------

#pragma package(smart_init)

// constructor
Scenario::Scenario(const string& nam, const FactorContainer& facs) {
   name = nam;
   factors = facs;
}

// copy constructor
Scenario::Scenario(const Scenario& from) {
   *this = from;
}

Scenario::~Scenario() {
   // Scenarios does not create the bitmaps so does not need to delete them
}

Scenario& Scenario::operator=(const Scenario& rhs) {
   if (this == &rhs)
      return *this;

   name = rhs.name;
   factors.assign(rhs.factors.begin(),rhs.factors.end());
   return *this;
}

void Scenario::setName(const string& new_name) {
   name = new_name;
}

string Scenario::getName(void) const {
   return name;
}

bool Scenario::setFactorValue(const string& factor_name,
                              const string& factor_value) {
   // find the factor.  If found then pass the new factor value to it.  If
   // not found then return false.
   FactorContainer::iterator found_pos = find(factors.begin(),
                                              factors.end(),
                                              factor_name);
   if (found_pos != factors.end()) {
      (*found_pos).setValue(factor_value);
      makeUsValid();
      return true;
   }
   else
      return false;
}

void Scenario::getFactors(std::vector<Factor>& copyOfFactors) const
   {
   copy(factors.begin(), factors.end(), back_inserter(copyOfFactors));
   }

void Scenario::getFactorNames(vector<string>& factorNames) const
   {
   for_each(factors.begin(), factors.end(),
            GetNameFunction<vector<string>, Factor >(factorNames));
   }
void Scenario::getFactorAttributes(const std::string& factorName,
                                   std::string&       factorValue,
                                   const Graphics::TBitmap*& factorBitmap) const
   {
   // find the factor.  If found then get factor's attributes.
   FactorContainer::const_iterator found_pos = find(factors.begin(),
                                                    factors.end(),
                                                    factorName);
   if (found_pos != factors.end())
      {
      factorValue = (*found_pos).getValue();
      factorBitmap = (*found_pos).getImage();
      }
   }
void Scenario::makeUsValid(void)
   {
   // take a temporary copy of all factors.
   FactorContainer newFactors;

   const AddInBase* currentAddIn = NULL;
   // loop through all factors.  Group all factors belonging to the same
   // add-in into a new Scenario and ask the add-in to validitate it.
   for (FactorContainer::iterator f = factors.begin();
                                  f != factors.end();
                                  f++)
      {
      if ((*f).getAddIn() != currentAddIn)
         {
         currentAddIn = (*f).getAddIn();
         Scenario newScenario = createScenarioForAddIn(currentAddIn);
         currentAddIn->makeScenarioValid(newScenario);
         newScenario.getFactors(newFactors);
         }
      }
   factors.assign(newFactors.begin(), newFactors.end());
   }

Scenario Scenario::createScenarioForAddIn(const AddInBase* addIn) const
   {
   // loop through all factors.  Create a new scenario that contains all
   // factors for the AddIn passed into this method.
   vector<Factor> allFactors;
   for (FactorContainer::const_iterator f = factors.begin();
                                        f != factors.end();
                                        f++)
      {
      if ((*f).getAddIn() == addIn)
         allFactors.push_back(*f);
      }
   return Scenario(name, allFactors);
   }

void Scenario::getFactorValues(const std::string& factorName,
                               std::vector<std::string>& values) const
   {
   FactorContainer::const_iterator found_pos = find(factors.begin(), factors.end(), factorName);
   if (found_pos != factors.end())
      {
      const AddInBase* addIn = (*found_pos).getAddIn();
      addIn->getFactorValues(factorName, values);
      }
   }

