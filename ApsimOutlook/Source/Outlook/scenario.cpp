//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Scenario.h"
#include "AddIn.h"
#include <general\stl_functions.h>
#include <general\StringTokenizer.h>
using namespace std;
//---------------------------------------------------------------------------

#pragma package(smart_init)

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Scenario::Scenario(const string& nam, const FactorContainer& facs)
   {
   name = nam;
   factors = facs;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Scenario::Scenario(const string& state)
   {
   setState(state);
   }
//---------------------------------------------------------------------------
// Return the factor value for the specified factor name
//---------------------------------------------------------------------------
string Scenario::getFactorValue(const string& factorName) const
   {
   FactorContainer::const_iterator factor = find(factors.begin(),
                                                 factors.end(),
                                                 factorName);
   if (factor != factors.end())
      return factor->getValue();
   else
      return "";
   }
//---------------------------------------------------------------------------
// Set the value of the specified factor.
//---------------------------------------------------------------------------
bool Scenario::setFactorValue(const string& factorName,
                              const string& factorValue)
   {
   FactorContainer::iterator factor = find(factors.begin(),
                                           factors.end(),
                                           factorName);
   if (factor != factors.end())
      {
      factor->setValue(factorValue);
      return true;
      }
   else
      return false;
   }
//---------------------------------------------------------------------------
// Return a list of factor names to caller.
//---------------------------------------------------------------------------
void Scenario::getFactorNames(vector<string>& factorNames) const
   {
   for_each(factors.begin(), factors.end(),
            GetName<Factor>(factorNames));
   }
//---------------------------------------------------------------------------
// Return a state string to caller.
//---------------------------------------------------------------------------
std::string Scenario::getState(void) const
   {
   string stateString;
   for (FactorContainer::const_iterator factor = factors.begin();
                                        factor != factors.end();
                                        factor++)
      {
      if (stateString != "")
         stateString += ";";
      stateString += factor->getState();
      }
   return name + "##" + stateString;
   }
//---------------------------------------------------------------------------
// Set the state of this scenario from the specified string
//---------------------------------------------------------------------------
void Scenario::setState(const string& state)
   {
   unsigned posEndName = state.find("##");
   if (posEndName == string::npos)
      throw runtime_error("Invalid scenario state.");
   name = state.substr(0, posEndName);
   StringTokenizer tokenizer(state.substr(posEndName+2), " ");

   string factorState = tokenizer.nextToken(";");
   while (factorState != "")
      {
      stripLeadingTrailing(factorState, " ");
      factors.push_back(Factor(factorState));
      factorState = tokenizer.nextToken(";");
      }
   }

