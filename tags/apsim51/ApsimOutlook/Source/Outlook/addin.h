//---------------------------------------------------------------------------

#ifndef AddInH
#define AddInH

#include <tapstable.h>
#include "scenario.h"

// ------------------------------------------------------------------
//  Short description:
//      base class for all add-ins.  All APSIM Outlook add-ins should
//      create a new class derived from this one and then override
//      any of the virtual methods as necessary.

//  Notes:           AddInBase now encapsulates potentially one or more simulation
//                   factors. For example, with the database addin, there is one
//                   instance of the addin which should give back multiple images
//                   and labels, representing the factors found in the database
//                   An economics addin would perhaps only have one image and label
//                   pair to return.
//    The Add-in DLL should have 1 exported DLL entry point that simply
//    creates an instance of the add-in and return a pointer to it.
//    Outlook will delete the add-in automatically when required.
//    For example:
//       extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters bool& success)
//          {
//          success = true;
//          return new MyAddIn;
//          }

//  Changes:
//    DPH 5/9/00
//    DAH 24/11/00
//    DAH 14/12/00

// ------------------------------------------------------------------
class AddInBase
	{
   public:
      // destructor
      virtual ~AddInBase(void) { };

      // set any startup parameters.
      virtual void setStartupParameters(const std::string& parameters) = 0;

      // return a default scenario to caller.
      virtual Scenario getDefaultScenario(void) const = 0;

      // return true if the simulation is valid.  False otherwise.
      virtual bool isScenarioValid(Scenario& scenario) const = 0;

      // make the scenario passed in a valid one.  This may mean adding
      // extra factors to the scenario or changing existing factors.
      virtual void makeScenarioValid(Scenario& scenario, const std::string factor_name) const = 0;

		// if your add-in wants to just supply values for the "Value Selection" form
		// then the following method needs to be implemented.  It returns a
      // list of values for the specified factor in the specified scenario.
		virtual void getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const { };

      // if your add-in wants to display a properties form then implement
      // the following 2 methods.
      virtual std::string getUIButtonCaption(void) const {return "";}
      virtual void showUI(void) { }

      // given the data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data, const Scenario& scenario) = 0;

      // gives the add-in the chance to return any information to the
      // 'displaySettings' window e.g. warnings, errors etc.
      virtual std::string getDisplaySettings(void) {return "";}

   };

//---------------------------------------------------------------------------
#endif
