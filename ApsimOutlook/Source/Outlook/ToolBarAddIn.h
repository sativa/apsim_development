//---------------------------------------------------------------------------

#ifndef ToolBarAddInH
#define ToolBarAddInH

#include <vector>
#include <tapstable.h>
#include "Scenarios.h"

// ------------------------------------------------------------------
//  Short description:
//      base class for all toolbar add-ins.  All APSIM Outlook toolbar add-ins should
//      create a new class derived from this one and then override
//      any of the virtual methods as necessary.

//  Notes:     ToolBarAddInBase encapsulates the idea of extra tools available
//             available on the toolbar for a particular skin of APSIM Outlook.
//             It should return a list of TToolButtons and is expected to provide
//             event handlers for each of these buttons. A ToolBarAddin has the
//             prerogative to modify data from the current source table, or provide
//             a new temporary source table (or do both).

//    The ToolBarAdd-in DLL should have 1 exported DLL entry point that simply
//    creates an instance of the add-in and return a pointer to it.
//    Outlook will delete the add-in automatically when required.
//    For example:
//       extern "C" ToolBarAddInBase* _export __stdcall createToolBarAddIn(const string& parameters)
//          {
//          return new MyAddIn;
//          }

//  Created:   DAH   10/5/01

// ------------------------------------------------------------------
class ToolBarAddInBase
	{
   public:
      // destructor
      virtual ~ToolBarAddInBase(void) { };

      // Add buttons etc. to the ToolBar passed in.
      virtual void decorateToolBar(TToolBar*) = 0;

      // given the source data object, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data) = 0;

      // returns true if this addin needs to run its calculations again
      virtual bool needsUpdate() = 0;

      // indicates that this addin needs to run its calculations again
      virtual void youNeedUpdating() = 0;


      void setWorkingData(Scenarios* current, TAPSTable* data)
         {  scenarios = current;
            working = data;
            youNeedUpdating();
         };

   protected:
      Scenarios* scenarios;
      TAPSTable* working;

   };

//---------------------------------------------------------------------------
#endif
