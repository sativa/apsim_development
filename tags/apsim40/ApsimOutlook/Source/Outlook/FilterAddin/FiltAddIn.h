//---------------------------------------------------------------------------
#ifndef FiltAddInH
#define FiltAddInH

#include "ToolBarAddIn.h"
#include "FieldValues.h"
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a database addin

//  Changes:
//    DPH 18/6/98
//    DAH 30/7/01    changed filterbutton and glyph to be a member variables so we
//                   can delete them in destructor. Also now keep a reference to
//                   ChildWin's toolbar so that we can access it to clean it up
//                   in the destructor

// ------------------------------------------------------------------
class FiltAddIn : public ToolBarAddInBase
   {
   public:
      FiltAddIn(const std::string& parameters);
      ~FiltAddIn(void);

      // Add buttons etc. to the ToolBar passed in.
      virtual void decorateToolBar(TToolBar* toolbar);

      // given the source data object, and the list of user selected
      // scenarios, perform all calculations and store all new data
      // in the returned TAPSTable.
      virtual void doCalculations(TAPSTable& data);

      // returns true if this addin needs to run its calculations again
      virtual bool needsUpdate() {return needsUpdating;}

      // indicates that this addin needs to run its calculations again
      virtual void youNeedUpdating() {needsUpdating = true;};

      // return a copy of all filters to caller.
      std::vector<std::string> getFilters(void) {return filters;}

      // set all filters
      void setFilters(const std::vector<std::string>& newFilters)
         {
         filters.erase(filters.begin(), filters.end());
         std::copy(newFilters.begin(), newFilters.end(), std::back_inserter(filters));
         }
   private:
      std::vector<std::string> filters;
      FieldValues fieldValues;
      TToolButton* filterButton;
      Graphics::TBitmap* glyph;
      TToolBar* Toolbar;

      bool needsUpdating;

      void __fastcall buttonClick(TObject* Sender);

      const Graphics::TBitmap* getImageForFactor(const std::string& factorName) const {return NULL;}
   };

#endif
