//---------------------------------------------------------------------------

#ifndef DEToolBarH
#define DEToolBarH

#include <vector>
#include "ToolBarAddIn.h"
#include "AddCostsBenefits.h"
#include "DEEconConfig.h"


class DEToolBar : public ToolBarAddInBase
{
   public:
      //const
      DEToolBar(const std::string& parameters);

      ~DEToolBar();

      // return a list of TToolButton*s
      virtual void decorateToolBar(TToolBar* toolbar);

      // given the source data object, perform all calculations and store all new data
      // in the returned TAPSTable. Can have the side effect of modifying the passed
      // in table, as well as returning a new copy.
      virtual void doCalculations(TAPSTable& data);

      // returns true if this addin needs to run its calculations again
      virtual bool needsUpdate();

      // indicates that this addin needs to run its calculations again
      virtual void youNeedUpdating();



   private:
      double Salvage_rate;
      std::string Base_case;
      int Investment_period;
      AnalysisType NPV_flag;
      bool needs_update;
      int Begin_year, End_year;
      void __fastcall buttonClick(TObject* Sender);
      vector<DEEconConfig> Econ_configs;
      vector<double> Tax_brackets, Tax_rates;

      void getConfigs();

      static int numObjects;
      static TToolButton* Costs_benefits_button;
      static Graphics::TBitmap* glyph;
      static int glyph_position;
      static TToolBar* Toolbar;


};



//---------------------------------------------------------------------------
#endif
