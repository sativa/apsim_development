//---------------------------------------------------------------------------

#ifndef DEToolBarH
#define DEToolBarH

#include <vector>
#include "ToolBarAddIn.h"
#include "AddCostsBenefits.h"
#include "DEEconConfig.h"
#include <ApsimShared\ApsimSettings.h>

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
      ApsimSettings settings;
      double Salvage_rate;
      std::string Base_case;
      int Investment_period;
      AnalysisType NPV_flag;
      bool edit_needs_update, npv_needs_update;
      int Begin_year, End_year;
      void __fastcall editButtonClick(TObject* Sender);
      void __fastcall costsButtonClick(TObject* Sender);
      vector<DEEconConfig> Econ_configs;
      vector<double> Tax_brackets, Tax_rates;

      void getConfigs();

      static int numObjects;
      static TToolButton* Edit_configs_button;
      static TToolButton* Costs_benefits_button;
      static Graphics::TBitmap* Edit_glyph;
      static Graphics::TBitmap* Costs_glyph;
      static int Edit_glyph_position, Costs_glyph_position;
      static TToolBar* Toolbar;

      void __fastcall updateConfigs();
      void saveConfigs();
      void getAllFactorValues(const std::string& factorName,
                                   std::vector<std::string>& factorValues) const;


};



//---------------------------------------------------------------------------
#endif
