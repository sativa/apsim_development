//---------------------------------------------------------------------------
#ifndef RotationAddInH
#define RotationAddInH

#include "ToolBarAddIn.h"
#include <general\ini_file.h>
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a crop rotation addin

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
class RotationAddIn : public ToolBarAddInBase
   {
   public:
      RotationAddIn(const std::string& parameters);
      ~RotationAddIn(void);

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

      // turn on/off the rotation analysis.
      void setRotationAnalysisOn(bool on) {rotationAnalysisOn = on;}
      bool getRotationAnalysisOn(void) {return rotationAnalysisOn;}

   private:
      bool needsUpdating;
      bool rotationAnalysisOn;
      std::vector<std::string> cropNames;
      TToolButton* rotationButton;
      Graphics::TBitmap* glyph;
      TToolBar* Toolbar;
      Ini_file ini;

      void __fastcall buttonClick(TObject* Sender);
      const Graphics::TBitmap* getImageForFactor(const std::string& factorName) const {return NULL;}

      bool processRotation(TAPSTable& data, TAPSTable& destData);
      void getCropNames(std::vector<std::string>& fieldNames);
      bool isCropVariable(const string& fieldName) const;
      bool cropWasSown(const TAPSRecord& recordI, const std::string& fieldName) const;
      bool CropHasNonZeroValue(const TAPSRecord& recordI, const std::string& crop_acronym) const;
      bool doTotalVariable(const std::string& fieldName);
   };

#endif
