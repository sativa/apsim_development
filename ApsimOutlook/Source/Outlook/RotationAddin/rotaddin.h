//---------------------------------------------------------------------------
#ifndef RotAddInH
#define RotAddInH

#include "ToolBarAddIn.h"
#include <vector>
#include <string>
#include <map>
// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a crop rotation addin

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
class RotationAddIn : public ToolBarAddInBase
   {
   private:
      bool needsUpdating;
      bool rotationAnalysisOn;
      std::vector<std::string> cropNames;
      TToolButton* rotationButton;
      Graphics::TBitmap* glyph;
      TToolBar* Toolbar;
      typedef std::vector<std::string> DataBlockNames;
      typedef std::map<std::string, DataBlockNames> Rotations;
      Rotations rotations;

      void __fastcall buttonClick(TObject* Sender);
      const Graphics::TBitmap* getImageForFactor(const std::string& factorName) const {return NULL;}

      void partitionFilesIntoRotations(void);
      bool processRotation(TAPSTable& data,
                           TAPSTable& destData,
                           Rotations::iterator rotationI);
      void getCropNames(std::vector<std::string>& fieldNames);
      bool isCropVariable(const string& fieldName) const;
      bool cropWasSown(const TAPSRecord& recordI, const std::string& crop_acronym) const;
      bool CropHasNonZeroValue(const TAPSRecord& recordI, const std::string& crop_acronym) const;
      bool doTotalVariable(const std::string& fieldName);
      void ensureRotationMappingStillValid(void);

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

      // Each file is categorised as belonging to a particular named rotation.
      // These methods allow callers to manipulate which files belong to which
      // rotations.
      int getNumRotations(void);
      void getRotationNames(std::vector<std::string>& names);
      void clearRotations(void) {rotations.erase(rotations.begin(), rotations.end());}
      bool getRotation(const std::string& name, DataBlockNames& dataBlockNames) const;
      void addRotation(const std::string& name, const DataBlockNames& dataBlockNames);
   };

// ------------------------------------------------------------------
// Make sure all field names have a per yr on the end.
// ------------------------------------------------------------------
std::string addPerYearToFieldName(std::string& fieldName);

#endif
