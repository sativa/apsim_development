//---------------------------------------------------------------------------
#ifndef TYPSetupFormH
#define TYPSetupFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWOutlookBar.hpp"
#include "IWCompEdit.hpp"
#include "IWCompListbox.hpp"
#include "IWTMSCal.hpp"
#include "IWCompButton.hpp"
#include <ImgList.hpp>
#include "IWTMSCtrls.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompRectangle.hpp"
#include "IWAdvWebGrid.hpp"
#include "IWWebGrid.hpp"
#include "IWCompCheckbox.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
class Data;
class TYPWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TYPSetupForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *IWLabel4;
      TIWLabel *UserLabel;
      TIWLabel *IWLabel1;
      TIWComboBox *WeatherStationCombo;
      TIWComboBox *SoilTypeCombo;
      TIWRectangle *IWRectangle1;
      TTIWAdvWebGrid *ResetGrid;
      TIWLabel *IWLabel5;
      TTIWDatePicker *ResetDate;
      TIWComboBox *SubSoilCombo;
      TIWLabel *IWLabel2;
      TTIWAdvWebGrid *ResetGrid2;
      TIWLabel *IWLabel3;
      TIWComboBox *RegionCombo;
      TIWLabel *IWLabel7;
      TIWLabel *IWLabel6;
      TIWImageFile *IWImageFile1;
      TIWLink *SaveButton;
      TIWLink *BackButton;
      TIWImageFile *IWImageFile2;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
      void __fastcall RegionComboChange(TObject *Sender);
   private:
      TYPWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      bool fromGrowerManagement;

      //---------------------------------------------------------------------------
      // populate the region combo
      //---------------------------------------------------------------------------
      void populateRegionCombo();

      //---------------------------------------------------------------------------
      // Populate the ResetGrid.
      //---------------------------------------------------------------------------
      void populateResetGrid();

      //---------------------------------------------------------------------------
      // Populate the ResetGrid2.
      //---------------------------------------------------------------------------
      void populateResetGrid2();

      //---------------------------------------------------------------------------
      // Save the ResetGrid.
      //---------------------------------------------------------------------------
      void saveResetGrid();

      //---------------------------------------------------------------------------
      // Save the ResetGrid2.
      //---------------------------------------------------------------------------
      void saveResetGrid2();

   public:
      __fastcall TYPSetupForm(TComponent* Owner);

      void setup(TYPWebSession* webSession,
                 Data* d,
                 const std::string& userName,
                 const std::string& paddockName,
                 bool fromGrowerManagement);

   };
//---------------------------------------------------------------------------
#endif
