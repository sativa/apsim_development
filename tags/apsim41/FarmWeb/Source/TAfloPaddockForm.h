//---------------------------------------------------------------------------
#ifndef TAfloPaddockFormH
#define TAfloPaddockFormH
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
#include "IWCompCheckbox.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
class Data;
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TAfloPaddockForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *IWLabel2;
      TIWLabel *IWLabel3;
      TTIWDatePicker *PlantingDate;
      TIWLabel *IWLabel4;
      TIWLabel *IWLabel5;
      TIWButton *SoilTempButton;
      TIWComboBox *CultivarCombo;
      TIWLabel *IWLabel1;
      TIWComboBox *WeatherStationCombo;
      TIWComboBox *SoilTypeCombo;
      TIWComboBox *StartingSWCombo;
      TIWButton *AirTempButton;
      TIWRectangle *IWRectangle1;
      TIWLabel *UserLabel;
      TIWComboBox *ReportCombo;
      TIWImageFile *IWImageFile1;
      TIWLink *SaveButton;
      TIWLink *RainfallButton;
      TIWImageFile *IWImageFile4;
      TIWImageFile *IWImageFile5;
      TIWLink *CreateReportButton;
      TIWCheckBox *EmailFilesCheckBox;
      TIWLabel *IWLabel7;
      TIWLabel *IWLabel8;
      TIWLabel *IWLabel9;
   TIWImageFile *HelpImage;
   TIWLink *HelpButton;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall RainfallEntryButtonClick(TObject *Sender);
      void __fastcall CreateReportButtonClick(TObject *Sender);
      void __fastcall SoilTempButtonClick(TObject *Sender);
      void __fastcall AirTempButtonClick(TObject *Sender);
   void __fastcall HelpButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;

      //---------------------------------------------------------------------------
      // Populate the report combo.
      //---------------------------------------------------------------------------
      void populateReportCombo();

   public:
      __fastcall TAfloPaddockForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* d,
                 const std::string& userName,
                 const std::string& paddockName,
                 bool readOnly);

   };
//---------------------------------------------------------------------------
#endif
