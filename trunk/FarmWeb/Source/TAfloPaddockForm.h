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
      TIWButton *RainfallEntryButton;
      TIWLabel *UserLabel;
      TIWButton *SaveButton;
      TIWButton *SoilTempButton;
      TIWComboBox *CultivarCombo;
      TIWLabel *IWLabel1;
      TIWComboBox *WeatherStationCombo;
      TIWComboBox *SoilTypeCombo;
      TIWComboBox *SoilDepthCombo;
      TIWLabel *IWLabel6;
      TIWComboBox *StartingSWCombo;
      TIWButton *AirTempButton;
      TTIWOutlookBar *Bar;
      TIWButton *EmailFilesButton;
      TIWEdit *EmailFilesEdit;
      TIWComboBox *ReportCombo;
      TIWButton *RequestButton;
      TIWLabel *IWLabel7;
      TIWLabel *IWLabel8;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall RainfallEntryButtonClick(TObject *Sender);
      void __fastcall RequestButtonClick(TObject *Sender);
      void __fastcall SoilTempButtonClick(TObject *Sender);
      void __fastcall AirTempButtonClick(TObject *Sender);
      void __fastcall EmailFilesButtonClick(TObject *Sender);
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
