//---------------------------------------------------------------------------
#ifndef TYPPaddockFormH
#define TYPPaddockFormH
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
#include "IWCompMemo.hpp"
class Data;
class TYPWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TYPPaddockForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *CultivarLabel;
      TIWLabel *IWLabel3;
      TTIWDatePicker *PlantingDate;
      TIWLabel *UserLabel;
      TIWComboBox *CultivarCombo;
      TIWComboBox *ReportCombo;
      TIWLabel *IWLabel7;
      TIWRectangle *IWRectangle1;
      TIWLabel *IWLabel6;
      TIWCheckBox *PlantingDateCheck;
      TIWLabel *DateLabel;
      TIWLabel *IWLabel2;
      TTIWAdvWebGrid *FertGrid;
      TIWLabel *IWLabel1;
      TIWLabel *InvalidDateLabel;
      TIWLink *BackButton;
      TIWImageFile *BackImage;
      TIWImageFile *IWImageFile1;
      TIWLink *SaveButton;
      TIWImageFile *IWImageFile3;
      TIWLink *SetupButton;
      TIWLink *RainfallButton;
      TIWImageFile *IWImageFile4;
      TIWImageFile *IWImageFile5;
      TIWLink *CreateReportButton;
      TIWLink *EmailFilesButton;
      TIWImageFile *EmailFilesImage;
      TIWLabel *IWLabel4;
      TIWLabel *IWLabel5;
      TIWLabel *IWLabel8;
   TIWLink *HelpFileLink;
   TIWLabel *IWLabel10;
   TIWLabel *IWLabel12;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall RainfallEntryButtonClick(TObject *Sender);
      void __fastcall CreateReportButtonClick(TObject *Sender);
      void __fastcall EmailFilesButtonClick(TObject *Sender);
      void __fastcall PlantingDateCheckClick(TObject *Sender);
      void __fastcall SetupButtonClick(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
   void __fastcall HelpFileLinkClick(TObject *Sender);
   private:
      TYPWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      bool fromGrowerManagement;

      //---------------------------------------------------------------------------
      // Populate the report combo.
      //---------------------------------------------------------------------------
      void populateReportCombo();

      //---------------------------------------------------------------------------
      // Populate the fertiliser grid
      //---------------------------------------------------------------------------
      void populateFertGrid();

      //---------------------------------------------------------------------------
      // Save the fertiliser grid
      //---------------------------------------------------------------------------
      void saveFertGrid();

      //---------------------------------------------------------------------------
      // User has finished entering an email address - send files.
      //---------------------------------------------------------------------------
      void __fastcall createReportCallback(bool okClicked,
                                           AnsiString text1,
                                           AnsiString text2,
                                           AnsiString text3,
                                           AnsiString text4);
      //---------------------------------------------------------------------------
      // User has finished entering an email address - send files.
      //---------------------------------------------------------------------------
      void __fastcall emailFilesCallback(bool okClicked,
                                         AnsiString text1,
                                         AnsiString text2,
                                         AnsiString text3,
                                         AnsiString text4);

      //---------------------------------------------------------------------------
      // generate report files and send to the specified email address.
      //---------------------------------------------------------------------------
      void generateReportFilesAndEmail(const std::string& reportName,
                                       const std::string& emailAddress);

      //---------------------------------------------------------------------------
      // Produce an error message if there is missing data.
      //---------------------------------------------------------------------------
      bool checkForMissingData();

   public:
      __fastcall TYPPaddockForm(TComponent* Owner);

      void setup(TYPWebSession* webSession,
                 Data* d,
                 const std::string& userName,
                 const std::string& paddockName,
                 bool readOnly,
                 bool fromGrowerMan);

   };
//---------------------------------------------------------------------------
#endif
