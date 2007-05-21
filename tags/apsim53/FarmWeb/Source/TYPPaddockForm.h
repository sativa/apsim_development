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
#include "Data.h"
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
      TIWLabel *IWLabel4;
      TIWLabel *IWLabel5;
      TIWLabel *IWLabel8;
      TIWLink *HelpFileLink;
      TIWLabel *IWLabel10;
      TIWLabel *IWLabel12;
      TIWCheckBox *EmailFilesCheckBox;
   TIWImageFile *HelpImage;
   TIWLink *HelpButton;
   TIWLabel *VisitorLabel1;
   TIWLabel *VisitorLabel2;
   TIWLabel *VisitorLabel3;
   TIWLabel *VisitorLabel4;
   TIWLabel *VisitorLabel5;
   TIWLabel *VisitorLabel6;
   TIWLabel *VisitorLabel7;
   TIWLabel *VisitorLabel8;
   TIWLabel *VisitorLabel9;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall RainfallEntryButtonClick(TObject *Sender);
      void __fastcall CreateReportButtonClick(TObject *Sender);
      void __fastcall PlantingDateCheckClick(TObject *Sender);
      void __fastcall SetupButtonClick(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
      void __fastcall HelpFileLinkClick(TObject *Sender);
   void __fastcall HelpButtonClick(TObject *Sender);
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
      // Produce an error message if there is missing data.
      //---------------------------------------------------------------------------
      bool checkForMissingData();

      //---------------------------------------------------------------------------
      // Nitrogen report callback.
      //---------------------------------------------------------------------------
      void __fastcall NitrogenReportCallback(bool okClicked,
                                             AnsiString reportDescription,
                                             const Data::Properties& properties);

   public:
      __fastcall TYPPaddockForm(TComponent* Owner);

      void setup(TYPWebSession* webSession,
                 Data* d,
                 const std::string& userName,
                 const std::string& paddockName,
                 bool fromGrowerMan);

   };
//---------------------------------------------------------------------------
#endif
