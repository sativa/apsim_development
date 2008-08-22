//---------------------------------------------------------------------------

#ifndef TYPNitrogenReportFormH
#define TYPNitrogenReportFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompEdit.hpp"
#include "IWCompLabel.hpp"
#include "IWCompRectangle.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWAdvWebGrid.hpp"
#include "IWCompListbox.hpp"
#include "IWTMSCal.hpp"
#include "IWWebGrid.hpp"
#include "IWOutlookBar.hpp"
#include "IWHTMLControls.hpp"
#include "IWExtCtrls.hpp"

#include "TYPWebSession.h"
#include "Data.h"

//---------------------------------------------------------------------------
class TYPNitrogenReportForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWRectangle *IWRectangle1;
      TIWLabel *IWLabel2;
      TTIWAdvWebGrid *FertGrid;
      TIWEdit *Scenario1Edit;
      TIWLabel *IWLabel1;
      TIWEdit *Scenario2Edit;
      TIWEdit *Scenario3Edit;
      TIWHRule *IWHRule1;
      TIWLabel *IWLabel3;
      TIWLabel *IWLabel4;
      TIWHRule *IWHRule2;
      TIWLabel *IWLabel5;
      TIWHRule *IWHRule3;
      TIWLabel *IWLabel6;
      TIWLink *BackButton;
      TIWImageFile *IWImageFile2;
      TIWImageFile *IWImageFile1;
      TIWLink *OkButton;
      TIWLabel *IWLabel7;
      TIWEdit *ReportDescription;
      void __fastcall BackButtonClick(TObject *Sender);
      void __fastcall OkButtonClick(TObject *Sender);
   private:
      TYPWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      TReportCallback callback;

      //---------------------------------------------------------------------------
      // Save the fertiliser grid
      //---------------------------------------------------------------------------
      void saveFertGrid(Data::Properties& properties);

      //---------------------------------------------------------------------------
      // Return true if all edit boxes are ok.
      //---------------------------------------------------------------------------
      bool allOk();

      //---------------------------------------------------------------------------
      // Return true a scenario has data ie fertiliser applications
      //---------------------------------------------------------------------------
      bool scenarioHasData(int scenarioNumber);

   public:		// User declarations
      __fastcall TYPNitrogenReportForm(TComponent* Owner);

      void setup(TYPWebSession* webSession,
                 Data* data,
                 const std::string& userName,
                 const std::string& paddockName,
                 TReportCallback callback);
   };
//---------------------------------------------------------------------------
#endif
