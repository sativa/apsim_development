//---------------------------------------------------------------------------
#ifndef TDataFormH
#define TDataFormH
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
#include "IWAdvWebGrid.hpp"
#include "IWWebGrid.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompRectangle.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
#include <boost\date_time\gregorian\gregorian.hpp>
#include "Data.h"
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TDataForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TTIWAdvWebGrid *grid;
      TIWLabel *PromptLabel;
      TIWRectangle *IWRectangle1;
      TIWLink *SaveButton;
      TIWImageFile *IWImageFile1;
      TIWImageFile *IWImageFile2;
      TIWLink *BackButton;
      TIWLabel *IWLabel2;
      TIWComboBox *YearCombo;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall YearComboChange(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      boost::gregorian::date sowDate;
      boost::gregorian::date startDate;
      bool fromGrowerManagement;

      void fillGrid(void);
      void saveGrid(void);

   public:
      __fastcall TDataForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* d,
               const std::string& userN,
               const std::string& paddockN,
               bool fromGrowerManagement);

   };
//---------------------------------------------------------------------------
#endif
