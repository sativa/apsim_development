//---------------------------------------------------------------------------
#ifndef TTemporalDataFormH
#define TTemporalDataFormH
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
class Data;
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TTemporalDataForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TTIWAdvWebGrid *grid;
      TIWLabel *PromptLabel;
      TIWButton *NextButton;
      TIWButton *PreviousButton;
      TIWRectangle *IWRectangle1;
   TIWLink *SaveButton;
   TIWImageFile *IWImageFile1;
   TIWImageFile *IWImageFile2;
   TIWLink *BackButton;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall gridGetCellProp(TObject *Sender, int RowIndex,
          int ColumnIndex, AnsiString AValue, TIWColor &AColor,
          TAlignment &AAlignment, TIWFont *Font);
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall PreviousButtonClick(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;
      std::string dataName;
      std::string description;
      boost::gregorian::date firstAllowableDate;
      boost::gregorian::date startDate;
      bool fromGrowerManagement;

      void fillGrid(void);
      void saveGrid(void);

   public:
      __fastcall TTemporalDataForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* data,
                 const std::string& userName,
                 const std::string& paddockName,
                 const std::string& description,
                 const std::string& dataName,
                 bool fromGrowerManagement);

   };
//---------------------------------------------------------------------------
#endif
