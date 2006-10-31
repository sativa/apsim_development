//---------------------------------------------------------------------------

#ifndef TRunFormH
#define TRunFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HTMLabel.hpp"
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include "HTMListB.hpp"
#include <ImgList.hpp>
#include <jpeg.hpp>
#include <ComCtrls.hpp>
#include "PreviousRuns.h"
#include "ApsimRuns.h"
#include "HTMLText.hpp"
#include "AdvMemo.hpp"
#include "advmws.hpp"
#include <general\date_class.h>
//---------------------------------------------------------------------------
class __declspec(dllexport) TRunForm : public TForm
   {
   __published:	// IDE-managed Components
      TImage *Image1;
      TButton *NextButton;
      TButton *CancelButton;
      TPageControl *MainPanel;
      TTabSheet *Page2;
      TPanel *Panel1;
      THTMListBox *StatusList;
      TTabSheet *Page1;
      TLabel *Label1;
      TLabel *Label3;
      TLabel *Label2;
      TLabel *Label4;
      TTabSheet *Page3;
      TLabel *Label6;
      TTabSheet *Page4;
      TLabel *ControlFileLabel;
      TImageList *ImageList1;
      TListView *SimulationList;
      TProgressBar *ProgressBar;
      TLabel *FileNameLabel;
      TCheckBox *PauseCheckBox;
      TLabel *CurrentDateLabel;
      TMemo *Memo1;
      TLabel *FinishedLabel;
      TLabel *ErrorLabel;
      TLabel *StartDateLabel;
      TLabel *EndDateLabel;
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall CancelButtonClick(TObject *Sender);
      void __fastcall checkOkButtonState(TObject *Sender);
      void __fastcall simulationListClick(TObject *Sender);
      void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
      void __fastcall PauseCheckBoxClick(TObject *Sender);
   void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
   private:	// User declarations
      PreviousRuns previousRuns;
      std::vector<std::string> filesNeedingConversion;
      ApsimRuns* runs;
      bool paused;
      bool pauseOnComplete;

      unsigned long simStartJDay;
      unsigned long simEndJDay;
      unsigned long dayCounter;

      void populatePage1(void);
      void populatePage2(void);
      void populatePage3();

      void getSelectedSimulations(std::vector<std::string>& simulations);
      std::string getSelectedConfiguration(void);
      void saveSelections(void);

      void setupProgressBar(const string &simFileName);
      void addLine(const string& line);
      void runSimulations();

   public:		// User declarations
      __fastcall TRunForm(TComponent* Owner);

      void setup(ApsimRuns& runs, bool autoRun);

      void __fastcall OnRunNotifyEvent(const std::string& simFileName);
      //void __fastcall OnProgressEvent(const std::string& simFileName);
      void __fastcall OnStdoutEvent(const string& text);

   };
//---------------------------------------------------------------------------
extern PACKAGE TRunForm *RunForm;
//---------------------------------------------------------------------------
#endif
