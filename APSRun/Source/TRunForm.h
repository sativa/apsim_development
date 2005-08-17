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
      TLabel *Label9;
      TLabel *Label10;
      TLabel *ControlFileLabel;
      TImageList *ImageList1;
      TLabel *FileNameLabel;
      TListView *SimulationList;
   TButton *MinimiseButton;
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall CancelButtonClick(TObject *Sender);
      void __fastcall checkOkButtonState(TObject *Sender);
      void __fastcall simulationListClick(TObject *Sender);
   void __fastcall MinimiseButtonClick(TObject *Sender);
   private:	// User declarations
      PreviousRuns previousRuns;
      std::vector<std::string> filesNeedingConversion;
      bool console;
      ApsimRuns* runs;

      void populatePage1(void);
      void populatePage2(void);
      void populatePage3();

      void fillSimulationList();

      void getSelectedSimulations(std::vector<std::string>& simulations);
      std::string getSelectedConfiguration(void);
      ApsimRuns saveSelections(void);

   public:		// User declarations
      __fastcall TRunForm(TComponent* Owner);

      void setup(ApsimRuns& runs, bool console);

      void __fastcall OnRunNotifyEvent(const std::string& simFileName);



   };
//---------------------------------------------------------------------------
extern PACKAGE TRunForm *RunForm;
//---------------------------------------------------------------------------
#endif
