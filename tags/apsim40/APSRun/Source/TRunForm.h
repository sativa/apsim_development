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
      TTreeView *simulationList;
      TImageList *ImageList1;
   TLabel *FileNameLabel;
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall CancelButtonClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
      void __fastcall checkOkButtonState(TObject *Sender);
   void __fastcall simulationListClick(TObject *Sender);
   void __fastcall simulationListMouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
   private:	// User declarations
      PreviousRuns previousRuns;
      std::vector<std::string> filesNeedingConversion;

      void populatePage1(void);
      void populatePage2(void);
      void populatePage3(void);

      void fillSimulationList(void);

      void getSelectedSimulations(std::vector<std::string>& simulations);
      std::string getSelectedConfiguration(void);
      void saveSelections(void);

   public:		// User declarations
      __fastcall TRunForm(TComponent* Owner);

      ApsimRuns* runs;

   };
//---------------------------------------------------------------------------
extern PACKAGE TRunForm *RunForm;
//---------------------------------------------------------------------------
#endif
