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
//---------------------------------------------------------------------------
class __declspec(dllexport) TRunForm : public TForm
   {
   __published:	// IDE-managed Components
      TImageList *ImageList1;
      TImage *Image1;
      TBevel *Bevel1;
      TButton *NextButton;
      TButton *CancelButton;
      TPageControl *PageControl1;
      TTabSheet *Page2;
      TPanel *Panel1;
      THTMListBox *StatusList;
      TTabSheet *Page1;
      TLabel *Label1;
      TLabel *Label3;
      TLabel *Label2;
      TLabel *Label4;
      TTabSheet *Page3;
      TLabel *Label5;
      TListBox *simulationList;
      TListBox *configurationList;
      TLabel *Label6;
      TLabel *Label7;
      TBevel *Bevel2;
      TLabel *Label8;
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall CancelButtonClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
      void __fastcall Page2Show(TObject *Sender);
      void __fastcall Page3Show(TObject *Sender);
      void __fastcall checkOkButtonState(TObject *Sender);
   private:	// User declarations
      TCursor savedCursor;
      PreviousRuns previousRuns;

      void fillConfigurationList(void);
      void fillSimulationList(void);
      void setupForm(void);
      void __fastcall ConverterCallback(const std::string& section);
   public:		// User declarations
      __fastcall TRunForm(TComponent* Owner);
      std::string controlFileName;

      void getSelectedSimulations(std::vector<std::string>& simulations);
      std::string getSelectedConfiguration(void);
   };
//---------------------------------------------------------------------------
extern PACKAGE TRunForm *RunForm;
//---------------------------------------------------------------------------
#endif
