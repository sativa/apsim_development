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
   TTabSheet *Page4;
   TLabel *Label9;
   TLabel *Label10;
   TTimer *Timer1;
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall CancelButtonClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
      void __fastcall Page2Show(TObject *Sender);
      void __fastcall Page3Show(TObject *Sender);
      void __fastcall checkOkButtonState(TObject *Sender);
   void __fastcall Timer1Timer(TObject *Sender);
   void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
   private:	// User declarations
      TCursor savedCursor;
      PreviousRuns previousRuns;
      bool createSIM;
      bool console;
      std::vector<std::string> sections;
      unsigned currentSection;
      std::string controlFileName;
      std::string configurationFile;
      HANDLE childProcessHandle;

      void fillConfigurationList(void);
      void fillSimulationList(void);
      void setupForm(void);
      void __fastcall ConverterCallback(const std::string& section);

      // This application will be passed either a control file (.CON), a
      // run file (.RUN), or a .SIM file depending on what the user has
      // right clicked on.  Returns true if we need to continue with this
      // form.
      // ------------------------------------------------------------------
      bool processCmdLine();

      void doApsimRun(void);

      void getSelectedSimulations(std::vector<std::string>& simulations);
      std::string getSelectedConfiguration(void);

   public:		// User declarations
      __fastcall TRunForm(TComponent* Owner);

   };
//---------------------------------------------------------------------------
extern PACKAGE TRunForm *RunForm;
//---------------------------------------------------------------------------
#endif
