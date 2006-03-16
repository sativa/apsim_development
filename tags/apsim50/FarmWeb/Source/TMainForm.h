//---------------------------------------------------------------------------
#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWExtCtrls.hpp"
#include <jpeg.hpp>
#include "IWCompEdit.hpp"
#include "IWCompButton.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"

//---------------------------------------------------------------------------
// Main Form for web server - login form.
//---------------------------------------------------------------------------
class TMainForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
   TIWLabel *Label1;
   TIWLabel *Label2;
   TIWLabel *Label3;
   TIWLabel *Label4;
   void __fastcall IWAppFormResize(TObject *Sender);
   private:
   public:
      __fastcall TMainForm(TComponent* Owner);
   };
//---------------------------------------------------------------------------
#endif
