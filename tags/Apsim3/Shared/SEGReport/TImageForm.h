//---------------------------------------------------------------------------

#ifndef TImageFormH
#define TImageFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <QRCTRLS.hpp>
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
//---------------------------------------------------------------------------
class TImageForm : public TForm
{
__published:	// IDE-managed Components
   TPageControl *PageControl1;
   TTabSheet *Properties;
   TLabel *Label1;
   TCheckBox *StretchCheckBox;
   TCheckBox *CentreCheckBox;
   TCheckBox *AutoSizeCheckBox;
   TAdvEditBtn *ImageFileEdit;
   TTabSheet *TabSheet1;
   TLabel *Label2;
   TEdit *NameEdit;
   TOpenPictureDialog *OpenPictureDialog;
   void __fastcall AdvEditBtn1Click(TObject *Sender);
   void __fastcall AutoSizeCheckBoxClick(TObject *Sender);
   void __fastcall CentreCheckBoxClick(TObject *Sender);
   void __fastcall StretchCheckBoxClick(TObject *Sender);
   void __fastcall NameEditExit(TObject *Sender);
private:	// User declarations
   TQRImage* image;
public:		// User declarations
   __fastcall TImageForm(TComponent* Owner);
public:		// User declarations
   void setComponent(TQRImage* image);
};
//---------------------------------------------------------------------------
extern PACKAGE TImageForm *ImageForm;
//---------------------------------------------------------------------------
#endif
