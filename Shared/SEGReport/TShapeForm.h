//---------------------------------------------------------------------------

#ifndef TShapeFormH
#define TShapeFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <QRCTRLS.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include "TShape.h"
#include "asgcombo.hpp"
#include "ColorCombo.hpp"
#include <QRCtrls.hpp>
#include <QuickRpt.hpp>
//---------------------------------------------------------------------------
class TShapeForm : public TForm
{
__published:	// IDE-managed Components
   TPageControl *PageControl1;
   TTabSheet *Properties;
   TLabel *Label1;
   TTabSheet *TabSheet1;
   TLabel *Label3;
   TEdit *NameEdit;
   TLabel *Label2;
   TLabel *Label4;
   TComboBox *ShapeCombo;
   TAdvColorComboBox *PenColourCombo;
   TAdvColorComboBox *BrushColourCombo;
   void __fastcall ShapeComboChange(TObject *Sender);
   void __fastcall PenColourComboChange(TObject *Sender);
   void __fastcall BrushColourComboChange(TObject *Sender);
private:	// User declarations
      ::TShape* shape;
public:		// User declarations
   __fastcall TShapeForm(TComponent* Owner);
      void setComponent(::TShape* shape);
};
//---------------------------------------------------------------------------
extern PACKAGE TShapeForm *ShapeForm;
//---------------------------------------------------------------------------
#endif
