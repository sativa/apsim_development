//---------------------------------------------------------------------------

#ifndef TFilterFormH
#define TFilterFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include "PSCFltBox.hpp"
#include "psccontrols.hpp"
#include "PSCListBox.hpp"
#include <ComCtrls.hpp>
#include <general\ini_file.h>
#include <vector>
class FiltAddIn;
//---------------------------------------------------------------------------
class TFilterForm : public TForm
{
__published:	// IDE-managed Components
   TBitBtn *BitBtn2;
   TBitBtn *BitBtn1;
   TPageControl *PageControl1;
   TTabSheet *Scenario1Sheet;
   TTabSheet *Scenario2Sheet;
   TTabSheet *Scenario3Sheet;
   TTabSheet *Scenario4Sheet;
   TTabSheet *Scenario5Sheet;
   TComboBox *FilterCombo5;
   TPSCFltBox *FilterBox5;
   TPSCFltBox *FilterBox4;
   TComboBox *FilterCombo4;
   TPSCFltBox *FilterBox3;
   TComboBox *FilterCombo3;
   TPSCFltBox *FilterBox2;
   TComboBox *FilterCombo2;
   TPSCFltBox *FilterBox1;
   TComboBox *FilterCombo1;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FilterCombo1Change(TObject *Sender);
   void __fastcall FilterBox1Change(TObject *Sender);
   void __fastcall FilterCombo2Change(TObject *Sender);
   void __fastcall FilterBox2Change(TObject *Sender);
   void __fastcall FilterCombo3Change(TObject *Sender);
   void __fastcall FilterBox3Change(TObject *Sender);
   void __fastcall FilterCombo4Change(TObject *Sender);
   void __fastcall FilterBox4Change(TObject *Sender);
   void __fastcall FilterCombo5Change(TObject *Sender);
   void __fastcall FilterBox5Change(TObject *Sender);
private:	// User declarations
   Ini_file ini;
   TComboBox* activeCombo;
   bool inFormShow;
   void addToVectorIfUnique(std::vector<std::string>& strings, const std::string& st);
   void PutFilterInCombo(TComboBox* combo, AnsiString filter);

public:		// User declarations
   __fastcall TFilterForm(TComponent* Owner);
   FiltAddIn* filterAddIn;
};
//---------------------------------------------------------------------------
extern PACKAGE TFilterForm *FilterForm;
//---------------------------------------------------------------------------
#endif