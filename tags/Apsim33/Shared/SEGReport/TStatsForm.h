//---------------------------------------------------------------------------

#ifndef TStatsFormH
#define TStatsFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include "TPropertyForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TStats.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TStatsForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TComboBox *FieldNameCombo;
   TCheckBox *MeanCheckBox;
   TCheckBox *MinCheckBox;
   TCheckBox *MaxCheckBox;
   TCheckBox *CountCheckBox;
   TCheckBox *Decile10CheckBox;
   TCheckBox *Decile20CheckBox;
   TCheckBox *Decile30CheckBox;
   TCheckBox *Decile40CheckBox;
   TCheckBox *Decile50CheckBox;
   TCheckBox *Decile60CheckBox;
   TCheckBox *Decile70CheckBox;
   TCheckBox *Decile80CheckBox;
   TCheckBox *Decile90CheckBox;
   TLabel *Label4;
   TLabel *Label5;
   TLabel *Label6;
   TLabel *Label7;
   TLabel *Label8;
   TLabel *Label9;
   TLabel *Label10;
   TLabel *Label11;
   TLabel *Label12;
   TLabel *Label13;
   TLabel *Label14;
   TLabel *Label15;
   TLabel *Label16;
   TCheckBox *SumCheckBox;
   TLabel *Label17;
   void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall CheckBoxClick(TObject *Sender);
private:	// User declarations
   TStats* stats;
   virtual void sourceHasChanged(TSEGTable* segTable);   
public:		// User declarations
   __fastcall TStatsForm(TComponent* Owner);
   void setComponent(TComponent* component);
};
//---------------------------------------------------------------------------
extern PACKAGE TStatsForm *StatsForm;
//---------------------------------------------------------------------------
#endif
