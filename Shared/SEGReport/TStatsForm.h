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
#include "TSEGTableForm.h"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include "TStats.h"
#include <DB.hpp>
#include "DBAdvGrd.hpp"
//---------------------------------------------------------------------------
class TStatsForm : public TSEGTableForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TComboBox *FieldNameCombo;
   TCheckBox *MeanCheckBox;
   TCheckBox *MinCheckBox;
   TCheckBox *MaxCheckBox;
   TCheckBox *CountCheckBox;
   TGroupBox *GroupBox1;
   TCheckBox *Decile60CheckBox;
   TCheckBox *Decile70CheckBox;
   TCheckBox *Decile80CheckBox;
   TCheckBox *Decile90CheckBox;
   TCheckBox *Decile10CheckBox;
   TCheckBox *Decile20CheckBox;
   TCheckBox *Decile30CheckBox;
   TCheckBox *Decile40CheckBox;
   TCheckBox *Decile50CheckBox;
   void __fastcall PropertiesSheetShow(TObject *Sender);
   void __fastcall FieldNameComboChange(TObject *Sender);
   void __fastcall CheckBoxClick(TObject *Sender);
private:	// User declarations
   TStats* stats;
public:		// User declarations
   __fastcall TStatsForm(TComponent* Owner);
   void setComponent(TStats* stats);
};
//---------------------------------------------------------------------------
extern PACKAGE TStatsForm *StatsForm;
//---------------------------------------------------------------------------
#endif
