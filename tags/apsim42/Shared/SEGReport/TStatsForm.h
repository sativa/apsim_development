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
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TStatsForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TCheckBox *Decile90CheckBox;
   TCheckBox *Decile80CheckBox;
   TCheckBox *Decile70CheckBox;
   TCheckBox *Decile60CheckBox;
   TCheckBox *Decile50CheckBox;
   TCheckBox *Decile40CheckBox;
   TCheckBox *Decile30CheckBox;
   TCheckBox *Decile20CheckBox;
   TCheckBox *Decile10CheckBox;
   TCheckBox *SumCheckBox;
   TCheckBox *CountCheckBox;
   TCheckBox *MaxCheckBox;
   TCheckBox *MinCheckBox;
   TCheckBox *MeanCheckBox;
   TComboBox *FieldNameCombo;
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
