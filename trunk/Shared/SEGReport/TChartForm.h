//---------------------------------------------------------------------------

#ifndef TChartFormH
#define TChartFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TGraph.h"
#include "TPropertyForm.h"
#include <Buttons.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "AsgLinks.hpp"
#include "BaseGrid.hpp"
#include <Grids.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TChartForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *ChartPropertyLabel;
   TImageList *ImageList1;
   TLabel *Label3;
   TEdit *SeriesNumberEdit;
   void __fastcall SeriesNumberEditChange(TObject *Sender);
   void __fastcall ChartPropertyLabelClick(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
private:	// User declarations
   TGraph* graph;
public:		// User declarations
   __fastcall TChartForm(TComponent* Owner);

   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TChartForm *ChartForm;
//---------------------------------------------------------------------------
#endif
