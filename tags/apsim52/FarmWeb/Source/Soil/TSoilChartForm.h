//---------------------------------------------------------------------------
#ifndef TSoilChartFormH
#define TSoilChartFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include <SEGReport\report.h>
//---------------------------------------------------------------------------
class TSoilChartForm : public TForm
   {
   __published:	// IDE-managed Components
      TToolBar *ToolBar1;
      TImageList *ImageList1;
      TToolButton *PrintButton;
      void __fastcall PrintButtonClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   private:	// User declarations
      Report report;
      void addRecords(TDataSet* dataset, TStringGrid* grid, int col,
                      AnsiString series, AnsiString title);

   public:		// User declarations
      __fastcall TSoilChartForm(TComponent* Owner);
      void populateFromGrid(TAdvStringGrid* grid, const std::string& seriesName);
   };
//---------------------------------------------------------------------------
extern PACKAGE TSoilChartForm *SoilChartForm;
//---------------------------------------------------------------------------
#endif
