//----------------------------------------------------------------------------
#ifndef ChildWinH
#define ChildWinH
//----------------------------------------------------------------------------
#include <Controls.hpp>
#include <Forms.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <Windows.hpp>
#include <System.hpp>
#include "TSelected_simulations.h"
#include <Db.hpp>
#include <DBTables.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ToolWin.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include "TAnalysis_chart.h"
#include "TProbability_chart.h"
#include <Chart.hpp>
#include <TeEngine.hpp>
#include <TeeProcs.hpp>
#include "OLEExcel.hpp"
#include <StdCtrls.hpp>
#include "TSOI.h"
#include "TAPSTable.h"
#include "TAPSTable_2_TDataSet.h"
#include "TAnalysis_panel.h"
#include <Menus.hpp>
#include "TSimulations.h"
#include "TSimulations_from_mdbs.h"
#include "MemTable.hpp"
#include "TGM_analysis.h"
#include "TChartSettingsForm.h"
//----------------------------------------------------------------------------
class TMDIChild : public TForm
{
__published:
   TDBGrid *Grid;
   TSplitter *Splitter;
   TMainMenu *MainMenu2;
   TMenuItem *ChartsMenu;
   TMenuItem *ChartsTimeSeriesMenu;
   TMenuItem *ChartsPieMenu;
   TMenuItem *ChartsDifferenceMenu;
   TMenuItem *ChartsProbabilityMenu;
   TMenuItem *N2;
   TMenuItem *ChartsViewDataMenu;
   TMenuItem *OptionsMenu;
   TMenuItem *OptionsSOIMenu;
   TMenuItem *ChartsPropertiesMenu;
   TMenuItem *ChartsNoChartMenu;
   TMenuItem *ChartsBoxMenu;
   TMenuItem *ChartsXYMenu;
   TMenuItem *ChartsSummaryTableMenu;
   TMenuItem *ChartsFrequencyMenu;
   TMenuItem *EditMenu;
   TMenuItem *EditCopyMenu;
   TMenuItem *OpionsSelectSimulationsMenu;
   TMenuItem *N3;
   TMenuItem *EditCopyWithoutMenu;
   TMenuItem *N4;
   TMenuItem *EditSendDatatoEXCELMenu;
   TMenuItem *N5;
   TMenuItem *OptionsPreferencesMenu;
   TMenuItem *N6;
   TMenuItem *N1;
   TDataSource *Grid_data_source;
   TOLEExcel *Excel;
   TSOI *SOI;
   TAPSTable_2_TDataSet *APSTable_2_TDataSet;
    TSelected_simulations *Raw_data;
    TSimulations *Selected_simulations;
   TGM_analysis *GM;
   TMenuItem *N7;
   TMenuItem *OptionsEconomicMenu;
   TMenuItem *ChartsViewSettingsMenu;
   TMenuItem *View1;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall SelectSimulations(TObject *Sender);
   void __fastcall TimeSeriesChart(TObject *Sender);
   void __fastcall PieChart(TObject *Sender);
   void __fastcall DifferenceChart(TObject *Sender);
   void __fastcall ProbabilityChart(TObject *Sender);
   void __fastcall ViewData(TObject *Sender);
   void __fastcall SendDataToEXCEL(TObject *Sender);
   void __fastcall SOIToggle(TObject *Sender);
   void __fastcall Properties(TObject *Sender);
   void __fastcall RawData(TObject *Sender);
   void __fastcall BoxChart(TObject *Sender);
   void __fastcall XYChart(TObject *Sender);

   void __fastcall SummaryTable(TObject *Sender);
   void __fastcall FrequencyChart(TObject *Sender);
   void __fastcall EditCopy(TObject *Sender);
   void __fastcall EditCopyWithout(TObject *Sender);
   void __fastcall OptionsPreferences(TObject *Sender);

   void __fastcall OptionsEconomicMenuClick(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall ChartsViewSettingsMenuClick(TObject *Sender);
   
   void __fastcall FormResize(TObject *Sender);
private:
   bool SOI_on;
   bool GM_on;
   bool Large_fonts;
   bool FirstTime;
   TToolBar* Toolbar;
   TChartSettingsForm* Settings_form;

   TAnalysis_panel* Analysis_panel;
   void Display_settings(void);
   void Read_soi_file_name (void);
   void Enable_options (void);
   void Select_simulations(void);
   void Create_chart (AnsiString Analysis_name);
   void Hook_components_together (void);
   void Refresh_components(void);
   void Edit_analysis_and_refresh(void);

   TToolButton* Get_button (const char* Button_name);

public:
	virtual __fastcall TMDIChild(TComponent *Owner);
   virtual __fastcall ~TMDIChild();
   void Set_all_simulations (TSimulations* All_simulations);
   void Set_toolbar (TToolBar* Toolbar);
   void SetPresentationFonts(bool Large_fonts);
};
//----------------------------------------------------------------------------
#endif
