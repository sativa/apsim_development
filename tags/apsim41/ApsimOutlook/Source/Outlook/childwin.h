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
#include <Db.hpp>
#include <DBTables.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ToolWin.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <Chart.hpp>
#include <TeEngine.hpp>
#include <TeeProcs.hpp>
#include <StdCtrls.hpp>
#include <Menus.hpp>
#include "TAnalysis_chart.h"
#include "OLEExcel.hpp"
#include "TAPSTable.h"
#include "TAPSTable_2_TDataSet.h"
#include "TAnalysis_panel.h"
#include "TChartSettingsForm.h"
#include "kbmMemTable.hpp"
#include "Scenarios.h"
#include "ToolBarAddIn.h"
#include <DB.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include <ApsimShared\ApsimSettings.h>

class AddInEventMap;  // see bottom of page for class definition
// a random comment of the day.
//----------------------------------------------------------------------------
class TMDIChild : public TForm
{
__published:
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
   TMenuItem *ChartsPropertiesMenu;
   TMenuItem *ChartsXYMenu;
   TMenuItem *ChartsSummaryMenu;
   TMenuItem *ChartsFrequencyMenu;
   TMenuItem *EditMenu;
   TMenuItem *EditCopyMenu;
   TMenuItem *OpionsSelectSimulationsMenu;
   TMenuItem *N3;
   TMenuItem *EditCopyWithoutMenu;
   TMenuItem *N4;
   TMenuItem *EditSendDatatoEXCELMenu;
   TMenuItem *OptionsPreferencesMenu;
   TMenuItem *N6;
   TMenuItem *N1;
   TDataSource *Grid_data_source;
   TAPSTable_2_TDataSet *APSTable_2_TDataSet;
   TMenuItem *ChartsViewSettingsMenu;
   TMenuItem *View1;
   TMenuItem *ChartsNoChartMenu;
   TAPSTable *AllData;
   TAPSTable *working;
   TDBAdvStringGrid *Grid;
   TMenuItem *CopyScenarioMenu;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall SelectSimulations(TObject *Sender);
   void __fastcall TimeSeriesChart(TObject *Sender);
   void __fastcall PieChart(TObject *Sender);
   void __fastcall DifferenceChart(TObject *Sender);
   void __fastcall ProbabilityChart(TObject *Sender);
   void __fastcall ViewData(TObject *Sender);
   void __fastcall SendDataToEXCEL(TObject *Sender);
   void __fastcall Properties(TObject *Sender);
//   void __fastcall BoxChart(TObject *Sender);
   void __fastcall XYChart(TObject *Sender);

   void __fastcall SummaryTable(TObject *Sender);
   void __fastcall FrequencyChart(TObject *Sender);
   void __fastcall EditCopy(TObject *Sender);
   void __fastcall EditCopyWithout(TObject *Sender);
   void __fastcall OptionsPreferences(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
   void __fastcall ChartsViewSettingsMenuClick(TObject *Sender);

   void __fastcall FormResize(TObject *Sender);
   void __fastcall ChartsNoChartMenuClick(TObject *Sender);
   void __fastcall FormActivate(TObject *Sender);
   void __fastcall CopyScenarioMenuClick(TObject *Sender);
private:
   bool Large_fonts;
   bool FirstTime;
   static int numObjects;
   static TToolBar* Toolbar;
   TChartSettingsForm* Settings_form;
   Scenarios* scenarios;
   ApsimSettings settings;
   static std::vector<ToolBarAddInBase*> addIns;
   static std::vector<HINSTANCE> dllHandles;

   TAnalysis_panel* Analysis_panel;
   void Display_settings(void);
   void Enable_options (void);
   void Select_simulations(void);
   bool Create_chart (TAnalysis_panel*);
   void Hook_panel_to_this_form (void);
   void __fastcall On_settings_form_close(TObject* Sender, TCloseAction& Action);

   TToolButton* Get_button (const char* Button_name);
   void loadAllToolbarAddIns(void);
   void decorateWithAddins(void);
   void Force_refresh();
   bool Refresh_is_needed();
   void Refresh_panel();


   bool Edit_panel();

   static std::vector<AddInEventMap>  Toolbar_events;
   static std::vector<int> vHow_many_precede;        // these used to build Toolbar_events
   static std::vector<int> vHow_many_this_addin;     // for each child window
   void buildToolbarEvents();
   void pointToolBarToThisInstance();

   void __fastcall  ToolBarAddInButtonClick(TObject* Sender);




public:
	virtual __fastcall TMDIChild(TComponent *Owner);
   virtual __fastcall ~TMDIChild();
   void Set_toolbar (TToolBar* Toolbar);
   void SetPresentationFonts(bool Large_fonts);
};


class AddInEventMap
{
   private:
      ToolBarAddInBase* addin;
      TControl* control;
      TNotifyEvent event;

   public:
      AddInEventMap(TControl* Control, TNotifyEvent Event, ToolBarAddInBase* Addin)
         : control(Control), event(Event), addin(Addin)   {};
      operator== (TControl* rhs)
         {  return rhs == control;    };
      TNotifyEvent getEvent()
         {  return event;  };
      ToolBarAddInBase* getToolBarAddIn()
         {  return addin;  };
};


//----------------------------------------------------------------------------
#endif
