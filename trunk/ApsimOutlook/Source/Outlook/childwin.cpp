//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ChildWin.h"
#include "TDrill_down_form.h"
#include "TPreferences_form.h"
#include <general\io_functions.h>
#include <general\date_class.h>
#include <general\vcl_functions.h>
#include <general\path.h>
#include <general\ini_file.h>
#include <assert.h>
#include <editchar.hpp>
#include <TProbability_panel.h>
#include <TTime_series_panel.h>
#include <TDifference_panel.h>
#include <TPie_frequency_panel.h>
#include <TXY_panel.h>
#include <TSummary_panel.h>
#include <TFrequency_panel.h>

//---------------------------------------------------------------------
#pragma link "TAnalysis_chart"
#pragma link "TAPSTable"
#pragma link "TAPSTable_2_TDataSet"
#pragma link "TAuto_size_panel"
#pragma link "MemTable"
#pragma link "kbmMemTable"
#pragma resource "*.dfm"

static const char* OPTIONS_SECTION = "options";
static const char* ECONOMICS_KEY_WORD = "economics";
//---------------------------------------------------------------------
__fastcall TMDIChild::TMDIChild(TComponent *Owner)
	: TForm(Owner)
   {
   Analysis_panel = NULL;
   FirstTime = true;
   }
//---------------------------------------------------------------------
__fastcall TMDIChild::~TMDIChild()
//    changes: DAH - 5/12/00: fixing d383
   {
   OnResize = NULL;
   delete Settings_form;
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::FormShow(TObject *Sender)
   {
   // Create child settings window.
   Settings_form = new TChartSettingsForm(this);
   Settings_form->Parent = this;
   Settings_form->Show();
   Settings_form->OnClose = On_settings_form_close;

   SelectSimulations(NULL);
   }

//---------------------------------------------------------------------------
void __fastcall TMDIChild::FormResize(TObject *Sender)
   {
   if (WindowState == wsMaximized)
      {
      Settings_form->Left = ClientWidth - 250;
      Settings_form->Top = 0;
      Settings_form->Width = 250;
      Settings_form->Height = 300;
      FirstTime = false;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::FormClose(TObject *Sender, TCloseAction &Action)
   {
	Action = caFree;
   }
//---------------------------------------------------------------------
void TMDIChild::Enable_options(void)
   {
   EditCopyMenu->Enabled = (Analysis_panel != NULL);
   EditCopyWithoutMenu->Enabled = (Analysis_panel != NULL);

   ChartsSummaryMenu->Enabled = (scenarios.count() > 0);
   ChartsTimeSeriesMenu->Enabled = (scenarios.count() > 0);
   ChartsPieMenu->Enabled = (scenarios.count() > 0);
   ChartsDifferenceMenu->Enabled = (scenarios.count() > 0);
   ChartsFrequencyMenu->Enabled = (scenarios.count() > 0);
   ChartsProbabilityMenu->Enabled = (scenarios.count() > 0);
   ChartsXYMenu->Enabled = (scenarios.count() > 0);
   ChartsPropertiesMenu->Enabled = (Analysis_panel != NULL);
   ChartsViewDataMenu->Enabled = (scenarios.count() > 0);
   EditSendDatatoEXCELMenu->Enabled = (scenarios.count() > 0);

   ChartsSummaryMenu->Checked = (dynamic_cast<TSummary_panel*> (Analysis_panel) != NULL);
   ChartsTimeSeriesMenu->Checked = (dynamic_cast<TTime_series_panel*> (Analysis_panel) != NULL);
   ChartsPieMenu->Checked = (dynamic_cast<TPie_frequency_panel*> (Analysis_panel) != NULL);
   ChartsDifferenceMenu->Checked = (dynamic_cast<TDifference_panel*> (Analysis_panel) != NULL);
   ChartsFrequencyMenu->Checked = (dynamic_cast<TFrequency_panel*> (Analysis_panel) != NULL);
   ChartsProbabilityMenu->Checked = (dynamic_cast<TProbability_panel*> (Analysis_panel) != NULL);
   ChartsXYMenu->Checked = (dynamic_cast<TXY_panel*> (Analysis_panel) != NULL);
   ChartsNoChartMenu->Checked = (Grid->Visible && Analysis_panel == NULL);

   // buttons on button bar.
   if (Get_button ("Time_series_button") != NULL)
      {
      Get_button ("Time_series_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Difference_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Pie_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Frequency_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Probability_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Summary_button")->Enabled = (scenarios.count() > 0);
      Get_button ("XY_button")->Enabled = (scenarios.count() > 0);
      Get_button ("Properties_button")->Enabled = (Analysis_panel != NULL && !ChartsSummaryMenu->Checked);
      }

   // setup button bar event handlers.
   if (Toolbar != NULL)
      {
      Get_button ("Time_series_button")->OnClick = TimeSeriesChart;
      Get_button ("Difference_button")->OnClick = DifferenceChart;
      Get_button ("Pie_button")->OnClick = PieChart;
      Get_button ("Frequency_button")->OnClick = FrequencyChart;
      Get_button ("Probability_button")->OnClick = ProbabilityChart;
      Get_button ("Summary_button")->OnClick = SummaryTable;
      Get_button ("XY_button")->OnClick = XYChart;
      Get_button ("Select_simulation_button")->OnClick = SelectSimulations;
      Get_button ("Properties_button")->OnClick = Properties;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::SelectSimulations(TObject *Sender)
   {
   bool ok;
   Drill_down_form = new TDrill_down_form(this);
   Drill_down_form->scenarios = &scenarios;
   ok = (Drill_down_form->ShowModal() == mrOk);
   delete Drill_down_form;

   if (ok)
      {
      scenarios.getAllData(AllData);
      Create_chart("Raw data");
      Display_settings();
      }
   }
//---------------------------------------------------------------------------
void TMDIChild::Create_chart(AnsiString Analysis_name)
   {
   Settings_form->Parent = this;
   TAnalysis_panel* Saved_panel = Analysis_panel;
   Analysis_panel = NULL;

   if (Analysis_name == "probability chart")
      Analysis_panel = new TProbability_panel(this);

   else if (Analysis_name == "time series chart")
      Analysis_panel = new TTime_series_panel(this);

   else if (Analysis_name == "difference chart")
      Analysis_panel = new TDifference_panel(this);

   else if (Analysis_name == "pie chart")
      Analysis_panel = new TPie_frequency_panel(this);

   else if (Analysis_name == "xy chart")
      Analysis_panel = new TXY_panel(this);

   else if (Analysis_name == "frequency chart")
      Analysis_panel = new TFrequency_panel(this);

   else if (Analysis_name == "summary table")
      Analysis_panel = new TSummary_panel(this);

   if (Analysis_panel != NULL)
      {
      Analysis_panel->Init();
      Hook_panel_to_this_form();
      Hook_components_together();
      if (!Edit_analysis_and_refresh())
         {
         delete Analysis_panel;
         Analysis_panel = Saved_panel;
         Hook_panel_to_this_form();
         Hook_components_together();
         }
      else
         delete Saved_panel;
      if (Saved_panel != NULL)
         Settings_form->Parent = Analysis_panel;
      }
   else
      {
      delete Saved_panel;
      Hook_components_together();
      Refresh_components();
      }
   if (Settings_form->Visible)
      Settings_form->Show();
   Enable_options();
   }
//---------------------------------------------------------------------------
void TMDIChild::Hook_panel_to_this_form (void)
   {
   if (Analysis_panel != NULL)
      {
      Analysis_panel->Parent = this;
      Analysis_panel->Align = alClient;
      Analysis_panel->Large_fonts = Large_fonts;

      Path p(Application->ExeName.c_str());
      p.Set_extension(".ini");
      Ini_file Ini;
      Ini.Set_file_name(p.Get_path().c_str());
      string Option;
      Ini.Read (OPTIONS_SECTION, "colour_background", Option);
      Analysis_panel->Colour_background = !Str_i_Eq(Option, "off");
      }
   }

//---------------------------------------------------------------------------
void TMDIChild::Hook_components_together (void)
   {
   // setup the analysis component.
   if (Analysis_panel != NULL)
      Analysis_panel->Source_data = AllData;

   // setup the apstable_2_dataset component
   if (Analysis_panel != NULL)
      APSTable_2_TDataSet->APSTable =  Analysis_panel->Destination_data;
   else
      APSTable_2_TDataSet->APSTable = AllData;
   }
//---------------------------------------------------------------------------
void TMDIChild::Refresh_components(void)
   {
   if (Analysis_panel != NULL)
      Analysis_panel->Refresh();

   APSTable_2_TDataSet->Refresh();

   // invalidate doesn't seem to work here!!!
   ClientWidth = ClientWidth + 1;
   ClientWidth = ClientWidth - 1;

   Display_settings();

   if (Analysis_panel != NULL && Analysis_panel->ShowData())
      {
      ChartsViewDataMenu->Checked = true;
      Grid->Visible = true;
      }
   }
//---------------------------------------------------------------------------
bool TMDIChild::Edit_analysis_and_refresh(void)
   {
   bool UserHitOk = false;
   if (Analysis_panel != NULL)
      {
      if (Analysis_panel->Edit())
         {
         Analysis_panel->Refresh();
         UserHitOk = true;
         }
      }
   if (Grid->Visible)
      APSTable_2_TDataSet->Refresh();
   Display_settings();
   return UserHitOk;
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::SummaryTable(TObject *Sender)
   {
   Create_chart ("summary table");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::TimeSeriesChart(TObject *Sender)
   {
   Create_chart ("time series chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::PieChart(TObject *Sender)
   {
   Create_chart ("pie chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::DifferenceChart(TObject *Sender)
   {
   Create_chart ("difference chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::BoxChart(TObject *Sender)
   {
   Create_chart ("box chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::FrequencyChart(TObject *Sender)
   {
   Create_chart ("frequency chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ProbabilityChart(TObject *Sender)
   {
   Create_chart ("probability chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::XYChart(TObject *Sender)
   {
   Create_chart ("xy chart");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::Properties(TObject *Sender)
   {
   Analysis_panel->Edit_chart();
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ViewData(TObject *Sender)
   {
   ChartsViewDataMenu->Checked = !ChartsViewDataMenu->Checked;
   Splitter->Visible = ChartsViewDataMenu->Checked;
   Grid->Visible = ChartsViewDataMenu->Checked;
   Hook_components_together();
   Refresh_components();
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ChartsNoChartMenuClick(TObject *Sender)
   {
   Create_chart ("raw data");
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::SendDataToEXCEL(TObject *Sender)
// Changes:
//    DAH - 10/8/2000:  saved and restored cursor
   {
   TCursor Saved_Cursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;
   if (!Excel->IsCreated() || !Excel->Visible)
      Excel->CreateExcelInstance();

   Excel->DataSetToExcel (APSTable_2_TDataSet);
   Excel->Visible = true;
   Screen->Cursor = Saved_Cursor;
   }
//---------------------------------------------------------------------------
void TMDIChild::Display_settings(void)
   {
   Settings_form->Settings_list->Text = "";

   vector<string> Simulation_names;
   scenarios.getScenarioNames(Simulation_names);

   string Text;
   for (vector<string>::iterator i = Simulation_names.begin();
                                 i != Simulation_names.end();
                                 i++)
      {
      scenarios.setCurrentScenario(*i);

      // build up a factor string.
      vector<string> factors;
      scenarios.getFactorNames(factors);
      Text += *i + ":";
      for (unsigned int i = 0; i < factors.size(); i++)
         {
         string value;
         Graphics::TBitmap* bitmap;
         scenarios.getFactorAttributes(factors[i], value, bitmap);

         Text += "\r\n";
         Text += "   " + factors[i] + "=" + value;
         }

      Text += "\r\n\r\n";
      }
   Settings_form->Settings_list->Text = Text.c_str();
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::EditCopy(TObject *Sender)
   {
   Analysis_panel->Copy_to_clipboard(true);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::EditCopyWithout(TObject *Sender)
   {
   Analysis_panel->Copy_to_clipboard(false);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::OptionsPreferences(TObject *Sender)
   {
   if (Preferences_form->ShowModal() == mrOk)
      Refresh_components();
   }
//---------------------------------------------------------------------------
void TMDIChild::Set_toolbar (TToolBar* toolbar)
   {
   Toolbar = toolbar;
   Enable_options();
   }
//---------------------------------------------------------------------------
TToolButton* TMDIChild::Get_button (const char* Button_name)
   {
   if (Toolbar != NULL)
      {
      for (int control = 0; control < Toolbar->ControlCount; control++)
         {
         TControl* Button = Toolbar->Controls[control];
         if (Button->Name == Button_name)
            return dynamic_cast<TToolButton*> (Button);
         }
      }
   return NULL;
   }
//---------------------------------------------------------------------------
void TMDIChild::SetPresentationFonts(bool large_fonts)
   {
   Large_fonts = large_fonts;
   if (Analysis_panel != NULL)
      Analysis_panel->Large_fonts = Large_fonts;

   if (Large_fonts)
      Settings_form->Settings_list->Font->Size = 14;

   else
      Settings_form->Settings_list->Font->Size = 8;
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ChartsViewSettingsMenuClick(TObject *Sender)
   {
   ChartsViewSettingsMenu->Checked = !ChartsViewSettingsMenu->Checked;
   Settings_form->Visible = ChartsViewSettingsMenu->Checked;
   if (Settings_form->Visible)
      Settings_form->Show();
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::On_settings_form_close(TObject* Sender, TCloseAction& Action)
   {
   ChartsViewSettingsMenu->Checked = false;
   }
//---------------------------------------------------------------------------

