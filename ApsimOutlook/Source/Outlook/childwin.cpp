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
#include <TBox_panel.h>
#include <TXY_panel.h>
#include <TSummary_panel.h>
#include <TFrequency_panel.h>
#include <TGM_analysis.h>

//---------------------------------------------------------------------
#pragma link "TAnalysis_chart"
#pragma link "TProbability_chart"
#pragma link "TSOI"
#pragma link "TAPSTable"
#pragma link "TAPSTable_2_TDataSet"
#pragma link "TAuto_size_panel"
#pragma link "TSimulations"
#pragma link "TSimulations_from_mdbs"
#pragma link "TSelected_simulations"
#pragma link "MemTable"
#pragma link "TGM_analysis"
#pragma link "TGM_analysis"
#pragma link "kbmMemTable"
#pragma resource "*.dfm"

static const char* SOI_SECTION = "soi";
static const char* SOI_FILE_KEY_WORD = "soi file";
static const char* OPTIONS_SECTION = "options";
static const char* ECONOMICS_KEY_WORD = "economics";
//---------------------------------------------------------------------
__fastcall TMDIChild::TMDIChild(TComponent *Owner)
	: TForm(Owner)
   {
   Analysis_panel = NULL;
   SOI_on = false;
   GM_on = false;
   FirstTime = true;
//   DamEasy = new TDamEasy(this);
   }
//---------------------------------------------------------------------
__fastcall TMDIChild::~TMDIChild()
   {
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

   // disable the economics menu & button if necessary.
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Ini;
   Ini.Set_file_name(p.Get_path().c_str());
   string Option;
   Ini.Read (OPTIONS_SECTION, ECONOMICS_KEY_WORD, Option);
   OptionsEconomicMenu->Enabled = Str_i_Eq(Option, "on");
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
void TMDIChild::Set_all_simulations (TStringList* MDBFilenames)
   {
   Simulations_from_mdbs->Database_file_names = MDBFilenames;
   Raw_data->All_possible_simulations = Simulations_from_mdbs;

   Read_soi_file_name();
   SelectSimulations(NULL);
   FormActivate(this);
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

   ChartsSummaryTableMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsTimeSeriesMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsPieMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsDifferenceMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsFrequencyMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsProbabilityMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsBoxMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsXYMenu->Enabled = (Selected_simulations->Count() > 0);
   ChartsPropertiesMenu->Enabled = (Analysis_panel != NULL);
   ChartsViewDataMenu->Enabled = (Selected_simulations->Count() > 0);
   EditSendDatatoEXCELMenu->Enabled = (Selected_simulations->Count() > 0);
   OptionsSOIMenu->Enabled = (Selected_simulations->Count() > 0);

   ChartsSummaryTableMenu->Checked = (dynamic_cast<TSummary_panel*> (Analysis_panel) != NULL);
   ChartsTimeSeriesMenu->Checked = (dynamic_cast<TTime_series_panel*> (Analysis_panel) != NULL);
   ChartsPieMenu->Checked = (dynamic_cast<TPie_frequency_panel*> (Analysis_panel) != NULL);
   ChartsDifferenceMenu->Checked = (dynamic_cast<TDifference_panel*> (Analysis_panel) != NULL);
   ChartsFrequencyMenu->Checked = (dynamic_cast<TFrequency_panel*> (Analysis_panel) != NULL);
   ChartsProbabilityMenu->Checked = (dynamic_cast<TProbability_panel*> (Analysis_panel) != NULL);
   ChartsBoxMenu->Checked = (dynamic_cast<TBox_panel*> (Analysis_panel) != NULL);
   ChartsXYMenu->Checked = (dynamic_cast<TXY_panel*> (Analysis_panel) != NULL);
   ChartsNoChartMenu->Checked = (Grid->Visible && Analysis_panel == NULL);

   // buttons on button bar.
   if (Get_button ("Time_series_button") != NULL)
      {
      Get_button ("Time_series_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Difference_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Pie_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Box_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Frequency_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Probability_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Summary_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("XY_button")->Enabled = (Selected_simulations->Count() > 0);
      Get_button ("Properties_button")->Enabled = (Analysis_panel != NULL && !ChartsSummaryTableMenu->Checked);
      Get_button ("SOI_button")->Enabled = (Selected_simulations->Count() > 0);
      OptionsSOIMenu->Visible = Get_button ("SOI_button")->Visible;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::SelectSimulations(TObject *Sender)
   {
   bool ok;
   Drill_down_form->Simulations = Raw_data;
   ok = (Drill_down_form->ShowModal() == mrOk);

   if (ok)
      {
      Raw_data->Refresh();
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

   else if (Analysis_name == "box chart")
      Analysis_panel = new TBox_panel(this);

   else if (Analysis_name == "xy chart")
      Analysis_panel = new TXY_panel(this);

   else if (Analysis_name == "summary table")
      Analysis_panel = new TSummary_panel(this);

   else if (Analysis_name == "frequency chart")
      Analysis_panel = new TFrequency_panel(this);

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
         {
         delete Saved_panel;
         Refresh_components();
         }
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
   // setup the SOI component.
   if (SOI_on)
      {
      SOI->Source_dataset = Raw_data;
      if (GM_on)
         DamEasy->Source_dataset = SOI;
      }
   else
      {
      if (GM_on)
         DamEasy->Source_dataset = Raw_data;
      }

   // setup the analysis component.
   if (Analysis_panel != NULL)
      {
      if (GM_on)
         Analysis_panel->Source_data = DamEasy;
      else if (SOI_on)
         Analysis_panel->Source_data = SOI;
      else
         Analysis_panel->Source_data = Raw_data;
      }

   // setup the apstable_2_dataset component
   if (Analysis_panel != NULL)
      APSTable_2_TDataSet->APSTable =  Analysis_panel->Destination_data;
   else if (GM_on)
      APSTable_2_TDataSet->APSTable = DamEasy;
   else if (SOI_on)
      APSTable_2_TDataSet->APSTable = SOI;
   else
      APSTable_2_TDataSet->APSTable = Raw_data;
   }
//---------------------------------------------------------------------------
void TMDIChild::Refresh_components(void)
   {
   if (SOI_on)
      SOI->Refresh();
   if (GM_on)
      DamEasy->Refresh();
   if (Analysis_panel != NULL)
      Analysis_panel->Refresh();

   APSTable_2_TDataSet->Refresh();

   // invalidate doesn't seem to work here!!!
   ClientWidth = ClientWidth + 1;
   ClientWidth = ClientWidth - 1;
//   Invalidate();
   Display_settings();
   }
//---------------------------------------------------------------------------
bool TMDIChild::Edit_analysis_and_refresh(void)
   {
   bool UserHitOk = false;
   if (Analysis_panel != NULL)
      {
      if (SOI_on)
         SOI->Refresh();
      if (GM_on)
         DamEasy->Refresh();
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
   Grid->Visible = false;
   ViewData(NULL);
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
void __fastcall TMDIChild::SOIToggle(TObject *Sender)
   {
   bool OriginalSOIStatus = SOI_on;
   if (!SOI_on)
      SOI_on = SOI->Edit();
   else
      SOI_on = false;

   if (OriginalSOIStatus != SOI_on)
      {
      Hook_components_together();
      Refresh_components();
      }
    OptionsSOIMenu->Checked = SOI_on;
   }
//---------------------------------------------------------------------------
void TMDIChild::Display_settings(void)
   {
   Settings_form->Settings_list->Text = "";

   list<string> Simulation_names;
   Raw_data->Get_selected_simulation_names(Simulation_names);

   string Text;
   int Simulation_number = 1;
   for (list<string>::iterator i = Simulation_names.begin();
                               i != Simulation_names.end();
                               i++)
      {
      TSimulation Selected_simulation;
      Raw_data->Get_selected_simulation((*i).c_str(), Selected_simulation);

      // add all title bits to display string
      string Title = Selected_simulation.Get_title();
      Replace_all (Title, ";", "\r\n   ");

      // prefix display string with a name.
      Text += Selected_simulation.Get_name() + ":\r\n   " + Title;

      // add SOI to settings list.
      if (SOI_on)
         {
         Text += "SOI phase = ";
         char Month_st[50];
         GDate d(1, SOI->Phase_month, 1990);
         d.Set_write_format("MMMMMM");
         d.Write(Month_st);
         Text += Month_st;
         Text += "\r\n";
         }
      Text += "\r\n";
      Simulation_number++;
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
      {
      Read_soi_file_name();
      Refresh_components();
      }
   }
//---------------------------------------------------------------------------
void TMDIChild::Read_soi_file_name (void)
   {
   // read a list of directory names from .ini file.
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Ini;
   Ini.Set_file_name(p.Get_path().c_str());
   string SOI_file_name;
   Ini.Read (SOI_SECTION, SOI_FILE_KEY_WORD, SOI_file_name);
   SOI->SOI_data_file = SOI_file_name.c_str();
   }
//---------------------------------------------------------------------------
void TMDIChild::Set_toolbar (TToolBar* toolbar)
   {
   Toolbar = toolbar;
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
      {
      SOI->FontHeight = 14;
      Settings_form->Settings_list->Font->Size = 14;
      }

   else
      {
      SOI->FontHeight = 8;
      Settings_form->Settings_list->Font->Size = 8;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::OptionsEconomicMenuClick(TObject *Sender)
   {
   if (DamEasy->Edit())
      {
      GM_on = true;
      Hook_components_together();
      Refresh_components();
      }
      
   OptionsEconomicMenu->Checked = GM_on;
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
void __fastcall TMDIChild::FormActivate(TObject *Sender)
   {
   Enable_options();
   ShowCursor(true);

   // setup button bar event handlers.
   if (Get_button ("Time_series_button") != NULL)
      {
      Get_button ("Time_series_button")->OnClick = TimeSeriesChart;
      Get_button ("Difference_button")->OnClick = DifferenceChart;
      Get_button ("Pie_button")->OnClick = PieChart;
      Get_button ("Box_button")->OnClick = BoxChart;
      Get_button ("Frequency_button")->OnClick = FrequencyChart;
      Get_button ("Probability_button")->OnClick = ProbabilityChart;
      Get_button ("Summary_button")->OnClick = SummaryTable;
      Get_button ("XY_button")->OnClick = XYChart;
      Get_button ("Select_simulation_button")->OnClick = SelectSimulations;
      Get_button ("Properties_button")->OnClick = Properties;
      Get_button ("SOI_button")->OnClick = SOIToggle;
      Get_button ("GM_button")->OnClick = OptionsEconomicMenuClick;
      }
   }
//---------------------------------------------------------------------------

