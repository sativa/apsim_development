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

// static member variable declarations:
int TMDIChild::numObjects;
vector<int> TMDIChild::vHow_many_precede;
vector<int> TMDIChild::vHow_many_this_addin;
vector<ToolBarAddInBase*> TMDIChild::addIns;
vector<HINSTANCE> TMDIChild::dllHandles;
vector<AddInEventMap>  TMDIChild::Toolbar_events;
TToolBar* TMDIChild::Toolbar;



//---------------------------------------------------------------------
__fastcall TMDIChild::TMDIChild(TComponent *Owner)
	: TForm(Owner)
   {
   Analysis_panel = NULL;
   FirstTime = true;
   numObjects++;
   }
//---------------------------------------------------------------------
__fastcall TMDIChild::~TMDIChild()
//    changes: DAH - 5/12/00: fixing d383
//    changes: DAH - 10/5/01: adding toolbar addin stuff
   {
   OnResize = NULL;
   delete Settings_form;
   delete scenarios;

   if (numObjects == 1)
   {
      while (!addIns.empty()) {
         ToolBarAddInBase* ptr = addIns.back();
         addIns.pop_back();
         delete ptr;
         }
      while (!dllHandles.empty()) {
         HINSTANCE dllHandle = dllHandles.back();
         //FreeLibrary(dllHandle);
         dllHandles.pop_back();
         }
      vHow_many_precede.clear();
      vHow_many_this_addin.clear();
   }
   numObjects--;
}


//---------------------------------------------------------------------------
void __fastcall TMDIChild::FormShow(TObject *Sender)
   {
   bool success;
   scenarios = new Scenarios(success);
   // Create child settings window.
   Settings_form = new TChartSettingsForm(this);
   Settings_form->Parent = this;
   Settings_form->Show();
   Settings_form->OnClose = On_settings_form_close;

   // could load the toolbar addins here
   // for each toolbar addin, add a divider, and place the buttons after it
   if (numObjects == 1)
      loadAllToolbarAddIns();

   if (success)
      {
      SelectSimulations(NULL);
      }
   else
      this->Close();
   }

//---------------------------------------------------------------------------
void __fastcall TMDIChild::FormResize(TObject *Sender)
   {
   if (true /*WindowState == wsMaximized*/)
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

   ChartsSummaryMenu->Enabled = (scenarios->count() > 0);
   ChartsTimeSeriesMenu->Enabled = (scenarios->count() > 0);
   ChartsPieMenu->Enabled = (scenarios->count() > 0);
   ChartsDifferenceMenu->Enabled = (scenarios->count() > 0);
   ChartsFrequencyMenu->Enabled = (scenarios->count() > 0);
   ChartsProbabilityMenu->Enabled = (scenarios->count() > 0);
   ChartsXYMenu->Enabled = (scenarios->count() > 0);
   ChartsPropertiesMenu->Enabled = (Analysis_panel != NULL);
   ChartsViewDataMenu->Enabled = (scenarios->count() > 0);
   EditSendDatatoEXCELMenu->Enabled = (scenarios->count() > 0);

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
      Get_button ("Time_series_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Difference_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Pie_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Frequency_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Probability_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Summary_button")->Enabled = (scenarios->count() > 0);
      Get_button ("XY_button")->Enabled = (scenarios->count() > 0);
      Get_button ("Properties_button")->Enabled = (Analysis_panel != NULL);
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
   Drill_down_form->scenarios = scenarios;
//   Drill_down_form->SetPresentationFonts(Large_fonts);
   ok = (Drill_down_form->ShowModal() == mrOk);
   delete Drill_down_form;

   if (ok)
      {
      scenarios->getAllData(AllData);
      Force_refresh();
      Display_settings();
      }
   }
//---------------------------------------------------------------------------
bool TMDIChild::Create_chart(TAnalysis_panel* new_panel)
{
   Settings_form->Parent = this;
   TAnalysis_panel* saved_panel;
   saved_panel = Analysis_panel;
   Analysis_panel = new_panel;
   Analysis_panel->Init();
   Analysis_panel->Source_data = working;

   bool ok = Edit_panel();
   if (ok)
   {
      delete saved_panel;
      Hook_panel_to_this_form();
      Enable_options();
   }
   else
   {
      Analysis_panel = saved_panel;
   }
   if (ChartsViewSettingsMenu->Checked)
      Settings_form->BringToFront();
   return ok;
}


//---------------------------------------------------------------------------
bool TMDIChild::Edit_panel()
{
   if (Analysis_panel->Edit())
   {
      Analysis_panel->Refresh();
      APSTable_2_TDataSet->APSTable =  Analysis_panel->Destination_data;
      if (ChartsViewDataMenu->Checked)
         APSTable_2_TDataSet->Refresh();
      return true;
   }
   else
   {
      delete Analysis_panel;
      Analysis_panel = NULL;
      return false;
   }
}


//---------------------------------------------------------------------------
void TMDIChild::Force_refresh()
{
   working->storeData(*AllData);

   for (vector<ToolBarAddInBase*>::iterator t = addIns.begin();
         t != addIns.end(); t++)
   {
      (*t)->setWorkingData(scenarios, working);
      (*t)->doCalculations(*working);
   }                                     

   if (Analysis_panel != NULL)
   {
      Analysis_panel->Refresh();
      APSTable_2_TDataSet->APSTable =  Analysis_panel->Destination_data;
   }
   else
      APSTable_2_TDataSet->APSTable =  working;

   if (ChartsViewDataMenu->Checked)
      APSTable_2_TDataSet->Refresh();
}

//---------------------------------------------------------------------------
bool TMDIChild::Refresh_is_needed()
{
   // if any addin needs updating, need to update all addIns, otherwise do nothing
   bool update_necessary = false;
   for (vector<ToolBarAddInBase*>::iterator t = addIns.begin();
         t != addIns.end(); t++)
   {
      update_necessary = update_necessary || (*t)->needsUpdate();
   }
   return update_necessary;
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
void __fastcall TMDIChild::SummaryTable(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TSummary_panel(this);
   if (Create_chart(new_panel))
      {
      TSummary_panel* summaryPanel = dynamic_cast<TSummary_panel*>(new_panel);
      if (!ChartsViewDataMenu->Checked &&
          summaryPanel != NULL &&
          summaryPanel->ShowData())
         ViewData(Sender);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::TimeSeriesChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TTime_series_panel(this);
   Create_chart (new_panel);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::PieChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TPie_frequency_panel(this);
   Create_chart (new_panel);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::DifferenceChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TDifference_panel(this);
   Create_chart (new_panel);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::FrequencyChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TFrequency_panel(this);
   Create_chart (new_panel);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ProbabilityChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TProbability_panel(this);
   Create_chart (new_panel);
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::XYChart(TObject *Sender)
   {
   TAnalysis_panel* new_panel = new TXY_panel(this);
   Create_chart (new_panel);
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
   APSTable_2_TDataSet->Refresh();
   }
//---------------------------------------------------------------------------
void __fastcall TMDIChild::ChartsNoChartMenuClick(TObject *Sender)
   {
      delete Analysis_panel;
      Analysis_panel = NULL;
      APSTable_2_TDataSet->APSTable =  working;
      if (ChartsViewDataMenu->Checked)
         APSTable_2_TDataSet->Refresh();
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

   APSTable_2_TDataSet->Refresh();
   Excel->DataSetToExcel (APSTable_2_TDataSet);
   Excel->Visible = true;
   Screen->Cursor = Saved_Cursor;
   }
//---------------------------------------------------------------------------
void TMDIChild::Display_settings(void)
   {
   Settings_form->Settings_list->Text = "";

   vector<string> Simulation_names;
   scenarios->getScenarioNames(Simulation_names);

   string Text;
   for (vector<string>::iterator i = Simulation_names.begin();
                                 i != Simulation_names.end();
                                 i++)
      {
      scenarios->setCurrentScenario(*i);

      // build up a factor string.
      vector<string> factors;
      scenarios->getFactorNames(factors);
      Text += *i + ":";
      for (unsigned int i = 0; i < factors.size(); i++)
         {
         string value;
         Graphics::TBitmap* bitmap;
         scenarios->getFactorAttributes(factors[i], value, bitmap);

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
   Preferences_form->ShowModal();
   }
//---------------------------------------------------------------------------
void TMDIChild::Set_toolbar (TToolBar* toolbar)
   {
   if (numObjects == 1)
      Toolbar = toolbar;
   Enable_options();
   if (numObjects == 1)
   {
      decorateWithAddins();
      buildToolbarEvents();
   }
   pointToolBarToThisInstance();
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



void TMDIChild::loadAllToolbarAddIns(void)
   {
   // get a list of add-in filenames from the .ini file.
   Path iniPath(Application->ExeName.c_str());
   iniPath.Set_extension(".ini");

   Ini_file ini;
   ini.Set_file_name(iniPath.Get_path().c_str());
   list<string> addInFileNames;
   ini.Read_list("ToolBarAddins", "addin", addInFileNames);

   // Loop through all filenames, load the DLL, call the DLL to create an
   // instance of an AddInBase and store in our list of addins.
   for (list<string>::iterator a = addInFileNames.begin();
                               a != addInFileNames.end();
                               a++)
      {
      // look for add in parameters after a space.
      unsigned int posSpace = (*a).find(" ");
      string addInParameters;
      if (posSpace != string::npos)
         {
         addInParameters = (*a).substr(posSpace+1);
         (*a).erase(posSpace);
         }
      HINSTANCE dllHandle = LoadLibrary( (*a).c_str() );
      if (dllHandle != NULL)
         {
         dllHandles.push_back(dllHandle);

         ToolBarAddInBase* __stdcall (*createToolBarAddInProc) (const string& addInParameters);
         (FARPROC) createToolBarAddInProc = GetProcAddress(dllHandle, "createToolBarAddIn");
         if (createToolBarAddInProc != NULL)
            addIns.push_back( (*createToolBarAddInProc)(addInParameters) );
         }
      }

   }


void TMDIChild::decorateWithAddins()
{
   for (vector<ToolBarAddInBase*>::iterator t = addIns.begin();
         t != addIns.end(); t++)
   {
      int how_many_controls = Toolbar->ButtonCount;
      (*t)->decorateToolBar(Toolbar); // ASSUMPTION: THAT THE WRITER OF
                                      // decorateToolbar has added the buttons
                                      // to the right hand side of Toolbar

      int how_many_new_controls = Toolbar->ButtonCount - how_many_controls;
      vHow_many_precede.push_back(how_many_controls);
      vHow_many_this_addin.push_back(how_many_new_controls);
   }
}


void TMDIChild::buildToolbarEvents()
{
   for (unsigned int j = 0; j < vHow_many_precede.size(); j++)
   {
      int how_many_controls = vHow_many_precede[j];
      int how_many_new_controls = vHow_many_this_addin[j];

      for (int i = how_many_controls; i < how_many_controls+how_many_new_controls;
               i++)
      {
         TNotifyEvent onClick = Toolbar->Buttons[i]->OnClick;
         AddInEventMap map(Toolbar->Buttons[i], onClick, addIns[j]);
         Toolbar_events.push_back(map);
      }
   }
}


void TMDIChild::pointToolBarToThisInstance()
{
   for (unsigned int j = 0; j < vHow_many_precede.size(); j++)
   {
      int how_many_controls = vHow_many_precede[j];
      int how_many_new_controls = vHow_many_this_addin[j];

      for (int i = how_many_controls; i < how_many_controls+how_many_new_controls;
               i++)
      {
         Toolbar->Buttons[i]->OnClick = ToolBarAddInButtonClick;
      }
   }
}


void __fastcall  TMDIChild::ToolBarAddInButtonClick(TObject* Sender)
{
   TControl* sender = dynamic_cast<TControl*>(Sender);

   vector<AddInEventMap>::iterator i;
   i = find(Toolbar_events.begin(), Toolbar_events.end(), sender);

   if (i != Toolbar_events.end())
   {
      ToolBarAddInBase* addin = (*i).getToolBarAddIn();
      addin->setWorkingData(scenarios, working);
      TNotifyEvent trueOnClick = (*i).getEvent();
      trueOnClick(Sender);
   }
   if (Refresh_is_needed())
      Force_refresh();
}

void __fastcall TMDIChild::FormActivate(TObject *Sender)
{
   pointToolBarToThisInstance();
   Enable_options();
}
//---------------------------------------------------------------------------

