//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TWebSession.h"
#include "TUserDetailsForm.h"
#include "TUserManagementForm.h"
#include "TTemporalDataMinMaxForm.h"
#include "TTemporalDataForm.h"
#include "TReportManagementForm.h"
#include "TQuestionForm.h"
#include "TInfoForm.h"
#include "TReportsForm.h"
#include "TDropDownManagementForm.h"
#include "TClimateForecastForm.h"
#include "TSoilsForm.h"
#include "TMetStationForm.h"
#include "Utilities.h"
#include <general\string_functions.h>

using namespace boost::gregorian;


//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TWebSession::TWebSession (TComponent* Owner)
        : TIWUserSessionBase(Owner)
   {
   data = NULL;
   userDetailsForm = NULL;
   userManagementForm = NULL;
   temporalDataForm = NULL;
   temporalDataMinMaxForm = NULL;
   reportManagementForm = NULL;
   questionForm = NULL;
   infoForm = NULL;
   reportsForm = NULL;
   dropDownManagementForm = NULL;
   climateForecastForm = NULL;
   soilsForm = NULL;
   metStationForm = NULL;
   }
//---------------------------------------------------------------------------
// Session has been created - set ourselves up.
//---------------------------------------------------------------------------
void TWebSession::setup(TIWApplication* webApp)
   {
   webApplication = webApp;
   if (data == NULL)
      {
      try
         {
         // open database.
         AnsiString mdbName = webApplication->RunParams->Values["db"];
         if (mdbName == "")
            throw runtime_error("No MDB file has been specified.");
         AnsiString mdbFileName = GServerController->FilesDir + mdbName;
         if (!FileExists(mdbFileName))
            throw runtime_error("Cannot find file: " + string(mdbFileName.c_str()));
         data = new Data;
         data->open(mdbFileName.c_str());
         }
      catch (const exception& err)
         {
         webApplication->ShowMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// Session is closing - delete our pointer to the database.
//---------------------------------------------------------------------------
void __fastcall TWebSession::OnSessionDestroy(TObject *Sender)
   {
   delete data;
   }
//---------------------------------------------------------------------------
// Show the specified form.
//---------------------------------------------------------------------------
void TWebSession::show(TIWAppForm *form)
   {
//   ((TIWAppForm*)WebApplication->ActiveForm)->Release();
   form->Show();
   }
//---------------------------------------------------------------------------
// Return true if the user login is ok.
//---------------------------------------------------------------------------
bool TWebSession::loginOk(const std::string& userN,
                          const std::string& password)
   {
   try
      {
      userName = userN;
      return data->loginOk(userName, password);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      return false;
      }
   }
//---------------------------------------------------------------------------
// Return a full URL for this web site.
//---------------------------------------------------------------------------
std::string TWebSession::getBaseURL(void)
   {
   if (webApplication->AppURLBase != "")
      return "http://www.apsim.info/apsim/farmweb/data";
   else
      return "http://127.0.0.1:8888";
   }
//---------------------------------------------------------------------------
// Return a full URL for this web site.
//---------------------------------------------------------------------------
std::string TWebSession::getFilesDir(void)
   {
   string filesDir = GServerController->FilesDir.c_str();
   return filesDir.substr(0, filesDir.length()-1); // remove trailing backslash
   }
//---------------------------------------------------------------------------
// Allow the user to do a save? Return true if a save is allowed
//---------------------------------------------------------------------------
bool TWebSession::isSaveAllowed(void)
   {
   return !Str_i_Eq(userName, "visitor");
   }
//---------------------------------------------------------------------------
// Show the default form
//---------------------------------------------------------------------------
void TWebSession::showDefaultForm(const std::string& userN)
   {
   userName = userN;
   vector<string> paddockNames;
   data->getPaddocks(userName, paddockNames);
   if (paddockNames.size() > 0)
      showPaddockForm(userName, paddockNames[0], false, false);
   else
      showUserDetailsForm(userName);
   }
//---------------------------------------------------------------------------
// Show the user details form.
//---------------------------------------------------------------------------
void TWebSession::showUserDetailsForm(const std::string& userName)
   {
   try
      {
      if (userDetailsForm == NULL)
         {
         userDetailsForm = new TUserDetailsForm(this);
         setupForm(userDetailsForm);
         }
      userDetailsForm->setup(this, data, userName);
      show(userDetailsForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the rainfall form.
//---------------------------------------------------------------------------
void TWebSession::showRainfallForm(const std::string& userName,
                                   const string& paddockName,
                                   bool fromGrowerManagement)
   {
   try
      {
      if (temporalDataForm == NULL)
         {
         temporalDataForm = new TTemporalDataForm(this);
         setupForm(temporalDataForm);
         }
      temporalDataForm->setup(this, data, userName, paddockName, "rainfall (mm)", "patch_rain", fromGrowerManagement);
      show(temporalDataForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the soil temperature form.
//---------------------------------------------------------------------------
void TWebSession::showSoilTempForm(const std::string& userName,
                                   const string& paddockName,
                                   bool fromGrowerManagement)
   {
   try
      {
      if (temporalDataMinMaxForm == NULL)
         {
         temporalDataMinMaxForm = new TTemporalDataMinMaxForm(this);
         setupForm(temporalDataMinMaxForm);
         }
      temporalDataMinMaxForm->setup(this, data, userName, paddockName, "soil temperature", "min_soilt", "max_soilt", fromGrowerManagement);
      show(temporalDataMinMaxForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the air temperature form.
//---------------------------------------------------------------------------
void TWebSession::showAirTempForm(const std::string& userName,
                                  const string& paddockName,
                                  bool fromGrowerManagement)
   {
   try
      {
      if (temporalDataMinMaxForm == NULL)
         {
         temporalDataMinMaxForm = new TTemporalDataMinMaxForm(this);
         setupForm(temporalDataMinMaxForm);
         }
      temporalDataMinMaxForm->setup(this, data, userName, paddockName, "air temperature", "patch_mint", "patch_maxt", fromGrowerManagement);
      show(temporalDataMinMaxForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the userManagement form.
//---------------------------------------------------------------------------
void TWebSession::showUserManagementForm()
   {
   try
      {
      if (userManagementForm == NULL)
         {
         userManagementForm = new TUserManagementForm(this);
         setupForm(userManagementForm);
         }
      userManagementForm->setup(this, data, userName);
      show(userManagementForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the report Management form.
//---------------------------------------------------------------------------
void TWebSession::showReportManagementForm(unsigned pageNumber)
   {
   try
      {
      if (reportManagementForm == NULL)
         {
         reportManagementForm = new TReportManagementForm(this);
         setupForm(reportManagementForm);
         }
      reportManagementForm->setup(this, data, pageNumber);
      show(reportManagementForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the question form.
//---------------------------------------------------------------------------
void TWebSession::showQuestionForm(const std::string& prompt, TQuestionEvent callback)
   {
   if (questionForm == NULL)
      questionForm = new TQuestionForm(this);

   questionForm->setup(prompt.c_str(), callback);
   show(questionForm);
   }
//---------------------------------------------------------------------------
// Show the info form.
//---------------------------------------------------------------------------
void TWebSession::showInfoForm(const std::string& prompt1,
                               const std::string& prompt2,
                               const std::string& prompt3,
                               const std::string& prompt4,
                               TInfoEvent callback)
   {
   if (infoForm == NULL)
      infoForm = new TInfoForm(this);
   infoForm->setup(prompt1.c_str(), prompt2.c_str(), prompt3.c_str(), prompt4.c_str(),
                   callback);
   show(infoForm);
   }
//---------------------------------------------------------------------------
// Show the reports form.
//---------------------------------------------------------------------------
void TWebSession::showReportsForm(const std::string& userName,
                                 bool readOnly,
                                 bool fromUserManagement)
   {
   try
      {
      if (reportsForm == NULL)
         {
         reportsForm = new TReportsForm(this);
         setupForm(reportsForm);
         }
      reportsForm->setup(this, data, userName, fromUserManagement);
      show(reportsForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the Drop Down Management form.
//---------------------------------------------------------------------------
void TWebSession::showDropDownManagementForm()
   {
   try
      {
      if (dropDownManagementForm == NULL)
         {
         dropDownManagementForm = new TDropDownManagementForm(this);
         setupForm(dropDownManagementForm);
         }
      dropDownManagementForm->setup(this, data);
      show(dropDownManagementForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show an SQL form
//---------------------------------------------------------------------------
void TWebSession::showSQLForm(void)
   {
   showInfoForm("Enter SQL statement (be carefull with this option!):", "", "", "", SQLCallBack);
   }
//---------------------------------------------------------------------------
// Show the climate forecast form.
//---------------------------------------------------------------------------
void TWebSession::showClimateForecastForm()
   {
   try
      {
      if (climateForecastForm == NULL)
         {
         climateForecastForm = new TClimateForecastForm(this);
         setupForm(climateForecastForm);
         }
      climateForecastForm->setup(this, data);
      show(climateForecastForm);
      }
   catch (const exception& err)
      {
      webApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// SQL callback.
//---------------------------------------------------------------------------
void __fastcall TWebSession::SQLCallBack(bool okClicked,
                                         AnsiString text1,
                                         AnsiString text2,
                                         AnsiString text3,
                                         AnsiString text4)
   {
   if (okClicked)
      {
      try
         {
         data->executeSQL(text1.c_str());
         }
      catch (const exception& err)
         {
         showMessage(err.what());
         }
      }
   else
      showUserDetailsForm(userName);
   }
//---------------------------------------------------------------------------
// Fill the outlook bar with panels and items.
//---------------------------------------------------------------------------
int TWebSession::setupForm(TIWAppForm* form)
   {
   form->Title = getApplicationName().c_str();

   // create a rectangle to house the menu.
   TIWRectangle* menuFrame = new TIWRectangle(form);
   menuFrame->Parent = form;
   menuFrame->Width = 135;
   menuFrame->Align = alLeft;
   menuFrame->Color = clWebBEIGE;
   menuFrame->Caption = "";

   drawMenuHeader(form, 1, getApplicationName());
   drawMenuItem(form, 2, "User details", "users3_preferences.gif", onMenuItemClick);

   // add all paddocks.
   vector<string> paddockNames;
   data->getPaddocks(userName, paddockNames);
   for (unsigned i = 0; i != paddockNames.size(); i++)
      drawMenuItem(form, 3+i, paddockNames[i], "paddock.gif", onMenuItemClick);

   int itemIndex = paddockNames.size() + 3;
   drawMenuItem(form, itemIndex++, "Reports", "column-chart.gif", onMenuItemClick);

   if (data->userIsOfType(userName, Data::consultant) ||
       data->userIsOfType(userName, Data::administrator))
      {
      itemIndex++;
      drawMenuHeader(form, itemIndex++, "Consultant");
      drawMenuItem(form, itemIndex++, "Growers", "user1_add.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex++, "Climate forecast", "earth_preferences.gif", onMenuItemClick);
      }
   if (data->userIsOfType(userName, Data::administrator))
      {
      itemIndex++;
      drawMenuHeader(form, itemIndex++, "Administration");
      drawMenuItem(form, itemIndex++, "con/par gen.", "text_code_c.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex++, "ApsimReport gen.", "text_code.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex++, "Soils", "soils.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex++, "Met Stations", "metstations.gif", onMenuItemClick);

      drawMenuItem(form, itemIndex++, "DropDowns", "element_add.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex++, "SQL", "data.gif", onMenuItemClick);
      }
   return itemIndex;
   }
//---------------------------------------------------------------------------
// user has clicked on something.
//---------------------------------------------------------------------------
void __fastcall TWebSession::onMenuItemClick(TObject* sender)
   {
   TIWLink* link = dynamic_cast<TIWLink*>(sender);
   if (link != NULL)
      processClickItem(link->Caption);
   }
//---------------------------------------------------------------------------
// Process the menu item click event.
//---------------------------------------------------------------------------
void TWebSession::processClickItem(AnsiString caption)
   {
   if (caption == "User details")
      showUserDetailsForm(userName);
   else if (caption == "Growers")
      showUserManagementForm();
   else if (caption == "con/par gen.")
      showReportManagementForm(1);
   else if (caption == "ApsimReport gen.")
      showReportManagementForm(2);
   else if (caption == "Reports")
      showReportsForm(userName, true, false);
   else if (caption == "DropDowns")
      showDropDownManagementForm();
   else if (caption == "Climate forecast")
      showClimateForecastForm();
   else if (caption == "SQL")
      showSQLForm();
   else if (caption == "Soils")
      showSoilsForm();
   else if (caption == "Met Stations")
      showMetStationsForm();

   else
      showPaddockForm(userName, caption.c_str(), true, false);
   }
//---------------------------------------------------------------------------
// Display a new window given the specified URL and title.
//---------------------------------------------------------------------------
void TWebSession::newWindow(const std::string& url, const std::string& caption, bool fullScreen)
   {
   if (fullScreen)
      {
      TIWWindowOptions options;
      options << woResizable;
      options << woScrollBars;
      options << woButtons;
      options << woStatusBar;
      options << woMenuBar;
      webApplication->NewWindow(url.c_str(), caption.c_str(), 600, 700, options);
      }
   else
      webApplication->NewWindow(url.c_str(), caption.c_str(), 300, 300, TIWWindowOptions());
   }
//---------------------------------------------------------------------------
// Display a message on screen
//---------------------------------------------------------------------------
void TWebSession::showMessage(const std::string& message)
   {
   webApplication->ShowMessage(message.c_str());
   }
//---------------------------------------------------------------------------
// Display a new window given the specified URL and title.
//---------------------------------------------------------------------------
bool TWebSession::existProperties(const std::string& userName,
                                  const std::string& paddockName,
                                  const vector<string>& dataNames)
   {
   bool ok = true;
   for (unsigned i = 0; i != dataNames.size() && ok; i++)
      ok = (ok && data->getProperty(userName, paddockName, dataNames[i]) != "");
   return ok;
   }
//---------------------------------------------------------------------------
// Show the soils form.
//---------------------------------------------------------------------------
void TWebSession::showSoilsForm()
   {
   try
      {
      if (soilsForm == NULL)
         {
         soilsForm = new TSoilsForm(webApplication);
         setupForm(soilsForm);
         }
      soilsForm->setup(this, data);
      show(soilsForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the met stations form.
//---------------------------------------------------------------------------
void TWebSession::showMetStationsForm()
   {
   try
      {
      if (metStationForm == NULL)
         {
         metStationForm = new TMetStationForm(webApplication);
         setupForm(metStationForm);
         }

      metStationForm->setup(this, data);
      show(metStationForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//--------------------------------------------------------
// User has clicked on Generate report.
//----------------------------------------------------------
void TWebSession::onGenerateReportClick(AnsiString userName,
                                        AnsiString paddockName,
                                        AnsiString reportName,
                                        AnsiString reportDescription,
                                        const Data::Properties& properties,
                                        bool emailFiles)
   {
   currentPaddockName = paddockName;
   currentReportName = reportName;
   currentReportDescription = reportDescription;
   currentUserName = userName;
   if (reportDescription == "")
      {
      if (emailFiles)
         showInfoForm("Enter a descriptive name for the report:",
                      "Enter your email address:", "", "", createReportCallback);
      else
         showInfoForm("Enter a descriptive name for the report:", "", "", "", createReportCallback);
      }
   else if (emailFiles)
      showInfoForm("", "Enter your email address:", "", "", createReportCallback);

   else
      generateReportFilesAndEmail("", properties);
   }
//---------------------------------------------------------------------------
// User has finished entering an description and optionally an email address - send files.
//---------------------------------------------------------------------------
void __fastcall TWebSession::createReportCallback(bool okClicked,
                                                  AnsiString text1,
                                                  AnsiString text2,
                                                  AnsiString text3,
                                                  AnsiString text4)
   {
   showPaddockForm(currentUserName.c_str(), currentPaddockName.c_str(), false, false);

   if (okClicked)
      {
      if (text1 != "")
         currentReportDescription = text1;

      Data::Properties properties;
      generateReportFilesAndEmail(text2.c_str(), properties);
      }
   }
//---------------------------------------------------------------------------
// generate report files and send to the specified email address.
//---------------------------------------------------------------------------
void TWebSession::generateReportFilesAndEmail(AnsiString emailAddress,
                                              const Data::Properties& properties)
   {
   if (isSaveAllowed())
      {
      try
         {
         bool generateTempFiles = Str_i_Eq(getApplicationName(), "Afloman");
         generateReport(emailAddress.c_str(), this, data, currentUserName.c_str(), currentPaddockName.c_str(),
                        currentReportName.c_str(),
                        properties, generateTempFiles, currentReportDescription.c_str());
         }
      catch (const exception& err)
         {
         showMessage(err.what());
         }
      }
   else
      showMessage("A visitor is not allowed to create a new report.");
   }
//---------------------------------------------------------------------------
// Return a URL to the help page.
//---------------------------------------------------------------------------
void TWebSession::showHelp(void)
   {
   newWindow(data->getHelpUrl().c_str(), "Help", true);
   }

