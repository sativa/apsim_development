//---------------------------------------------------------------------------

#ifndef TWebSessionH
#define TWebSessionH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <IWUserSessionBase.hpp>
#include <IWApplication.hpp>
#include <IWInit.hpp>
#include <IWOutlookBar.hpp>
#include <Controls.hpp>
#include <ImgList.hpp>
#include <IWAppForm.hpp>
#include "Data.h"
class TTemporalDataForm;
class TDataForm;
class TTemporalDataMinMaxForm;
class TUserDetailsForm;
class TQuestionForm;
class TInfoForm;
class TUserManagementForm;
class TReportManagementForm;
class TReportsForm;
class TDropDownManagementForm;
class TClimateForecastForm;
class TSoilsForm;
class TMetStationForm;
class TGenerateForm;
class TViewReportForm;

typedef void __fastcall (__closure *TQuestionEvent)(bool userClickedYes);
typedef void __fastcall (__closure *TInfoEvent)(bool OkClicked,
                                                AnsiString text1,
                                                AnsiString text2,
                                                AnsiString text3,
                                                AnsiString text4);
typedef void __fastcall (__closure *TReportCallback)(bool OkClicked,
                                                     AnsiString reportDescription,
                                                     const Data::Properties& properties);
//---------------------------------------------------------------------------
// This class looks after all information about a user session.
//---------------------------------------------------------------------------
class TWebSession : public TIWUserSessionBase
   {
   __published:	// IDE-managed Components
      void __fastcall OnSessionDestroy(TObject *Sender);
   public:
      //---------------------------------------------------------------------------
      // constructor
      //---------------------------------------------------------------------------
        __fastcall TWebSession(TComponent* Owner);

      //---------------------------------------------------------------------------
      // Session has been created - set ourselves up.
      //---------------------------------------------------------------------------
      void setup(TIWApplication* webApp);
        
      //---------------------------------------------------------------------------
      // Return true if the user login is ok.
      //---------------------------------------------------------------------------
      bool loginOk(const std::string& userName,
                   const std::string& password);

      //---------------------------------------------------------------------------
      // Return currently logged in user.
      //---------------------------------------------------------------------------
      std::string getCurrentLoggedInUser(void) {return userName;}

      //---------------------------------------------------------------------------
      // Return a full URL for this web site.
      //---------------------------------------------------------------------------
      std::string getBaseURL(void);

      //---------------------------------------------------------------------------
      // Return the files directory - with a trailing backslash.
      //---------------------------------------------------------------------------
      std::string getFilesDir(void);

      //---------------------------------------------------------------------------
      // Get the application name
      //---------------------------------------------------------------------------
      virtual std::string getApplicationName(void) = 0;

      //---------------------------------------------------------------------------
      // Get the application url
      //---------------------------------------------------------------------------
      virtual std::string getApplicationUrl(void) = 0;

      //---------------------------------------------------------------------------
      // Show the default form
      //---------------------------------------------------------------------------
      void showDefaultForm(const std::string& userName);

      //---------------------------------------------------------------------------
      // Show the user details form.
      //---------------------------------------------------------------------------
      void showUserDetailsForm(const std::string& userName);

      //---------------------------------------------------------------------------
      // Show the paddock form.
      //---------------------------------------------------------------------------
      virtual void showPaddockForm(const std::string& userName,
                                   const std::string& paddockName,
                                   bool readOnly,
                                   bool fromGrowerManagement) = 0;

      //---------------------------------------------------------------------------
      // Show the rainfall form.
      //---------------------------------------------------------------------------
      void showRainfallForm(const std::string& userName,
                            const std::string& paddockName,
                            bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the irrigation form.
      //---------------------------------------------------------------------------
      void showIrrigationForm(const std::string& userName,
                              const std::string& paddockName,
                              bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the soil temp form.
      //---------------------------------------------------------------------------
      void showSoilTempForm(const std::string& userName,
                            const std::string& paddockName,
                            bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the air temp form.
      //---------------------------------------------------------------------------
      void showAirTempForm(const std::string& userName,
                           const std::string& paddockName,
                           bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the data form.
      //---------------------------------------------------------------------------
      void showDataForm(const std::string& userName,
                        const std::string& paddockName,
                        bool fromGrowerManagement);

      //---------------------------------------------------------------------------
      // Show the reports form.
      //---------------------------------------------------------------------------
      void showReportsForm(const std::string& userName, bool readOnly,
                           bool fromUserManagement);

      // -----------------------------------------
      // Show the generate report form.
      // -----------------------------------------
      void showGenerateReportForm(const std::string& userName, const std::string& paddockName, bool readOnly);


      // -----------------------------------------
      // Show the view report form.
      // -----------------------------------------
      void showViewReportForm(const std::string& fileName, const std::string& userName,
                              const std::string& paddockName, bool backToReportsForm);

      //---------------------------------------------------------------------------
      // Show the question form.
      //---------------------------------------------------------------------------
      void showQuestionForm(const std::string& prompt, TQuestionEvent callback);

      //---------------------------------------------------------------------------
      // Show the info form.
      //---------------------------------------------------------------------------
      void showInfoForm(const std::string& prompt1,
                        const std::string& prompt2,
                        const std::string& prompt3,
                        const std::string& prompt4,
                        TInfoEvent callback);

      //---------------------------------------------------------------------------
      // Display a new window given the specified URL and title.
      //---------------------------------------------------------------------------
      void newWindow(const std::string& url, const std::string& caption, bool fullScreen=false);

      //---------------------------------------------------------------------------
      // Display a message on screen
      //---------------------------------------------------------------------------
      void showMessage(const std::string& message);

      //---------------------------------------------------------------------------
      // Display a new window given the specified URL and title.
      //---------------------------------------------------------------------------
      bool existProperties(const std::string& userName,
                           const std::string& paddockName,
                           const std::vector<std::string>& dataNames);
      //---------------------------------------------------------------------------
      // Display a climate forecast form.
      //---------------------------------------------------------------------------
      void showClimateForecastForm(void);

      //---------------------------------------------------------------------------
      // Show the userManagement form.
      //---------------------------------------------------------------------------
      void showUserManagementForm();

      //---------------------------------------------------------------------------
      // Show the specified form.
      //---------------------------------------------------------------------------
      void show(TIWAppForm* form);

      //--------------------------------------------------------
      // User has clicked on Generate report.
      //----------------------------------------------------------
      void onGenerateReportClick(AnsiString userName,
                                 AnsiString PaddockName,
                                 AnsiString ReportName,
                                 AnsiString ReportDescription,
                                 const Data::Properties& properties,
                                 bool emailFiles);

      //---------------------------------------------------------------------------
      // Allow the user to do a save? Return true if a save is allowed
      //---------------------------------------------------------------------------
      bool isSaveAllowed(void);

      //---------------------------------------------------------------------------
      // Return a URL to the help page.
      //---------------------------------------------------------------------------
      void showHelp(void);

   protected:
      Data* data;
      std::string userName;
      TIWApplication* webApplication;

      //---------------------------------------------------------------------------
      // Fill the outlook bar with panels and items.
      //---------------------------------------------------------------------------
      virtual int setupForm(TIWAppForm* form);

      //---------------------------------------------------------------------------
      // Process the menu item click event.
      //---------------------------------------------------------------------------
      virtual void processClickItem(AnsiString caption);

      //---------------------------------------------------------------------------
      // user has clicked on something.
      //---------------------------------------------------------------------------
      void __fastcall onMenuItemClick(TObject* sender);


   private:
      TUserDetailsForm* userDetailsForm;
      TTemporalDataForm* temporalDataForm;
      TDataForm* dataForm;
      TTemporalDataMinMaxForm* temporalDataMinMaxForm;
      TUserManagementForm* userManagementForm;
      TQuestionForm* questionForm;
      TInfoForm* infoForm;
      TReportManagementForm* reportManagementForm;
      TReportsForm* reportsForm;
      TDropDownManagementForm* dropDownManagementForm;
      TClimateForecastForm* climateForecastForm;
      TSoilsForm* soilsForm;
      TMetStationForm* metStationForm;
      TGenerateForm* generateForm;
      TViewReportForm* viewReportForm;

      // these next 3 variables are only used during a report generation.
      AnsiString currentReportName;
      AnsiString currentReportDescription;
      AnsiString currentPaddockName;
      AnsiString currentUserName;


      //---------------------------------------------------------------------------
      // Show the ReportManagement form.
      //---------------------------------------------------------------------------
      void showReportManagementForm(unsigned pageNumber);

      //---------------------------------------------------------------------------
      // Show the Drop Down Management form.
      //---------------------------------------------------------------------------
      void showDropDownManagementForm();

      //---------------------------------------------------------------------------
      // Send an email requesting a report.
      //---------------------------------------------------------------------------
      void sendEmail(const string& paddockName,
                     const std::vector<std::string>& files,
                     const std::vector<std::string>& toEmailAddresses);
      //---------------------------------------------------------------------------
      // Report properties form callback.
      //---------------------------------------------------------------------------
      void __fastcall ReportPropertiesCallBack(bool okClicked,
                                               AnsiString text1,
                                               AnsiString text2,
                                               AnsiString text3,
                                               AnsiString text4);

      //---------------------------------------------------------------------------
      // Show an SQL form
      //---------------------------------------------------------------------------
      void showSQLForm(void);

      //---------------------------------------------------------------------------
      // SQL callback.
      //---------------------------------------------------------------------------
      void __fastcall SQLCallBack(bool OkClicked,
                                  AnsiString text1,
                                  AnsiString text2,
                                  AnsiString text3,
                                  AnsiString text4);

      //---------------------------------------------------------------------------
      // Show the soils form.
      //---------------------------------------------------------------------------
      void showSoilsForm();

      //---------------------------------------------------------------------------
      // Show the met stations form.
      //---------------------------------------------------------------------------
      void showMetStationsForm();

      //---------------------------------------------------------------------------
      // User has finished entering an description and optionally an email address - send files.
      //---------------------------------------------------------------------------
      void __fastcall TWebSession::createReportCallback(bool okClicked,
                                                        AnsiString text1,
                                                        AnsiString text2,
                                                        AnsiString text3,
                                                        AnsiString text4);

      //---------------------------------------------------------------------------
      // generate report files and send to the specified email address.
      //---------------------------------------------------------------------------
      void generateReportFilesAndEmail(AnsiString emailAddress,
                                       const Data::Properties& properties);

   };


extern ::TWebSession* WebSession();

#endif