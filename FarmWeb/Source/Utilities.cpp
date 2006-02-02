//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Utilities.h"
#include "Data.h"
#include <Soil\Soil.h>
#include "TWebSession.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\io_functions.h>
#include <IWHTMLControls.hpp>
#include <boost\lexical_cast.hpp>

using namespace boost;
using namespace boost::gregorian;

#pragma package(smart_init)

//---------------------------------------------------------------------------
// populate an edit box by setting the text property
//---------------------------------------------------------------------------
void populateEdit(TIWEdit* edit,
                  Data* data,
                  const string& userName,
                  const string& paddockName,
                  const string& dataName)
   {
   edit->Text = data->getProperty(userName, paddockName, dataName).c_str();
   }
//---------------------------------------------------------------------------
// populate a combo box by setting the text property and initialising
// the lookup items.
//---------------------------------------------------------------------------
void populateCombo(TIWComboBox* combo,
                   Data* data,
                   const string& userName,
                   const string& paddockName,
                   const string& dataName)
   {
   vector<string> values;
   data->getLookupValues(dataName, values);
   Stl_2_tstrings(values, combo->Items);

   AnsiString value = data->getProperty(userName, paddockName, dataName).c_str();
   combo->ItemIndex = combo->Items->IndexOf(value);
   }
//---------------------------------------------------------------------------
// populate a date picker control from the database for the specified
// data name.
//---------------------------------------------------------------------------
void populateDatePicker(TTIWDatePicker* datePicker,
                        Data* data,
                        const string& userName,
                        const string& paddockName,
                        const string& dataName)

   {
   string dateString = data->getProperty(userName, paddockName, dataName);
   if (dateString != "")
      {
      date d(from_string(dateString));
      datePicker->Day = d.day();
      datePicker->Month = d.month();
      datePicker->Year = d.year();
      }
   else
      datePicker->Date = TDateTime::CurrentDate();
   }
//---------------------------------------------------------------------------
// save an edit box value
//---------------------------------------------------------------------------
void saveEdit(TIWEdit* edit,
               Data* data,
               const string& userName,
               const string& paddockName,
               const string& dataName)
   {
   data->setProperty(userName, paddockName, dataName, edit->Text.c_str());
   }
//---------------------------------------------------------------------------
// save a combo box value
//---------------------------------------------------------------------------
void saveCombo(TIWComboBox* combo,
               Data* data,
               const string& userName,
               const string& paddockName,
               const string& dataName)
   {
   if (combo->ItemIndex >= 0)
      {
      string value = combo->Items->Strings[combo->ItemIndex].c_str();
      data->setProperty(userName, paddockName, dataName, value);
      }
   }
//---------------------------------------------------------------------------
// save a DatePicker value
//---------------------------------------------------------------------------
void saveDatePicker(TTIWDatePicker* datePicker,
                    Data* data,
                    const string& userName,
                    const string& paddockName,
                    const string& dataName)
   {
   string value = datePicker->Date.FormatString("yyyy-mm-dd").c_str();
   data->setProperty(userName, paddockName, dataName, value);
   }
// -------------------------------------------
// create a job xml file and return it's name
// -------------------------------------------
string createJobXML(const string& userName, const string& paddockName, const string& reportDescription)
   {
   string jobXml = webSession->getFilesDir() + "\\" + userName + ".xml";
   ofstream out(jobXml.c_str());
   out << "<job name=\"" + JobName + "\" owner=\"" + UserName + "\" folder=\"" + JobFolder + "\">\r\n" +
		 << "   <cluster>\r\n";
			int CommandNumber = 1;
			foreach (string File in Directory.GetFiles(JobFolder, "*.sim"))
				{
				JobXML += "      <commandline>job.bat " + Path.GetFileName(File) + " " + CommandNumber.ToString() + "</commandline>\r\n";
				CommandNumber++;
				}

			string ftpSite = ExtractParamFromMessage(MailItem, "applicationftp");
			string ftpLoginName = "apsru";
			string ftpFolder = ExtractParamFromMessage(MailItem, "reportdirectory");
			if (ftpSite == "")
				{
				// For Afloman only.
				ftpSite = "www.apsim.info";
				ftpLoginName = "vs27262";
				ftpFolder = "/apsim/farmweb/data/files";
				string ReportDescription = ExtractParamFromMessage(MailItem, "reportdescription");
				GIFFileName = UserName + " - " + ReportDescription + " - " + DateString + "(" + TimeString + ").gif";
				GIFFileName = GIFFileName.Replace(":", "");
				}

			JobXML +=
				"   </cluster>\r\n" +
				"   <localcommand>\r\n" +
				"	   <commandline>\"c:\\program files\\apsim42\\bin\\apsimreport.exe\" \"" + JobFolder + "\\" + ReportFileName + "\" \"" + GIFFileName + "\"</commandline>\r\n" +
				"   </localcommand>\r\n" +
				"	<ftp>\r\n" +
				"		<from>" + GIFFileName + "</from>\r\n" +
				"       <tosite>" + ftpSite + "</tosite>\r\n"+
			    "       <loginname>" + ftpLoginName + "</loginname>\r\n"+
				"       <loginpassword>Thor19216805</loginpassword>\r\n"+
				"       <tofolder>" + ftpFolder + "</tofolder>\r\n"+
				"	</ftp>\r\n" +
				"	<email>\r\n" +
				"		<to>" + ExtractParamFromMessage(MailItem, "useremail") + "</to>\r\n" +
				"		<subject>Automated reply from " + ExtractParamFromMessage(MailItem, "applicationname") + "</subject>\r\n" +
				"		<body>Your report has been generated and stored on your web page.</body>\r\n" +
				"	</email>\r\n" +
				"	<archive>\r\n" +
				"		<to>" + ArchiveFile + "</to>\r\n" +
				"		<description>" + ExtractParamFromMessage(MailItem, "reportdescription") + "</description>\r\n" +
				"	</archive>\r\n" +
				"</job>\r\n";



   vector<string> consultantEmails;
   data->getConsultantEmailAddresses(userName, consultantEmails);
   string applicationftp = "www.apsim.info";
   string reportUrl = "/apsim/farmweb/reports/" + userName;
   string reportDirectory = webSession->getFilesDir() + "..\\reports\\" + userName;
   CreateDirectory(reportDirectory);
   string body = "JMail.AppendText \"username=" + userName + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"paddockname=" + paddockName + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"useremail=" + data->getUserEmail(userName) + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"applicationname=" + webSession->getApplicationName() + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"applicationurl=" + webSession->getApplicationUrl() + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"reportdescription=" + reportDescription + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"applicationftp=" + applicationFtp + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"reportdirectory=" + reportUrl + "~~\" & vbCrLf\n";
   for (unsigned i = 0; i != consultantEmails.size(); i++)
      body += "JMail.AppendText \"consultantemail" + lexical_cast<string>(i+1) + "=" + consultantEmails[i] + "~~\" & vbCrLf\n";

   }
//---------------------------------------------------------------------------
// Send an email requesting a report.
//---------------------------------------------------------------------------
void sendEmail(TWebSession* webSession,
               Data* data,
               const string& userName,
               const string& paddockName,
               const string& reportDescription,
               const vector<string>& files,
               const vector<string>& toEmailAddresses)
   {
   string emailScriptFileName = webSession->getFilesDir() + "\\" + userName + "Mail.asp";
   string emailScript =
       "<%@ Language=VBScript %>\n"
       "<HTML>\n"
       "<HEAD>\n"
       "</HEAD>\n"
       "<BODY><%\n"
       "Set JMail = Server.CreateObject(\"JMail.Message\")\n"
       "JMail.Logging = True\n"
       "JMail.From = \"Dean.Holzworth@csiro.au\"\n"
       "JMail.FromName = \"APSIM web server\"\n"
       "zzzz"
       "JMail.AddHeader \"X-Originating-IP\", Request.ServerVariables(\"REMOTE_ADDR\")\n"
       "JMail.AddHeader \"X-Originating-URL\", Request.ServerVariables(\"URL\")\n"
       "JMail.Priority = 1\n"
       "JMail.Subject = \"APSIM run\"\n"
       "$ATTACHMENTS$"
       "JMail.Send (\"smtp-au.server-mail.com\")\n"
//       "Response.Write \"<PRE>\" & vbCRLF\n"
//       "Response.Write (JMail.Log)\n"
//       "Response.Write \"</PRE>\"\n"
       "JMail.Close\n"
       "set JMail = nothing\n"
       "%>\n"
       "<p>Your request for a report has been submitted. You will be informed "
       "via email once it has been generated</p>"
       "</BODY>\n"
       "</HTML>\n";

   files.push_back(createJobXML(userName, paddockName, reportDescription));

   // replace the $ATTACHMENTS$ macro with a list of attachments.
   string attachmentsString;
   for (unsigned i = 0; i != files.size(); i++)
      {
      string fullFile = files[i];
      string fullFileNoSpaces = fullFile;
      replaceAll(fullFileNoSpaces, " ", "_");
      if (!Str_i_Eq(fullFileNoSpaces, fullFile))
         {
         if (CopyFile(fullFile.c_str(), fullFileNoSpaces.c_str(), false) == 0)
            {
            char* lpMsgBuf;

            FormatMessage(
                FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                NULL,
                GetLastError(),
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                (LPTSTR) &lpMsgBuf,
                0,
                NULL);
            MessageBox( NULL, lpMsgBuf, "GetLastError", MB_OK|MB_ICONINFORMATION );
            LocalFree( lpMsgBuf );
            }
         }

      string file = ExtractFileName(fullFile.c_str()).c_str();
      string fileNoSpaces = ExtractFileName(fullFileNoSpaces.c_str()).c_str();
      string fileUrlNoSpaces = webSession->getBaseURL() + "/files/" + fileNoSpaces;
      attachmentsString += "JMail.AddURLAttachment " + doubleQuoted(fileUrlNoSpaces)
                        + ", " + doubleQuoted(file) + "\n";
      }
   replaceAll(emailScript, "$ATTACHMENTS$", attachmentsString);


   // replace zzzz macro with recipients.
   string recipString;
   for (unsigned i = 0; i != toEmailAddresses.size(); i++)
      recipString += "JMail.AddRecipient \"" + toEmailAddresses[i] + "\"\n";
   replaceAll(emailScript, "zzzz", recipString);

   // replace the wwww macro with VBScript to delete unwanted files.
/*   string deleteVBScript = "dim fso\n";
   deleteVBScript       += "Set fso = CreateObject(\"Scripting.FileSystemObject\")\n";
   for (unsigned i = 0; i != files.size(); i++)
      {
      string fullFile = webSession->getFilesDir() + ExtractFileName(files[i].c_str()).c_str();
      string fullFileNoSpaces = fullFile;
      replaceAll(fullFileNoSpaces, " ", "_");
      if (!Str_i_Eq(fullFileNoSpaces, fullFile))
         deleteVBScript += "fso.DeleteFile " + doubleQuoted(fullFileNoSpaces) + "\n";
      deleteVBScript += "fso.DeleteFile " + doubleQuoted(fullFile) + "\n";
      }
   deleteVBScript += "fso.DeleteFile " + doubleQuoted(emailScriptFileName) + "\n";
   replaceAll(emailScript, "wwww", deleteVBScript);
*/
   ofstream out(emailScriptFileName.c_str());
   out << emailScript;
   out.close();

   string emailScriptURL = webSession->getBaseURL() + "/files/" + userName + "Mail.asp";
   webSession->newWindow(emailScriptURL.c_str(), "SendMail");
   }

//---------------------------------------------------------------------------
// Email all report files to specified addresses. Return true if the
// soil water parameters were bounded.
//---------------------------------------------------------------------------
bool generateReport(std::string toEmailAddress,
                    TWebSession* webSession,
                    Data* data,
                    const string& userName,
                    const string& paddockName,
                    const string& reportName,
                    const Data::Properties& properties,
                    bool generateTempFiles,
                    const string& reportDesc)
   {
   if (toEmailAddress == "")
      toEmailAddress = data->getApsimRunEmailAddress();

   string reportDescription = reportDesc;
   removeInvalidFileNameChars(reportDescription);
   
   bool soilProfileModified = false;
   try
      {
      vector<string> files;
      data->generateReportFiles(userName, paddockName, reportName, properties,
                                webSession->getFilesDir(), files, reportDescription);

      // To generate the rainfall file try and use the resetdate if it exists.
      // Otherwise use the sowing date.
      date firstRainfallDate(pos_infin);
      string sowDateString = data->getProperty(userName, paddockName, "sowdate");
      date sowDate = date(from_string(sowDateString));
      if (sowDate.month() >= 4 && sowDate.month() <= 8)
         firstRainfallDate = date(sowDate.year(), 4, 1);
      else
         firstRainfallDate = date(sowDate.year(), 9, 1);

      if (!firstRainfallDate.is_infinity())
         {
         date yesterday(day_clock::local_day());
         yesterday = yesterday - date_duration(2);

         // generate a rainfall file for this grower.
         string rainfallFileName = webSession->getFilesDir() + "\\" + userName + ".rai";
         string rainName[1] = {"patch_rain"};
         data->generateCompleteDataFile(userName, paddockName, vector<string>(rainName, rainName+1),
                                        rainfallFileName, "grower.Rainfall.data",
                                        firstRainfallDate, yesterday);
         files.push_back(rainfallFileName);

         // generate an irrigation file for this grower.
         string irrigationFileName = webSession->getFilesDir() + "\\" + userName + ".irr";
         string irrigationName[1] = {"patch_irr"};
         data->generateCompleteDataFile(userName, paddockName, vector<string>(irrigationName, irrigationName+1),
                                        irrigationFileName, "grower.Irrigation.data",
                                        firstRainfallDate, yesterday);
         files.push_back(irrigationFileName);

         if (generateTempFiles)
            {
            // generate a soil temperature file.
            string soilTempFileName = webSession->getFilesDir() + "\\" + userName + ".soiltemp";
            string tempNames[2] = {"max_soilt", "min_soilt"};
            data->generateDataFile(userName, paddockName, vector<string>(tempNames, tempNames+2),
                                   soilTempFileName,
                                   "grower.SoilTemps.data");
            files.push_back(soilTempFileName);

            // generate an air temperature file.
            string airTempFileName = webSession->getFilesDir() + "\\" + userName + ".airtemp";
            string airtempNames[2] = {"patch_maxt", "patch_mint"};
            data->generateDataFile(userName, paddockName, vector<string>(airtempNames, airtempNames+2),
                                   airTempFileName,
                                   "grower.AirTemps.data");
            files.push_back(airTempFileName);
            }
         }
      // generate a soil file
      string soilFileName;
      bool withSW = !Str_i_Eq(webSession->getApplicationName(), "afloman");
      soilProfileModified = data->generateSoilFile(userName, paddockName,
                                                   webSession->getFilesDir(),
                                                   withSW,
                                                   soilFileName);
      files.push_back(soilFileName);

      vector<string> toEmailAddresses;
      toEmailAddresses.push_back(toEmailAddress);
      sendEmail(webSession, data, userName, paddockName, reportDescription, files, toEmailAddresses);
      }
   catch (const exception& err)
      {
      webSession->showMessage(err.what());
      }
   return soilProfileModified;
   }
static const int menuLeft = 5;
static const int itemLeft = menuLeft + 25;
static const int itemTop = 50;
static const int itemHeight = 30;
static const int itemIndent = 30;
//---------------------------------------------------------------------------
// Draw a menu header at the specified position
//---------------------------------------------------------------------------
void drawMenuHeader(TIWAppForm* form, int lineNumber, const string& text)
   {
   TIWLabel* item = new TIWLabel(form);
   item->Parent = form;
   item->Left = menuLeft;
   item->Top = itemTop + (lineNumber-1)*itemHeight + 7;
   item->Font->FontName = "Arial";
   item->Caption = text.c_str();
   item->Font->Color = clWebDARKBLUE;
   item->Font->Style << fsItalic << fsBold;
   }
//---------------------------------------------------------------------------
// Draw a menu item at the specified position
//---------------------------------------------------------------------------
void drawMenuItem(TIWAppForm* form, int lineNumber, const string& text,
                  const string& gif, TNotifyEvent event)
   {
   TIWLink* item = new TIWLink(form);
   item->Parent = form;
   item->Left = itemLeft;
   item->Top = itemTop + (lineNumber-1)*itemHeight + 7;
   item->Font->FontName = "Arial";
   item->Caption = text.c_str();
   item->Font->Color = clWebBLUE;
   item->OnClick = event;

   TIWImageFile* image = new TIWImageFile(form);
   image->Parent = form;
   image->ImageFile->Filename = gif.c_str();
   image->Left = menuLeft;
   image->Top = itemTop + (lineNumber-1)*itemHeight;
   }
//---------------------------------------------------------------------------
// populate the station number combo
//---------------------------------------------------------------------------
void populateStationNumberCombo(Data* data,
                                const std::string& userName,
                                const std::string& paddockName,
                                const std::string& region,
                                TIWComboBox* WeatherStationCombo)
   {
   if (region != "")
      {
      vector<string> names;
      data->getMetStations(region, names);
      Stl_2_tstrings(names, WeatherStationCombo->Items);

      AnsiString value = data->getProperty(userName, paddockName, "metstation").c_str();
      WeatherStationCombo->ItemIndex = WeatherStationCombo->Items->IndexOf(value);
      }
   else
      WeatherStationCombo->Items->Clear();
   }
//---------------------------------------------------------------------------
// remove unwanted grower soils from SoilCombo.
//---------------------------------------------------------------------------
void removeGrowerSoils(vector<string>& soilTypes, const string& userN)
   {
   string userName = userN;
   stripLeadingTrailing(userName, " ");
   vector<string> returnSoils;
   for (unsigned i = 0; i != soilTypes.size(); i++)
      {
      bool addSoil;
      unsigned posGrower = findSubString(soilTypes[i], "Grower soil:");
      if (posGrower == string::npos)
         addSoil = true;
      else
         {
         // only include soil if user name matches grower name.
         string growerName = soilTypes[i].substr(posGrower + strlen("Grower soil:"));
         stripLeadingTrailing(growerName, " ");
         addSoil = Str_i_Eq(growerName, userName);
         }
      if (addSoil)
         returnSoils.push_back(soilTypes[i]);
      }
   soilTypes = returnSoils;
   }
//---------------------------------------------------------------------------
// populate the soil type combo
//---------------------------------------------------------------------------
void populateSoilTypeCombo(Data* data,
                           const std::string& userName,
                           const std::string& paddockName,
                           const std::string& region,
                           TIWComboBox* SoilTypeCombo)
   {
   if (region != "")
      {
      vector<string> values;
      data->getSoils(region, values);
      removeGrowerSoils(values, userName);
      Stl_2_tstrings(values, SoilTypeCombo->Items);

      AnsiString value = data->getProperty(userName, paddockName, "soiltype").c_str();
      SoilTypeCombo->ItemIndex = SoilTypeCombo->Items->IndexOf(value);
      }
   }

