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
       "yyyy"
       "XXXX"
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

   // replace the xxxx macro with a list of attachments.
   string attachmentsString;
   for (unsigned i = 0; i != files.size(); i++)
      {
      string fullFile = files[i];
      string fullFileNoSpaces = fullFile;
      replaceAll(fullFileNoSpaces, " ", "_");
      if (!Str_i_Eq(fullFileNoSpaces, fullFile))
         CopyFile(fullFile.c_str(), fullFileNoSpaces.c_str(), false);

      string file = ExtractFileName(fullFile.c_str()).c_str();
      string fileNoSpaces = ExtractFileName(fullFileNoSpaces.c_str()).c_str();
      string fileUrlNoSpaces = webSession->getBaseURL() + "/files/" + fileNoSpaces;
      attachmentsString += "JMail.AddURLAttachment " + doubleQuoted(fileUrlNoSpaces)
                        + ", " + doubleQuoted(file) + "\n";
      }
   replaceAll(emailScript, "XXXX", attachmentsString);

   // replace the yyyy macro with the body of the email.
   vector<string> consultantEmails;
   data->getConsultantEmailAddresses(userName, consultantEmails);
   string body = "JMail.AppendText \"username=" + userName + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"paddockname=" + paddockName + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"useremail=" + data->getUserEmail(userName) + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"applicationname=" + webSession->getApplicationName() + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"applicationurl=" + webSession->getApplicationUrl() + "~~\" & vbCrLf\n"
               + "JMail.AppendText \"reportdescription=" + reportDescription + "~~\" & vbCrLf\n";
   for (unsigned i = 0; i != consultantEmails.size(); i++)
      body += "JMail.AppendText \"consultantemail" + lexical_cast<string>(i+1) + "=" + consultantEmails[i] + "~~\" & vbCrLf\n";
   replaceAll(emailScript, "yyyy", body);

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
                    bool generateSoilFile,
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
      string resetDateString = data->getProperty(userName, paddockName, "resetdate");
      string sowDateString = data->getProperty(userName, paddockName, "sowdate");
      if (resetDateString != "")
         firstRainfallDate = date(2004, 4, 1);
      else if (sowDateString != "")
         firstRainfallDate = date(from_string(sowDateString));

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

         if (generateTempFiles)
            {
            // generate a soil temperature file.
            string soilTempFileName = userName + ".soiltemp";
            string tempNames[2] = {"max_soilt", "min_soilt"};
            data->generateDataFile(userName, paddockName, vector<string>(tempNames, tempNames+2),
                                   webSession->getFilesDir() + "\\" + soilTempFileName,
                                   "grower.SoilTemps.data");
            files.push_back(soilTempFileName);

            // generate an air temperature file.
            string airTempFileName = userName + ".airtemp";
            string airtempNames[2] = {"patch_maxt", "patch_mint"};
            data->generateDataFile(userName, paddockName, vector<string>(airtempNames, airtempNames+2),
                                   webSession->getFilesDir() + "\\" + airTempFileName,
                                   "grower.AirTemps.data");
            files.push_back(airTempFileName);
            }
         }
      // generate a soil file if necessary.
      if (generateSoilFile)
         {
         string soilFileName;
         soilProfileModified = data->generateSoilFile(userName, paddockName,
                                                      webSession->getFilesDir(),
                                                      soilFileName);
         files.push_back(soilFileName);
         }

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

