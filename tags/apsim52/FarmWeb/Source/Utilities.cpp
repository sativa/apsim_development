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
// work out how many simulations are in control file.
//---------------------------------------------------------------------------
int countSimulationsForUser(TWebSession* webSession, const string& userName)
   {
   string conFileName = webSession->getFilesDir() + "\\" + userName + ".con";
   char buffer[100];
   GetPrivateProfileString(NULL, NULL, NULL, buffer, sizeof(buffer), conFileName.c_str());
   int count = 0;
   char* ptr = buffer;
   while (*ptr != 0)
      {
      ptr = strchr(ptr, '\0');
      ptr++;
      count++;
      }
   return count;
   }

// -------------------------------------------
// create a job xml file and return it's name
// -------------------------------------------
string createJobXML(TWebSession* webSession, Data* data, const string& userName,
                    const string& reportDescription)
   {
   string DateTimeStamp = TDateTime::CurrentDateTime().FormatString("dd mmm yyyy(hnnam/pm)").c_str();
   string GIFFileName = DateTimeStamp + " " + reportDescription + ".gif";
   string userEmail = data->getUserEmail(userName);

   string xml = "<job owner=\"[USERNAME]\">\n"
                "   <localcommand>\n"
                "      <commandline>\"c:\\program files\\apsim42\\bin\\apsrun.exe\" /createsim \"[USERNAME].con\"</commandline>\n"
                "   </localcommand>\n"
                "   <cluster>\n";

   int count = countSimulationsForUser(webSession, userName);
   for (int i = 1; i <= count; i++)
      {
      string IAsString = string(IntToStr(i).c_str());
      xml += "      <commandline>\"[USERNAME]" + IAsString + ".bat\"</commandline>\n";
      }

   xml += "   </cluster>\n"
          "   <localcommand>\n"
          "	     <commandline>\"c:\\program files\\apsim42\\bin\\apsimreport.exe\" \"[USERNAME].report\" \"" + GIFFileName + "\"</commandline>\n"
          "   </localcommand>\n"
          "   <ftp>\n"
          "	     <from>" + GIFFileName + "</from>\n"
          "      <tosite>www.apsim.info</tosite>\n"
          "      <loginname>vs27262</loginname>\n"
          "      <loginpassword>Thor19216805</loginpassword>\n"
          "      <tofolder>/apsim/farmweb/data/files/[USERNAME]</tofolder>\n"
          "	  </ftp>\n"
          "	  <email>\n"
          "	     <to>" + userEmail + "</to>\n"
          "		  <subject>Automated reply from Afloman</subject>\n"
          "		  <body>Your report has been generated and stored on your web page.</body>\n"
          "	  </email>\n"
          "	  <archive>\n"
          "		  <to>[USERNAME]\\[USERNAME].zip</to>\n"
          "		  <description>" + reportDescription + ", Afloman</description>\n"
          "   </archive>\n"
          "</job>\n";
   replaceAll(xml, "[USERNAME]", userName);

   // write file.
   string xmlFileName = webSession->getFilesDir() + "\\" + userName + ".xml";
   ofstream out(xmlFileName.c_str());
   out << xml;
   return xmlFileName;
   }
// ----------------------------------
// Create a series of job._bat files
// ----------------------------------
void createJobBat(TWebSession* webSession, const string& userName, vector<string>& filesCreated)
   {
   int count = countSimulationsForUser(webSession, userName);
   for (int i = 1; i <= count; i++)
      {
      string IAsString = string(IntToStr(i).c_str());
      string batFileName = webSession->getFilesDir() + "\\" + userName + IAsString + "._bat";
      ofstream out(batFileName.c_str());
      if (count == 1)
         out << "\"c:\\program files\\apsim42\\bin\\apsim.exe\" \"" << userName << ".sim\"" << endl;
      else
         out << "\"c:\\program files\\apsim42\\bin\\apsim.exe\" \"" << userName << IAsString << ".sim\"" << endl;
      out << "echo Finished > Run" << IAsString << ".finished" << endl;
      filesCreated.push_back(batFileName);
      }
   }
//---------------------------------------------------------------------------
// Send an email requesting a report.
//---------------------------------------------------------------------------
void sendEmail(TWebSession* webSession,
               Data* data,
               const string& userName,
               const string& paddockName,
               const string& reportDescription,
               vector<string>& files,
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

   files.push_back(createJobXML(webSession, data, userName, reportDescription));
   createJobBat(webSession, userName, files);

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
      string alias = doubleQuoted(file);
      if (alias.find(".xml") != string::npos)
         alias = "\"job.xml\"";
      attachmentsString += "JMail.AddURLAttachment " + doubleQuoted(fileUrlNoSpaces)
                        + ", " + alias + "\n";
      }
   replaceAll(emailScript, "$ATTACHMENTS$", attachmentsString);

   // replace zzzz macro with recipients.
   string recipString;
   for (unsigned i = 0; i != toEmailAddresses.size(); i++)
      recipString += "JMail.AddRecipient \"" + toEmailAddresses[i] + "\"\n";
   replaceAll(emailScript, "zzzz", recipString);

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

