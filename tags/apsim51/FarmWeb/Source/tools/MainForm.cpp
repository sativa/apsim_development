//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include <general\db_functions.h>
#include <general\string_functions.h>
#include <general\xml.h>
#include <soil\soilsample.h>
#include <general\math_functions.h>
#include <general\date_functions.h>
#include <boost\lexical_cast.hpp>
#include "..\Data.h"
#include <sstream>
using namespace boost;
using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompButton"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TIWForm1::TIWForm1(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
__fastcall TIWForm1::~TIWForm1()
   {
   delete connection;
   }
//---------------------------------------------------------------------------

void setAsMainForm() {
  TIWForm1::SetAsMainForm(__classid(TIWForm1));
 }
#pragma startup setAsMainForm

//---------------------------------------------------------------------------
// user has clicked go.
//---------------------------------------------------------------------------
void __fastcall TIWForm1::IWButton1Click(TObject *Sender)
   {
   changeReportDates();
   }
//---------------------------------------------------------------------------
// Change report dates.
//---------------------------------------------------------------------------
void TIWForm1::changeReportDates()
   {
   try
      {
      AnsiString mdbFileName = GServerController->FilesDir + "yp.mdb";
      open(mdbFileName.c_str());

      string sql = "SELECT Reports.* FROM Users, Reports WHERE users.name = 'Visitor' "
                   " and Users.id = Reports.userId";

      TDataSet* query = runQuery(connection, sql);
      query->First();
      while (!query->Eof)
         {
         int id = query->FieldValues["id"];
         string reportName = AnsiString(query->FieldValues["name"]).c_str();

         string timeString = splitOffBracketedValue(reportName, '(', ')');

         unsigned posDash = reportName.find_last_of('-');
         if (posDash != string::npos && timeString != "")
            {
            string dateString = reportName.substr(posDash+2);
            istringstream dateIn(dateString.c_str());
            short day, month, year;
            string monthStr;
            dateIn >> day >> monthStr >> year;
            bool found = false;
            for (month = 1; month <= 12 && !found; month++)
               found = Str_i_Eq(getShortMonthString(month), monthStr);

            if (found)
               {
               month--;

               int time = atoi(timeString.c_str());
               int hour = time / 100;
               int minute = time - hour * 100;

               double julianDay = TDateTime(year, month,day) - TDateTime(1899, 12, 30);
               int secsInDay = 24 * 60 * 60;
               julianDay += (hour * 24.0 * 60 + minute * 60.0) / secsInDay;
               TDateTime reportDate(julianDay);

               ostringstream sql;
               sql << "UPDATE [Reports] SET [date] = \'" << reportDate << "\'"
                   << " WHERE id = " << id;
               executeQuery(connection, sql.str());


               }
            }
         query->Next();
         }
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Copy reports from one paddock to another.
//---------------------------------------------------------------------------
void TIWForm1::copyReportsFromOnePaddockToAnother()
   {
   try
      {
      AnsiString mdbFileName = GServerController->FilesDir + "yp.mdb";

      Data data;
      data.open(mdbFileName.c_str());
      vector<string> reportNames;
      data.getReports("BCG", reportNames);

      for (unsigned i = 0; i != reportNames.size(); i++)
         {
         string reportName = reportNames[i];
         if (reportName.find("inetpub") == string::npos)
            {
            string fileName = reportName;
            stripLeadingTrailing(fileName, " ");
            unsigned posGIF = fileName.find("#GIF#");
            if (posGIF != string::npos)
               fileName.erase(posGIF, strlen("#GIF#"));
            fileName = string(GServerController->FilesDir.c_str()) + "Visitor - " + fileName.c_str() + ".gif";
            data.generateReport("BCG", reportName, fileName);
            }
         }
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Convert all soils
//---------------------------------------------------------------------------
void TIWForm1::convertSoils()
   {
   try
      {
      AnsiString mdbFileName = GServerController->FilesDir + "yp.mdb";
      open(mdbFileName.c_str());

      string sql = "SELECT * FROM Data WHERE name = 'SoilSample1' or name = 'SoilSample2'";

      TDataSet* query = runQuery(connection, sql);
      query->First();
      while (!query->Eof)
         {
         AnsiString oldXmlString = query->FieldValues["value"];

         XMLDocument oldXml(oldXmlString.c_str(), XMLDocument::xmlContents);
         SoilSample soilSample;

         vector<unsigned> thickness;
         vector<double> sw, no3, nh4, oc, ec, esp, ph;
         thickness = getLayeredInt(&oldXml, "thickness");
         if (thickness.size() > 0)
            {
            sw = getLayered(&oldXml, "sw");
            no3 = getLayered(&oldXml, "no3");
            nh4 = getLayered(&oldXml, "nh4");
            oc = getLayered(&oldXml, "oc");
            ec = getLayered(&oldXml, "ec");
            esp = getLayered(&oldXml, "esp");
            ph = getLayered(&oldXml, "ph");

            soilSample.setThickness(thickness);
            soilSample.setSw(sw);
            soilSample.setNo3(no3);
            soilSample.setNh4(nh4);
            soilSample.setOc(oc);
            soilSample.setEc(ec);
            soilSample.setEsp(esp);
            soilSample.setPh(ph);

            ostringstream newXmlString;
            soilSample.write(newXmlString);

            ostringstream sql;
            sql << "UPDATE [Data] SET [value] = " << singleQuoted(newXmlString.str())
                << " WHERE id = " << (int) query->FieldValues["id"];
            executeQuery(connection, sql.str());
            }

         query->Next();
         }
      delete query;


      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }

//---------------------------------------------------------------------------
// Open the specified database file. Will close any existing open connections.
// Will throw a runtime_error on error (e.g. file not found)
//---------------------------------------------------------------------------
void TIWForm1::open(const std::string& fileName)
   {
   if (FileExists(fileName.c_str()))
      {
      delete connection;
      connection = new TADOConnection(NULL);

      string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                                "Data Source=?;"
                                "Persist Security Info=False";
      replaceAll(connectionString, "?", fileName);
      connection->Connected = false;
      connection->LoginPrompt = false;
      connection->ConnectionString = connectionString.c_str();
      connection->Connected = true;
      }
   else
      throw runtime_error("Cannot find file: " + fileName);
   }
// ------------------------------------------------------------------
// Get a layer property from a specified layer
// ------------------------------------------------------------------
template<class T>
struct GetLayerValue
   {
   string name;
   vector<T>& values;
   GetLayerValue(const string& n, vector<T>& v)
      : name(n), values(v)
      {}

   void operator() (XMLNode& node)
      {
      XMLNode::iterator i = find_if(node.begin(), node.end(),
                                    EqualToName<XMLNode>(name));
      if (i == node.end())
         values.push_back(missingValue<T>());
      else
         {
         string value = i->getValue();
         stripLeadingTrailing(value, " ");
         values.push_back(lexical_cast<T>(value));
         }
      }
   };
// ------------------------------------------------------------------
// Return a specific layered data
// ------------------------------------------------------------------
vector<double> TIWForm1::getLayered(XMLDocument* doc, const char* layeredType)
   {
   vector<double> values;
   GetLayerValue<double> getLayerValue(layeredType, values);
   for_each_if(doc->documentElement().begin(),
               doc->documentElement().end(),
               getLayerValue,
               EqualToName<XMLNode>("layer"));
   return values;
   }
// ------------------------------------------------------------------
// Return a specific layered data
// ------------------------------------------------------------------
vector<unsigned> TIWForm1::getLayeredInt(XMLDocument* doc, const char* layeredType)
   {
   vector<unsigned> values;
   GetLayerValue<unsigned> getLayerValue(layeredType, values);
   for_each_if(doc->documentElement().begin(),
               doc->documentElement().end(),
               getLayerValue,
               EqualToName<XMLNode>("layer"));
   return values;
   }
//---------------------------------------------------------------------------

