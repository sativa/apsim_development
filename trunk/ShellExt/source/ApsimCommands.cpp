//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimCommands.h"
#include <general\string_functions.h>
#include <general\path.h>
#include <general\stream_functions.h>
#include <apsimshared\apsimdirectories.h>
#pragma package(smart_init)

//---------------------------------------------------------------------------
// Send all files to EXCEL.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall excelFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      // open input stream
      ifstream in (fileNames[file].c_str());

      // output output stream.
      Path OutPath (fileNames[file]);
      OutPath.Set_extension (".csv");
      ofstream out (OutPath.Get_path().c_str());

      // copy first two lines as is.
      string Line;
      getline (in, Line);
      out << Line << endl;
      getline (in, Line);
      out << Line << endl;

      // convert file.
      Convert_2_CSV(in, out);

      // close files.
      in.close();
      out.close();

      // give output file to EXCEL.
      ShellExecute (NULL, "open", OutPath.Get_path().c_str(), NULL, "", SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Send all files to APSVis.  Files is a CSV list of filenames.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsvisFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   // write response file.
   string responseFile = Path::getTempFolder().Get_path() + "\\response.file";
   ofstream out(responseFile.c_str());
   for (unsigned i = 0; i != fileNames.size(); ++i)
      out << fileNames[i] << endl;
   out.close();

   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\bin\\apsvis\" " + responseFile;
   WinExec(command.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------
// Send all files to Apsim Outlook.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimoutlookFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   // write response file.
   string responseFile = Path::getTempFolder().Get_path() + "\\response.file";
   ofstream out(responseFile.c_str());
   for (unsigned i = 0; i != fileNames.size(); ++i)
      out << fileNames[i] << endl;
   out.close();

   // pass response file to apsimoutlook.
   string command = "\"" + getApsimDirectory() + "\\bin\\apsimoutlook\" " + responseFile;
   WinExec(command.c_str(), SW_SHOW);
   }
//---------------------------------------------------------------------------
// Send all files to Apsim.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall runFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsrun\" \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Convert all files to SIM format.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall createSimFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned int file = 0; file != fileNames.size(); file++)
      {
      string command = "\"" + getApsimDirectory() + "\\bin\\apsrun\" /CreateSIM \"" + fileNames[file] + "\"";

      // run command
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// View a .out file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall viewFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = getApsimDirectory() + "\\bin\\Viewer \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Send all files to ApsimReport
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimReportFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      // pass response file to apsimoutlook.
      string command = "\"" + getApsimDirectory() + "\\bin\\ApsimReport\" \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }
//---------------------------------------------------------------------------
// Open an interface file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall interfaceFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = "explorer \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }

//---------------------------------------------------------------------------
// Open an .apsim file.
//---------------------------------------------------------------------------
extern "C" _export void __stdcall apsimFiles(const char* csvFiles)
   {
   vector<string> fileNames;
   Split_string(csvFiles, ",", fileNames);

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string command = getApsimDirectory() + "\\bin\\ApsimUI \"" + fileNames[f] + "\"";
      WinExec(command.c_str(), SW_SHOW);
      }
   }


