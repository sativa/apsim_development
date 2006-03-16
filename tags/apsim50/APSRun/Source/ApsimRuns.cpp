//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\TreeNodeIterator.h>
#include <general\xml.h>
#include <general\path.h>
#include <general\exec.h>

#include <ApsimShared\ApsimComponentData.h>
#include <ApsimShared\ApsimServiceData.h>
#include <ApsimShared\ApsimSystemData.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ControlFileConverter.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>
#include <general\xml.h>
#include "ApsimRuns.h"
#include "TRunForm.h"

#define bzero(a) memset(a,0,sizeof(a)) //easier -- shortcut


#pragma package(smart_init)
using namespace std;

void ApsimRuns::clearSimulations()
   //---------------------------------------------------------------------------
   // Clear this object of all simulations.
   {
   fileNames.erase(fileNames.begin(), fileNames.end());
   simNames.erase(simNames.begin(), simNames.end());
   }

void ApsimRuns::addSimulationsFromFile(const std::string& fileName)
   //---------------------------------------------------------------------------
   // Add simulations from the specified file to the pending list of runs.
   {
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find file: " + fileName);

   string fileExtension = Path(fileName).Get_extension();
   if (fileExtension == ".run")
      {
      ApsimRunFile runFile(fileName);
      vector<string> runNames;
      runFile.getRuns(runNames);
      for (unsigned r = 0; r != runNames.size(); r++)
         {
         string fileName;
         vector<string> conFileSections;
         runFile.getRun(runNames[r], fileName, conFileSections);
         if (conFileSections.size() == 0)
            addSimulationsFromFile(fileName);
         else
            addSimulationsFromConFile(fileName, conFileSections);
         }
      }
   else if (fileExtension == ".con")
      {
      vector<string> conFileSections;
      addSimulationsFromConFile(fileName, conFileSections);
      }
   else if (fileExtension == ".sim")
      addSimulation(fileName, Path(fileName).Get_name_without_ext());
   else if (fileExtension == ".apsim")
      addSimulationsFromApsimFile(fileName);
   else
      throw runtime_error("Invalid simulation file: " + fileName);
   }

void ApsimRuns::addSimulationsFromConFile(const std::string& fileName,
                                          const std::vector<std::string>& sims)
   //---------------------------------------------------------------------------
   // Add the specified simulations from the specified CON file.
   {
   vector<string> sections = sims;
   if (sections.size() == 0)
      {
      ApsimControlFile con(fileName);
      con.getAllSectionNames(sections);
      }
   for (unsigned s = 0; s != sections.size(); s++)
      addSimulation(fileName, sections[s]);
   }

void ApsimRuns::addSimulation(const std::string& fileName, const std::string& simName)
   //---------------------------------------------------------------------------
   // Add a specific simulation to the pending list of runs.
   {
   fileNames.push_back(fileName);
   simNames.push_back(simName);
   }

void ApsimRuns::addSimulationsFromApsimFile(const std::string& fileName)
   //---------------------------------------------------------------------------
   // Add the specified simulations from the specified .APSIM file.
   {
   vector<string> simulationNames;
   XMLDocument doc(fileName);
   for (XMLNode::iterator node = doc.documentElement().begin();
                          node != doc.documentElement().end();
                          node++)
      if (Str_i_Eq(node->getName(), "simulation"))
         simulationNames.push_back(node->getAttribute("name"));

   for (unsigned s = 0; s != simulationNames.size(); s++)
      addSimulation(fileName, simulationNames[s]);
   }

void ApsimRuns::getFilesNeedingConversion(std::vector<std::string>& filesNeedingConversion)
   //---------------------------------------------------------------------------
   // Get a list of control files that need converting.
   {
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string fileName = fileNames[f];
      if (Path(fileName).Get_extension() == ".con"
          && ControlFileConverter::needsConversion(fileName))
         filesNeedingConversion.push_back(fileName);
      }
   }

void ApsimRuns::runApsim(bool quiet,  TApsimRunEvent notifyEvent, TApsimRunEvent msgEvent)
   //---------------------------------------------------------------------------
   // Perform all Apsim runs.
   {
   for (unsigned f = 0; f != fileNames.size() && !stopApsim; f++)
      {
      Path filePath(fileNames[f]);
      string simFileName = fileNames[f];
      try
         {
         // Convert file to .sim if necessary (i.e. if it's a .con or a .apsim
         if (filePath.Get_extension() == ".con")
            {
            Path currentDir = Path::getCurrentFolder();
            filePath.Change_directory();
            string commandLine = "\"" + getApsimDirectory() + "\\bin\\contosim.exe\" \""
                               + filePath.Get_name() + "\" \"" + simNames[f] + "\"";
            Exec(commandLine.c_str(), SW_HIDE, true);
            simFileName = filePath.Get_name_without_ext() + ".sim";
            }
         else if (filePath.Get_extension() == ".apsim")
            {
            Path currentDir = Path::getCurrentFolder();
            filePath.Change_directory();
            string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsimtosim.exe\" \""
                               + filePath.Get_name() + "\" \"" + simNames[f] + "\"";
            Exec(commandLine.c_str(), SW_HIDE, true);
            simFileName = simNames[f] + ".sim";
            }

         // go build a command line and pass it to ApsExec to do the real work.
         if (FileExists(simFileName.c_str()))
            {
            string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsim.exe\" ";
            commandLine += "\"" + simFileName + "\"";

            if (notifyEvent != NULL)
               notifyEvent(simFileName);

            ApsExec(commandLine.c_str(), msgEvent);
            }
         }
      catch (const runtime_error& err)
         {
         if (quiet)
            {
            filePath.Set_extension(".log");
            ofstream log(filePath.Get_path().c_str());
            log << err.what();
            }
         else
            ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   }

void ApsimRuns::convertFiles()
   //---------------------------------------------------------
   // This is only called by main program to call control file
   // converter on all simulations.
   {
   ControlFileConverter converter;

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      Path filePath(fileNames[f]);
      try
         {
         if (filePath.Get_extension() == ".con")
            {
            converter.convert(filePath.Get_path(),
                               (TControlFileConverterEvent)NULL);
            }
         }
      catch (const exception& err)
         {
         filePath.Set_extension("log");
         ofstream log(filePath.Get_path().c_str());
         log << err.what();
         }
      }
   }


bool IsWinNT()
   //------------------------------------------------------------------------------
   //check if we're running NT
   {
   OSVERSIONINFO osv;
   osv.dwOSVersionInfoSize = sizeof(osv);
   GetVersionEx(&osv);
   return (osv.dwPlatformId == VER_PLATFORM_WIN32_NT);
   }

void ThrowWindowsError()
   // --------------------------------------------------------------------------
   // Throw an exception using Windows API "GetLastError" for the error message.
   {
   LPVOID lpMsgBuf;
   FormatMessage(
       FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
       NULL,
       GetLastError(),
       MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
       (LPTSTR) &lpMsgBuf,
       0,
       NULL);

   string message = (char*) lpMsgBuf;
   LocalFree( lpMsgBuf );
   throw runtime_error(message);
   }


void ReadStdOut(HANDLE hChildStdoutRd, TApsimRunEvent msgEvent)
   // --------------------------------------------------------------------------
   // Read from the specified handle and send to msgEvent.
   {
   const int bufsize = 8192;
   static char buffer[bufsize];

   // Check to see if there is any data to read
   unsigned long nBytesRead;
   unsigned long nBytesAvail;
   if (!PeekNamedPipe(hChildStdoutRd,buffer,bufsize,&nBytesRead,&nBytesAvail,NULL))
      ThrowWindowsError();

   if (nBytesRead != 0)
      {
      bzero(buffer);
      if (nBytesAvail > bufsize)
         {
         while (nBytesRead >= bufsize)
             {
             if (!ReadFile(hChildStdoutRd,buffer, bufsize, &nBytesRead, NULL))
                ThrowWindowsError();
             if (msgEvent != NULL)
                 msgEvent(string(buffer, nBytesRead));
             bzero(buffer);
             }
         }
      else
         {
         if (!ReadFile(hChildStdoutRd,buffer,bufsize,&nBytesRead,NULL))
            ThrowWindowsError();

         if (msgEvent != NULL)
             msgEvent(string(buffer, nBytesRead));
         bzero(buffer);
         }
      }
   }

void ApsimRuns::ApsExec(const char* Command_line, TApsimRunEvent msgEvent)
   //------------------------------------------------------------------------------
   // Run a console process and capture stdout/err.
   {
   STARTUPINFO StartupInfo;
   PROCESS_INFORMATION ProcessInfo;

   memset(&StartupInfo, '\0', sizeof(STARTUPINFO));
   StartupInfo.cb = sizeof(StartupInfo);

   HANDLE hChildStdoutRd, hChildStdoutWr, hChildStdinRd;
   SECURITY_ATTRIBUTES sa;
   SECURITY_DESCRIPTOR sd;

   if (IsWinNT())        //initialize security descriptor (Windows NT)
      {
      InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
      SetSecurityDescriptorDacl(&sd, true, NULL, false);
      sa.lpSecurityDescriptor = &sd;
      }
   else
      sa.lpSecurityDescriptor = NULL;

   sa.nLength = sizeof(SECURITY_ATTRIBUTES);
   sa.bInheritHandle = true;
   sa.nLength = sizeof(sa);

   // Create the pipe apsim will write to
   if (! CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &sa, 0)) ThrowWindowsError();
   SetHandleInformation( hChildStdoutRd, HANDLE_FLAG_INHERIT, 0);

   // Stdin stream - /dev/null
   hChildStdinRd = CreateFileA("NUL:", GENERIC_WRITE, 0, &sa, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

   StartupInfo.hStdError = hChildStdoutWr;
   StartupInfo.hStdOutput = hChildStdoutWr;
   StartupInfo.hStdInput = hChildStdinRd;
   StartupInfo.wShowWindow = SW_HIDE;
   StartupInfo.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;

   if (!CreateProcess( NULL,
                       (char*) Command_line,   // pointer to command line string
                       NULL,                    // pointer to process security attributes
                       NULL,                    // pointer to thread security attributes
                       true,                  // handle inheritance flag
                       CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS,  // creation flags
                       NULL,                   // pointer to new environment block
                       NULL,                   // pointer to current directory name
                       &StartupInfo,           // pointer to STARTUPINFO
                       &ProcessInfo) )         // pointer to PROCESS_INF
      ThrowWindowsError();

   while (1)
      {
      Application->ProcessMessages();

      // See if we need to kill apsim.
      if (stopApsim)
         TerminateProcess(ProcessInfo.hProcess, -1);

      // Read stdout.
      if (!paused)
         ReadStdOut(hChildStdoutRd, msgEvent);

      // See if the process has exited
      unsigned long exitCode=0;
      if (!GetExitCodeProcess(ProcessInfo.hProcess, &exitCode))
         ThrowWindowsError();

      if (exitCode != STILL_ACTIVE)
          break;

      Sleep(100);
      }
   ReadStdOut(hChildStdoutRd, msgEvent);

   if (!CloseHandle(ProcessInfo.hProcess) ) ThrowWindowsError();
   if (!CloseHandle(ProcessInfo.hThread)) ThrowWindowsError();
   if (!CloseHandle(hChildStdoutRd)) ThrowWindowsError();
   if (!CloseHandle(hChildStdoutWr)) ThrowWindowsError();
   if (!CloseHandle(hChildStdinRd)) ThrowWindowsError();
   }

