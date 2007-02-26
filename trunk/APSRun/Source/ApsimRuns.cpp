//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <dir.h>

#include <general/string_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/path.h>

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

   if (fileExtensionEquals(fileName, "run"))
      {
      ApsimRunFile runFile(fileName);
      vector<string> runNames;
      runFile.getRuns(runNames);
      for (unsigned r = 0; r != runNames.size(); r++)
         {
         string fileName;
         vector<string> conFileSections;
         runFile.getRun(runNames[r], fileName, conFileSections);
         Replace_all(fileName, "%apsuite", getApsimDirectory().c_str());
         addSimulationsFromFile(fileName);
         }
      }
   else if (fileExtensionEquals(fileName, "con"))
      {
      vector<string> conFileSections;
      addSimulationsFromConFile(fileName, conFileSections);
      }
   else if (fileExtensionEquals(fileName, "sim"))
      addSimulation(fileName, fileRoot(fileName));
   else if (fileExtensionEquals(fileName, "apsim"))
      {
      XMLDocument doc(fileName);
      addSimulationsFromApsimFile(fileName, doc.documentElement());
      }
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

void ApsimRuns::addSimulationsFromApsimFile(const std::string& fileName, XMLNode parent)
   //---------------------------------------------------------------------------
   // Add the specified simulations from the specified .APSIM file.
   {
   for (XMLNode::iterator node = parent.begin();
                          node != parent.end();
                          node++)
      {
      if (Str_i_Eq(node->getName(), "simulation"))
         addSimulation(fileName, node->getAttribute("name"));
      else if (Str_i_Eq(node->getName(), "folder"))
         addSimulationsFromApsimFile(fileName, *node);
      }
   }

void ApsimRuns::getFilesNeedingConversion(std::vector<std::string>& filesNeedingConversion)
   //---------------------------------------------------------------------------
   // Get a list of control files that need converting.
   {
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string fileName = fileNames[f];
      if (fileExtension(fileName) == "con"
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
      string simFileName = fileNames[f];
      string summaryFileName;
      bool conversionOk = false;
      bool needsDeletion = true;

      // Convert file to .sim if necessary (i.e. if it's a .con or a .apsim)
      // There's a subtle difference in the name of each simulation that the converter writes
      if (fileExtensionEquals(simFileName, "con"))
         {
         // Delete any old simfiles lying about
         string newSimName = fileRoot(simFileName) + "." + simNames[f] + ".sim";
         if (fileExists(newSimName.c_str())) unlink(newSimName.c_str());

         string commandLine = "\"" + getApsimDirectory() + "\\bin\\contosim.exe\" \""
                            + simFileName + "\" \"" + simNames[f] + "\"";

         if (!ApsExec(commandLine.c_str(), NULL, msgEvent))
            {
            msgEvent(".con To .sim file conversion failed");
            conversionOk = false;
            }
         else
            conversionOk = true;

         simFileName = newSimName;
         summaryFileName = fileRoot(simFileName) + ".sum";
         }
      else if (fileExtensionEquals(simFileName, "apsim"))
         {
         // A sim file is created for each "simulation name" in the apsim file
         string newSimName;
         string dir = fileDirName(simFileName);
         if (dir == "")
            newSimName = simNames[f] + ".sim";
         else
            newSimName = dir + "\\" + simNames[f] + ".sim";

         // Delete any old simfiles lying about
         if (fileExists(newSimName.c_str())) unlink(newSimName.c_str());

         string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsimtosim.exe\" \""
                            + simFileName + "\" \"" + simNames[f] + "\"";

         if (!ApsExec(commandLine.c_str(), NULL, msgEvent))
            {
            msgEvent(".apsim to .sim file conversion failed");
            conversionOk = false;
            }
         else
            conversionOk = true;

         simFileName = newSimName;
         summaryFileName = fileRoot(newSimName) + ".sum";
         }
      else if (fileExtensionEquals(simFileName, "sim"))
         {
         // No conversion needed
         conversionOk = true;
         summaryFileName = fileRoot(simFileName) + ".sum";
         needsDeletion = false;
         }
      // go build a command line and pass it to ApsExec to do the real work.
      if (conversionOk)
         {
         string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsim.exe\" ";
         commandLine += "\"" + simFileName + "\"";

         if (fileExists(summaryFileName.c_str())) unlink(summaryFileName.c_str());

         if (notifyEvent != NULL)
            notifyEvent(simFileName);

         if (!ApsExec(commandLine.c_str(), summaryFileName.c_str(), msgEvent))
            {
            msgEvent("apsim simulation terminated with non-zero error code");
            }
         else 
            {
            if (needsDeletion) unlink(simFileName.c_str());
            }   
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
      try
         {
         if (fileExtension(fileNames[f]) == "con")
            {
            converter.convert(fileNames[f],
                               (TControlFileConverterEvent)NULL);
            }
         }
      catch (const exception& err)
         {
         ofstream log((fileRoot(fileNames[f]) + ".log").c_str());
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


void ReadStdOut(HANDLE hChildStdoutRd, FILE *summaryFP, TApsimRunEvent msgEvent)
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
             if (summaryFP) {fwrite(buffer, 1, nBytesRead, summaryFP);}
             bzero(buffer);
             }
         }
      else
         {
         if (!ReadFile(hChildStdoutRd,buffer,bufsize,&nBytesRead,NULL))
            ThrowWindowsError();

         if (msgEvent != NULL)
             msgEvent(string(buffer, nBytesRead));
         if (summaryFP) {fwrite(buffer, 1, nBytesRead, summaryFP);}
         bzero(buffer);
         }
      }
   }

bool ApsimRuns::ApsExec(const char* Command_line, 
                        const char * summaryFileName, 
                        TApsimRunEvent msgEvent)
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
   
   FILE *summaryFP;
   
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

   if (summaryFileName != NULL) {
      summaryFP = fopen(summaryFileName, "wb");
   } else {
      summaryFP = NULL;
   }

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

   unsigned long exitCode;
   while (1)
      {
      Application->ProcessMessages();

      // See if we need to kill apsim.
      if (stopApsim)
         TerminateProcess(ProcessInfo.hProcess, -1);

      // Read stdout.
      if (!paused)
         ReadStdOut(hChildStdoutRd, summaryFP, msgEvent);

      // See if the process has exited
      exitCode=0;
      if (!GetExitCodeProcess(ProcessInfo.hProcess, &exitCode))
         ThrowWindowsError();

      if (exitCode != STILL_ACTIVE)
          break;

      Sleep(100);
      }
   ReadStdOut(hChildStdoutRd, summaryFP, msgEvent);
   if (summaryFP) {fclose(summaryFP);}
   if (!CloseHandle(ProcessInfo.hProcess) ) ThrowWindowsError();
   if (!CloseHandle(ProcessInfo.hThread)) ThrowWindowsError();
   if (!CloseHandle(hChildStdoutRd)) ThrowWindowsError();
   if (!CloseHandle(hChildStdoutWr)) ThrowWindowsError();
   if (!CloseHandle(hChildStdinRd)) ThrowWindowsError();

   return (exitCode==0);
   }

