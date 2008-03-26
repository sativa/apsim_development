//---------------------------------------------------------------------------
#pragma hdrstop

#include <sstream>
#include <fstream>
#include <iostream>

#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/path.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>
#include <ApsimShared/SimCreator.h>
#include <ApsimShared/ApsimDirectories.h>

#include <ComponentInterface/Interfaces.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Messages.h>

#include <Protocol/Transport.h>
#include <Protocol/Computation.h>

#include "Simulation.h"

using namespace std;
using namespace protocol;

static const unsigned parentID = 0;
static const unsigned masterPMID = 0;

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Simulation::Simulation(void)
   {
   data = NULL;
   masterPM = NULL;
   initMessages();
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Simulation::~Simulation(void)
   {
   deleteMessages();
   delete data;
   }

// ------------------------------------------------------------------
// Start the simulation.
// ------------------------------------------------------------------
void Simulation::go(const string& simFilename)
   {
   // remove existing log file.
   string logFile = fileRoot(simFilename) + ".log";
   if (fileExists(logFile))
     unlink(logFile.c_str());

   try
      {
      ifstream in(simFilename.c_str());
      if (in)
         {
         init(simFilename);
         commence();
         }
      else
         throw runtime_error("Cannot find simulation file: " + simFilename);
      }
   catch (const runtime_error& error)
      {
      // At this point there is no log file to report the error so
      // create and write to a log file and also write to screen.
      logError(simFilename.c_str(), error.what());
      }
   catch (const char* msg)
      {
      logError(simFilename.c_str(), msg);
      }
   term();
   }

// ------------------------------------------------------------------
// Write the specified error to a log file and screen.
// ------------------------------------------------------------------
void Simulation::logError(const char* simFilename, const char* msg)
   {
   string logFile = fileRoot(simFilename) + ".log";
   ofstream log(logFile.c_str(), ios::app);
   log << endl;
   log << "APSIM Infrastructure Error" << endl;
   log << "--------------------------" << endl;
   log << msg << endl;
   cout << "APSIM Infrastructure Error" << endl;
   cout << "--------------------------" << endl;
   cout << msg << endl;
   }

// ------------------------------------------------------------------
//  Short description:
//    create the simulation.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Simulation::init(const string& fileName)
   {
   data = new ApsimSimulationFile(fileName);

   // get dll filename for master PM.
   string dllFilename = data->getExecutableFileName();
   replaceAll(dllFilename, "%apsuite", getApsimDirectory());

   // create a Master PM and give it to the transport layer.
   masterPM = new Computation(".MasterPM", dllFilename, "", parentID, masterPMID);
   Transport::getTransport().addComponent(masterPMID, ".MasterPM", masterPM);

   // get sdml contents.
   ifstream in(fileName.c_str());
   ostringstream sdml;
   sdml << in.rdbuf();

   // resolve any includes.
   string sdmlContents = sdml.str();
   resolveIncludes(sdmlContents);

   // initialise the simulation
   string pmName = ".MasterPM";
   Message* message = constructMessage(Init1, parentID, masterPMID , false,
                                       memorySize(sdmlContents) + memorySize(pmName) + memorySize(true));
   MessageData messageData(message);
   messageData << sdmlContents << pmName << true;
   Transport::getTransport().deliverMessage(message);
   deleteMessage(message);

   // initialise the simulation
   message = constructMessage(Init2, parentID, masterPMID, false, 0);
   Transport::getTransport().deliverMessage(message);
   deleteMessage(message);
   }

// ------------------------------------------------------------------
//  Short description:
//     terminate the run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Simulation::term(void)
   {
   if (masterPM != NULL)
      delete masterPM;
   }

// ------------------------------------------------------------------
//  Short description:
//    start this run

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Simulation::commence(void)
   {
   Message* message = newCommenceMessage(parentID, masterPMID);
   Transport::getTransport().deliverMessage(message);
   deleteMessage(message);
   }
// ------------------------------------------------------------------
//  Short description:
//    set the global log callback function

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Simulation::setMessageHook(IMessageHook* hook)
   {
   Transport::getTransport().setMessageHook(hook);
   }

// ------------------------------------------------------------------
// resolve any includes in the specified sdml.
// ------------------------------------------------------------------
void Simulation::resolveIncludes(string& sdml)
   {
   replaceAll(sdml, "%apsuite", getApsimDirectory());

   unsigned posInclude = sdml.find("<include>");
   while (posInclude != string::npos)
      {
      unsigned posFileName = posInclude + strlen("<include>");
      unsigned posEndFileName = sdml.find("</include>", posFileName);
      if (posEndFileName == string::npos)
         throw runtime_error("Cannot find </include> tag");
      string includeFileName = sdml.substr(posFileName, posEndFileName-posFileName);

      if (! fileExists(includeFileName))
         throw runtime_error("Cannot find include file: " + includeFileName);
      string contents;
      if (includeFileName.find(".ini") != string::npos)
         {
         SimCreator simCreator(true);
         contents = simCreator.convertIniToSim(includeFileName);
         }
      else
         {
         ostringstream buffer;
         ifstream in(includeFileName.c_str());
         buffer << in.rdbuf();
         contents = buffer.str();
         }

      unsigned posEndInclude = posEndFileName + strlen("</include>");
      sdml.replace(posInclude, posEndInclude - posInclude, contents);

      posInclude = sdml.find("<include>");
      }
   }

