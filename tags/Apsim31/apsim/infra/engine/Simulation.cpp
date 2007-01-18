//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Simulation.h"
#include <sstream>
#include <fstream>
#include <general\path.h>
#include <ComponentInterface\MessageDataExt.h>
#include <dir.h>

#include <ComponentInterface\messages.h>
#include <Protocol\Transport.h>
#include <Protocol\Computation.h>

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
   try
      {
      ifstream in(simFilename.c_str());
      if (in)
         {
         Path currentPath = Path::getCurrentFolder();
         Path simPath(simFilename);
         simPath.Change_directory();
         init(simPath.Get_name());
         commence();
         currentPath.Change_directory();
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
   Path logPath(simFilename);
   logPath.Set_extension(".log");
   ofstream log(logPath.Get_path().c_str());
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

   // create a Master PM and give it to the transport layer.
   masterPM = new Computation("MasterPM", dllFilename, parentID, masterPMID);
   Transport::getTransport().addComponent(masterPMID, "MasterPM", masterPM);

   // get sdml contents.
   ifstream in(fileName.c_str());
   ostringstream sdml;
   sdml << in.rdbuf();

   // initialise the simulation
   string pmName = "MasterPM";
   Message* message = constructMessage(Init1, parentID, masterPMID , false,
                                       memorySize(sdml.str()) + memorySize(pmName) + memorySize(true));
   MessageData messageData(message);
   messageData << sdml.str() << pmName << true;
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
