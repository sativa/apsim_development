//---------------------------------------------------------------------------
#include "Simulation.h"
#include <aps\SOMSimulation.h>
#include <general\XMLTreeData.h>
#include <sstream>
#include <fstream>
#include <general\path.h>
#include <dir.h>

#include "messages.h"
#include "Transport.h"
#include "Computation.h"

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
   simulationConfiguration = NULL;
   data = NULL;
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
   delete simulationConfiguration;
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
         chdir(Path(simFilename.c_str()).Get_directory().c_str());
         stringstream sdmlStream;
         sdmlStream << in.rdbuf();
         string sdml = sdmlStream.str();

         init(sdml);
         commence();
         term();
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
void Simulation::init(const string& sdml)
   {
   istringstream in(sdml.c_str());
   data = new XMLTreeData;
   data->read(in);
   simulationConfiguration = new SOMSimulation(data->getRootNode());

   // get dll filename for master PM.
   string dllFilename = simulationConfiguration->getExecutableFilename();

   // create a Master PM and give it to the transport layer.
   masterPM = new Computation("MasterPM", dllFilename, parentID, masterPMID);
   transport.addComponent(masterPMID, "MasterPM", masterPM);

   // initialise the simulation
   string pmName = "MasterPM";
   Message* message = constructMessage(Init1, parentID, masterPMID , false,
                                       memorySize(sdml) + memorySize(pmName) + memorySize(true));
   MessageData messageData(message);
   messageData << sdml << pmName << true;
   transport.deliverMessage(message);
   deleteMessage(message);

   // initialise the simulation
   message = constructMessage(Init2, parentID, masterPMID, false, 0);
   transport.deliverMessage(message);
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
   transport.deliverMessage(message);
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
   transport.setMessageHook(hook);
   }

