//---------------------------------------------------------------------------
#include "Simulation.h"
#include <aps\SOMSimulation.h>
#include <general\XMLTreeData.h>
#include <sstream>
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
//  Short description:
//    create the simulation.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Simulation::init(const string& sdml)
   {
   data = new XMLTreeData;
   istringstream sdmlStream(sdml.c_str());
   data->read(sdmlStream);
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

