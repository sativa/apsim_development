//---------------------------------------------------------------------------
#ifndef SimulationH
#define SimulationH
#include "interfaces.h"
#include "protocolexport.h"

class SOMSimulation;
class XMLTreeData;
namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a top-level run.  It is passed a configuration
//     and a simulation and sets the simulation running.  Exceptions
//     are thrown when an error occurs.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT Simulation : public ISimulation
   {
   public:
      Simulation(void);
      ~Simulation(void);

      // initialise the simulation
      virtual void init(const std::string& sdml);

      // commence the simulation
      virtual void commence(void);

      // terminate the simulation
      virtual void term (void);

      // set a hook into the message stream.
      void setMessageHook(IMessageHook* hook);

   private:
      IComputation* masterPM;
      XMLTreeData* data;
      SOMSimulation* simulationConfiguration;
   };

} // namespace protocol
#endif
