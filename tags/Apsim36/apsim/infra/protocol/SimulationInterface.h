//---------------------------------------------------------------------------
#ifndef SimulationInterfaceH
#define SimulationInterfaceH
// ------------------------------------------------------------------
//  Short description:
//    Each component creates an instance of this class to obtain
//    access to simulation wide services/resources.

//  Notes:

//  Changes:
//    dph 13/3/2000

// ------------------------------------------------------------------
class SimulationInterface
   {
   public
      SimulationInterface(TComputation* computation);

      // Return initialisation data to caller.
      bool getInitData (const string& Name, DataType** Data);
      void getInitDataNames(const string& Name, list<string>& Names);

      // Error handling methods.
//      void fatalError(const string& msg);
//      void warningError(const string& msg);

      // return the title of the simulation
//      string getTitle(void);

      // write to screen/summary/log streams
//      void writeScreen(const string& msg);
//      void writeSummary(const string& msg);
//      void writeLog(const string& msg);

   private:
      TComputation* computation;
   };
#endif
