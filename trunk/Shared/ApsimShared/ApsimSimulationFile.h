//---------------------------------------------------------------------------
#ifndef ApsimSimulationFileH
#define ApsimSimulationFileH
#include <string>
// ------------------------------------------------------------------
// This class encapsulates an APSIM simulation file (.SIM)
// ------------------------------------------------------------------
class ApsimSimulationFile
   {
   public:
      ApsimSimulationFile(const std::string& filename);

      std::string getFileName(void) {return fileName;}

      void run(bool quiet = false);
   protected:
      std::string fileName;
   };
#endif
 