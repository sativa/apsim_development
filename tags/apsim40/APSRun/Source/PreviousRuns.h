//---------------------------------------------------------------------------

#ifndef PreviousRunsH
#define PreviousRunsH
#include <string>
#include <vector>
// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates details previous runs of APSIM.
//     The number of most recently run control files is
//     retrieved from the apsuite.ini file.

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
class PreviousRuns
   {
   public:
      PreviousRuns(void);

      bool getPreviousRun(const std::string& controlFilename,
                          std::vector<std::string>&simulationNames);

      void setCurrentRun(const std::string& previousControlFilename,
                         const std::vector<std::string>&simulationNames);

   private:
      unsigned int maxNumRememberedRuns;
      typedef std::vector<std::string> RememberedRuns;
      RememberedRuns rememberedRuns;
   };
#endif
