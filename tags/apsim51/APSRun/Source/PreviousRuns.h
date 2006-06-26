//---------------------------------------------------------------------------

#ifndef PreviousRunsH
#define PreviousRunsH
#include <string>
#include <vector>
class ApsimRuns;
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

      bool wasPreviouslyRun(const string& controlFilename,
                            const string& simulationName);

      void saveSelectedRunNames(ApsimRuns& selectedRuns);

   private:
      unsigned int maxNumRememberedRuns;
   };
#endif
