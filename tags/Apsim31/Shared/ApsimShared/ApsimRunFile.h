//---------------------------------------------------------------------------
#ifndef ApsimRunFileH
#define ApsimRunFileH
#include <vector>
#include <string>
// ------------------------------------------------------------------
// This class encapsulates a run file which is a collection of
// control file | configuration file pairs.
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimRunFile
   {
   public:
      ApsimRunFile(const std::string& filename);

      void run(bool quiet) const;

   private:
      std::string fileName;
   };
#endif
