//---------------------------------------------------------------------------
#ifndef ApsimRunFileH
#define ApsimRunFileH

class IniFile;
// ------------------------------------------------------------------
// This class encapsulates a run file which is a collection of
// control file | configuration file pairs.
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimRunFile
   {
   public:
      ApsimRunFile(const std::string& filename);
      ~ApsimRunFile(void);

      // ------------------------------------------------------------------
      // Return a list of runs to caller.
      // ------------------------------------------------------------------
      void getRuns(std::vector<std::string>& runNames) const;

      // ------------------------------------------------------------------
      // Return a specified run to caller.
      // ------------------------------------------------------------------
      void getRun(const std::string& runName,
                  std::string& fileName,
                  std::vector<std::string>& conFileSections) const;

   private:
      IniFile* ini;
   };
#endif
