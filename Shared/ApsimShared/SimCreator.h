//---------------------------------------------------------------------------

#ifndef SimCreatorH
#define SimCreatorH
#include <string>
class ApsimControlFile;
typedef void __fastcall (__closure *TSimCreatorEvent)(const std::string& simFileName);
//---------------------------------------------------------------------------
// This class creates SIM files given a control file.
//---------------------------------------------------------------------------
class __declspec(dllexport) SimCreator
   {
   public:
      SimCreator(const std::string& controlFileName);
      ~SimCreator(void);

      //---------------------------------------------------------------------------
      // Create a single SIM file for the specified control
      // file section name.  The sim will be stored in the
      // specified sim directory.  If sim directory is null then the control file
      // directory is used
      //---------------------------------------------------------------------------
      void createSim(const std::string& sectionName,
                      const std::string& simDirectory);

      //---------------------------------------------------------------------------
      // Create SIM files for the specified control
      // file section names.  All sims are stored in the
      // specified sim directory.  If sim directory is null then the control file
      // directory is used. The simCreator event is called every time
      // a new sim file is created.
      //---------------------------------------------------------------------------
      void createSims(const std::vector<std::string>& sectionNames,
                      const std::string& simDirectory,
                      TSimCreatorEvent simCreatorEvent);

      //---------------------------------------------------------------------------
      // Create SIM files.  All sims are stored in the
      // specified sim directory.  If sim directory is null then the control file
      // directory is used. The simCreator event is called every time
      // a new sim file is created.
      //---------------------------------------------------------------------------
      void createSims(const std::string& simDirectory,
                      TSimCreatorEvent simCreatorEvent);

   private:
      ApsimControlFile* con;

      // ------------------------------------------------------------------
      // Create a SIM file for the specified section and return its filename.
      // Return true if sim file was created.
      // ------------------------------------------------------------------
      void createSim(const std::string& sectionName,
                     int simNumber,
                     const std::string& simDirectory,
                     TSimCreatorEvent simCreatorEvent);

   };
#endif
