//---------------------------------------------------------------------------
#ifndef APSIMControlFileH
#define APSIMControlFileH

#include <vector>
#include <string>
#include <general\StringTokenizer.h>
#include "ApsimParameterFile.h"
#include <ApsimShared\ApsimConfigurationFile.h>
// ------------------------------------------------------------------
// This class encapsulates an apsim control file.  It provides several
// methods to extract information from the control file and associated
// parameter files.
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimControlFile
   {
   public:
      ApsimControlFile(void) { }
      ApsimControlFile (const std::string& controlFilename,
                        const std::string& section) throw(std::runtime_error);

      string getFileName(void) const {return fileName;}
      string getSection(void) const {return section;}

      // return a list of all section names in the control file.
      static void getAllSectionNames(const std::string& fileName,
                                     std::vector<std::string>& sectionNames);

      // return a list of all filenames specified in the control file section.
      // NB: This list doesn't include output file names.
      void getAllFiles(std::vector<std::string>& fileNames) const throw(std::runtime_error);

      // get and set the control file section contents.
      std::string getSectionContents(void) const;
      void setSectionContents(const std::string& contents);

      // return a list of output/summary filenames
      void getOutputFileNames(std::vector<std::string>& fileNames) const;
      void getSummaryFileNames(std::vector<std::string>& fileNames) const;

      // Run apsim using the specified configuration file.
      // If sections.size() > 0 then only those sections specified will be run
      // If sections.size() == 0 then all sections will be run.
      void run(const std::string& configurationFile,
               bool console = false) const throw(std::runtime_error);

      // Create a SIM file for the specified section and return its filename.
      // Throws an exception on error.
      void createSIM(const std::string& configurationFile,
                     std::string& simFile) const throw(std::runtime_error);

      // Return all the parameter files for the specified section and instance.
      void getParameterFiles(const std::string& moduleName,
                             std::vector<ApsimParameterFile>& paramFiles,
                             bool oneFilePerInstance = false,
                             bool constants = false) const;

      // Return a single parameter value for the specified module
      // and parameter name.
      std::string getParameterValue(const std::string& moduleName,
                                    const std::string& parameterName) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Set the value of a parameter for a module.
      // If moduleName is blank then parameter will be written to control file
      // ------------------------------------------------------------------
      void setParameterValues(const string& moduleName,
                              const string& sectionName,
                              const string& parameterName,
                              const vector<string>& parameterValues,
                              const string& filename = "") const throw(std::runtime_error);
      void setParameterValue(const string& moduleName,
                             const string& sectionName,
                             const string& parameterName,
                             const string& parameterValue,
                             const string& filename = "") const throw(std::runtime_error);

      // Return a list of all parameter values for the specified module
      // and parameter name.
      void getParameterValues(const std::string& moduleName,
                              const std::string& parameterName,
                              std::vector<std::string>& values) const;

      // change the name of a module in the control file.
      bool changeModuleName(const std::string& oldModuleName,
                            const std::string& newModuleName) const;

      std::string getTitle(void) const;

      // remove all references to this control file from the list of
      // parameter files for all modules.
      void removeSelfReferences(const std::string& parFileForConParams);

      // return a version number for this control file.
      static int getVersionNumber(const std::string& fileName);

      // ------------------------------------------------------------------
      // Set the version number in the control file to the current
      // apsim version number
      // ------------------------------------------------------------------
      static void setVersionNumber(const std::string& fileName, int versionNumber);

      // ------------------------------------------------------------------
      // Get a parameter file from the control file - any module will do.
      // Also return a section name.
      // ------------------------------------------------------------------
      void getDefaultParFileAndSection(std::string& defaultFile,
                                       std::string& defaultSection) const;

      // Return a list of instance names for the specified module name.
      void getInstances(const std::string& moduleName,
                        std::vector<std::string>& instanceNames) const;

      // ------------------------------------------------------------------
      // write new module= line to control file.
      // ------------------------------------------------------------------
      void addModuleLine(const std::string& moduleName,
                         const std::string& instanceName,
                         const std::string& fileName,
                         const std::string& sectionName) const;

   private:
      string fileName;
      string section;

      // convert a module name to an instance name.
      std::string moduleToInstance(const std::string& moduleName) const;

   };
#endif
