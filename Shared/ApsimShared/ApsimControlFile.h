//---------------------------------------------------------------------------
#ifndef APSIMControlFileH
#define APSIMControlFileH

#include <vector>
#include <string>
#include <general\StringTokenizer.h>
#include "ApsimParameterFile.h"
#include <ApsimShared\ApsimConfigurationFile.h>
#include "TControlFileConversionForm.h"
// ------------------------------------------------------------------
// This class encapsulates an apsim control file.  It provides several
// methods to extract information from the control file and associated
// parameter files.
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimControlFile
   {
   public:
      ApsimControlFile (const std::string& controlFilename,
                        const std::string& section) throw(std::runtime_error);

      string getFileName(void) const {return fileName;}

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
      // Return true if SIM was created.
      bool createSIM(const std::string& configurationFile,
                     std::string& simFile) const throw(std::runtime_error);

   private:
      string fileName;
      string section;

      // Return all the parameter files for the specified section and instance.
      void getParameterFiles(const std::string& moduleName,
                             std::vector<ApsimParameterFile>& paramFiles,
                             bool oneFilePerInstance = false,
                             bool constants = false) const;

      // Return a list of all parameter values for the specified module
      // and parameter name.
      void getParameterValues(const std::string& moduleName,
                              const std::string& parameterName,
                              std::vector<std::string>& values) const;

      // Return a single parameter value for the specified module
      // and parameter name.
      std::string getParameterValue(const std::string& moduleName,
                                    const std::string& parameterName) const throw(std::runtime_error);

      // convert a module name to an instance name.
      std::string moduleToInstance(const std::string& moduleName) const;

      // Return a list of instance names for the specified module name.
      void getInstances(const std::string& moduleName,
                        std::vector<std::string>& instanceNames) const;

      // change the name of a module in the control file.
      void changeModuleName(const std::string& oldModuleName,
                            const std::string& newModuleName) const;

      // Create default services in specified simulation
      void createServices(ApsimSimulationFile& simulation,
                          ApsimConfigurationFile& configuration) const;

      // Convert the control / parameter file.
      bool convertControlFile(void) const throw(std::runtime_error);

      void putDescriptionsInForm(const std::string& scriptFileName,
                                 TControlFileConversionForm* form) const;
      void parseName(StringTokenizer& tokenizer,
                     std::string& moduleName, std::string& parameterName) const;
      std::string parseValue(const ApsimParameterFile& paramFile,
                             StringTokenizer& tokenizer) const;
      std::string parseDate(const ApsimParameterFile& paramFile,
                            StringTokenizer& tokenizer) const;
      void parseCreateParameter(const ApsimParameterFile& paramFile,
                                const std::string& line) const throw(std::runtime_error);
      void parseDeleteParameter(const ApsimParameterFile& paramFile,
                                const std::string& line) const throw(std::runtime_error);
      void parseMoveParameter(const ApsimParameterFile& paramFile,
                              const std::string& line) const throw(std::runtime_error);
      void parseNewFormatReportVariables(const ApsimParameterFile& paramFile,
                                         const std::string& line) const throw(std::runtime_error);
      void parseRemoveReportOutputSwitch(const ApsimParameterFile& paramFile,
                                         const std::string& line) const throw(std::runtime_error);
      void parseChangeModuleName(const std::string& line) const throw(std::runtime_error);

   };
#endif
