//---------------------------------------------------------------------------
#ifndef APSIMControlFileH
#define APSIMControlFileH

#include <vector>
#include <string>
class IniFile;
// ------------------------------------------------------------------
// This class encapsulates an apsim control file.  It provides several
// methods to extract information from the control file and associated
// parameter files.
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimControlFile
   {
   public:
      ApsimControlFile(const std::string& controlFilename);
      ~ApsimControlFile(void);

      std::string getFileName(void) const;

      // ------------------------------------------------------------------
      // return true if the specified section is a valid one.
      // ------------------------------------------------------------------
      bool isValid(const std::string& section);

      // ------------------------------------------------------------------
      // return a list of all section names in the control file.
      // ------------------------------------------------------------------
      void getAllSectionNames(std::vector<std::string>& sectionNames);

      // ------------------------------------------------------------------
      // return a list of all filenames specified in the control file section.
      // NB: This list doesn't include output file names.
      // ------------------------------------------------------------------
      void getAllFiles(const std::string& section,
                       std::vector<std::string>& fileNames) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Return the referenced file for the specified module e.g. met and soi.
      // ------------------------------------------------------------------
      std::string getFileForModule(const std::string& section,
                                   const std::string& module) const;

      // ------------------------------------------------------------------
      // return a list of output/summary filenames
      // ------------------------------------------------------------------
      void getOutputFileNames(const std::string& section,
                              std::vector<std::string>& fileNames) const;
      std::string getSummaryFileName(const std::string& section) const;

      // ------------------------------------------------------------------
      // Return a list of all parameter values for the specified module
      // and parameter name.
      // ------------------------------------------------------------------
      void getParameterValues(const std::string& section,
                              const std::string& instanceName,
                              const std::string& parameterName,
                              std::vector<std::string>& values) const;

      // ------------------------------------------------------------------
      // Return a single parameter value for the specified module
      // and parameter name.
      // ------------------------------------------------------------------
      std::string getParameterValue(const std::string& section,
                                    const std::string& instanceName,
                                    const std::string& parameterName) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Set the value of a parameter for a module.
      // If moduleName is blank then parameter will be written to control file
      // ------------------------------------------------------------------
      void setParameterValues(const string& sectionName,
                              const string& instanceName,
                              const string& parameterName,
                              const vector<string>& parameterValues) throw(std::runtime_error);
      void setParameterValue(const string& sectionName,
                             const string& instanceName,
                             const string& parameterName,
                             const string& parameterValue) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Rename the specified parameter in all instances of module.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool renameParameter(const std::string& sectionName,
                           const std::string& moduleName,
                           const std::string& oldParameterName,
                           const std::string& newParameterName);

      // ------------------------------------------------------------------
      // Delete the specified parameter from all instances of module.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool deleteParameter(const std::string& sectionName,
                           const std::string& moduleName,
                           const std::string& parameterName);

      // ------------------------------------------------------------------
      // Moves the specified parameter from all instances of module to the
      // specified destination instance.
      // Return true if change was made.
      // ------------------------------------------------------------------
      bool moveParameter(const std::string& sectionName,
                         const std::string& moduleName,
                         const std::string& parameterName,
                         const std::string& destModuleName);

      // ------------------------------------------------------------------
      // change the name of a module in the control file.  Return true
      // on success.
      // ------------------------------------------------------------------
      bool changeModuleName(const std::string& section,
                            const std::string& oldModuleName,
                            const std::string& newModuleName);

      // ------------------------------------------------------------------
      // return title to caller.
      // ------------------------------------------------------------------
      std::string getTitle(const std::string& section) const;
      void setTitle(const std::string& section, const std::string& title);

      // ------------------------------------------------------------------
      // Return true if the control file has parameters in it.
      // ------------------------------------------------------------------
      bool hasParametersInCon(const std::string& section);

      // ------------------------------------------------------------------
      // remove all references to this control file from the list of
      // parameter files for all modules.  Return true if parameters were moved.
      // ------------------------------------------------------------------
      bool moveParametersOutOfCon(const std::string& section,
                                  const std::string& parFileForConParams);

      // ------------------------------------------------------------------
      // Get a parameter file from the control file - any module will do.
      // Also return a section name.
      // ------------------------------------------------------------------
      void getDefaultParFileAndSection(const std::string& section,
                                       std::string& defaultFile,
                                       std::string& defaultSection) const;

      // ------------------------------------------------------------------
      // return a version number for this control file.
      // ------------------------------------------------------------------
      static int getVersionNumber(const std::string& fileName);

      // ------------------------------------------------------------------
      // Set the version number in the control file to the current
      // apsim version number
      // ------------------------------------------------------------------
      static void setVersionNumber(const std::string& fileName, int versionNumber);

      // ------------------------------------------------------------------
      // Return a list of instance names for the specified module name.
      // ------------------------------------------------------------------
      void getInstances(const std::string& section,
                        const std::string& moduleName,
                        std::vector<std::string>& instanceNames) const;

      // ------------------------------------------------------------------
      // Return a list of module names and their instance names.
      // ------------------------------------------------------------------
      struct ModuleInstance
         {
         std::string moduleName;
         std::string instanceName;
         std::string dllFileName;
         };
      typedef std::vector<ModuleInstance> ModuleInstances;
      void getAllModuleInstances(const std::string& section,
                                 ModuleInstances& moduleInstances) const;

      // ------------------------------------------------------------------
      // Enumerate all parameter sections for the specified module name.
      // ------------------------------------------------------------------
      typedef void (__closure *ParamCallbackEvent)
         (IniFile* par, const std::string& section);
      void enumerateParameters(const std::string& section,
                               const std::string& moduleName,
                               bool includeConstants,
                               ParamCallbackEvent callback);
   private:
      IniFile* ini;
      std::string fileName;
      mutable std::vector<IniFile*> openedParFiles;

      // ------------------------------------------------------------------
      // remove all references to this control file from the list of
      // parameter files for all modules.
      // ------------------------------------------------------------------
      void removeSelfReferences(const std::string& section,
                                const std::string& parFileForConParams);

      // ------------------------------------------------------------------
      // return an opened parameter file ready to read.
      // ------------------------------------------------------------------
      IniFile* getParFile(const std::string& fileName) const;

      // ------------------------------------------------------------------
      // Find a parameter in parameter file.  Return true and the par and
      // section name where parameter is located.
      // ------------------------------------------------------------------
      bool findParameterName(const std::string& section,
                             const std::string& instanceName,
                             const std::string& parameterName,
                             IniFile*& par,
                             std::string& parameterFileName,
                             std::string& parameterSection) const;
      // ------------------------------------------------------------------
      // write new module= line to control file.
      // ------------------------------------------------------------------
      void addModuleLine(const string& section,
                         const string& moduleName,
                         const string& instanceName,
                         const string& parFileName,
                         const string& parSectionName);

      // ------------------------------------------------------------------
      // convert a module name to an instance name.
      // ------------------------------------------------------------------
      std::string moduleToInstance(const std::string& section,
                                   const std::string& moduleName) const;
   };

#endif
