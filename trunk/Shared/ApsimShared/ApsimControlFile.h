//---------------------------------------------------------------------------
#ifndef APSIMControlFileH
#define APSIMControlFileH

#include <vector>
#include <string>
// ------------------------------------------------------------------
// This class encapsulates an apsim control file.  It provides several
// methods to extract information from the control file and associated
// parameter files.
// ------------------------------------------------------------------
class ApsimControlFile
   {
   public:
      ApsimControlFile (void);
      ApsimControlFile (const std::string& controlFilename);

      string getFileName(void) const {return fileName;}

      // return a list of all section names in the control file.
      void getAllSectionNames(std::vector<std::string>& sectionNames) const;

      // return a list of all filenames specified in the control file section.
      // NB: This list doesn't include output file names.
      void getAllFiles(const std::string& section,
                       std::vector<std::string>& fileNames) const throw(std::runtime_error);

      // get and set the control file section contents.
      std::string getSectionContents(const std::string& section) const;
      void setSectionContents(const std::string& section,
                              const std::string& contents);

      // return a list of output/summary filenames
      void getOutputFileNames(const std::string& section,
                              std::vector<std::string> fileNames) const;
      void getSummaryFileNames(const std::string& section,
                               std::vector<std::string> fileNames) const;

   private:
      string fileName;

      // Return the values of the specified parameter for the specified
      // module and section.
      void getParamValues(const std::string& controlSection,
                          const std::string& instanceName,
                          const std::string& parameterName,
                          std::vector<std::string>& values) const;

      // Return a list of instance names for the specified module name.
      void getInstances(const std::string& section, const std::string& moduleName,
                        std::vector<std::string>& instanceNames) const;


      // parameter file reading methods.
/*
      bool getParametersFromFile(const std::string& FileName,
                                 const std::string& controlSection,
                                 const std::string& parameterName,
                                 std::list<std::string>& parameterValues);
      bool parseSectionName (const std::string& controlSection,
                             string& firstWord,
                             string& secondWord,
                             string& thirdWord);
      bool findParameterSection (std::istream& in,
                                 const std::string& moduleName,
                                 const std::string& controlSection,
                                 string& thirdWord);
      void enumerateSection (const std::string& moduleName,
                             const std::string& instName,
                             const std::string& paramFile,
                             const std::string& controlSection,
                             CallbackFunction<Parameter&>& function);

      void getParamFromModule (const std::string& controlSection,
                               const std::string& moduleName,
                               const std::string& parameterName,
                               std::list<std::string>& parameterValues);

      void getAllParamFiles   (const std::string& controlSection,
                               std::list<std::string>& fileNames);
      void getAllConstantFiles(const std::string& controlSection,
                               std::list<std::string>& fileNames);


      void getModuleNames     (const std::string& controlSection,
                               std::list<string>& moduleNames);
      void enumerateInstances (const std::string& controlSection,
                               CallbackFunction<Instance&>& function);
      void enumerateParameters (const std::string& controlSection,
                                CallbackFunction<Parameter&>& function);
*/   };
#endif
