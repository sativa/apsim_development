//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimControlFile.h"
#include <ApsimShared\ApsimDirectories.h>

#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\stristr.h>
#include <general\IniFile.h>
#include <general\path.h>
#include <general\date_class.h>
#include <fstream>

using namespace std;
#pragma package(smart_init)

struct ParamFile
   {
   string moduleName;
   string dllFileName;
   string instanceName;
   string fileName;
   string sectionName;

   bool operator==(const ParamFile& rhs)
      {
      return (Str_i_Eq(moduleName, rhs.moduleName) &&
              Str_i_Eq(instanceName, rhs.instanceName) &&
              Str_i_Eq(fileName, rhs.fileName) &&
              Str_i_Eq(sectionName, rhs.sectionName));
      }

   void fixUpModuleName(void)
      {
      if (moduleName.find('\\') != string::npos)
         {
         Path dllPath(moduleName);
         moduleName = dllPath.Get_name_without_ext();
         dllFileName = dllPath.Get_path();
         }
      else
         {
         dllFileName = getApsimDirectory() + "\\apsim\\" + moduleName + "\\lib\\"
                     + moduleName + ".dll";
         }
      }
   };

// ------------------------------------------------------------------
// Parse the specified 'module=' line to extract the module name,
// the instance name and the parameter files and sections referenced.
// Throws exception on error.
// ------------------------------------------------------------------
void parseModuleLine(const string& controlFileName, const string& moduleLine,
                     vector<ParamFile>& paramFiles,
                     bool removeMacros = true) throw(runtime_error)
   {
   string line = moduleLine;

   // remove any comments
   unsigned posComment = line.find('!');
   if (posComment != string::npos)
      line.erase(posComment);

   // initialise everything to zero.
   string moduleName;
   string instanceName;
   string paramFile;
   string section;
   bool foundAFile = false;

   // setup state names
   static const int READ_MODULE_NAME = 0;
   static const int READ_INSTANTIATION = 1;
   static const int READ_PARAM_FILE = 2;
   static const int READ_SECTION = 3;

   int state = READ_MODULE_NAME;
   int currentPos = 0;
   bool inQuotes = false;
   while (line[currentPos] != 0)
      {
      char ch = line[currentPos];

      switch (state)
         {
         case READ_MODULE_NAME :
                      if (ch == '\'' || ch == '\"')
                         inQuotes = !inQuotes;
                      if (inQuotes)
                         moduleName += ch;
                      else if (ch == ' ')
                         state = READ_PARAM_FILE;
                      else if (ch == '(')
                         state = READ_INSTANTIATION;
                      else if (ch == '[')
                         state = READ_SECTION;
                      else
                         moduleName += ch;
                      break;
         case READ_INSTANTIATION :
                      if (ch == ')')
                         state = READ_PARAM_FILE;
                      else
                         instanceName += ch;
                      break;
         case READ_PARAM_FILE :
                      foundAFile = true;
                      if (ch == '\'' || ch == '\"')
                         inQuotes = !inQuotes;
                      if (inQuotes)
                         paramFile += ch;
                      else if (ch == '(')
                         state = READ_INSTANTIATION;
                      else if (ch == '[')
                         state = READ_SECTION;
                      else
                         paramFile += ch;
                      break;
         case READ_SECTION :
                      if (ch == ']')
                         {
                         state = READ_PARAM_FILE;
                         Strip(paramFile, " ");
                         Strip(section, " ");
                         if (paramFile.length() == 0)
                            paramFile = controlFileName;
                         else if (removeMacros)
                            {
                            // replace any apsuite macros.
                            Replace_all(paramFile, "%apsuite", getApsimDirectory().c_str());
                            }
                         if (instanceName.length() == 0)
                            instanceName = moduleName;

                         ParamFile p;
                         p.fileName = paramFile;
                         p.sectionName = section;
                         p.moduleName = moduleName;
                         p.instanceName = instanceName;
                         if (instanceName == "")
                            p.instanceName = moduleName;
                         p.fixUpModuleName();
                         paramFiles.push_back(p);
                         paramFile = "";
                         section = "";
                         }
                      else
                         section += ch;
                      break;
         }
      currentPos++;
      }
   if (!foundAFile && moduleName != "")
      {
      if (instanceName.length() == 0)
         instanceName = moduleName;

      ParamFile p;
      p.moduleName = moduleName;
      p.instanceName = instanceName;
      p.fixUpModuleName();
      paramFiles.push_back(p);
      return;
      }

   if (state != READ_PARAM_FILE)
      throw runtime_error("Invalid control file line: " + moduleLine);
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void getParameterFiles(IniFile* ini,
                       const string& section,
                       vector<ParamFile>& paramFiles)
   {
   // read in all 'module=' lines from control file.
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);

   // loop through all module lines
   for (vector<string>::const_iterator moduleLineI = moduleLines.begin();
                                       moduleLineI != moduleLines.end();
                                       moduleLineI++)
      {
      // remove any param files with a blank filename.
      vector<ParamFile> parFiles;
      parseModuleLine(ini->getFileName(), *moduleLineI, parFiles);
      for (unsigned p = 0; p != parFiles.size(); p++)
         {
         if (parFiles[p].fileName != "")
            paramFiles.push_back(parFiles[p]);
         }
      }
   }
// ------------------------------------------------------------------
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void getParameterFilesForInstance(IniFile* ini,
                                  const string& section,
                                  const string& instanceName,
                                  vector<ParamFile>& paramFiles,
                                  bool constants)
   {
   vector<ParamFile> parFiles;
   getParameterFiles(ini, section, parFiles);
   for (vector<ParamFile>::const_iterator paramFile = parFiles.begin();
                                          paramFile != parFiles.end();
                                          paramFile++)
      {
      bool doAdd = (instanceName == "" || Str_i_Eq(paramFile->instanceName, instanceName));
      if (doAdd)
         {
         if (!constants)
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) != ".ini");
         }
      if (doAdd)
         paramFiles.push_back(*paramFile);
      }
   }
// ------------------------------------------------------------------
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void getParameterFilesForModule(IniFile* ini,
                                const string& section,
                                const string& moduleName,
                                vector<ParamFile>& paramFiles,
                                bool constants)
   {
   vector<ParamFile> parFiles;
   getParameterFiles(ini, section, parFiles);
   for (vector<ParamFile>::const_iterator paramFile = parFiles.begin();
                                          paramFile != parFiles.end();
                                          paramFile++)
      {
      bool doAdd = (moduleName == "" || Str_i_Eq(paramFile->moduleName, moduleName));
      if (doAdd)
         {
         if (!constants)
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) != ".ini");
         }
      if (doAdd)
         paramFiles.push_back(*paramFile);
      }
   }
// ------------------------------------------------------------------
// return a list of parameter file sections for the given par
// that match the specified ParamFile struct.
// ------------------------------------------------------------------
class MatchPrefixStringAndStore
   {
   private:
      vector<string>& matchingStrings;
      const string& prefixToMatch;
   public:
      MatchPrefixStringAndStore(const string& prefix, vector<string>& matchingstrings)
         : prefixToMatch(prefix), matchingStrings(matchingstrings) { }
      void operator() (const string& st)
         {
         if (Str_i_Eq(st.substr(0, prefixToMatch.length()), prefixToMatch))
            matchingStrings.push_back(st);
         }
   };

void getParFileSectionsMatching(IniFile* par, ParamFile paramFile,
                                vector<string>& paramFileSections)
   {
   string sectionToMatch = paramFile.sectionName + "." + paramFile.instanceName + ".";

   vector<string> sections;
   par->readSectionNames(sections);
   for_each(sections.begin(), sections.end(),
            MatchPrefixStringAndStore(sectionToMatch, paramFileSections));
   }

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimControlFile::ApsimControlFile(const string& fileName)
   {
   ini = new IniFile(fileName);
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimControlFile::~ApsimControlFile(void)
   {
   delete ini;
   for (unsigned i = 0; i != openedParFiles.size(); i++)
      delete openedParFiles[i];
   }
// ------------------------------------------------------------------
// return filename to caller.
// ------------------------------------------------------------------
string ApsimControlFile::getFileName(void) const
   {
   return ini->getFileName();
   }
// ------------------------------------------------------------------
// return true if the specified section is a valid one.
// ------------------------------------------------------------------
bool ApsimControlFile::isValid(const std::string& section)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   return (paramFiles.size() > 0);
   }
// ------------------------------------------------------------------
// Return a list of all section names to caller.
// ------------------------------------------------------------------
void ApsimControlFile::getAllSectionNames(vector<string>& sectionNames)
   {
   ini->readSectionNames(sectionNames);
   }
// ------------------------------------------------------------------
// return a list of all filenames specified in the control file section.
// NB: This list doesn't include output file names.
// ------------------------------------------------------------------
void ApsimControlFile::getAllFiles(const string& section,
                                   vector<string>& fileNames) const throw(runtime_error)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (find(fileNames.begin(), fileNames.end(), paramFileI->fileName) == fileNames.end())
         fileNames.push_back(paramFileI->fileName);
      }
   }
// ------------------------------------------------------------------
// Return the referenced file for the specified module e.g. met and soi.
// ------------------------------------------------------------------
string ApsimControlFile::getFileForModule(const string& section,
                                          const string& module) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      if (paramFiles.size() > 0 && Str_i_Eq(paramFiles[0].moduleName, module))
         return paramFiles[0].fileName;
      }
   return "";
   }
// ------------------------------------------------------------------
// Return the .ini file name for the specified instance.
// ------------------------------------------------------------------
std::string ApsimControlFile::getIniFileForInstance(const std::string& section,
                                                    const std::string& instanceName) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      for (unsigned p = 0; p != paramFiles.size(); p++)
         {
         if (paramFiles[0].instanceName != instanceName)
            break;
         if (Str_i_Eq(Path(paramFiles[p].fileName).Get_extension(), ".ini"))
            return paramFiles[p].fileName;
         }
      }
   return "";
   }
// ------------------------------------------------------------------
// Return a list of all output file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getOutputFileNames(const string& section,
                                          vector<string>& fileNames) const
   {
   vector<string> instanceNames;
   getInstances(section, "report", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      getParameterValues(section, instanceNames[i], "outputfile", fileNames);
   }
// ------------------------------------------------------------------
// Return the name of the summary file
// ------------------------------------------------------------------
string ApsimControlFile::getSummaryFileName(const string& section) const
   {
   return getParameterValue(section, "summaryfile", "summaryfile");
   }
// ------------------------------------------------------------------
// Return a list of instance names for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::getInstances(const string& section,
                                    const string& moduleName,
                                    vector<string>& instanceNames) const
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (Str_i_Eq(moduleName, paramFileI->moduleName) &&
          find(instanceNames.begin(), instanceNames.end(),
               paramFileI->instanceName) == instanceNames.end())
         instanceNames.push_back(paramFileI->instanceName);
      }
   }
// ------------------------------------------------------------------
// Return a list of module names and their instance names.
// ------------------------------------------------------------------
void ApsimControlFile::getAllModuleInstances(const std::string& section,
                                             ModuleInstances& moduleInstances) const
   {
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles);
      if (paramFiles.size() > 0)
         {
         ModuleInstance instance;
         instance.moduleName = paramFiles[0].moduleName;
         instance.instanceName = paramFiles[0].instanceName;
         instance.dllFileName = paramFiles[0].dllFileName;

         moduleInstances.push_back(instance);
         }
      }
   }
// ------------------------------------------------------------------
// Return a list of all parameter values for the specified module
// and parameter name.
// ------------------------------------------------------------------
void ApsimControlFile::getParameterValues(const string& section,
                                          const string& instanceName,
                                          const string& parameterName,
                                          vector<string>& values) const
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, false);
   for (vector<ParamFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      {
      IniFile* par = getParFile(paramFile->fileName);
      if (par != NULL)
         {
         vector<string> paramFileSections;
         getParFileSectionsMatching(par, *paramFile, paramFileSections);
         for (unsigned p = 0; p != paramFileSections.size(); p++)
            par->read(paramFileSections[p], parameterName, values);
         }
      }
   }
// ------------------------------------------------------------------
// Return a single parameter value for the specified module
// and parameter name.
// ------------------------------------------------------------------
string ApsimControlFile::getParameterValue(const string& section,
                                           const string& instanceName,
                                           const string& parameterName) const throw(runtime_error)
   {
   vector<string> values;
   getParameterValues(section, instanceName, parameterName, values);
   if (values.size() == 0)
      return "";
   if (values.size() > 1)
      return "";
   return values[0];
   }
// ------------------------------------------------------------------
// Set the values of a parameter for a module
// If moduleName is blank then parameter will be written to control file
// ------------------------------------------------------------------
void ApsimControlFile::setParameterValues(const string& sectionName,
                                          const string& instanceName,
                                          const string& parameterName,
                                          const vector<string>& parameterValues) throw(std::runtime_error)
   {
   if (instanceName == "")
      ShowMessage("shouldn't be here");

   else
      {
      IniFile* par = NULL;
      string paramFileName;
      string paramSectionName;
      if (!findParameterName(sectionName, instanceName, parameterName,
                             par, paramFileName, paramSectionName))
         {
         // see if there are any parameter files for this instance.  If so
         // then use the first one to write the parameter to under a default
         // section name.  If not then use a default parameter file and section name.
         vector<ParamFile> paramFiles;
         getParameterFilesForInstance(ini, sectionName, instanceName, paramFiles, false);
         if (paramFiles.size() > 0)
            {
            paramFileName = paramFiles[0].fileName;
            par = getParFile(paramFileName);
            if (par != NULL)
               {
               vector<string> paramFileSections;
               getParFileSectionsMatching(par, paramFiles[0], paramFileSections);
               if (paramFileSections.size() > 0)
                  paramSectionName = paramFileSections[0];
               else
                  paramSectionName = "default";
               }
            }
         else
            {
            getDefaultParFileAndSection(sectionName, paramFileName, paramSectionName);
            par = getParFile(paramFileName);
            if (par != NULL)
               paramSectionName = paramSectionName + "." + instanceName + ".parameters";
            }
         }
      if (par != NULL)
         {
         par->write(paramSectionName, parameterName, parameterValues);
         addModuleLine(sectionName, instanceName, instanceName,
                       paramFileName, paramSectionName);
         }
      }
   }
// ------------------------------------------------------------------
// Set the value of a parameter for a module
// If moduleName is blank then parameter will be written to control file
// ------------------------------------------------------------------
void ApsimControlFile::setParameterValue(const string& sectionName,
                                         const string& instanceName,
                                         const string& parameterName,
                                         const string& parameterValue) throw(std::runtime_error)
   {
   vector<string> values;
   values.push_back(parameterValue);
   setParameterValues(sectionName, instanceName, parameterName, values);
   }
// ------------------------------------------------------------------
// change the name of a module in the control file.  Return true
// on success.
// ------------------------------------------------------------------
bool ApsimControlFile::changeModuleName(const std::string& section,
                                        const std::string& oldModuleName,
                                        const std::string& newModuleName)
   {
   bool changeMade = false;

   vector<string> lines;
   ini->read(section, "module", lines);


   // loop through all lines in control file looking for a module = oldModuleName
   for(vector<string>::iterator line = lines.begin();
                                line != lines.end();
                                line++)
      {
      unsigned posStartModuleName =line->find_first_not_of(' ');
      if (posStartModuleName != string::npos)
         {
         unsigned posEndModuleName = line->find(' ', posStartModuleName);
         if (posEndModuleName != string::npos)
            {
            int moduleNameLength = posEndModuleName - posStartModuleName;
            if (Str_i_Eq(line->substr(posStartModuleName, moduleNameLength),
                         oldModuleName))
               {
               line->replace(posStartModuleName, moduleNameLength, newModuleName);
               changeMade = true;
               }
            }
         }
      }
   if (changeMade)
      ini->write(section, "module", lines);
   return changeMade;
   }
// ------------------------------------------------------------------
// Return a title to caller.
// ------------------------------------------------------------------
string ApsimControlFile::getTitle(const std::string& section) const
   {
   string title;
   ini->read(section, "title", title);
   return title;
   }
// ------------------------------------------------------------------
// set the title of control file.
// ------------------------------------------------------------------
void ApsimControlFile::setTitle(const std::string& section,
                                const std::string& title)
   {
   ini->write(section, "title", title);
   }
// ------------------------------------------------------------------
// Return the version number as an integer
// ------------------------------------------------------------------
int ApsimControlFile::getVersionNumber(const std::string& fileName)
   {
   ifstream control(fileName.c_str());
   string line;
   while (getline(control, line) && getSectionName(line) == "")
      {
      string version = getKeyValue(line, "version");
      if (version != "")
         return StrToFloat(version.c_str())*10;
      }
   return 21; // assumes if no version number then it is version 2.1
   }
// ------------------------------------------------------------------
// Set the version number in the control file to the current
// apsim version number
// ------------------------------------------------------------------
void ApsimControlFile::setVersionNumber(const std::string& fileName,
                                        int versionNumber)
   {
   // Get entire control file contents.
   ifstream control(fileName.c_str());

   string linesSoFar;
   string line;
   while (getline(control, line) && getSectionName(line) == "")
      {
      string version = getKeyValue(line, "version");
      if (version != "")
         break;
         linesSoFar += line + "\n";
      }
   ostringstream out;
   out << control.rdbuf();
   control.close();
   ofstream controlOut(fileName.c_str());
   controlOut << linesSoFar;
   controlOut << "version = " << versionNumber / 10.0 << endl;
   if (getSectionName(line) != "")
      controlOut << line << endl;
   controlOut << out.str();
   }
// ------------------------------------------------------------------
// return an opened parameter file ready to read.
// ------------------------------------------------------------------
IniFile* ApsimControlFile::getParFile(const std::string& parFileName) const
   {
   Path(ini->getFileName()).Change_directory();
   string filePath = ExpandFileName(parFileName.c_str()).c_str();
   for (unsigned i = 0; i != openedParFiles.size(); i++)
      {
      if (Str_i_Eq(openedParFiles[i]->getFileName(), filePath))
         return openedParFiles[i];
      }
   if (Path(filePath).Get_extension() != ".met" &&
       Path(filePath).Get_extension() != ".soi")
      {
      IniFile* par = new IniFile(filePath);
      openedParFiles.push_back(par);
      return par;
      }
   else
      return NULL;
   }
// ------------------------------------------------------------------
// Find a parameter in parameter file.  Return true and the par and
// section name where parameter is located.
// ------------------------------------------------------------------
bool ApsimControlFile::findParameterName(const string& section,
                                         const string& instanceName,
                                         const string& parameterName,
                                         IniFile*& par,
                                         string& parameterFileName,
                                         string& parameterSection) const
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, false);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      parameterFileName = paramFiles[p].fileName;
      par = getParFile(parameterFileName);
      if (par != NULL)
         {
         vector<string> paramFileSections;
         getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
         for (unsigned s = 0; s != paramFileSections.size(); s++)
            {
            string value;
            if (par->read(paramFileSections[s], parameterName, value))
               {
               parameterSection = paramFileSections[s];
               return true;
               }
            }
         }
      }
   par = NULL;
   parameterFileName = "";
   parameterSection = "";
   return false;
   }
// ------------------------------------------------------------------
// create a new module line from the given ParFiles.
// ------------------------------------------------------------------
string createModuleLine(vector<ParamFile>& paramFiles)
   {
   if (paramFiles.size() > 0)
      {
      string newModuleLine =paramFiles[0].moduleName;
      if (paramFiles[0].moduleName != paramFiles[0].instanceName)
         newModuleLine += "(" + paramFiles[0].instanceName + ")";
      newModuleLine += "  ";
      for (unsigned p = 0; p != paramFiles.size(); p++)
         {
         if (paramFiles[p].fileName != "")
            newModuleLine += " " + paramFiles[p].fileName + " [" + paramFiles[p].sectionName + "]";
         }
      return newModuleLine;
      }
   return "";
   }
// ------------------------------------------------------------------
// write new module= line to control file.
// ------------------------------------------------------------------
void ApsimControlFile::addModuleLine(const string& section,
                                     const string& moduleName,
                                     const string& instanceName,
                                     const string& parFileName,
                                     const string& parSectionName)
   {
   bool found = false;
   ParamFile newParamFile;
   newParamFile.moduleName = moduleName;
   newParamFile.instanceName = instanceName;
   newParamFile.fileName = parFileName;
   newParamFile.sectionName = parSectionName;
   newParamFile.sectionName.erase(parSectionName.find('.'));

   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);
   for (unsigned i = 0; i != moduleLines.size(); i++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[i], paramFiles, false);
      if (paramFiles[0].instanceName == instanceName)
         {
         if (find(paramFiles.begin(), paramFiles.end(), newParamFile) != paramFiles.end())
            return;
         paramFiles.push_back(newParamFile);

         moduleLines[i] = createModuleLine(paramFiles);
         found = true;
         }
      }
   if (!found)
      {
      vector<ParamFile> paramFiles;
      paramFiles.push_back(newParamFile);
      moduleLines.push_back(createModuleLine(paramFiles));
      }
   ini->write(section, "module", moduleLines);
   }
// ------------------------------------------------------------------
// convert a module name to an instance name.
// ------------------------------------------------------------------
string ApsimControlFile::moduleToInstance(const string& section,
                                          const string& moduleName) const
   {
   vector<string> instanceNames;
   getInstances(section, moduleName, instanceNames);
   if (instanceNames.size() == 0)
      return "";
   return instanceNames[0];
   }
// ------------------------------------------------------------------
// Get a parameter file from the control file - any module will do.
// Also return a section name.
// ------------------------------------------------------------------
void ApsimControlFile::getDefaultParFileAndSection(const string& section,
                                                   string& defaultFile,
                                                   string& defaultSection) const
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   defaultFile = "";
   defaultSection = "";

   unsigned i = 0;
   while (i < paramFiles.size()
      && (defaultFile == ""
          ||ExtractFileExt(defaultFile.c_str()) == ".ini"
          || ExtractFileExt(defaultFile.c_str()) == ".met"))
      {
      defaultFile = paramFiles[i].fileName;
      defaultSection = paramFiles[i].sectionName;
      i++;
      }
   if (defaultFile == ""
       || ExtractFileExt(defaultFile.c_str()) == ".ini"
       || ExtractFileExt(defaultFile.c_str()) == ".met")
      {
      defaultFile = "default.par";
      defaultSection = "default";
      }
   }
// ------------------------------------------------------------------
// Rename the specified parameter in all instances of module.
// Return true if change was made.
// ------------------------------------------------------------------
class RenameParameter
   {
   public:
      RenameParameter(const string& oldP, const string& newP, bool& m)
         : oldName(oldP), newName(newP), modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         if (par->renameKey(section, oldName, newName))
            modified = true;
         }

      bool& modified;
   private:
      const string& oldName;
      const string& newName;
   };
bool ApsimControlFile::renameParameter(const std::string& sectionName,
                                       const std::string& moduleName,
                                       const std::string& oldParameterName,
                                       const std::string& newParameterName)
   {
   bool modified = false;
   RenameParameter rename(oldParameterName, newParameterName, modified);
   enumerateParameters(sectionName, moduleName, false, rename.callback);
   return modified;
   }
// ------------------------------------------------------------------
// Delete the specified parameter from all instances of module.
// ------------------------------------------------------------------
class DeleteParameter
   {
   public:
      DeleteParameter(const string& pName, vector<string>& v)
         : paramName(pName), values(v) { }

      void callback(IniFile* par, const string& section)
         {
         par->read(section, paramName, values);
         par->deleteKey(section, paramName);
         }

   private:
      const string& paramName;
      vector<string>& values;
   };
bool ApsimControlFile::deleteParameter(const std::string& sectionName,
                                       const std::string& moduleName,
                                       const std::string& parameterName)
   {
   vector<string> values;
   DeleteParameter deleteParam(parameterName, values);
   enumerateParameters(sectionName, moduleName, false, deleteParam.callback);
   return (values.size() > 0);
   }
// ------------------------------------------------------------------
// Moves the specified parameter from all instances of module to the
// specified destination instance.  Return true if change was made.
// ------------------------------------------------------------------
bool ApsimControlFile::moveParameter(const std::string& sectionName,
                                     const std::string& moduleName,
                                     const std::string& parameterName,
                                     const std::string& destModuleName)
   {
   vector<string> values;
   DeleteParameter deleteParam(parameterName, values);
   enumerateParameters(sectionName, moduleName, false, deleteParam.callback);
   if (values.size() > 0)
      {
      if (destModuleName == "")
         {
         if (Str_i_Eq(parameterName, "title"))
            setTitle(sectionName, values[0]);
         }
      else
         {
         string instanceName = moduleToInstance(sectionName,destModuleName);
         if (instanceName == "")
            instanceName = destModuleName;
         setParameterValues(sectionName, instanceName, parameterName, values);
         }
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// Return true if the control file has parameters in it.
// ------------------------------------------------------------------
bool ApsimControlFile::hasParametersInCon(const std::string& section)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   for (unsigned i = 0; i != paramFiles.size(); ++i)
      {
      if (paramFiles[i].fileName == ini->getFileName())
         return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// remove all references to this control file from the list of
// parameter files for all modules.
// ------------------------------------------------------------------
void ApsimControlFile::removeSelfReferences(const std::string& section,
                                            const std::string& parFileForConParams)
   {
   // read in all 'module=' lines from control file.
   vector<string> moduleLines;
   ini->read(section, "module", moduleLines);

   // loop through all module lines
   vector<string> lines;
   for (unsigned m = 0; m != moduleLines.size(); m++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(ini->getFileName(), moduleLines[m], paramFiles, false);
      bool badReferenceFound = false;
      for (unsigned p = 0; p != paramFiles.size(); ++p)
         {
         if (paramFiles[p].fileName == ini->getFileName())
            {
            paramFiles[p].fileName = parFileForConParams;
            badReferenceFound = true;
            }
         }
      if (badReferenceFound)
         moduleLines[m] = createModuleLine(paramFiles);
      }
   ini->write(section, "module", moduleLines);
   }
// ------------------------------------------------------------------
// move all parameters out of the control file and into a par
// file.  Return true if parameters were moved.
// ------------------------------------------------------------------
bool ApsimControlFile::moveParametersOutOfCon(const std::string& section,
                                              const std::string& parFileForConParams)
   {
   vector<ParamFile> paramFiles;
   getParameterFiles(ini, section, paramFiles);

   // if parFileToUse is blank then ask user for a file to put all
   // parameters into.
   IniFile* par = getParFile(parFileForConParams);
   if (par != NULL)
      {
      bool selfReferencesFound = false;
      for (unsigned i = 0; i != paramFiles.size(); ++i)
         {
         if (paramFiles[i].fileName == ini->getFileName())
            {
            selfReferencesFound = true;

            vector<string> paramFileSections;
            getParFileSectionsMatching(ini, paramFiles[i], paramFileSections);
            for (unsigned s = 0; s != paramFileSections.size(); s++)
               {
               string contents;
               ini->readSection(paramFileSections[s], contents);
               ini->deleteSection(paramFileSections[s]);
               par->writeSection(paramFileSections[s], contents);
               }
            }
         }
      // tell control file to remove the self reference for this instance.
      if (selfReferencesFound)
         removeSelfReferences(section, parFileForConParams);
      return selfReferencesFound;
      }
   return false;
   }
// ------------------------------------------------------------------
// Enumerate all parameter sections for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::enumerateParameters(const std::string& section,
                                           const std::string& moduleName,
                                           bool includeConstants,
                                           ParamCallbackEvent callback)
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForModule(ini, section, moduleName, paramFiles, includeConstants);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      if (paramFiles[p].fileName != "")
         {
         IniFile* par = getParFile(paramFiles[p].fileName);
         if (par != NULL)
            {
            vector<string> paramFileSections;
            getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
            for (unsigned s = 0; s != paramFileSections.size(); s++)
               callback(par, paramFileSections[s]);
            }
         }
      }
   }
// ------------------------------------------------------------------
// Enumerate all parameter sections for the specified instance name.
// ------------------------------------------------------------------
void ApsimControlFile::enumerateParametersForInstance(const std::string& section,
                                                      const std::string& instanceName,
                                                      bool constantsOnly,
                                                      ParamCallbackEvent callback)
   {
   vector<ParamFile> paramFiles;
   getParameterFilesForInstance(ini, section, instanceName, paramFiles, constantsOnly);
   for (unsigned p = 0; p != paramFiles.size(); p++)
      {
      if (paramFiles[p].fileName != "")
         {
         if (!constantsOnly
             || Str_i_Eq(Path(paramFiles[p].fileName).Get_extension(), ".ini"))
            {
            IniFile* par = getParFile(paramFiles[p].fileName);
            if (par != NULL)
               {
               vector<string> paramFileSections;
               getParFileSectionsMatching(par, paramFiles[p], paramFileSections);
               for (unsigned s = 0; s != paramFileSections.size(); s++)
                  callback(par, paramFileSections[s]);
               }
            }
         }
      }
   }

