//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimControlFile.h"
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimConfigurationFile.h>
#include "ApsimSimulationFile.h"
#include "ControlFileConverter.h"
#include "ApsimVersion.h"

#include <general\string_functions.h>
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
   string instanceName;
   string fileName;
   string sectionName;
   };

// ------------------------------------------------------------------
// This class implements a component compare method based on the
// order of components listed in the component.ordering file.
// This class is used to sort modules before writing the .sim file.
// ------------------------------------------------------------------
class ComponentOrder
   {
   public:

      // ------------------------------------------------------------------
      // constructor - read in component order.
      // ------------------------------------------------------------------
      ComponentOrder(void)
         {
         IniFile componentOrdering(getApsimDirectory() + "\\apsim\\component.ordering");
         componentOrdering.read("component_order", "component", components);
         }
      // ------------------------------------------------------------------
      // Compare method used by sort.  Returns true if arg1 < arg2.
      // ------------------------------------------------------------------
      bool operator() (const ParamFile& arg1, const ParamFile& arg2)
         {
         for (unsigned i = 0; i != components.size(); i++)
            {
            if (Str_i_Eq(components[i], arg1.instanceName))
               return true;
            if (Str_i_Eq(components[i], arg2.instanceName))
               return false;
            }
         return true; // neither are in list!!
         }

   private:
      vector<string> components;
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
   while (line[currentPos] != 0)
      {
      char ch = line[currentPos];

      switch (state)
         {
         case READ_MODULE_NAME :
                      if (ch == ' ')
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
                      if (ch == '(')
                         state = READ_INSTANTIATION;
                      else if (ch == '[')
                         state = READ_SECTION;
                      else if (ch != ' ')
                         paramFile += ch;
                      break;
         case READ_SECTION :
                      if (ch == ']')
                         {
                         if (instanceName.length() == 0)
                            instanceName = moduleName;

                         state = READ_PARAM_FILE;
                         if (paramFile.length() == 0)
                            paramFile = controlFileName;
                         else if (removeMacros)
                            {
                            // replace any apsuite macros.
                            Replace_all(paramFile, "%apsuite", getApsimDirectory().c_str());
                            }

                         ParamFile p;
                         p.fileName = paramFile;
                         p.sectionName = section;
                         p.moduleName = moduleName;
                         p.instanceName = instanceName;
                         if (instanceName == "")
                            p.instanceName = moduleName;
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
      ParamFile p;
      p.moduleName = moduleName;
      if (instanceName.length() == 0)
       instanceName = moduleName;
      p.instanceName = instanceName;
      paramFiles.push_back(p);
      return;
      }
   if (state != READ_PARAM_FILE)
      throw runtime_error("Invalid control file line: " + moduleLine);
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void parseControlSection(const string& controlFileName, const string& section,
                         vector<ParamFile>& paramFiles) throw(runtime_error)
   {
   // read in all 'module=' lines from control file.
   IniFile controlFile(controlFileName);
   vector<string> moduleLines;
   controlFile.read(section, "module", moduleLines);

   // loop through all module lines
   for (vector<string>::const_iterator moduleLineI = moduleLines.begin();
                                       moduleLineI != moduleLines.end();
                                       moduleLineI++)
      parseModuleLine(controlFileName, *moduleLineI, paramFiles);
   }

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimControlFile::ApsimControlFile (const string& filename,
                                    const string& s) throw(std::runtime_error)
   {
   fileName = filename;
   section = s;
   }
// ------------------------------------------------------------------
// Return a list of all section names to caller.
// ------------------------------------------------------------------
void ApsimControlFile::getAllSectionNames (const string& fileName,
                                           vector<string>& sectionNames)
   {
   string line, sectionName;

   ifstream controlStream(fileName.c_str());
   while (getline(controlStream, line))
      {
      sectionName = getSectionName(line);
      if (sectionName.length() > 0)
         sectionNames.push_back(sectionName);
      }
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void ApsimControlFile::getAllFiles(vector<string>& fileNames) const throw(runtime_error)
   {
   vector<ParamFile> paramFiles;
   parseControlSection(fileName, section, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (find(fileNames.begin(), fileNames.end(), paramFileI->fileName) != fileNames.end())
         fileNames.push_back(paramFileI->fileName);
      }
   }

// ------------------------------------------------------------------
// Return the contents of the specified control file section.
// ------------------------------------------------------------------
string ApsimControlFile::getSectionContents(void) const
   {
   IniFile controlFile(fileName);
   string contents;
   controlFile.readSection(section, contents);
   return contents;
   }

// ------------------------------------------------------------------
// Set the contents of the specified section.
// ------------------------------------------------------------------
void ApsimControlFile::setSectionContents(const string& contents)
   {
   IniFile controlFile(fileName);
   controlFile.writeSection(section, contents);
   }
// ------------------------------------------------------------------
// Return a list of all output file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getOutputFileNames(vector<string>& fileNames) const
   {
   vector<ApsimParameterFile> paramFiles;
   getParameterFiles("report", paramFiles);

   for (vector<ApsimParameterFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      paramFile->getParamValues("outputfile", fileNames);
   }
// ------------------------------------------------------------------
// Return a list of all summary file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getSummaryFileNames(vector<string>& fileNames) const
   {
   vector<ApsimParameterFile> paramFiles;
   getParameterFiles("report", paramFiles);

   for (vector<ApsimParameterFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      paramFile->getParamValues("summaryfile", fileNames);
   }

// ------------------------------------------------------------------
// Run apsim using the specified configuration file.
// ------------------------------------------------------------------
void ApsimControlFile::run(const string& configurationFile,
                           bool console) const throw(runtime_error)
   {
   try
      {
      string simFileName;
      createSIM(configurationFile, simFileName);
      ApsimSimulationFile simulation(simFileName);
      simulation.run(console);
      }
   catch (const runtime_error& error)
      {
      throw runtime_error(string(error.what()) + ".  Cannot run APSIM.");
      }
   }
// ------------------------------------------------------------------
// Return a list of instance names for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::getInstances(const string& moduleName,
                                    vector<string>& instanceNames) const
   {
   vector<ParamFile> paramFiles;
   parseControlSection(fileName, section, paramFiles);
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
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void ApsimControlFile::getParameterFiles(const string& instanceName,
                                         vector<ApsimParameterFile>& paramFiles,
                                         bool oneFilePerInstance,
                                         bool constants) const
   {
   vector<string> instancesSoFar;

   vector<ParamFile> parFiles;
   parseControlSection(fileName, section, parFiles);
   for (vector<ParamFile>::const_iterator paramFile = parFiles.begin();
                                          paramFile != parFiles.end();
                                          paramFile++)
      {
      bool doAdd = (instanceName == "" || Str_i_Eq(paramFile->instanceName, instanceName));
      if (doAdd)
         {
         if (constants)
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) == ".ini");
         else
            doAdd = (ExtractFileExt(paramFile->fileName.c_str()) != ".ini");
         }
      if (doAdd && oneFilePerInstance)
         doAdd = (find(instancesSoFar.begin(), instancesSoFar.end(),
                       paramFile->instanceName) == instancesSoFar.end());
      if (doAdd)
         {
         vector<string> sections;
         ApsimParameterFile::getSectionNames(paramFile->fileName,
                                    paramFile->sectionName,
                                    paramFile->instanceName,
                                    sections);
         for (vector<string>::iterator section = sections.begin();
                                       section != sections.end();
                                       section++)
            paramFiles.push_back(ApsimParameterFile(paramFile->fileName,
                                                    paramFile->moduleName,
                                                    paramFile->instanceName,
                                                    *section));
         instancesSoFar.push_back(paramFile->instanceName);
         }
      }
   }
// ------------------------------------------------------------------
// Return a list of all parameter values for the specified module
// and parameter name.
// ------------------------------------------------------------------
void ApsimControlFile::getParameterValues(const string& instanceName,
                                          const string& parameterName,
                                          vector<string>& values) const
   {
   vector<ApsimParameterFile> paramFiles;
   getParameterFiles(instanceName, paramFiles);

   for (vector<ApsimParameterFile>::const_iterator
                 paramFile = paramFiles.begin();
                 paramFile != paramFiles.end();
                 paramFile++)
      paramFile->getParamValues(parameterName, values);
   }
// ------------------------------------------------------------------
// Return a single parameter value for the specified module
// and parameter name.
// ------------------------------------------------------------------
string ApsimControlFile::getParameterValue(const string& instanceName,
                                           const string& parameterName) const throw(runtime_error)
   {
   vector<string> values;
   getParameterValues(instanceName, parameterName, values);
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
void ApsimControlFile::setParameterValues(const string& moduleName,
                                          const string& sectionName,
                                          const string& parameterName,
                                          const vector<string>& parameterValues) const throw(std::runtime_error)
   {
   if (moduleName == "")
      {
      ApsimParameterFile conFile(fileName, "", "", section);
      conFile.setParamValues(parameterName, parameterValues);
      }
   else
      {
      // Return all the parameter files for the specified section and instance.
      vector<ApsimParameterFile> paramFiles;
      getParameterFiles(moduleName, paramFiles);
      if (paramFiles.size() > 0)
         paramFiles[0].setParamValues(parameterName, parameterValues);
      else
         {
         string defaultFile, defaultSection;
         getDefaultParFileAndSection(defaultFile, defaultSection);
         if (sectionName != "")
            defaultSection = sectionName;

         // write new module= line to control file.
         vector<string> moduleLines;
         IniFile conAsIni(fileName);
         conAsIni.read(section, "module", moduleLines);

         string newControlLine = moduleName + "  " + defaultFile + "["
                               + defaultSection + "]";
         moduleLines.push_back(newControlLine);
         conAsIni.write(section, "module", moduleLines);

         // now write to the parameter file.
         ApsimParameterFile parFile(defaultFile, moduleName, moduleName,
                                    defaultSection + "." + moduleName + ".parameters");
         parFile.setParamValues(parameterName, parameterValues);
         }
      }
   }
// ------------------------------------------------------------------
// Set the value of a parameter for a module
// If moduleName is blank then parameter will be written to control file
// ------------------------------------------------------------------
void ApsimControlFile::setParameterValue(const string& moduleName,
                                         const string& sectionName,
                                         const string& parameterName,
                                         const string& parameterValue) const throw(std::runtime_error)
   {
   vector<string> values;
   values.push_back(parameterValue);
   setParameterValues(moduleName, sectionName, parameterName, values);
   }
// ------------------------------------------------------------------
// Create a SIM file for the specified section and return its filename.
// Return true if sim file was created.
// ------------------------------------------------------------------
void ApsimControlFile::createSIM(const string& configurationFile,
                                 string& simulationFileName) const throw(runtime_error)
   {
   ControlFileConverter converter;
   converter.convert(fileName);

   SetCurrentDir(ExtractFileDir(fileName.c_str()));
   simulationFileName = ChangeFileExt(fileName.c_str(), ".sim").c_str();

   ApsimSimulationFile simulation;
   simulation.setFileName(simulationFileName);
   ApsimConfigurationFile configuration(configurationFile);
   string dllFileName = configuration.getDllForComponent("protocolmanager");
   if (dllFileName == "")
      throw runtime_error("Cannot find a DLL filename in configuration file for module: protocolmanager");

   simulation.setExecutableFileName(dllFileName);
   simulation.setTitle(getTitle());

   vector<ParamFile> modules;
   parseControlSection(fileName, section, modules);
   stable_sort(modules.begin(), modules.end(), ComponentOrder());
   for (vector<ParamFile>::iterator m = modules.begin();
                                    m != modules.end();
                                    m++)
      {
      ApsimComponentData component = simulation.addComponent(m->instanceName);
      string dllFileName = configuration.getDllForComponent(m->moduleName);
      component.setExecutableFileName(dllFileName);
      if (dllFileName == "")
         throw runtime_error("Cannot find a DLL filename in configuration file for module: "
                      + m->moduleName);
      }

   // Get a complete list of files and sections we're to convert.
   vector<ApsimParameterFile> paramFiles;
   getParameterFiles("", paramFiles);
   getParameterFiles("", paramFiles, false, true);

   // Loop through all files | sections and parse each.
    for (vector<ApsimParameterFile>::iterator paramFile = paramFiles.begin();
                                             paramFile != paramFiles.end();
                                             paramFile++)
      {
      paramFile->importIntoSIM(simulation);
      }
   simulation.write();
   }
// ------------------------------------------------------------------
// convert a module name to an instance name.
// ------------------------------------------------------------------
string ApsimControlFile::moduleToInstance(const string& moduleName) const
   {
   vector<string> instanceNames;
   getInstances(moduleName, instanceNames);
   if (instanceNames.size() == 0)
      throw runtime_error("Control file converter error: Cannot find an instance of module: "
                          + moduleName + ".");
   else if (instanceNames.size() > 1)
      throw runtime_error("Control file converter error: More than 1 instance of module: "
                          + moduleName + "has been found.");
   return instanceNames[0];
   }
// ------------------------------------------------------------------
// change the name of a module in the control file.  Return true
// on success.
// ------------------------------------------------------------------
bool ApsimControlFile::changeModuleName(const std::string& oldModuleName,
                                        const std::string& newModuleName) const
   {
   bool changeMade = false;

   ApsimParameterFile controlFile(fileName, "", "", section);
   vector<string> lines;
   controlFile.getParamValues("module", lines);

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
   controlFile.setParamValues("module", lines);
   return changeMade;
   }
// ------------------------------------------------------------------
// Return a title to caller.
// ------------------------------------------------------------------
string ApsimControlFile::getTitle(void) const
   {
   ApsimParameterFile controlFile(fileName, "", "", section);
   return controlFile.getParamValue("title");
   }
// ------------------------------------------------------------------
// remove all references to this control file from the list of
// parameter files for all modules.
// ------------------------------------------------------------------
void ApsimControlFile::removeSelfReferences(const std::string& parFileForConParams)
   {
   // read in all 'module=' lines from control file.
   IniFile controlFile(fileName);
   vector<string> moduleLines;
   controlFile.read(section, "module", moduleLines);

   // loop through all module lines
   bool needToRewrite = false;
   vector<string> lines;
   for (vector<string>::const_iterator moduleLineI = moduleLines.begin();
                                       moduleLineI != moduleLines.end();
                                       moduleLineI++)
      {
      vector<ParamFile> paramFiles;
      parseModuleLine(fileName, *moduleLineI, paramFiles, false);

      for (unsigned p = 0; p != paramFiles.size() && !needToRewrite; ++p)
         {
         if (paramFiles[p].fileName == fileName)
            needToRewrite = true;
         }
      if (needToRewrite)
         {
         string line;
         line += paramFiles[0].moduleName;
         if (paramFiles[0].moduleName != paramFiles[0].instanceName)
            line += "(" + paramFiles[0].instanceName + ")";
         for (unsigned p = 0; p != paramFiles.size(); ++p)
            {
            line += " ";
            if (paramFiles[p].fileName == fileName)
               line += parFileForConParams;
            else if (paramFiles[p].fileName != "")
               line += paramFiles[p].fileName;
            if (paramFiles[p].sectionName != "")
               line += "[" + paramFiles[p].sectionName + "]";
            }
         lines.push_back(line);
         }
      }
   if (lines.size() > 0)
      controlFile.write(section, "module", lines);
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
// Get a parameter file from the control file - any module will do.
// Also return a section name.
// ------------------------------------------------------------------
void ApsimControlFile::getDefaultParFileAndSection(string& defaultFile,
                                                   string& defaultSection) const
   {
   vector<ApsimParameterFile> paramFiles;
   getParameterFiles("", paramFiles);

   defaultFile = "";
   defaultSection = "";

   unsigned i = 0;
   while (i < paramFiles.size() && ExtractFileExt(defaultFile.c_str()) != ".par")
      {
      defaultFile = paramFiles[i].getFileName();
      defaultSection = paramFiles[i].getSection();
      //remove the .clock.parameters bit
      defaultSection.erase(defaultSection.find('.'));
      i++;
      }
   if (ExtractFileExt(defaultFile.c_str()) != ".par")
      defaultFile = "";
   }


