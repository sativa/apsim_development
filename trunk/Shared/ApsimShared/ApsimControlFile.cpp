//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimControlFile.h"
#include "ApsimDirectories.h"
#include "ApsimSimulationFile.h"
#include <general\string_functions.h>
#include <general\stristr.h>
#include <general\IniFile.h>
#include <general\path.h>
#include <fstream>

using namespace std;
#pragma package(smart_init)

class Instance
   {
   public:
      string moduleName;
      string instanceName;
   };

class Parameter
   {
   public:
      string moduleName;
      string instanceName;
      string sectionName;
      string name;

      bool isParameter;
      bool isConstant;
      bool isTable;
      bool isRule;
      bool isError;

      string parameterValue;

      string tableFileName;
      unsigned long tableStartPos;

      string ruleContents;

      string errorMessage;

   };

struct ParamFile
   {
   string moduleName;
   string instanceName;
   string fileName;
   string sectionName;
   };
// ------------------------------------------------------------------
// Parse the specified 'module=' line to extract the module name,
// the instance name and the parameter files and sections referenced.
// Throws exception on error.
// ------------------------------------------------------------------
void parseModuleLine(const string& controlFileName, const string& moduleLine,
                     vector<ParamFile> paramFiles) throw(runtime_error)
   {
   string line = moduleLine;

   // remove any comments
   line.erase(line.find('!'));

   // initialise everything to zero.
   string moduleName;
   string instanceName;
   string paramFile;
   string section;

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

                         // replace any apsuite macros.
                         Replace_all(paramFile, "%apsuite", getApsimDirectory().c_str());

                         // make sure we add a path to param file if necessary.
                         if (paramFile.find(":") == string::npos &&
                             paramFile[0] != '\\')
                            {
                            Path p(controlFileName);
                            p.Append_path (paramFile.c_str());
                            paramFile = p.Get_path();
                            }
                         ParamFile p;
                         p.fileName = paramFile;
                         p.sectionName = section + "." + instanceName;
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
   if (state != READ_PARAM_FILE)
      throw runtime_error("Invalid control file line: " + moduleLine);
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void parseControlSection(const string& controlFileName, const string& section,
                         vector<ParamFile>& paramFiles) throw(runtime_error)
   {
   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   // read in all 'module=' lines from control file.
   IniFile controlFile(controlFileName);
   vector<string> moduleLines;
   controlFile.read(fullSection, "module", moduleLines);

   // loop through all module lines
   for (vector<string>::const_iterator moduleLineI = moduleLines.begin();
                                       moduleLineI != moduleLines.end();
                                       moduleLineI++)
      parseModuleLine(controlFileName, *moduleLineI, paramFiles);
   }

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimControlFile::ApsimControlFile (void)
   {
   }

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimControlFile::ApsimControlFile (const string& filename)
   {
   fileName = filename;
   }

// ------------------------------------------------------------------
// Return a list of all section names to caller.
// ------------------------------------------------------------------
void ApsimControlFile::getAllSectionNames (vector<string>& sectionNames) const
   {
   string line, sectionName;

   ifstream controlStream(fileName.c_str());
   while (getline(controlStream, line))
      {
      sectionName = getSectionName(line);
      if (sectionName.length() > 0)
         {
         char* posPrefix = stristr(sectionName.c_str(), "APSIM.");
         if (posPrefix != NULL)
            {
            posPrefix += strlen("APSIM.");
            sectionNames.push_back(posPrefix);
            }
         }
      }
   }
// ------------------------------------------------------------------
// Return a list of all modules that this control file references.
// ------------------------------------------------------------------
void ApsimControlFile::getAllFiles(const string& section,
                                   vector<string>& fileNames) const throw(runtime_error)
   {
   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   vector<ParamFile> paramFiles;
   parseControlSection(fileName, fullSection, paramFiles);
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
string ApsimControlFile::getSectionContents(const string& section) const
   {
   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   IniFile controlFile(fileName);
   string contents;
   controlFile.readSection(fullSection, contents);
   return contents;
   }

// ------------------------------------------------------------------
// Set the contents of the specified section.
// ------------------------------------------------------------------
void ApsimControlFile::setSectionContents(const string& section,
                                          const string& contents)
   {
   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   IniFile controlFile(fileName);
   controlFile.writeSection(fullSection, contents);
   }
// ------------------------------------------------------------------
// Return a list of all output file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getOutputFileNames(const string& section,
                                          vector<string>& fileNames) const
   {
   vector<string> reportInstances;
   getInstances(section, "report", reportInstances);
   for (vector<string>::iterator instanceI = reportInstances.begin();
                                 instanceI != reportInstances.end();
                                 instanceI++)
      getParamValues(section, *instanceI, "outputfile", fileNames);
   }
// ------------------------------------------------------------------
// Return a list of all summary file names for the specified section
// ------------------------------------------------------------------
void ApsimControlFile::getSummaryFileNames(const string& section,
                                           vector<string>& fileNames) const
   {
   vector<string> reportInstances;
   getInstances(section, "report", reportInstances);
   for (vector<string>::iterator instanceI = reportInstances.begin();
                                 instanceI != reportInstances.end();
                                 instanceI++)
      getParamValues(section, *instanceI, "summaryfile", fileNames);
   }

// ------------------------------------------------------------------
// Run apsim using the specified configuration file.
// If sections.size() > 0 then only those sections specified will be run
// If sections.size() == 0 then all sections will be run.
// ------------------------------------------------------------------
void ApsimControlFile::run(const vector<string>& sections,
                           const string& configurationFile,
                           bool quiet) const
   {
   vector<string> sectionsToRun = sections;
   if (sectionsToRun.size() == 0)
      getAllSectionNames(sectionsToRun);

   for (vector<string>::const_iterator sectionI = sectionsToRun.begin();
                                       sectionI != sectionsToRun.end();
                                       sectionI++)
      {
      ApsimSimulationFile simulation(createSIM(*sectionI, configurationFile));
      simulation.run(quiet);
      }
   }
// ------------------------------------------------------------------
// Return a list of instance names for the specified module name.
// ------------------------------------------------------------------
void ApsimControlFile::getInstances(const string& section,
                                    const string& moduleName,
                                    vector<string>& instanceNames) const
   {
   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   vector<ParamFile> paramFiles;
   parseControlSection(fileName, fullSection, paramFiles);
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
// Return the values of the specified parameter for the specified
// module and section.
// ------------------------------------------------------------------
void ApsimControlFile::getParamValues(const string& section,
                                      const string& instanceName,
                                      const string& parameterName,
                                      vector<string>& values) const
   {
   // empty parameter values
   values.erase(values.begin(), values.end());

   // create the section name to search for in the control file.
   string fullSection = "APSIM." + section;

   vector<ParamFile> paramFiles;
   parseControlSection(fileName, fullSection, paramFiles);
   for (vector<ParamFile>::const_iterator paramFileI = paramFiles.begin();
                                          paramFileI != paramFiles.end();
                                          paramFileI++)
      {
      if (Str_i_Eq(paramFileI->instanceName, instanceName))
         {
         IniFile ini(paramFileI->fileName);
         ini.read(paramFileI->sectionName + ".parameters", parameterName,
                  values);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     get a list of all module names for specified section.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
/*void ApsimControlFile::getModuleNames (const string& controlSection,
                                       list<string>& moduleNames)
   {
   string sectionName = "APSIM.";
   sectionName += controlSection;

   Ini_file controlFile;
   controlFile.Set_file_name (controlFilename.c_str());
   controlFile.Read_list(sectionName.c_str(), "module", moduleNames);

   for (list<string>::iterator moduleI = moduleNames.begin();
                               moduleI != moduleNames.end();
                               moduleI++)
      {
      // remove everything after the first space.
      (*moduleI).erase((*moduleI).find(" "));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     enumerate through all parameters for the APSIM specified section.

//  Notes:

//  Changes:
//    DPH 15/10/98
//    Dph 12/4/2000 changed from foreachfunction to CallbackFunction

// ------------------------------------------------------------------
void ApsimControlFile::enumerateInstances(const string& controlSection,
                                          CallbackFunction<Instance&>& function)
   {
   // create the section name to search for in the control file.
   string sectionName = "APSIM.";
   sectionName += controlSection;

   // read in all 'module=' lines from control file.
   Ini_file controlFile;
   controlFile.Set_file_name (controlFilename.c_str());
   list<string> moduleLines;
   controlFile.Read_list (sectionName.c_str(), "module", moduleLines);

   // loop through all module lines
   string moduleName, instantiationName;
   for (list<string>::iterator moduleI = moduleLines.begin();
                               moduleI != moduleLines.end();
                               moduleI++)
      {
      ParamFileListType paramFiles;
      parseModuleLine (*moduleI, moduleName, instantiationName, paramFiles);

      Instance i;
      i.moduleName = moduleName;
      i.instanceName = instantiationName;
      function.callback(i);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     enumerate through all parameters for the APSIM specified section.

//  Notes:

//  Changes:
//    DPH 15/10/98
//    Dph 12/4/2000 changed from foreachfunction to CallbackFunction
// ------------------------------------------------------------------
void ApsimControlFile::enumerateParameters
   (const string& controlSection, CallbackFunction<Parameter&>& function)
   {
   // create the section name to search for in the control file.
   string sectionName = "APSIM.";
   sectionName += controlSection;

   // read in all 'module=' lines from control file.
   Ini_file controlFile;
   controlFile.Set_file_name (controlFilename.c_str());
   list<string> moduleLines;
   controlFile.Read_list (sectionName.c_str(), "module", moduleLines);

   // loop through all module lines
   string moduleName, instantiationName;
   for (list<string>::iterator moduleI = moduleLines.begin();
                               moduleI != moduleLines.end();
                               moduleI++)
      {
      ParamFileListType paramFiles;
      parseModuleLine (*moduleI, moduleName, instantiationName, paramFiles);

      // loop through all parameter files and sections.
      for (ParamFileListType::iterator paramFileI = paramFiles.begin();
                                       paramFileI != paramFiles.end();
                                       paramFileI++)
         {
         string paramFile = (*paramFileI).filename;
         string sectionName = (*paramFileI).sectionName;
         // remove leading instance name.
         sectionName.erase(sectionName.find("."));

         enumerateSection (moduleName, instantiationName, paramFile,
                           sectionName, function);
         }

      if (paramFiles.size() == 0)
         {
         // special case - no parameter for module.
         Parameter p;
         p.isParameter = true;
         p.moduleName = moduleName;
         p.instanceName = instantiationName;
         p.isError = false;
         function.callback(p);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Parse an APSIM section name into it's components.  Returns
//     true if a valid section name.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
bool ApsimControlFile::parseSectionName(const string& line,
                                        string& firstWord,
                                        string& secondWord,
                                        string& thirdWord)
   {
   string sectionName = Get_section_name(line.c_str());
   if (sectionName.length() > 0)
      {
      size_t posFirstPeriod = sectionName.find(".");
      if (posFirstPeriod != string::npos)
         {
         size_t posSecondPeriod = sectionName.substr
                                  (posFirstPeriod+1).find(".");
         if (posSecondPeriod != string::npos)
            {
            posSecondPeriod += posFirstPeriod + 1;
            firstWord = sectionName.substr(0, posFirstPeriod);
            secondWord = sectionName.substr
                         (posFirstPeriod+1,
                          posSecondPeriod - posFirstPeriod - 1);
            thirdWord = sectionName.substr(posSecondPeriod+1);
            return true;
            }
         }
      }
   return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     Go find an APSIM section name on the specified stream that
//     matches the modulename and section name passed in.  Returns
//     true and the third part of the section name if found.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
bool ApsimControlFile::findParameterSection(istream& in,
                                            const string& moduleName,
                                            const string& sectionName,
                                            string& thirdWord)
   {
   // go find a parameter section.
   string firstWord, secondWord;
   string line;
   getline(in, line);
   while (in)
      {
      if (parseSectionName (line, firstWord, secondWord, thirdWord))
         {
         if (Str_i_Eq(firstWord, sectionName) &&
             Str_i_Eq(secondWord, moduleName))
            return true;
         }
      getline(in, line);
      }
   return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     Enumerate through all parameters for the specified section in the
//     specified file.

//  Notes:

//  Changes:
//    DPH 15/10/98
//    Dph 12/4/2000 changed from foreachfunction to CallbackFunction
// ------------------------------------------------------------------
void ApsimControlFile::enumerateSection (const string& moduleName,
                                         const string& instName,
                                         const string& paramFile,
                                         const string& sectionName,
                                         CallbackFunction<Parameter&>& function)
   {
   ifstream in (paramFile.c_str());

   Parameter p;
   p.moduleName = moduleName;
   p.instanceName = instName;
   bool foundASection = false;

   while (findParameterSection (in, instName, sectionName, p.sectionName))
      {
      foundASection = true;
      long previousLinePos;

      // found section - are we dealing with manager?
      if (Str_i_Eq(moduleName, "manager") || Str_i_Eq(moduleName, "operatns"))
         {
         // yes manager - pass section contents to function
         p.ruleContents = "";
         string line;
         previousLinePos = in.tellg();
         getline(in, line);
         while (in)
            {
            if (Get_section_name(line.c_str()).length() == 0)
               {
               p.ruleContents += line + "[cr]";
               previousLinePos = in.tellg();
               getline(in, line);
               }
            else
               break;
            }

         p.isParameter = false;
         p.isConstant = false;
         p.isTable = false;
         p.isRule = true;
         p.isError = false;
         p.name = p.sectionName;
         function.callback(p);
         }
      else
         {
         // no not manager - can be either a data table section or a parameters section
         string line;
         previousLinePos = in.tellg();
         getline(in, line);
         while (in && Get_section_name(line.c_str()).length() == 0)
            {
            size_t posComment = line.find("!");
            if (posComment != string::npos)
               line.erase(posComment);
            Strip(line, " ");

            if (line.length() > 0)
               {
               // get a parameter name and value, and remove the units and comments from end
               Get_keyname_and_value (line.c_str(), p.name, p.parameterValue);

               if (!Str_i_Eq(moduleName, "report"))
                  {
                  size_t posUnits = p.parameterValue.find("(");
                  if (posUnits != string::npos)
                     p.parameterValue.erase(posUnits);
                  }
               Strip(p.parameterValue, " ");

               if (p.name.length() > 0)
                  {
                  p.isParameter = (stristr((char*) paramFile.c_str(), ".ini")
                                   == NULL);
                  p.isConstant = !p.isParameter;
                  p.isTable = false;
                  p.isRule = false;
                  p.isError = false;
                  function.callback(p);
                  }
               else
                  {
                  // must be a data table.
                  p.isParameter = false;
                  p.isConstant = false;
                  p.isTable = true;
                  p.isRule = false;
                  p.isError = false;
                  p.name = "default";
                  p.tableFileName = paramFile;
                  p.tableStartPos = previousLinePos;
                  function.callback(p);

                  if (Str_i_Eq(moduleName, "met") ||
                      Str_i_Eq(moduleName, "input") ||
                      Str_i_Eq(moduleName, "soi"))
                     break;
                  }
               }

            // goto next line.
            previousLinePos = in.tellg();
            getline(in, line);
            }
         }
      if (in)
         in.seekg(previousLinePos);
      }
   if (!foundASection)
      {
      p.isParameter = false;
      p.isConstant = false;
      p.isTable = false;
      p.isRule = false;
      p.isError = true;
      p.errorMessage = "Cannot find section " + sectionName + " in file " +
                       paramFile;
      function.callback(p);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     get a list of parameter files from the control file for the
//     specified module

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
void ApsimControlFile::getParamFilesForModule (const string&  controlsection,
                                               const string&  moduleName,
                                               ParamFileListType& paramFiles)
   {
   paramFiles.erase (paramFiles.begin(), paramFiles.end());

   // create the section name to search for in the control file.
   string sectionName = "APSIM.";
   sectionName += controlsection;

   // read in all 'module=' lines from control file.
   Ini_file controlFile;
   controlFile.Set_file_name (controlFilename.c_str());
   list<string> moduleLines;
   controlFile.Read_list (sectionName.c_str(), "module", moduleLines);

   // loop through all module lines looking for our module name
   string module, instantiationName;
   for (list<string>::iterator moduleLineI = moduleLines.begin();
                               moduleLineI != moduleLines.end();
                               moduleLineI++)
      {
      ParamFileListType ourParamFiles;
      parseModuleLine (*moduleLineI, module,
                        instantiationName, ourParamFiles);
      if (moduleName.length() == 0 || Str_i_Eq(module, moduleName))
         {
         std::copy (ourParamFiles.begin(), ourParamFiles.end(),
                    std::back_inserter(paramFiles));
         }
      }
   }


// ------------------------------------------------------------------
//  Short description:
//     Get a parameter from the specified file in the specified APSIM section.
//     Return true if parameter found.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
bool ApsimControlFile::getParametersFromFile(const string& fileName,
                                             const string& sectionName,
                                             const string& parameterName,
                                             list<string>& parameterValues)
   {
   // read parameters from file.
   Ini_file paramFile;
   paramFile.Set_file_name (fileName.c_str());
   list<string> values;
   paramFile.Read_list (sectionName.c_str(), parameterName.c_str(), values);

   // copy our values to the return list.
   std::copy (values.begin(), values.end(), std::back_inserter(parameterValues));

   return (values.size() > 0);
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of all parameter file names for all modules.
//     Excludes .ini files.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
void ApsimControlFile::getAllParamFiles (const string& controlSection,
                                         list<string>& fileNames)
   {
   ParamFileListType files;
   getParamFilesForModule (controlSection, "", files);

   // remove all files that have a .ini in their name.
   for (ParamFileListType::iterator fileI = files.begin();
                                    fileI != files.end();
                                    fileI++)
      {
      string file = (*fileI).filename;
      if ( stristr((char*) file.c_str(), ".ini") == NULL)
         fileNames.push_back ( file);
      }
   fileNames.erase(std::unique(fileNames.begin(), fileNames.end()),
                    fileNames.end());
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of all constant (.ini) file names for all modules.

//  Notes:

//  Changes:
//    DPH 15/10/98

// ------------------------------------------------------------------
void ApsimControlFile::getAllConstantFiles(const string& controlSection,
                                           list<string>& fileNames)
   {
   ParamFileListType files;
   getParamFilesForModule (controlSection, "", files);

   // remove all files that have a .ini in their name.
   for (ParamFileListType::iterator fileI = files.begin();
                                    fileI != files.end();
                                    fileI++)
      {
      string file = (*fileI).filename;
      if ( stristr((char*) file.c_str(), ".ini") != NULL)
         fileNames.push_back (file);
      }
   fileNames.erase(std::unique(fileNames.begin(), fileNames.end()),
                    fileNames.end());
   }
*/
// ------------------------------------------------------------------
// Create a SIM file for the specified section and return its filename.
// ------------------------------------------------------------------
std::string ApsimControlFile::createSIM(const string& section,
                                        const string& configurationFile) const
   {
   string simulationFileName;

   return simulationFileName;
   }

