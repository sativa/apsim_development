//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimControlFile.h"
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimConfigurationFile.h>
#include "ApsimSimulationFile.h"
#include "TControlFileConversionForm.h"
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
// Parse the specified 'module=' line to extract the module name,
// the instance name and the parameter files and sections referenced.
// Throws exception on error.
// ------------------------------------------------------------------
void parseModuleLine(const string& controlFileName, const string& moduleLine,
                     vector<ParamFile>& paramFiles) throw(runtime_error)
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
                            {
                            throw runtime_error("APSIM 2.2 and above no longer supports module parameters being "
                                                " in the control file.  Please move all module parameters "
                                                " from the control file to a parameter file.");
                            }

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

   // This will throw if there is an error.
   vector<ParamFile> paramFiles;
   parseControlSection(fileName, section, paramFiles);
   if (paramFiles.size() == 0)
       throw runtime_error("APSIM 2.2 and above no longer supports module parameters being "
                           " in the control file.  Please move all module parameters "
                           " from the control file to a parameter file.");
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
   convertControlFile();

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
   convertControlFile();

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
      if (createSIM(configurationFile, simFileName))
         {
         ApsimSimulationFile simulation(simFileName);
         simulation.run(console);
         }
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
// change the name of a module in the control file.
// ------------------------------------------------------------------
void ApsimControlFile::changeModuleName(const std::string& oldModuleName,
                                        const std::string& newModuleName) const
   {
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
            line->replace(posStartModuleName, moduleNameLength, newModuleName);
            }
         }
      }
   controlFile.setParamValues("module", lines);

   // now go through all sections in all parameters for the module and
   // rename the module.
   vector<ApsimParameterFile> files;
   getParameterFiles(oldModuleName, files);
   for (vector<ApsimParameterFile>::iterator file = files.begin();
                                             file != files.end();
                                             file++)
      {
      file->changeModuleName(newModuleName);
      }
   }
// ------------------------------------------------------------------
// Return all the parameter files for the specified section and instance.
// ------------------------------------------------------------------
void ApsimControlFile::getParameterFiles(const string& moduleName,
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
      bool doAdd = (paramFile->fileName != "");
      if (doAdd)
         doAdd = (moduleName == "" || Str_i_Eq(paramFile->moduleName, moduleName));
      if (doAdd)
         {
         if (constants)
            doAdd = Str_i_Eq(paramFile->sectionName, "standard");
         else
            doAdd = !Str_i_Eq(paramFile->sectionName, "standard");
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
void ApsimControlFile::getParameterValues(const string& moduleName,
                                          const string& parameterName,
                                          vector<string>& values) const
   {
   convertControlFile();

   vector<ApsimParameterFile> paramFiles;
   getParameterFiles(moduleName, paramFiles);

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
string ApsimControlFile::getParameterValue(const string& moduleName,
                                           const string& parameterName) const throw(runtime_error)
   {
   vector<string> values;
   getParameterValues(moduleName, parameterName, values);
   if (values.size() == 0)
      throw runtime_error("Expected a single parameter value but none was found. "
                          "Module name: " + moduleName +
                          "Parameter name: " + parameterName);
   if (values.size() > 1)
      throw runtime_error("Expected a single parameter value but multiple were found. "
                          "Module name: " + moduleName +
                          "Parameter name: " + parameterName);
   return values[0];
   }
// ------------------------------------------------------------------
// Create a SIM file for the specified section and return its filename.
// Return true if sim file was created.
// ------------------------------------------------------------------
bool ApsimControlFile::createSIM(const string& configurationFile,
                                 string& simulationFileName) const throw(runtime_error)
   {
   if (convertControlFile())
      {
      simulationFileName = ChangeFileExt(fileName.c_str(), ".sim").c_str();

      ApsimSimulationFile simulation;
      simulation.setFileName(simulationFileName);
      ApsimConfigurationFile configuration(configurationFile);
      string dllFileName = configuration.getDllForComponent("protocolmanager");
      if (dllFileName == "")
         {
         string msg = "Cannot find a DLL filename in configuration file for module: protocolmanager";
         ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP);
         return false;
         }
      simulation.setExecutableFileName(dllFileName);

      createServices(simulation, configuration);

      vector<ParamFile> modules;
      parseControlSection(fileName, section, modules);
      for (vector<ParamFile>::iterator m = modules.begin();
                                       m != modules.end();
                                       m++)
         {
         ApsimComponentData component = simulation.addComponent(m->instanceName);
         string dllFileName = configuration.getDllForComponent(m->moduleName);
         component.setExecutableFileName(dllFileName);
         if (dllFileName == "")
            {
            string msg = "Cannot find a DLL filename in configuration file for module: "
                         + m->moduleName;
            ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP);
            return false;
            }
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
      return true;
      }
   return false;
   }

// ------------------------------------------------------------------
// Create default services in specified simulation
// ------------------------------------------------------------------
void ApsimControlFile::createServices(ApsimSimulationFile& simulation,
                                      ApsimConfigurationFile& configuration) const
   {
   // give simulation a title.
   ApsimParameterFile controlFile(fileName, "", "", section);
   string title = controlFile.getParamValue("title");
   if (title != "")
      simulation.setTitle(title);

   // create a screen service if necessary
   ApsimServiceData screen = simulation.addService("screen");
   screen.setExecutableFileName(configuration.getDllForComponent("screen"));
   string screen_output = controlFile.getParamValue("screen_output");
   if (screen_output != "")
      screen.setProperty("screen_output", screen_output);
   
   // create an setup a log service.
   ApsimServiceData log = simulation.addService("log");
   log.setExecutableFileName(configuration.getDllForComponent("log"));
   string logFileName = Path(fileName).Get_name_without_ext() + ".log";
   log.setProperty("filename", logFileName);
   string debug_output = controlFile.getParamValue("debug_output");
   if (debug_output != "")
      log.setProperty("debug_output", debug_output);

   // create a summaryfile service.
   ApsimServiceData summaryfile = simulation.addService("summaryfile");
   summaryfile.setExecutableFileName(configuration.getDllForComponent("summaryfile"));
   string summaryfilename = controlFile.getParamValue("summaryfile");
   summaryfile.setProperty("filename", summaryfilename);
   }
// ------------------------------------------------------------------
// Convert the control / parameter file.  Return true if user pressed ok
// and conversion was performed.
// ------------------------------------------------------------------
bool ApsimControlFile::convertControlFile(void) const throw(runtime_error)
   {
   string version = getApsimVersion();
   ApsimParameterFile controlFile(fileName, "", "", section);
   string controlFileVersion = controlFile.getParamValue("version");
   if (controlFileVersion != version)
      {
      Path logPath(fileName);
      logPath.Set_extension(".conversions");
      ofstream log(logPath.Get_path().c_str(), ios::app);

      string scriptFileName = getAppHomeDirectory() + "\\conversion.script";

      TControlFileConversionForm* form = new TControlFileConversionForm(Application);
      form->Show();
      putDescriptionsInForm(scriptFileName, form);
      if (!form->waitForOkButton())
         return false;
      form->clearStatusStrings();

      ifstream in(scriptFileName.c_str());
      controlFile.setParamValue("version", version);

      unsigned statusIndex = -1;
      string description;
      bool isOptional = false;
      string msg, logmsg;
      string line;
      while (getline(in, line))
         {
         try
            {
            if (line == "")
               isOptional = false;
            else if (line != "")
               {
               unsigned pos = line.find(' ');
               string routineName = line.substr(0, pos);

               if (Str_i_Eq(routineName, "optional"))
                  {
                  pos++;
                  unsigned newPos = line.find(' ', pos);
                  routineName = line.substr(pos, newPos-pos);
                  isOptional = true;
                  }
               if (Str_i_Eq(routineName, "changeModuleName"))
                  parseChangeModuleName(line.substr(pos+1));

               else if (Str_i_Eq(routineName, "description"))
                  {
                  if (logmsg != "")
                     log << logmsg << endl;
                  description = line.substr(line.find(' ')+1);
                  msg = "<IMG \"idx:0\">" + description;
                  logmsg = "OK: " + description;
                  statusIndex++;
                  }
               else
                  {
                  unsigned posPeriod = routineName.find('.');
                  if (posPeriod == string::npos)
                     throw runtime_error("Invalid format for script command: " + routineName);
                  string moduleName = routineName.substr(0, posPeriod);
                  routineName.erase(0, posPeriod+1);

                  vector<ApsimParameterFile> paramFiles;
                  getParameterFiles(moduleName, paramFiles);
                  for (vector<ApsimParameterFile>::const_iterator
                           paramFile = paramFiles.begin();
                           paramFile != paramFiles.end();
                           paramFile++)
                     {
                     if (Str_i_Eq(routineName, "createParameter"))
                        parseCreateParameter(*paramFile, line.substr(pos+1));
                     else if (Str_i_Eq(routineName, "deleteParameter"))
                        parseDeleteParameter(*paramFile, line.substr(pos+1));
                     else if (Str_i_Eq(routineName, "newFormatReportVariables"))
                        parseNewFormatReportVariables(*paramFile, line.substr(pos+1));
                     else if (Str_i_Eq(routineName, "moveParameter"))
                        parseMoveParameter(*paramFile, line.substr(pos+1));
                     else if (Str_i_Eq(routineName, "removeReportOutputSwitch"))
                        parseRemoveReportOutputSwitch(*paramFile, line.substr(pos+1));
                     else
                        throw runtime_error("Invalid control file script routine name: " + routineName);
                     }
                  }
               }
            }
         catch (const runtime_error& error)
            {
            // output status text.
            if (!isOptional)
               {
               msg = "<IMG \"idx:1\">" + description + ".<BR>Warning: " + error.what();
               logmsg = "WARNING: " + description + "  " + error.what();
               }
            }
         form->addStatusString(msg.c_str());
         Application->ProcessMessages();
         }

      // write out last log message.
      log << logmsg << endl;
      string logCaption = "Log file: " + logPath.Get_path() + " has been writen.";
      form->addStatusString(logCaption.c_str());

      form->waitForOkButton();
      delete form;
      }
   return true;
   }

// ------------------------------------------------------------------
// Get all descriptions from the specified script file and send to form.
// ------------------------------------------------------------------
void ApsimControlFile::putDescriptionsInForm(const std::string& scriptFileName,
                                             TControlFileConversionForm* form) const
   {
   ifstream in(scriptFileName.c_str());
   string line;
   while (getline(in, line))
      {
      unsigned posDescription = line.find("description ");
      if (posDescription != string::npos)
         form->addStatusString(line.substr(posDescription+strlen("description ")).c_str());
      }
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
// Parse a parameter name (e.g. clock.start_day) string and return
// the module and parameter name.
// ------------------------------------------------------------------
void ApsimControlFile::parseName(StringTokenizer& tokenizer,
                                 string& moduleName, string& parameterName) const
   {
   parameterName = tokenizer.nextToken("(),=");
   unsigned posPeriod = parameterName.find('.');
   if (posPeriod == string::npos)
      throw runtime_error("Control file converter error: Invalid format for "
                          "parameter name.  Parameter: "
                          + parameterName);
   moduleName = parameterName.substr(0, posPeriod);
   Strip(moduleName, " ");
   parameterName = parameterName.substr(posPeriod+1);
   Strip(parameterName, " ");
   }
// ------------------------------------------------------------------
// Parse a value string and return it's value.
// ------------------------------------------------------------------
string ApsimControlFile::parseValue(const ApsimParameterFile& paramFile,
                                    StringTokenizer& tokenizer) const
   {
   // Syntax is either a literal value e.g. 10.6
   // or a function call e.g. getValue(clock.start_day) or date(1, 2000)
   string word = tokenizer.nextToken("(),=");
   Strip(word, " ");
   if (Str_i_Eq(word, "date"))
      return parseDate(paramFile, tokenizer);
   else
      {
      string value = paramFile.getParamValue(word);
      if (value == "")
         {
         // assume word is a literal and just return it as the value.
         return word;
         }
      else
         return value;
      }
   }
// ------------------------------------------------------------------
// Parse a date string and return it's value.
// ------------------------------------------------------------------
string ApsimControlFile::parseDate(const ApsimParameterFile& paramFile, StringTokenizer& tokenizer) const
   {
   // This function expects 2 values: day_of_year and year
   string dayString = parseValue(paramFile, tokenizer);
   string yearString = parseValue(paramFile, tokenizer);
   if (dayString == "" || yearString == "")
      throw runtime_error("Control file converter error: Format of date function is: "
                          "date(dayOfYear, year)");
   int day = atoi(dayString.c_str());
   int year = atoi(yearString.c_str());
   if (day == 0)
      throw runtime_error("Cannot find parameter: " + dayString);
   if (year == 0)
      throw runtime_error("Cannot find parameter: " + yearString);

   GDate date;
   date.Set(day, year);
   ostringstream buffer;
   date.Write(buffer);
   buffer << "     ! dd/mm/yyyy";
   return buffer.str();
   }
// ------------------------------------------------------------------
// Parse the CreateParameter function call
// ------------------------------------------------------------------
void ApsimControlFile::parseCreateParameter(const ApsimParameterFile& paramFile,
                                            const std::string& line) const throw(runtime_error)
   {
   // E.g. Syntax of this function:
   //    clock.createParameter simulation_start_date = date(simulation_start_day, simulation_start_year)
   StringTokenizer tokenizer(line, "(),=");
   string parameterName = tokenizer.nextToken();
   string parameterValue = parseValue(paramFile, tokenizer);
   paramFile.setParamValue(parameterName, parameterValue);
   }
// ------------------------------------------------------------------
// Parse the DeleteParameter function call
// ------------------------------------------------------------------
void ApsimControlFile::parseDeleteParameter(const ApsimParameterFile& paramFile,
                                            const std::string& line) const throw(runtime_error)
   {
   // E.g. Syntax of this function:
   //    clock.deleteParameter simulation_start_day
   StringTokenizer tokenizer(line, "(),=");
   string parameterName = tokenizer.nextToken();
   Strip(parameterName, " ");
   paramFile.deleteParam(parameterName);
   }
// ------------------------------------------------------------------
// Parse the moveParameter function call
// ------------------------------------------------------------------
void ApsimControlFile::parseMoveParameter(const ApsimParameterFile& paramFile,
                                          const std::string& line) const throw(runtime_error)
   {
   // E.g. Syntax of this function:
   //    report.moveParameter title
   StringTokenizer tokenizer(line, "(),=");
   string parameterName = tokenizer.nextToken();
   Strip(parameterName, " ");
   string value = paramFile.getParamValue(parameterName);
   if (value == "")
      return;

   if (!tokenizer.hasMoreTokens())
      {
      // parameter is being moved to control file.
      paramFile.deleteParam(parameterName);
      ApsimParameterFile controlFile(fileName, "", "", section);
      controlFile.setParamValue(parameterName, value);
      }
   }
// ------------------------------------------------------------------
// Parse the newFormatReportVariables function call
// ------------------------------------------------------------------
void ApsimControlFile::parseNewFormatReportVariables(const ApsimParameterFile& paramFile,
                                                     const std::string& line) const throw(runtime_error)
   {
   // Syntax of this function:
   //

   // Get the module_names, variable_names and variable_alias lines for all
   // instances of REPORT.  Keep in mind that there may be multiple variable 'blocks'
   // for each section.

   vector<string> moduleLines, variableLines, aliasLines;
   paramFile.getParamValues("module_names", moduleLines);
   paramFile.getParamValues("variable_names", variableLines);
   paramFile.getParamValues("variable_alias", aliasLines);

   if (moduleLines.size() != variableLines.size()
       || moduleLines.size() != aliasLines.size())
      throw runtime_error("Control file converter error: Invalid report variables section in "
                          "parameter file.  The number of values in the module_names, "
                          "variable_names and variable_alias lines must be the same. ");

   // For each variable on each variable line, create a new variable.
   vector<string> newVariables;
   for (unsigned variableLineI = 0;
                 variableLineI != variableLines.size();
                 variableLineI++)
      {
      vector<string> modules, variables, aliases;
      Split_string(moduleLines[variableLineI], " ", modules);
      Split_string(variableLines[variableLineI], " ", variables);
      Split_string(aliasLines[variableLineI], " ", aliases);
      if (modules.size() != variables.size() || modules.size() != aliases.size())
         throw runtime_error("Control file converter error: Invalid report variables section in "
                             "parameter file.  The number of values in the module_names, "
                             "variable_names and variable_alias lines must be the same. "
                             "Variable_names: " + variableLines[variableLineI]);
      for (unsigned int variable = 0; variable < variables.size(); variable++)
         {
         string variableName = modules[variable] + "." + variables[variable];
         if (aliases[variable] != "-")
            variableName += " as " + aliases[variable];
         newVariables.push_back(variableName);
         }
      }

   // write all new variables.
   paramFile.setParamValues("variable", newVariables);
   }
// ------------------------------------------------------------------
// Parse the newFormatReportVariables function call
// ------------------------------------------------------------------
void ApsimControlFile::parseRemoveReportOutputSwitch
   (const ApsimParameterFile& paramFile,
    const std::string& line) const throw(runtime_error)
   {
   // E.g. Syntax of this function:
   //    report.removeReportOutputSwitch outputfile
   StringTokenizer tokenizer(line, "(),=");
   string parameterName = tokenizer.nextToken();
   Strip(parameterName, " ");
   string value = paramFile.getParamValue(parameterName);
   if (value != "")
      {
      unsigned posOverwrite = value.find("/overwrite");
      if (posOverwrite == string::npos)
         paramFile.setParamValue(parameterName + "_increment", "true");
      else
         {
         value.erase(posOverwrite);
         Strip(value, " ");
         paramFile.setParamValue(parameterName, value);
         }
      }
   }
// ------------------------------------------------------------------
// Parse the newFormatReportVariables function call
// ------------------------------------------------------------------
void ApsimControlFile::parseChangeModuleName(const std::string& line) const throw(runtime_error)
   {
   // E.g. Syntax of this function:
   //    changeModuleName met input(met)

   StringTokenizer tokenizer(line, " ");
   string oldModuleName = tokenizer.nextToken();
   Strip(oldModuleName, " ");
   string newModuleName = tokenizer.nextToken();
   Strip(newModuleName, " ");

   changeModuleName(oldModuleName, newModuleName);
   }
