//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ControlFileConverter.h"
#include "ApsimParameterFile.h"
#include "ApsimControlFile.h"
#include "ApsimVersion.h"
#include "ApsimDirectories.h"
#include "TMoveParametersForm.h"
#include <general\stringTokenizer.h>
#include <general\path.h>
#include <general\date_class.h>
#include <general\inifile.h>
using namespace std;

// ------------------------------------------------------------------
// Returns true if the specified control file needs converting
// ------------------------------------------------------------------
bool ControlFileConverter::needsConversion(const std::string& fileName)
   {
   int fileVersion = ApsimControlFile::getVersionNumber(fileName);
   int apsimVersion = StrToFloat(getApsimVersion().c_str())*10;
   return (fileVersion != apsimVersion);
   }
//---------------------------------------------------------------------------
// convert the specified control file using the version number of
// APSIM and the version number in the control file.
// Throws an exception if a problem was encountered.
// If callback is not null, then it will be called for every section
// in con file being converter.
//---------------------------------------------------------------------------
void ControlFileConverter::convert(const string& fileName,
                                   TControlFileConverterEvent callback) throw(runtime_error)
   {
   int fileVersion = ApsimControlFile::getVersionNumber(fileName);
   int apsimVersion = StrToFloat(getApsimVersion().c_str())*10;
   if (fileVersion != apsimVersion)
      {
      string homeDir = getApsimDirectory() + "\\apsim\\";
      for (int version = fileVersion+1; version <= apsimVersion; version++)
         {
         ostringstream conversionFile;
         conversionFile << homeDir << "conversions." << version;
         if (FileExists(conversionFile.str().c_str()))
            convert(fileName, conversionFile.str(), callback);
         }
      ApsimControlFile::setVersionNumber(fileName, apsimVersion);
      }
   }
//---------------------------------------------------------------------------
// convert the specified control file using the commands in the specified
// script file name.  Throws an exception if a problem was encountered.
// If callback is not null, then it will be called for every section
// in con file being converter.
//---------------------------------------------------------------------------
void ControlFileConverter::convert(const string& fileName,
                                   const string& scriptFileName,
                                   TControlFileConverterEvent callback) throw(runtime_error)
   {
   // Make the working directory the same directory as the where the
   // control file resides.
   Path p(fileName);
   p.Change_directory();

   // create a log file that we're going to write to.
   Path logPath(fileName);
   logPath.Set_extension(".conversions");
   ofstream log(logPath.Get_path().c_str());

   vector<string> controlFileSections;
   ApsimControlFile::getAllSectionNames(fileName, controlFileSections);
   for (unsigned section = 0; section != controlFileSections.size(); section++)
      {
      bool ok;
      // only convert this section if it has a module= line in it.
      if (ApsimControlFile::getVersionNumber(fileName) == 21)
         {
         string moduleLine;
         IniFile conAsIni(fileName);
         conAsIni.read(controlFileSections[section], "module", moduleLine);
         ok = (moduleLine != "");
         }
      else
         ok = true;

      if (ok)
         {
         if (callback != NULL)
            callback(controlFileSections[section]);
         log << "-------------------------------------------------------" << endl;
         log << "Converting section: " << controlFileSections[section] << endl;
         log << "-------------------------------------------------------" << endl;

         controlFile = ApsimControlFile(fileName, controlFileSections[section]);

         // Loop through all lines in script and perform required actions
         script.setFileName(scriptFileName.c_str());
         vector<string> conversions;
         script.readSectionNames(conversions);
         for (unsigned i = 0; i != conversions.size(); ++i)
            {
            bool ok = convertSection(conversions[i]);
            if (ok)
               log << conversions[i] << endl;
            }
         }
      }
   }
//---------------------------------------------------------------------------
// convert the control file using the commands in the specified section
// in the script file.  Throws an exception if a problem was encountered.
// Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::convertSection(const string& sectionName) throw(runtime_error)
   {
   bool ok = false;
   vector<string> commands;
   script.read(sectionName, "command", commands);
   for (unsigned c = 0; c != commands.size(); ++c)
      {
      // yield control to windows.
      Application->ProcessMessages();

      // extract the routine name
      unsigned posOpenBracket = commands[c].find('(');
      if (posOpenBracket == string::npos)
         throw runtime_error("Bad control file converter command: " + commands[c]);
      string routineName = commands[c].substr(0, posOpenBracket);

      // extract the arguments inside the brackets.
      unsigned posCloseBracket = commands[c].rfind(')');
      if (posCloseBracket == string::npos)
         throw runtime_error("Bad control file converter command: " + commands[c]);
      string arguments = commands[c].substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
      Strip(arguments, " ");

      // call the appropriate routine to do the conversion.
      if (routineName == "SetParameterValue")
         ok = executeSetParameterValue(arguments) || ok;
      else if (routineName == "RenameParameter")
         ok = executeRenameParameter(arguments) || ok;
      else if (routineName == "DeleteParameter")
         ok = executeDeleteParameter(arguments) || ok;
      else if (routineName == "ChangeInstantiation")
         ok = executeChangeInstantiation(arguments) || ok;
      else if (routineName == "RemoveReportOutputSwitch")
         ok = executeRemoveReportOutputSwitch(arguments) || ok;
      else if (routineName == "MoveParameter")
         ok = executeMoveParameter(arguments) || ok;
      else if (routineName == "NewFormatReportVariables")
         ok = executeNewFormatReportVariables(arguments) || ok;
      else if (routineName == "MoveParametersOutOfCon")
         ok = executeMoveParametersOutOfCon(arguments) || ok;
      else if (routineName == "RemoveSumAvgToTracker")
         ok = executeRemoveSumAvgToTracker(arguments) || ok;

      if (!ok)
         return false;
      }
   return ok;
   }
//---------------------------------------------------------------------------
// Find the specified parameter in the parameter file somewhere.  Name
// should be a fully qualified name (module.name).  On success the routine
// returns true, along with a list of parameter files containing the value.
//---------------------------------------------------------------------------
bool ControlFileConverter::findParameters
  (const string& fqn, string& parameterName,
   std::vector<ApsimParameterFile>& parameterFiles) const throw(runtime_error)
   {
   // extract the module name from the name.
   unsigned posPeriod = fqn.find('.');
   if (posPeriod == string::npos)
      throw runtime_error("Invalid parameter name: " + fqn);
   string moduleName = fqn.substr(0, posPeriod);
   parameterName = fqn.substr(posPeriod+1);

   vector<string> instanceNames;
   controlFile.getInstances(moduleName, instanceNames);

   for (unsigned i = 0; i != instanceNames.size(); ++i)
      {
      vector<ApsimParameterFile> paramFiles;
      controlFile.getParameterFiles(instanceNames[i], paramFiles);
      for (vector<ApsimParameterFile>::const_iterator
               paramFile = paramFiles.begin();
               paramFile != paramFiles.end();
               paramFile++)
         {
         if (paramFile->getParamValue(parameterName) != "")
            parameterFiles.push_back(*paramFile);
         }
      }

   return parameterFiles.size();
   }
//---------------------------------------------------------------------------
// Evalulate the specified expression and return a value.  Returns true on
// success.
//---------------------------------------------------------------------------
bool ControlFileConverter::evaluate(const string& expression, string& value) const throw(runtime_error)
   {
   value = expression;
   
   // look for a macro.
   if (value.find("%controlfilenamebase%") != string::npos)
      {
      Replace_all(value, "%controlfilenamebase%",
                  Path(controlFile.getFileName()).Get_name_without_ext().c_str());
      return true;
      }

   // look for date function(
   unsigned posFunction = value.find("date(");
   if (posFunction != string::npos)
      {
      // extract the arguments inside the brackets.
      unsigned posCloseBracket = value.rfind(')');
      if (posCloseBracket == string::npos)
         throw runtime_error("Invalid date function usage: " + value);
      posFunction += strlen("date(");
      string arguments = value.substr(posFunction, posCloseBracket-posFunction);
      return evaluateDate(arguments, value);
      }
   else
      {
      // check for a module.name
      unsigned posPeriod = value.find('.');
      if (posPeriod != string::npos)
         {
         string parameterName;
         vector<ApsimParameterFile> parameterFiles;
         if (findParameters(value, parameterName, parameterFiles)
             && parameterFiles.size() == 1)
            value = parameterFiles[0].getParamValue(parameterName);

         else
            return false;
         }
      }
   return true;
   }
// ------------------------------------------------------------------
// evaluate the date arguments passed in and return a date.
// ------------------------------------------------------------------
bool ControlFileConverter::evaluateDate(const string& arguments, string& value) const throw(runtime_error)
   {
   // This function expects 2 values: day_of_year and year
   StringTokenizer tokenizer(arguments, ", ");
   string arg1 = tokenizer.nextToken();
   string arg2 = tokenizer.nextToken();
   if (arg1 == "" || arg2 == "")
      throw runtime_error("Invalid arguments to date function: " + arguments);

   string dayString;
   string yearString;
   if (!evaluate(arg1, dayString) || !evaluate(arg2, yearString))
      return false;
   int day = atoi(dayString.c_str());
   int year = atoi(yearString.c_str());
   if (day == 0 || year == 0)
      return false;

   GDate date;
   date.Set(day, year);
   ostringstream buffer;
   date.Write(buffer);
   buffer << "     ! dd/mm/yyyy";
   value = buffer.str();
   return true;
   }
//---------------------------------------------------------------------------
// Execute the SetParameterValue command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSetParameterValue(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to setParameterValue: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   Strip(arg1, " ");
   Strip(arg2, " ");

   ApsimParameterFile parFile;
   string value;
   if (evaluate(arg2, value))
      {
      string moduleName;
      string parameterName = arg1;
      unsigned posPeriod = arg1.find('.');
      if (posPeriod != string::npos)
         {
         moduleName = parameterName.substr(0, posPeriod);
         parameterName.erase(0, posPeriod+1);
         }
      controlFile.setParameterValue(moduleName, "", parameterName, value);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Execute the RenameParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRenameParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to RenameParameter: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   Strip(arg1, " ");
   Strip(arg2, " ");
   vector<ApsimParameterFile> parFiles;
   string parameterName, value;
   findParameters(arg1, parameterName, parFiles);
   for (unsigned i = 0; i != parFiles.size(); ++i)
      {
      string value = parFiles[i].getParamValue(parameterName);
      parFiles[i].deleteParam(parameterName);
      parFiles[i].setParamValue(arg2, value);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Execute the DeleteParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteParameter(const string& arguments) throw(runtime_error)
   {
   vector<ApsimParameterFile> parFiles;
   string parameterName, value;
   findParameters(arguments, parameterName, parFiles);
   for (unsigned i = 0; i != parFiles.size(); ++i)
      parFiles[i].deleteParam(parameterName);

   return (parFiles.size() > 0);
   }
//---------------------------------------------------------------------------
// Execute the ChangeInstantiation command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeChangeInstantiation(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to ChangeInstantiation: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   Strip(arg1, " ");
   Strip(arg2, " ");

   return controlFile.changeModuleName(arg1, arg2);
   }
// ------------------------------------------------------------------
// Execute the newFormatReportVariables function call
// ------------------------------------------------------------------
bool ControlFileConverter::executeRemoveReportOutputSwitch(const string& arguments) throw(runtime_error)
   {
   bool somethingDone = false;
   vector<ApsimParameterFile> parFiles;
   string parameterName;
   findParameters("report." + arguments, parameterName, parFiles);
   for (unsigned i = 0; i != parFiles.size(); ++i)
      {
      string value = parFiles[i].getParamValue(parameterName);
      unsigned posOverwrite = value.find("/overwrite");
      if (posOverwrite != string::npos)
         {
         value.erase(posOverwrite);
         Strip(value, " ");
         parFiles[i].setParamValue(parameterName, value);
         somethingDone = true;
         }
      }
   return somethingDone;
   }
//---------------------------------------------------------------------------
// Execute the MoveParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeMoveParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to MoveParameter: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   Strip(arg1, " ");
   Strip(arg2, " ");
   vector<ApsimParameterFile> parFiles;
   string parameterName;
   findParameters(arg1, parameterName, parFiles);
   for (unsigned i = 0; i != parFiles.size(); ++i)
      {
      StringTokenizer fullSection(parFiles[i].getSection(), ".");

      string value = parFiles[i].getParamValue(parameterName);
      parFiles[i].deleteParam(parameterName);
      controlFile.setParameterValue(arg2, fullSection.nextToken(), parameterName,
                                    value, parFiles[i].getFileName());
      return true;
      }
   return false;
   }

// ------------------------------------------------------------------
// Execute the NewFormatReportVariables command. Returns true on success
// ------------------------------------------------------------------
bool ControlFileConverter::executeNewFormatReportVariables(const std::string& arguments) throw(runtime_error)
   {
   bool doneSomething = false;
   // Return all the parameter files for the specified section and instance.
   vector<ApsimParameterFile> paramFiles;
   controlFile.getParameterFiles("report", paramFiles);
   string name;
   findParameters("report.module_names", name, paramFiles);
   for (unsigned par = 0; par != paramFiles.size(); ++par)
      {
      // Get the module_names, variable_names and variable_alias lines for all
      // instances of REPORT.  Keep in mind that there may be multiple variable 'blocks'
      // for each section.

      vector<string> moduleLines, variableLines, aliasLines;
      paramFiles[par].getParamValues("module_names", moduleLines);
      paramFiles[par].getParamValues("variable_names", variableLines);
      paramFiles[par].getParamValues("variable_alias", aliasLines);

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
      if (newVariables.size() > 0)
         {
         paramFiles[par].setParamValues("variable", newVariables);
         doneSomething = true;
         }
      }
   return doneSomething;
   }
//---------------------------------------------------------------------------
// move all parameters out of the control file and into a parameter file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeMoveParametersOutOfCon(const std::string arguments) throw(runtime_error)
   {
   if (arguments != "")
      parFileToUse = arguments;

   vector<ApsimParameterFile> paramFiles;
   controlFile.getParameterFiles("", paramFiles);
   bool selfReferencesFound = false;
   for (unsigned i = 0; i != paramFiles.size(); ++i)
      {
      string file = paramFiles[i].getFileName();
      if (paramFiles[i].getFileName() == controlFile.getFileName())
         {
         selfReferencesFound = true;

         // if parFileToUse is blank then ask user for a file to put all
         // parameters into.
         if (parFileToUse == "")
            {
            string defaultFile, defaultSection;
            controlFile.getDefaultParFileAndSection(defaultFile, defaultSection);

            MoveParametersForm = new TMoveParametersForm(Application);
            MoveParametersForm->FileEdit->Text = defaultFile.c_str();
            if (!MoveParametersForm->ShowModal())
               {
               delete MoveParametersForm;
               return false;
               }
            parFileToUse = MoveParametersForm->FileEdit->Text.c_str();
            delete MoveParametersForm;
            }

         ApsimParameterFile newParamFile (parFileToUse,
                                          paramFiles[i].getModuleName(),
                                          paramFiles[i].getInstanceName(),
                                          paramFiles[i].getSection() );
         string contents = paramFiles[i].getSectionContents();
         newParamFile.setSectionContents(contents);

         // delete entire section.
         paramFiles[i].deleteSectionContents();
         }
      }
   // tell control file to remove the self reference for this instance.
   if (selfReferencesFound)
      controlFile.removeSelfReferences(parFileToUse);
   return selfReferencesFound;
   }
// ------------------------------------------------------------------
// Execute the RemoveSumAvgToTracker command. Returns true on success
// ------------------------------------------------------------------
bool ControlFileConverter::executeRemoveSumAvgToTracker(const std::string& arguments) throw(runtime_error)
   {
   bool sumAvgFound = false;
   string name;

   vector<string> instanceNames;
   controlFile.getInstances("report", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); ++i)
      {
      bool sumAvgFoundThisInstance = false;
      bool alreadyConverted = false;

      vector<string> trackerVariables;
      string trackerInstanceName = string("tracker") + IntToStr(i+1).c_str();
      vector<ApsimParameterFile> paramFiles;
      controlFile.getParameterFiles(instanceNames[i], paramFiles);
      for (vector<ApsimParameterFile>::const_iterator
               paramFile = paramFiles.begin();
               paramFile != paramFiles.end();
               paramFile++)
         {
         if (paramFile->getParamValue("variable") != "")
            {
            for (unsigned par = 0; par != paramFiles.size(); ++par)
               {
               vector<string> variables;
               paramFiles[par].getParamValues("variable", variables);

               // For each variable on each variable line, create a new variable.
               vector<string> newVariables;
               for (unsigned variableI = 0;
                             variableI != variables.size();
                             variableI++)
                  {
                  if (variables[variableI].find("sum@") != string::npos ||
                      variables[variableI].find("avg@") != string::npos)
                     {
                     sumAvgFoundThisInstance = true;
                     sumAvgFound = true;
                     if (variables[variableI].find("tracker") == string::npos)
                        {
                        StringTokenizer tokenizer(variables[variableI], ".@");
                        string moduleName = tokenizer.nextToken();
                        string functionName = tokenizer.nextToken();
                        string variableName = tokenizer.nextToken();
                        Replace_all(variableName, "(", "[");
                        Replace_all(variableName, ")", "]");

                        string alias;
                        unsigned posAlias = variableName.find(" as ");
                        if (posAlias != string::npos)
                           {
                           alias = variableName.substr(posAlias+4);
                           variableName.erase(posAlias);
                           }

                        string trackerFunctionName = functionName;
                        if (Str_i_Eq(trackerFunctionName, "avg"))
                           trackerFunctionName = "average";

                        // Change the report variable.
                        string reportVariable = trackerInstanceName + "."
                                              + functionName +  "@"
                                              + moduleName + "." + variableName;
                        if (alias != "")
                           reportVariable += " as " + alias;
                        newVariables.push_back(reportVariable);

                        // set the tracker variable.
                        string trackerVariable = trackerFunctionName + " of " + moduleName + "."
                               + variableName + " since " + paramFiles[par].getInstanceName()
                               + ".reported as ";
                        trackerVariable += functionName + "@" + moduleName + "." + variableName;
                        trackerVariables.push_back(trackerVariable);
                        }
                     else
                        newVariables.push_back(variables[variableI]);
                     }
                  else
                     newVariables.push_back(variables[variableI]);
                  }

               // write all new variables.
               if (newVariables.size() > 0)
                  paramFiles[par].setParamValues("variable", newVariables);
               }
            if (trackerVariables.size() > 0)
               {
               controlFile.setParameterValues(trackerInstanceName, "", "variable", trackerVariables);
               controlFile.changeModuleName
                  (trackerInstanceName, "tracker(" + trackerInstanceName + ")");
               }
            }
         }

      if (sumAvgFoundThisInstance)
         {
         vector<string> instanceNames;
         controlFile.getInstances("tracker", instanceNames);
         if (find(instanceNames.begin(), instanceNames.end(),
             trackerInstanceName) == instanceNames.end())
            {
            string defaultFile, defaultSection;
            controlFile.getDefaultParFileAndSection(defaultFile, defaultSection);
            controlFile.addModuleLine("tracker", trackerInstanceName, defaultFile, defaultSection);
            }
         }
      }
   return sumAvgFound;
   }

