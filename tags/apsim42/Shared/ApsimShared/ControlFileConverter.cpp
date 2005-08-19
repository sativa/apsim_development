#include <vcl.h>

#include <vector>
#include <map>
#include <string>
#include <fstream>
#include <stdexcept>
#include <iosfwd.h>

#include <general\stringTokenizer.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\date_class.h>
#include <general\inifile.h>
#include <general\stristr.h>

#include <boost\lexical_cast.hpp>

#include "ApsimControlFile.h"
#include "ApsimVersion.h"
#include "ApsimDirectories.h"
#include "TMoveParametersForm.h"
#include "ApsimDirectories.h"

#include "ControlFileConverter.h"

using namespace std;
using namespace boost;

//---------------------------------------------------------------------------
// Return a list of script files to use to convert the specified control file.
//---------------------------------------------------------------------------
void getScriptFilesToUse(const string& fileName, vector<string>& scriptFileNames)
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
            scriptFileNames.push_back(conversionFile.str());
         }
      if (scriptFileNames.size() == 0)
         ApsimControlFile::setVersionNumber(fileName, apsimVersion);
      }
   }
// ------------------------------------------------------------------
// evaluate an expression and return result.
// ------------------------------------------------------------------
string evaluateExpression(const string& value1, const string& value2, const string& oper)
   {
   if (value2 == "" || oper == "")
      return value1;
   else if (!Is_numerical(value1.c_str()) || !Is_numerical(value2.c_str()))
      return "";
   else
      {
      double value;
      double number1 = lexical_cast<double>(value1);
      double number2 = lexical_cast<double>(value2);

      if (oper == "+")
         value = number1 + number2;
      else if (oper == "-")
         value = number1 - number2;
      else if (oper == "*")
         value = number1 * number2;
      else if (oper == "/")
         value = number1 / number2;
      return ftoa(value, 2);
      }
   }
// ------------------------------------------------------------------
// Returns true if the specified control file needs converting
// ------------------------------------------------------------------
bool ControlFileConverter::needsConversion(const std::string& fileName)
   {
   vector<string> scriptFileNames;
   getScriptFilesToUse(fileName, scriptFileNames);
   return (scriptFileNames.size() > 0);
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
   vector<string> scriptFileNames;
   getScriptFilesToUse(fileName, scriptFileNames);

   // create a log file that we're going to write to.
   Path logPath(fileName);
   logPath.Set_extension(".conversions");
   log.open(logPath.Get_path().c_str());

   for (unsigned f = 0; f != scriptFileNames.size(); f++)
      convert(fileName, scriptFileNames[f], callback);

   log.close();

   int apsimVersion = StrToFloat(getApsimVersion().c_str())*10;
   ApsimControlFile::setVersionNumber(fileName, apsimVersion);
   }
//---------------------------------------------------------------------------
// convert the specified control file using the commands in the specified
// script file name.  Throws an exception if a problem was encountered.
// If callback is not null, then it will be called for every section
// in con file being converter. Return true if something was converted.
//---------------------------------------------------------------------------
bool ControlFileConverter::convert(const string& fileName,
                                   const string& scriptFileName,
                                   TControlFileConverterEvent callback) throw(runtime_error)
   {
   typedef map<string, vector<string> > Output;
   Output output;

   bool somethingWasConverted = false;
   con = new ApsimControlFile(fileName);

   // Make the working directory the same directory as the where the
   // control file resides.
   Path p(fileName);
   p.Change_directory();

   vector<string> controlFileSections;
   con->getAllSectionNames(controlFileSections);
   for (unsigned section = 0; section != controlFileSections.size(); section++)
      {
      conSection = controlFileSections[section];
      bool ok;
      // only convert this section if it has a module= line in it.
      if (ApsimControlFile::getVersionNumber(fileName) == 21)
         ok = con->isValid(conSection);

      else
         ok = true;

      if (ok && con->isValid(controlFileSections[section]))
         {
         if (callback != NULL)
            callback(conSection);

         // Loop through all lines in script and perform required actions
         script = new IniFile(scriptFileName);
         vector<string> conversions;
         script->readSectionNames(conversions);
         for (unsigned i = 0; i != conversions.size(); ++i)
            {
            bool ok = convertSection(conversions[i]);
            if (ok)
               {
               somethingWasConverted = true;
               output[conSection].push_back(conversions[i]);
               }
            }
         delete script;
         }
      }
   delete con;
   if (somethingWasConverted)
      {
      for (Output::iterator out = output.begin(); out != output.end(); out++)
         {
         log << "Converting section: " << out->first << endl;
         for (unsigned i = 0; i != out->second.size(); i++)
            log << "   " << out->second[i] << endl;
         log << endl;
         }
      }
   return somethingWasConverted;
   }
//---------------------------------------------------------------------------
// convert the control file using the commands in the specified section
// in the script file.  Throws an exception if a problem was encountered.
// Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::convertSection(const string& sectionName) throw(runtime_error)
   {
   bool ok = false;
   try
      {
      vector<string> commands;
      script->read(sectionName, "command", commands);
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
         stripLeadingTrailing(arguments, " ");

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
         else if (routineName == "RemoveTrackerDefault")
            ok = executeRemoveTrackerDefault(arguments) || ok;
         else if (routineName == "SearchReplaceReportVariables")
            ok = executeSearchReplaceReportVariables(arguments) || ok;
         else if (routineName == "AddParamFileToModule")
            ok = executeAddParamFileToModule(arguments) || ok;
         else if (routineName == "RemovePeriodsInReportAndTracker")
            ok = removePeriodsInReportAndTracker(arguments) || ok;
         else if (routineName == "ReworkTrackerVariables")
            ok = ReworkTrackerVariables(arguments) || ok;
         else if (routineName == "RenameModule")
            ok = executeRenameModule(arguments) || ok;
         else if (routineName == "SearchReplace")
            ok = executeSearchReplace(arguments) || ok;
         else if (routineName == "SetManagerActionParameter")
            ok = executeSetManagerActionParameter(arguments) || ok;
         else if (routineName == "DeleteManagerActionParameter")
            ok = executeDeleteManagerActionParameter(arguments) || ok;
         else if (routineName == "FindModuleLocalIniFile")
            ok = executeFindModuleLocalIniFile(arguments) || ok;
         else if (routineName == "RemoveReportVariable")
            ok = executeRemoveReportVariable(arguments) || ok;

         if (!ok)
            return false;
         }
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   return ok;
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
                  Path(con->getFileName()).Get_name_without_ext().c_str());
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
      StringTokenizer tokenizer(value, " ");
      string value1 = tokenizer.nextToken();
      string oper = tokenizer.nextToken();
      string value2 = tokenizer.nextToken();
      resolveVariableRef(value1);
      resolveVariableRef(value2);
      value = evaluateExpression(value1, value2, oper);
      }
   return true;
   }
// ------------------------------------------------------------------
// resolve a module.variable to a value if necessary.
// ------------------------------------------------------------------
void ControlFileConverter::resolveVariableRef(string& value) const
   {
   // check for a module.name
   unsigned posPeriod = value.find('.');
   if (posPeriod != string::npos)
      {
      string moduleName = value.substr(0, posPeriod);
      string parameterName = value.substr(posPeriod+1);
      vector<string> instances;
      con->getInstances(conSection, moduleName, instances);
      for (unsigned i = 0; i != instances.size(); i++)
         {
         string st = con->getParameterValue(conSection, instances[i], parameterName);
         if (st != "")
            {
            value = st;
            splitOffBracketedValue(value, '(', ')');
            stripLeadingTrailing(value, " ");
            return;
            }
         }
      }
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
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 2 && args.size() != 3)
      throw runtime_error("Bad arguments in call to setParameterValue: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");
   bool alwaysCreate = true;
   if (args.size() == 3)
      {
      stripLeadingTrailing(args[2], "\" ");
      if (Str_i_Eq(args[2], "OnlyWhenNecessary"))
         alwaysCreate = false;
      else
         throw runtime_error("Invalid 3rd argument to SetParameterValue : " + args[2]);
      }

   unsigned posPeriod = args[0].find('.');
   if (posPeriod == string::npos)
      throw runtime_error("Bad 1st argument to SetParameterValue - " + args[0]);
   string moduleName = args[0].substr(0, posPeriod);
   string parameterName = args[0].substr(posPeriod+1);

   string value;
   if (evaluate(args[1], value))
      {
      vector<string> instanceNames;
      con->getInstances(conSection, moduleName, instanceNames);
      if (alwaysCreate || instanceNames.size() > 0)
         {
         con->setParameterValue(conSection, moduleName, parameterName, value);
         return true;
         }
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
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   unsigned posPeriod = arg1.find('.');
   string moduleName = arg1.substr(0, posPeriod);
   string parameterName = arg1.substr(posPeriod+1);
   return con->renameParameter(conSection, moduleName, parameterName, arg2);
   }
//---------------------------------------------------------------------------
// Execute the DeleteParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posPeriod = arguments.find('.');
   string moduleName = arguments.substr(0, posPeriod);
   string parameterName = arguments.substr(posPeriod+1);
   return con->deleteParameter(conSection, moduleName, parameterName);
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
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   return con->changeModuleName(conSection, arg1, arg2);
   }
// ------------------------------------------------------------------
// Execute the newFormatReportVariables function call
// ------------------------------------------------------------------
class RemoveReportSwitch
   {
   public:
      RemoveReportSwitch(const string& pName, bool& m)
         : paramName(pName), modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         string value;
         par->read(section, paramName, value);
         unsigned posOverwrite = value.find("/overwrite");
         if (posOverwrite != string::npos)
            {
            value.erase(posOverwrite);
            stripLeadingTrailing(value, " ");
            par->write(section, paramName, value);
            modified = true;
            }
         }

   private:
      const string& paramName;
      bool& modified;
   };

bool ControlFileConverter::executeRemoveReportOutputSwitch(const string& arguments) throw(runtime_error)
   {
   bool modified = false;
   RemoveReportSwitch removeSwitch(arguments, modified);
   con->enumerateParameters(conSection, "report", false, removeSwitch.callback);
   return modified;
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
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   unsigned posPeriod = arg1.find('.');
   string moduleName = arg1.substr(0, posPeriod);
   string parameterName = arg1.substr(posPeriod+1);
   if (arg2 == "" && Str_i_Eq(parameterName, "title"))
      {
      string title;
      evaluate("report.title", title);
      con->deleteParameter(conSection, moduleName, parameterName);
      con->setTitle(conSection, title);
      return true;
      }
   else
      return con->moveParameter(conSection, moduleName, parameterName, arg2);
   }

// ------------------------------------------------------------------
// Execute the NewFormatReportVariables command. Returns true on success
// ------------------------------------------------------------------
class FormatReportVariables
   {
   public:
      FormatReportVariables(bool& m)
         : modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         // Get the module_names, variable_names and variable_alias lines for all
         // instances of REPORT.  Keep in mind that there may be multiple variable 'blocks'
         // for each section.

         vector<string> moduleLines, variableLines, aliasLines;
         par->read(section, "module_names", moduleLines);
         par->read(section, "variable_names", variableLines);
         par->read(section, "variable_alias", aliasLines);

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
            par->write(section, "variable", newVariables);
            modified = true;
            }
         }

   private:
      bool& modified;
   };

bool ControlFileConverter::executeNewFormatReportVariables(const std::string& arguments) throw(runtime_error)
   {
   bool modified = false;
   FormatReportVariables formatReportVariables(modified);
   con->enumerateParameters(conSection, "report", false, formatReportVariables.callback);
   return modified;
   }
//---------------------------------------------------------------------------
// move all parameters out of the control file and into a parameter file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeMoveParametersOutOfCon(const std::string arguments) throw(runtime_error)
   {
   if (con->hasParametersInCon(conSection))
      {
      string parFileToUse = arguments;
      if (parFileToUse == "")
         {
         static string parFile;
         if (parFile == "")
            {
            parFileToUse = con->getFileName();
            MoveParametersForm = new TMoveParametersForm(Application);
            MoveParametersForm->FileEdit->Text = parFileToUse.c_str();
            if (!MoveParametersForm->ShowModal())
               {
               delete MoveParametersForm;
               return false;
               }
            parFile = MoveParametersForm->FileEdit->Text.c_str();
            delete MoveParametersForm;
            }
         parFileToUse = parFile;
         }
      return con->moveParametersOutOfCon(conSection, parFileToUse);
      }
   return false;
   }
// ------------------------------------------------------------------
// Execute the RemoveSumAvgToTracker command. Returns true on success
// ------------------------------------------------------------------
class RemoveSumAvg
   {
   public:
      RemoveSumAvg(ApsimControlFile* c, const string& section, bool& m)
         : con(c), conSection(section), modified(m), trackerNum(1) { }

      void callback(IniFile* par, const string& section)
         {
         unsigned posFirstPeriod = section.find('.');
         unsigned posSecondPeriod = section.find('.', posFirstPeriod+1);
         string instanceName = section.substr(posFirstPeriod+1, posSecondPeriod-posFirstPeriod-1);

         string trackerInstanceName = string("tracker") + IntToStr(trackerNum).c_str();

         vector<string> trackerVariables;
         vector<string> variables;
         par->read(section, "variable", variables);

         vector<string> newVariables;
         bool found = false;
         for (unsigned v = 0; v != variables.size(); v++)
            {
            if ((variables[v].find("sum@") != string::npos ||
                 variables[v].find("avg@") != string::npos)
                 && variables[v].find("tracker") == string::npos)
               {
               found = true;
               modified = true;
               StringTokenizer tokenizer(variables[v], ".@");
               string moduleName = tokenizer.nextToken();
               string functionName = tokenizer.nextToken();
               string realVariableName = tokenizer.nextToken();

               string alias;
               unsigned posAlias = realVariableName.find(" as ");
               if (posAlias != string::npos)
                  {
                  alias = realVariableName.substr(posAlias+4);
                  realVariableName.erase(posAlias);
                  }

               string variableName = realVariableName;
               Replace_all(variableName, "(", "[");
               Replace_all(variableName, ")", "]");
               string reportVariable = trackerInstanceName + "."
                                     + functionName +  "@"
                                     + moduleName + "." + variableName;
               if (alias != "")
                  reportVariable += " as " + alias;
               variables[v] = reportVariable;

               string trackerFunctionName = functionName;
               if (Str_i_Eq(trackerFunctionName, "avg"))
                  trackerFunctionName = "average";

               // set the tracker variable.
               string trackerVariable = trackerFunctionName + " of " + moduleName + "."
                      + realVariableName + " since " + instanceName
                      + ".reported as ";
               trackerVariable += functionName + "@" + moduleName + "." + variableName;
               trackerVariable += " on post";
               trackerVariables.push_back(trackerVariable);
               }
            }

         // write all new variables.
         if (found)
            {
            par->write(section, "variable", variables);

            con->setParameterValues(conSection, trackerInstanceName, "variable", "", trackerVariables);
            con->changeModuleName(conSection, trackerInstanceName, "tracker(" + trackerInstanceName + ")");

            trackerNum++;
            }
         }

   private:
      ApsimControlFile* con;
      const string& conSection;
      bool& modified;
      int trackerNum;
   };
bool ControlFileConverter::executeRemoveSumAvgToTracker(const std::string& arguments) throw(runtime_error)
   {
   bool modified = false;
   RemoveSumAvg removeSumAvg(con, conSection, modified);
   con->enumerateParameters(conSection, "report", false, removeSumAvg.callback);
   return modified;
   }
//---------------------------------------------------------------------------
// Execute the executeRemoveTrackerDefault command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRemoveTrackerDefault(const string& arguments) throw(runtime_error)
   {
   vector<string> instanceNames;
   con->getInstances(conSection, "tracker", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         if (variables[v].find(" on ") == string::npos)
            {
            variables[v] += " on process";
            someHaveChanged = true;
            }
         }
      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }
   return someHaveChanged;
   }
//---------------------------------------------------------------------------
// To a search and replace on all report variables
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplaceReportVariables(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to SearchReplaceReportVariables: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         someHaveChanged = (replaceAll(variables[v], arg1, arg2) || someHaveChanged);

      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }
   return someHaveChanged;
   }
//---------------------------------------------------------------------------
// Add a parameter file reference to all instances of a module in con file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeAddParamFileToModule(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to AddParamFileToModule: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   posComma = arg2.find(',');
   string arg3 = arg2.substr(posComma+1);
   arg2.erase(posComma);

   return con->addParameterFileReference(conSection, arg1, arg2, arg3);
   }
//---------------------------------------------------------------------------
// Remove any period characters in tracker variables.
//---------------------------------------------------------------------------
bool ControlFileConverter::removePeriodsInReportAndTracker(const string& arguments) throw(runtime_error)
   {
   // change report variables first.
   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         unsigned posPeriod = variables[v].find('.');
         someHaveChanged = (replaceAll(variables[v], posPeriod+1, ".", "_") || someHaveChanged);
         }

      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }

   // change tracker variables.
   bool someHaveChanged2 = false;
   instanceNames.erase(instanceNames.begin(), instanceNames.end());
   con->getInstances(conSection, "tracker", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         unsigned posAs = variables[v].find(" as ");
         someHaveChanged2 = (replaceAll(variables[v], posAs, ".", "_") || someHaveChanged);
         }

      if (someHaveChanged2)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }


   return (someHaveChanged || someHaveChanged2);
   }
//---------------------------------------------------------------------------
// Rework the tracker variables to new format.
//---------------------------------------------------------------------------
bool ControlFileConverter::ReworkTrackerVariables(const string& arguments) throw(runtime_error)
   {
   // change tracker variables.
   vector<string> instanceNames;
   con->getInstances(conSection, "tracker", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         if (variables[v].find(" from ") == string::npos
             && variables[v].find(" last ") == string::npos)
            {
            StringTokenizer tokenizer(variables[v], " \t\n");
            string stat = tokenizer.nextToken();
            string word = tokenizer.nextToken();
            string variable = tokenizer.nextToken();
            string eventName, startPeriod, endPeriod, as;
            word = tokenizer.nextToken();
            while (word != "")
               {
               if (Str_i_Eq(word, "since"))
                  {
                  startPeriod = tokenizer.nextToken();
                  endPeriod = "now";
                  }
               else if (Str_i_Eq(word, "between"))
                  {
                  startPeriod = tokenizer.nextToken();
                  string and = tokenizer.nextToken();
                  endPeriod = tokenizer.nextToken();
                  }
               else if (Str_i_Eq(word, "on"))
                  eventName = tokenizer.nextToken();
               else if (Str_i_Eq(word, "as"))
                  as = tokenizer.nextToken();
               word = tokenizer.nextToken();
               }
            if (Str_i_Eq(eventName, "prepare"))
               eventName = "start_of_day";
            if (Str_i_Eq(eventName, "post"))
               eventName = "end_of_day";

            variables[v] = stat + " of " + variable;
            if (eventName != "")
               variables[v] += " on " + eventName;
            if (startPeriod != "")
               variables[v] += " from " + startPeriod + " to " + endPeriod;
            if (as != "")
               variables[v] += " as " + as;
            }
         }

      if (variables.size() > 0)
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
      }

   return (instanceNames.size() > 0);
   }
//---------------------------------------------------------------------------
// Rename a module in the control file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRenameModule(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to RenameModule: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   return con->renameModule(conSection, arg1, arg2);
   }
//---------------------------------------------------------------------------
// Perform a search and replace on a param file section
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplace(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 3)
      throw runtime_error("Bad arguments in call to SearchReplace: " + arguments);

   stripLeadingTrailing(args[1], " ");
   stripLeadingTrailing(args[2], " ");

   stripLeadingTrailing(args[0], "\"");
   stripLeadingTrailing(args[1], "\"");
   stripLeadingTrailing(args[2], "\"");
   return con->searchReplace(conSection, args[0], args[1], args[2]);
   }
//---------------------------------------------------------------------------
// Set a manager action parameter.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSetManagerActionParameter(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 3)
      throw runtime_error("Bad arguments in call to SetManagerActionParameter: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");
   stripLeadingTrailing(args[2], "\" ");

   managerActionNewParameter = args[1];
   StringTokenizer tokenizer(args[2], " ");
   managerActionValue1 = tokenizer.nextToken();
   managerActionOper = tokenizer.nextToken();
   managerActionValue2 = tokenizer.nextToken();

   return con->enumerateManagerActionLines(conSection, args[0], SetManagerActionCallback);
   }
//---------------------------------------------------------------------------
// Callback for SetmanagerActionParameter.
//---------------------------------------------------------------------------
void ControlFileConverter::SetManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                                    bool& modified)
   {
   // go resolve all manager parameters.
   for (unsigned p = 0; p != parameters.size(); p++)
      {
      if (Str_i_Eq(managerActionValue1, parameters[p].name))
         managerActionValue1 = parameters[p].value;
      if (Str_i_Eq(managerActionValue2, parameters[p].name))
         managerActionValue2 = parameters[p].value;
      }
   ApsimControlFile::ManagerActionParameter newParam;
   newParam.name = managerActionNewParameter;
   newParam.value = evaluateExpression(managerActionValue1, managerActionValue2,
                                       managerActionOper);
   parameters.push_back(newParam);
   modified = true;
   }
//---------------------------------------------------------------------------
// Delete a manager action parameter.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteManagerActionParameter(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 2)
      throw runtime_error("Bad arguments in call to DeleteManagerActionParameter: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");

   managerActionValue1 = args[1];

   return con->enumerateManagerActionLines(conSection, args[0], DeleteManagerActionCallback);
   }
//---------------------------------------------------------------------------
// Callback for DeleteManagerActionParameter.
//---------------------------------------------------------------------------
void ControlFileConverter::DeleteManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                                       bool& modified)
   {
   // go resolve all manager parameters.
   for (unsigned p = 0; p != parameters.size(); p++)
      {
      if (Str_i_Eq(managerActionValue1, parameters[p].name))
         {
         parameters.erase(parameters.begin()+p);
         modified = true;
         return;
         }
      }
   modified = false;
   }
//---------------------------------------------------------------------------
// Find a module.ini file. Return true if found.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeFindModuleLocalIniFile(const string& arguments)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 1)
      throw runtime_error("Invalid arguments to FindModuleLocalIniFile: " + arguments);

   stripLeadingTrailing(args[0], "\" ");

   string apsimDirectory = getApsimDirectory();

   vector<string> instanceNames;
   con->getInstances(conSection, args[0], instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      string iniFile = con->getIniFileForInstance(conSection, instanceNames[i]);
      if (stristr(ExtractFileDir(iniFile.c_str()).c_str(), apsimDirectory.c_str()) == NULL)
         return true;
      }
   return false;
   }

//---------------------------------------------------------------------------
// Remove report variable. Return true if found.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRemoveReportVariable(const string& arguments)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 1)
      throw runtime_error("Invalid arguments to RemoveReportVariable: " + arguments);

   stripLeadingTrailing(args[0], "\" ");

   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> newVariables;
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      bool found = false;
      for (unsigned v = 0; v != variables.size(); v++)
         {
         if (Str_i_Eq(args[0], variables[v]))
            found = true;
         else
            newVariables.push_back(variables[v]);
         }

      if (found)
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", newVariables);

      someHaveChanged = (someHaveChanged || found);
      }
   return someHaveChanged;

   }

