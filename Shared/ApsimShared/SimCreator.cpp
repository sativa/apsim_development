//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <string>
#include <map>

#include <general\TreeNodeIterator.h>
#include <general\xml.h>
#include <general\iniFile.h>
#include <general\stringTokenizer.h>
#include <general\Path.h>
#include <general\stl_functions.h>

#include "ApsimControlFile.h"
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"
#include "ApsimServiceData.h"
#include "ApsimSimulationFile.h"
#include "ApsimDirectories.h"
#include "ApsimSettings.h"
#include "SimCreator.h"


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
         string componentOrderFileName = getApsimDirectory() + "\\apsim\\component.ordering";
         if (!FileExists(componentOrderFileName.c_str()))
            throw runtime_error("Cannot find file: " + componentOrderFileName);
         IniFile componentOrdering(componentOrderFileName);
         componentOrdering.read("component_order", "component", components);
         }
      // ------------------------------------------------------------------
      // Compare method used by sort.  Returns true if arg1 < arg2.
      // ------------------------------------------------------------------
      bool operator() (const ApsimControlFile::ModuleInstance& arg1,
                       const ApsimControlFile::ModuleInstance& arg2)
         {
         if (arg1.moduleName == arg2.moduleName)
            return (arg1.instanceName < arg2.instanceName);
         for (unsigned i = 0; i != components.size(); i++)
            {
            if (Str_i_Eq(components[i], arg1.moduleName))
               return true;
            if (Str_i_Eq(components[i], arg2.moduleName))
               return false;
            }
         return true; // neither are in list!!
         }

   private:
      vector<string> components;
   };
//---------------------------------------------------------------------------
// Calculate and return a group name to write to based on the section name.
//---------------------------------------------------------------------------
void getPropertyTypeAndGroupName(const string& section,
                                 string& propertyType,
                                 string& groupName)
   {
   StringTokenizer tokenizer(section, ".");
   groupName = tokenizer.nextToken();
   tokenizer.nextToken();
   propertyType = tokenizer.nextToken();
   stripLeadingTrailing(propertyType, " ");
   stripLeadingTrailing(groupName, " ");
   }
//---------------------------------------------------------------------------
// This class imports a specific parameter section into a specific component.
//---------------------------------------------------------------------------
class ImportSection
   {
   public:
      ImportSection(ApsimComponentData& c, const string& modName)
         : component(c), moduleName(modName)
         {
         ApsimSettings settings;
         settings.read("Apsim Manager Modules|module", managerModules);
         }

      void callback(IniFile* par, const string& section)
         {
         if (find_if(managerModules.begin(), managerModules.end(),
                     PartialStringComparison(moduleName)) != managerModules.end())
            importWholeSection(par, section);

         else
            importSection(par, section);
         }

   private:
      ApsimComponentData& component;
      const string& moduleName;
      vector<string> managerModules;

      //---------------------------------------------------------------------------
      // Import the whole section as a rule
      //---------------------------------------------------------------------------
      void importWholeSection(IniFile* par, const string& section) const
         {
         string contents;
         par->readSection(section, contents);
         replaceAll(contents, "\t", "   ");
         string propertyType, groupName;
         getPropertyTypeAndGroupName(section, propertyType, groupName);
         component.addRule(groupName + "." + propertyType, propertyType, contents);
         }
      //---------------------------------------------------------------------------
      // Import the specified lines in to the specified operations component.
      //---------------------------------------------------------------------------
      void importSection(IniFile* par, const string& section) const
         {
         string propertyType, groupName;
         getPropertyTypeAndGroupName(section, propertyType, groupName);

         // problem with XML elements starting with numbers.
         char *endptr;
         strtod(propertyType.c_str(), &endptr);
         if (endptr != propertyType.c_str())
            propertyType = "_" + propertyType;

         // Read & parse buffer into lines
         string buffer;
         par->readSection(section, buffer);
         replaceAll(buffer, "\t", " ");

         vector<string> lines;
         Split_string(buffer, "\n", lines);

         // Add each key/variable value
         for (unsigned k = 0; k != lines.size(); k++)
            {
            string key, value, units, line;
            line = lines[k];
            stripComments(line);
            //getKeyNameValueUnits(line, key, value, units);
            getKeyNameAndValue(line, key, value);
            To_lower(key);
            if (key != "")
               {
               if (Str_i_Eq(key, "variable"))
                  component.addVariable(value);
               else
                  component.setProperty(propertyType, groupName, key, value);
               }
            }
         };
   // ------------------------------------------------------------------
   // Strip all comments from a line.
   // ------------------------------------------------------------------
   void stripComments(std::string& line) const
      {
      unsigned posComment = line.find_first_of("!");
      if (posComment != string::npos)
         line.erase(posComment);
      }
   }; // End class ImportSection
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
SimCreator::SimCreator(const std::string& controlFileName)
   {
   con = new ApsimControlFile(controlFileName);
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
SimCreator::~SimCreator(void)
   {
   delete con;
   }
//---------------------------------------------------------------------------
// Create SIM files for the given control.  All sims are stored in the
// specified sim directory.  If sim directory is null then the control file
// directory is used. The simCreator event is called every time
// a new sim file is created.
//---------------------------------------------------------------------------
void SimCreator::createSims(const std::string& simDirectory,
                            TSimCreatorEvent simCreatorEvent)
   {
   vector<string> sectionNames;
   con->getAllSectionNames(sectionNames);
   createSims(sectionNames, simDirectory, simCreatorEvent);
   }
//---------------------------------------------------------------------------
// Create a single SIM file for the specified control
// file section name.  The sim will be stored in the
// specified sim directory.  If sim directory is null then the control file
// directory is used
//---------------------------------------------------------------------------
void SimCreator::createSim(const std::string& sectionName,
                           const std::string& simDirectory)
   {
   createSim(sectionName, 0, simDirectory, (TSimCreatorEvent)NULL);
   }
//---------------------------------------------------------------------------
// Create SIM files for the given control for the specified control
// file section names.  All sims are stored in the
// specified sim directory.  If sim directory is null then the control file
// directory is used. The simCreator event is called every time
// a new sim file is created.
//---------------------------------------------------------------------------
void SimCreator::createSims(const std::vector<std::string>& sectionNames,
                            const std::string& simDirectory,
                            TSimCreatorEvent simCreatorEvent)
   {
   if (sectionNames.size() == 1)
      createSim(sectionNames[0], 0, simDirectory, simCreatorEvent);
   else
      {
      for (unsigned s = 0; s != sectionNames.size(); s++)
         createSim(sectionNames[s], s+1, simDirectory, simCreatorEvent);
      }
   }
// ------------------------------------------------------------------
// Create a SIM file for the specified section.
// ------------------------------------------------------------------
void SimCreator::createSim(const string& sectionName,
                           int simNumber,
                           const std::string& simDirectory,
                           TSimCreatorEvent simCreatorEvent)
   {
   Path conPath(con->getFileName());
   conPath.Change_directory();

   // create the name of the simulation file.
   string simulationFileName;
   if (simDirectory == "")
      simulationFileName = conPath.Get_directory();
   else
      simulationFileName = simDirectory;
   simulationFileName += "\\" + conPath.Get_name_without_ext();
   if (simNumber > 0)
      simulationFileName += IntToStr(simNumber).c_str();
   simulationFileName += ".sim";
   simulationFileName = ExpandFileName(simulationFileName.c_str()).c_str();
   DeleteFile(simulationFileName.c_str());

   if (con->isValid(sectionName))
      {
      ApsimSimulationFile simulation;
      simulation.setFileName(simulationFileName);

      simulation.setExecutableFileName(getApsimDirectory() + "\\apsim\\protocolmanager\\lib\\protocolmanager.dll");
      simulation.setTitle(con->getTitle(sectionName));

      // get the names of all input modules.
      ApsimSettings settings;
      vector<string> inputModules;
      settings.read("Apsim Input Modules|module", inputModules);


      ApsimControlFile::ModuleInstances moduleInstances;
      con->getAllModuleInstances(sectionName, moduleInstances);
      stable_sort(moduleInstances.begin(), moduleInstances.end(), ComponentOrder());
      for (ApsimControlFile::ModuleInstances::iterator m = moduleInstances.begin();
                                                       m != moduleInstances.end();
                                                       m++)
         {
         string moduleName = m->moduleName;
         string instanceName = m->instanceName;
         string dllFileName = m->dllFileName;

         ApsimComponentData* component;

         // See if we've already parsed the .ini file this component.
         string iniFileName = con->getIniFileForInstance(sectionName, instanceName);
         if (iniFileName != "")
            {
            string componentXML;

            Components::iterator c = components.find(iniFileName + ":" + instanceName);
            if (c == components.end())
               {
               ApsimComponentData iniComponent;
               ImportSection importSection(iniComponent, moduleName);
               con->enumerateParametersForInstance(sectionName, instanceName, true, importSection.callback);
               componentXML = iniComponent.getXML();
               components.insert(make_pair(iniFileName + ":" + instanceName,
                                           componentXML));
               }
            else
               componentXML = c->second;
            component = new ApsimComponentData(componentXML);
            component->setName(instanceName);
            *component = simulation.addComponent(*component);
            }
         else
            component = new ApsimComponentData(simulation.addComponent(instanceName));

         component->setExecutableFileName(dllFileName);

         if (find_if(inputModules.begin(), inputModules.end(),
                     PartialStringComparison(moduleName)) != inputModules.end())
            {
            string fileName = con->getFileForInstance(sectionName, instanceName);
            component->setProperty("parameters", "file", "filename", fileName);
            }

         else
            {
            ImportSection importSection(*component, moduleName);
            con->enumerateParametersForInstance(sectionName, instanceName, false, importSection.callback);
            }
         delete component;
         }
      simulation.write();
      if (simCreatorEvent != NULL)
         simCreatorEvent(simulationFileName);
      }
   }
//---------------------------------------------------------------------------
// Treat the file passed in as an .ini file and convert it
// to a sim file format. Return the converted contents as a string.
//---------------------------------------------------------------------------
std::string SimCreator::convertIniToSim(const std::string& filename)
   {
   if (filename != "" && FileExists(filename.c_str()))
      {
      string moduleName = Path(filename).Get_name_without_ext();
      ApsimComponentData iniComponent;
      ImportSection importSection(iniComponent, moduleName);

      IniFile* par = new IniFile(filename, true);
      vector<string> paramFileSections;

      string sectionToMatch = "standard." + moduleName + ".";
      par->readSectionNames(paramFileSections);

      for (unsigned s = 0; s != paramFileSections.size(); s++)
         {
         if (Str_i_Eq(paramFileSections[s].substr(0, sectionToMatch.length()), sectionToMatch))
            importSection.callback(par, paramFileSections[s]);
         }


      string contents = iniComponent.getXML();

      // Only return the bit between the <initdata> and </initdata> tags.
      const char* START_TAG = "<initdata>";
      const char* END_TAG = "</initdata>";

      unsigned startPos = contents.find(START_TAG);
      unsigned endPos = contents.find(END_TAG);
      if (startPos != string::npos && endPos != string::npos)
         {
         startPos += strlen(START_TAG);
         contents = contents.substr(startPos, endPos-startPos);
         }
      else
         throw runtime_error("Cannot find <initdata> tags in .ini file: " + filename);
      delete par;
      return contents;
      }
   return "";
   }
//---------------------------------------------------------------------------
// Treat the file passed in as an .ini file and convert it
// to a sim file format. Return the converted contents as a string.
//---------------------------------------------------------------------------
extern "C" void _export __stdcall convertIniToSim(const char* fileName, char* contents)
   {
   SimCreator simCreator;
   string xml = simCreator.convertIniToSim(fileName);
   strcpy(contents, xml.c_str());
   }

