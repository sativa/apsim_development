//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SimCreator.h"
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include <ApsimShared\ApsimDirectories.h>
#include <general\path.h>
#include <general\iniFile.h>
#include <general\stringTokenizer.h>

#pragma package(smart_init)

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
   Strip(propertyType, " ");
   Strip(groupName, " ");
   }
//---------------------------------------------------------------------------
// This class imports a specific parameter section into a specific component.
//---------------------------------------------------------------------------
class ImportSection
   {
   public:
      ImportSection(ApsimComponentData& c, const string& modName)
         : component(c), moduleName(modName) { }

      void callback(IniFile* par, const string& section)
         {
         if (Str_i_Eq(moduleName, "manager")
             || Str_i_Eq(moduleName, "operatns")
             || Str_i_Eq(moduleName, "tcllink"))
            importWholeSection(par, section);

         else
            importSection(par, section);
         }

   private:
      ApsimComponentData& component;
      const string& moduleName;

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

         // Loop through all keys in section.
         vector<string> keys;
         par->getKeysInSection(section, keys);
         for (unsigned k = 0; k != keys.size(); k++)
            {
            string value;
            par->read(section, keys[k], value);

            if (keys[k] != "" && !Str_i_Eq(keys[k], "variable"))
               {
               // strip any units off the value.
               unsigned posUnits = value.find('(');
               if (posUnits != string::npos)
                  value.erase(posUnits);
               Strip(value, " ");
               if (value != "")
                  {
                  // problem with XML elements starting with numbers.
                  char *endptr;
                  strtod(propertyType.c_str(), &endptr);
                  if (endptr != propertyType.c_str())
                     propertyType = "_" + propertyType;
                     component.setProperty(propertyType, groupName, keys[k], value);
                  }
               }
            }
         // Treat report variables differently.
         vector<string> variables;
         par->read(section, "variable", variables);
         for (unsigned v = 0; v != variables.size(); v++)
            component.addVariable(variables[v]);
         }

   };
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
   for (unsigned s = 0; s != sectionNames.size(); s++)
      createSim(sectionNames[s], s+1, simDirectory, simCreatorEvent);
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

   ApsimSimulationFile simulation;
   simulation.setFileName(simulationFileName);

   simulation.setExecutableFileName(getApsimDirectory() + "\\apsim\\protocolmanager\\lib\\protocolmanager.dll");
   simulation.setTitle(con->getTitle(sectionName));

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

      ApsimComponentData component = simulation.addComponent(instanceName);
      component.setExecutableFileName(dllFileName);

      if (Str_i_Eq(moduleName, "input") || Str_i_Eq(moduleName, "soi"))
         {
         string fileName = con->getFileForModule(sectionName, moduleName);
         component.setProperty("parameters", "file", "filename", fileName);
         }

      else
         {
         ImportSection importSection(component, moduleName);
         con->enumerateParametersForInstance(sectionName, instanceName, true, importSection.callback);
         }
      }
   simulation.write();
   if (simCreatorEvent != NULL)
      simCreatorEvent(simulationFileName);
   }

