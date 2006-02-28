#include <stdlib.h>

#include <string>
#include <vector>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/path.h>
#include "FStringExt.h"
#include "ApsimRegistrationData.h"
#include "ApsimDataTypeData.h"
#include "ApsimDataTypesFile.h"
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(void)
   : node(NULL, NULL), dataTypesFile(NULL)
   {
   xmlDoc = new XMLDocument("component", XMLDocument::rootName);
   node = xmlDoc->documentElement();
   node.appendChild("initdata");
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const std::string& xml)
   : node(NULL, NULL), dataTypesFile(NULL)
   {
   xmlDoc = new XMLDocument(xml, XMLDocument::xmlContents);
   node = xmlDoc->documentElement();
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const XMLNode& n)
   : node(n), xmlDoc(NULL), dataTypesFile(NULL)
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   if (initData == node.end())
      node.appendChild("initdata");
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimComponentData::~ApsimComponentData()
   {
   delete xmlDoc;
   delete dataTypesFile;
   }
// ------------------------------------------------------------------
// copy constructor.
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const ApsimComponentData& rhs)
   : node(rhs.node), xmlDoc(NULL), dataTypesFile(NULL)
   {
   haveReadBaseProperties = false;
   }
// ------------------------------------------------------------------
// Assignment operator.
// ------------------------------------------------------------------
ApsimComponentData& ApsimComponentData::operator=(const ApsimComponentData& rhs)
   {
   node = rhs.node;
   delete xmlDoc;
   xmlDoc = NULL;
   dataTypesFile = NULL;
   haveReadBaseProperties = false;
   return *this;
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getExecutableFileName(void) const
   {
   return node.getAttribute("executable");
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
string ApsimComponentData::getComponentInterfaceFileName(void) const
   {
   XMLNode::iterator interfaceNode = find_if(node.begin(),
                                             node.end(),
                                             EqualToName<XMLNode>("componentinterface"));
   if (interfaceNode == node.end())
      return "";

   else
      return interfaceNode->getValue();
   }
// ------------------------------------------------------------------
// Return an iterator to the initdata node.
// ------------------------------------------------------------------
XMLNode ApsimComponentData::getInitData(void) const
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   return *initData;
   }
// ------------------------------------------------------------------
// Set the name of the component.
// ------------------------------------------------------------------
void ApsimComponentData::setName(const std::string& name)
   {
   node.setAttribute("name", name);
   }
// ------------------------------------------------------------------
// Return name of component to caller.
// ------------------------------------------------------------------
void ApsimComponentData::setExecutableFileName(const std::string& executable)
   {
   node.setAttribute("executable", executable);
   }
// ------------------------------------------------------------------
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
std::string ApsimComponentData::getProperty(const std::string& propType,
                                            const std::string& name) const
   {
   string propertyType = propType;
   char *endptr;
   strtod(propertyType.c_str(), &endptr);
   if (endptr != propertyType.c_str()) propertyType = "_" + propertyType;

   vector<string> matches;
   if (Str_i_Eq(propertyType, "parameters") ||
       Str_i_Eq(propertyType, "constants"))
      {
      if (!haveReadBaseProperties)
         {
         getProperties(propertyType, baseNames, baseValues);
         haveReadBaseProperties = true;
         }
      return matchProperty(name, baseNames, baseValues);
      }
   else
      {
      vector<string> names, values, matches;
      getProperties(propertyType, names, values);
      return matchProperty(name, names, values);
      }
   }

// ------------------------------------------------------------------
// Match and return the specified name
// ------------------------------------------------------------------
string ApsimComponentData::matchProperty(const std::string& name,
                                         const vector<string>& names,
                                         const vector<string>& values) const
   {
   vector<string> matches;
   for (unsigned int i = 0; i != names.size(); i++)
      if (Str_i_Eq(names[i].c_str(), name.c_str()))
        matches.push_back(values[i]);

   if (matches.size() > 1)
      throw std::runtime_error("Parameter " + name + " has multiple definitions");
   if (matches.size() == 1) return matches[0];

   // look in all tables if we can't find the parameter.
   if (find_if(names.begin(), names.end(), CaseInsensitiveStringComparison(name))
       != names.end())
      {
      for (XMLNode::iterator propertyI = getInitData().begin();
                             propertyI != getInitData().end() && matches.size() == 0;
                             propertyI++)
         if (Str_i_Eq(propertyI->getName(), "table"))
            matches.push_back(getValuesFromTable(name, *propertyI));
      }
   if (matches.size() == 1)
      return matches[0];
   else
      return "";
   }
// ------------------------------------------------------------------
// Return values from the specified table for the specific property
// ------------------------------------------------------------------
string ApsimComponentData::getValuesFromTable(const std::string& name, XMLNode tableNode) const
   {
   string returnString;
   for (XMLNode::iterator row = tableNode.begin();
                          row != tableNode.end();
                          row++)
      for (XMLNode::iterator property = row->begin();
                             property != row->end();
                             property++)
         {
         if (Str_i_Eq(property->getName(), name))
            {
            if (returnString != "")
               returnString += " ";
            returnString += property->getValue();
            }
         }

   return returnString;
   }

// ------------------------------------------------------------------
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getProperties(const std::string& propertyType,
                                       vector<string>& names,
                                       vector<string>& values) const
   {
   if ((Str_i_Eq(propertyType, "parameters") ||
        Str_i_Eq(propertyType, "constants")) && haveReadBaseProperties)
      {
      names = baseNames;
      values = baseValues;
      }
   else
      {
      XMLNode parentNode = getInitData();
      if (propertyType != "" && !Str_i_Eq(propertyType, "parameters") &&
          !Str_i_Eq(propertyType, "constants"))
          parentNode = findNode(getInitData(), propertyType);

      for (XMLNode::iterator propertyI = parentNode.begin();
                             propertyI != parentNode.end();
                             propertyI++)
         {
         names.push_back(propertyI->getName());
         values.push_back(propertyI->getValue());
         }
      }
   }
/*
// ------------------------------------------------------------------
// Try and replace the value of the specified property.  Return true
// if property was found.  False otherwise.
// ------------------------------------------------------------------
bool ApsimComponentData::replaceProperty(const std::string& propertyType,
                                         const std::string& name,
                                         const std::string& value)
   {
   XMLNode initData = getInitData();
   for (XMLNode::iterator groupI = initData.begin();
                          groupI != initData.end();
                          groupI++)
      {
      if (Str_i_Eq(groupI->getName(), propertyType))
         {
         XMLNode::iterator propertyI = find_if(groupI->begin(),
                                               groupI->end(),
                                               NodeEquals<XMLNode>("property", name));
         if (propertyI != groupI->end())
            {
            propertyI->setValue(value);
            return true;
            }
         }
      }
   return false;
   }
// ------------------------------------------------------------------
// Set the value of a specified property.
// ------------------------------------------------------------------
void ApsimComponentData::setProperty(const string& propertyType,
                                     const string& groupName,
                                     const string& name,
                                     const string& value)
   {
   XMLNode initData = getInitData();
   XMLNode groupNode = appendChildIfNotExist(initData, propertyType, groupName);
   XMLNode property = groupNode.appendChild("property", true);
   property.setAttribute("name", name);
   property.setValue(value);
   }
// ------------------------------------------------------------------
// Delete all properties with the specified type.
// ------------------------------------------------------------------
void ApsimComponentData::clearProperties(const std::string& propertyType)
   {
   XMLNode initData = getInitData();
   eraseNodes(initData, propertyType);
   }
// ------------------------------------------------------------------
// Return a list of group names to caller for the specified type
// of properties.
// ------------------------------------------------------------------
void ApsimComponentData::getGroupNames(const std::string& propertyType,
                                       std::vector<std::string>& groupNames)
   {
   XMLNode initData = getInitData();
   for_each_if(initData.begin(), initData.end(),
               GetNameAttributeFunction<XMLNode>(groupNames),
               EqualToName<XMLNode>(propertyType));
   }
// ------------------------------------------------------------------
// Clear all variables.
// ------------------------------------------------------------------
void ApsimComponentData::clearVariables(void)
   {
   XMLNode initData = getInitData();
   eraseNodes(initData, "variables");
   }*/
// ------------------------------------------------------------------
// return a list of variables to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getVariables(vector<string>& variables) const
   {
   XMLNode initData = getInitData();
   for_each_if(initData.begin(), initData.end(),
               GetValueFunction<vector<string>, XMLNode>(variables),
               EqualToName<XMLNode>("variable"));
   }
/*
// ------------------------------------------------------------------
// Add a variable if it doesn't already exist.
// ------------------------------------------------------------------
void ApsimComponentData::addVariable(const string& name)
   {
   XMLNode initData = getInitData();
   XMLNode group = initData.appendChild("variables");
   XMLNode child = group.appendChild("variable", true);
   child.setValue(name);
   }
// ------------------------------------------------------------------
// Clear all rules
// ------------------------------------------------------------------
void ApsimComponentData::clearRules(void)
   {
   XMLNode initData = getInitData();
   eraseNodes(initData, "rules");
   } */
// ------------------------------------------------------------------
// return a list of rule names to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getRuleNames(vector<string>& names) const
   {
   XMLNode initData = getInitData();
   for_each(initData.begin(), initData.end(),
            GetNameAttributeFunction<XMLNode>(names));
   }
// ------------------------------------------------------------------
// return a rule to caller or blank if not found.
// ------------------------------------------------------------------
void ApsimComponentData::getRule(const std::string& name,
                                 std::string& condition,
                                 std::string& contents) const
   {
   XMLNode initData = getInitData();
   XMLNode::iterator rule = find_if(initData.begin(), initData.end(),
                                    NodeEquals<XMLNode>("rule", name));
   if (rule != initData.end())
      {
      condition = rule->getAttribute("condition");
      contents = rule->getValue();
      Replace_all(contents, "[cr]", "\n");
      replaceAllMacros(rule, condition);
      replaceAllMacros(rule, contents);
      }
   }  /*
// ------------------------------------------------------------------
// Add a rule if it doesn't already exist.  If it does exist then
// update its contents.
// ------------------------------------------------------------------
void ApsimComponentData::addRule(const string& name,
                                 const string& condition,
                                 const string& contents)
   {
   XMLNode initData = getInitData();
   XMLNode rules = initData.appendChild("rules");
   XMLNode::iterator rule = find_if(rules.begin(),
                                    rules.end(),
                                    NodeAttributesEquals<XMLNode>
                                       ("rule",
                                        "name", name,
                                        "condition", condition));
   string sanitisedContents = contents;
   Replace_all(sanitisedContents, "\n", "[cr]");
   if (rule == rules.end())
      {
      XMLNode child = rules.appendChild("rule", true);
      child.setAttribute("name", name);
      child.setAttribute("condition", condition);
      child.setValue(sanitisedContents);
      }
   else
      rule->setValue(sanitisedContents);
   }    */
// ------------------------------------------------------------------
// Return the contents of this service as an xml string.
// ------------------------------------------------------------------
std::string ApsimComponentData::getXML(void) const
   {
   return node.write();
   }
// ------------------------------------------------------------------
// Return an iterator to the first registration
// ------------------------------------------------------------------
ApsimComponentData::RegIterator ApsimComponentData::regBegin(void) const
   {
   XMLNode::iterator i = find_if(node.begin(),
                                 node.end(),
                                 EqualToName<XMLNode>("registrations"));
   if (i != node.end())
      return i->begin();
   else
      return regEnd();
   }
// ------------------------------------------------------------------
// Return an iterator to the last registration
// ------------------------------------------------------------------
ApsimComponentData::RegIterator ApsimComponentData::regEnd(void) const
   {
   return RegIterator(node.end());
   }

//---------------------------------------------------------------------------
// Return a specific data type to caller.  Will throw if that type doesn't
// exist.
//---------------------------------------------------------------------------
ApsimDataTypeData ApsimComponentData::getDataType
   (const string& name) const
   {
   XMLNode::iterator types = find_if(node.begin(),
                                     node.end(),
                                     EqualToName<XMLNode>("types"));
   if (types != node.end())
      {
      XMLNode::iterator i = find_if(types->begin(),
                                    types->end(),
                                    NodeEquals<XMLNode>("type", name));
      if (i != types->end())
         return ApsimDataTypeData(*i);
      }
   if (dataTypesFile == NULL)
      dataTypesFile = new ApsimDataTypesFile;
   return dataTypesFile->getDataType(name);
   }

// ------------------------------------------------------------------
// return the name of the interface file for this component
// ------------------------------------------------------------------
std::string ApsimComponentData::getInterfaceFileName(void) const
   {
   string dll = getExecutableFileName();
   if (dll != "")
      {
      Path interfaceFilePath(dll);
      interfaceFilePath.Back_up_directory();
      interfaceFilePath.Set_extension(".interface");
      if (interfaceFilePath.Exists())
         return interfaceFilePath.Get_path();
      }
   return "";
   }
// ------------------------------------------------------------------
// Replace all macros between square brackets.
// ------------------------------------------------------------------
void ApsimComponentData::replaceAllMacros(XMLNode::iterator rulesNode, string& contents) const
   {
   unsigned posOpenBracket = contents.find('[');
   while (posOpenBracket != string::npos)
      {
      unsigned posCloseBracket = contents.find(']', posOpenBracket);
      string macroName = contents.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);

      // try and resolve macroName from a properties child.
      for (XMLNode::iterator category = rulesNode->begin();
                             category != rulesNode->end();
                             category++)
         {
         if (category->getName() == "category")
            {
            XMLNode::iterator property = find_if(category->begin(), category->end(), NodeEquals<XMLNode>("property", macroName));
            if (property != category->end())
               {
               // found a property with the macro name - replace macro name.
               string macroValue = property->getAttribute("value");
               contents.replace(posOpenBracket, posCloseBracket-posOpenBracket+1, macroValue);
               }
            }
         }

      posOpenBracket = contents.find('[', posOpenBracket+1);
      }
   }


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
extern "C" unsigned _export __stdcall newApsimComponentData
   (const char* xml, unsigned xmlLength)
   {
   string xmlString(xml, xmlLength);
   return (unsigned) new ApsimComponentData(xmlString);
   }
extern "C" void _export __stdcall deleteApsimComponentData
   (ApsimComponentData* componentData)
   {
   delete componentData;
   }
extern "C" bool _export __stdcall ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const FString& propertyType,
    const FString& name,
    FString& value)
   {
   string lowerValue = componentData->getProperty(asString(propertyType), asString(name));
   strlwr((char*)lowerValue.c_str());
   value = lowerValue.c_str();
   return (value.length() > 0);
   }
extern "C" void _export __stdcall ApsimComponentData_getRuleNames
   (ApsimComponentData** componentData,
    char* names,
    unsigned* maxNumNames,
    unsigned* numNames,
    unsigned namesLength)
   {
   vector<string> ruleNames;
   (*componentData)->getRuleNames(ruleNames);
   FStrings(names, namesLength, *maxNumNames, 0) = ruleNames;
   *numNames = ruleNames.size();
   }
vector<string> ruleLines;
string ruleCondition;
extern "C" void _export __stdcall ApsimComponentData_loadRule
   (ApsimComponentData** componentData,
    const char* name,
    unsigned nameLength)
   {
   string contents;
   (*componentData)->getRule(asString(FString(name, nameLength, FORString)),
                             ruleCondition,
                             contents);
   Split_string(contents, "\n", ruleLines);
   }
extern "C" unsigned _export __stdcall ApsimComponentData_getNumRuleLines
   (void)
   {
   return ruleLines.size();
   }
extern "C" void _export __stdcall ApsimComponentData_getRuleLine
   (unsigned* lineNumber,
    char* line,
    unsigned lineLength)
   {
   FString l(line, lineLength, FORString);
   l = ruleLines[*lineNumber].c_str();
   }
extern "C" void _export __stdcall ApsimComponentData_getRuleCondition
   (char* condition,
    unsigned conditionLength)
   {
   FString cond(condition, conditionLength, FORString);
   cond = ruleCondition.c_str();
   }

