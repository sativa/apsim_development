//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimComponentData.h"
#include <general\xml.h>
#include <general\stl_functions.h>
#pragma package(smart_init)


// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const std::string& xml)
   : node(NULL)
   {
   xmlDoc = new XMLDocument;
   xmlDoc->readXML(xml);
   node = XMLNode(xmlDoc->documentElement());
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ApsimComponentData::ApsimComponentData(const XMLNode& n)
   : node(n), xmlDoc(NULL)
   {
   XMLNode::iterator initData = find_if(node.begin(),
                                        node.end(),
                                        EqualToName<XMLNode>("initdata"));
   if (initData == node.end())
      node.appendChild("initdata");
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ApsimComponentData::~ApsimComponentData()
   {
   delete xmlDoc;
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
// Return the value of a specific property to caller.
// ------------------------------------------------------------------
std::string ApsimComponentData::getProperty(const std::string& name) const
   {
   XMLNode initData = getInitData();
   for (XMLNode::iterator groupI = initData.begin();
                          groupI != initData.end();
                          groupI++)
      {
      XMLNode::iterator propertyI = find_if(groupI->begin(),
                                            groupI->end(),
                                            NodeEquals<XMLNode>("property", name));
      if (propertyI != groupI->end())
         return propertyI->getValue();
      }
   return "";
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
// Set the value of a specified property.
// ------------------------------------------------------------------
void ApsimComponentData::setProperty(const string& group,
                                     const string& name,
                                     const string& value)
   {
   XMLNode initData = getInitData();
   XMLNode groupNode = initData.appendChild(group);
   XMLNode property = appendChildIfNotExist(groupNode, "property", name);
   property.setValue(value);
   }
// ------------------------------------------------------------------
// Clear all variables.
// ------------------------------------------------------------------
void ApsimComponentData::clearVariables(void)
   {
   getInitData().deleteChild("variables");
   }
// ------------------------------------------------------------------
// return a list of variables to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getVariables(vector<string>& variables) const
   {
   XMLNode initData = getInitData();
   XMLNode::iterator group = find_if(initData.begin(),
                                     initData.end(),
                                     EqualToName<XMLNode>("variables"));
   if (group != initData.end())
      for_each(group->begin(), group->end(),
               GetValueFunction<vector<string>, XMLNode>(variables));
   }
// ------------------------------------------------------------------
// Add a variable if it doesn't already exist.
// ------------------------------------------------------------------
void ApsimComponentData::addVariable(const string& name)
   {
   XMLNode initData = getInitData();
   XMLNode group = initData.appendChild("variables");
   XMLNode::iterator variable = find_if(group.begin(),
                                        group.end(),
                                        ValueEquals<XMLNode>(name));
   if (variable == group.end())
      {
      XMLNode child = group.appendChild("variable", true);
      child.setValue(name);
      }
   }
// ------------------------------------------------------------------
// Clear all rules
// ------------------------------------------------------------------
void ApsimComponentData::clearRules(void)
   {
   getInitData().deleteChild("rules");
   }
// ------------------------------------------------------------------
// return a list of rule names to caller.
// ------------------------------------------------------------------
void ApsimComponentData::getRuleNames(vector<string>& names) const
   {
   XMLNode initData = getInitData();
   XMLNode::iterator rules = find_if(initData.begin(),
                                     initData.end(),
                                     EqualToName<XMLNode>("rules"));
   if (rules != initData.end())
      for_each(rules->begin(), rules->end(),
               GetNameAttributeFunction<vector<string>, XMLNode>(names));
   }
// ------------------------------------------------------------------
// return a rule to caller or blank if not found.
// ------------------------------------------------------------------
string ApsimComponentData::getRule(const std::string& name) const
   {
   XMLNode initData = getInitData();
   XMLNode::iterator rules = find_if(initData.begin(),
                                     initData.end(),
                                     EqualToName<XMLNode>("rules"));
   if (rules != initData.end())
      {
      XMLNode::iterator rule = find_if(rules->begin(), rules->end(),
                                       NodeEquals<XMLNode>("rule", name));
      if (rule != rules->end())
         {
         string sanitisedContents = rule->getValue();
         Replace_all(sanitisedContents, "[cr]", "\n");
         return sanitisedContents;
         }
      }
   return "";
   }
// ------------------------------------------------------------------
// Add a rule if it doesn't already exist.  If it does exist then
// update its contents.
// ------------------------------------------------------------------
void ApsimComponentData::addRule(const string& name, const string& ruleString)
   {
   XMLNode initData = getInitData();
   XMLNode rules = initData.appendChild("rules");
   XMLNode::iterator rule = find_if(rules.begin(),
                                    rules.end(),
                                    NodeEquals<XMLNode>("rule", name));
   string sanitisedContents = ruleString;
   Replace_all(sanitisedContents, "\n", "[cr]");
   if (rule == rules.end())
      {
      XMLNode child = rules.appendChild("rule", true);
      child.setAttribute("name", name);
      child.setValue(sanitisedContents, true);
      }
   else
      rule->setValue(sanitisedContents, true);
   }
// ------------------------------------------------------------------
// Return the contents of this service as an xml string.
// ------------------------------------------------------------------
std::string ApsimComponentData::getXML(void) const
   {
   string xml;
   node.writeXML(xml);
   return xml;
   }
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
extern "C" ApsimComponentData* _export __stdcall newApsimComponentData
   (const char* xml, unsigned xmlLength)
   {
   string xmlString(xml, xmlLength);
   return new ApsimComponentData(xmlString);
   }
extern "C" void _export __stdcall deleteApsimComponentData
   (ApsimComponentData* componentData)
   {
   delete componentData;
   }
extern "C" bool _export __stdcall ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const char* name,
    char* value,
    unsigned nameLength,
    unsigned valueLength)
   {
   string property(name, nameLength);
   string valueSt = componentData->getProperty(property);
   unsigned charsCopied = valueSt.copy(value, valueLength);
   return (charsCopied > 0);
   }

