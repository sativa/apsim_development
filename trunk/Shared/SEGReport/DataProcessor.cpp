//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <general\pch.h>
#include <vcl.h>

#include "DataProcessor.h"
#include <general\xml.h>
#include <general\stl_functions.h>
#include "ApsimFileReader.h"
#include "Probability.h"
#include "PredObs.h"
#include "XmlFileReader.h"
#include "Filter.h"
#include "Cumulative.h"
#include "Depth.h"
#include "Diff.h"
#include "ExcelReader.h"
#include "Frequency.h"
#include "KWTest.h"
#include "REMS.h"
#include "Regression.h"
#include "SOI.h"
#include "Stats.h"
#include "DataContainer.h"
#include "RecordFilter.h"
#include "ReportMacros.h"

const int numTypes = 3 * 5 + 1;
const char* validTypes[numTypes] = {"ApsimFileReader", "Probability", "PredObs",
                                    "XmlFileReader",   "Filter",      "Cumulative",
                                    "Depth",           "Diff",        "ExcelReader",
                                    "Frequency",       "KWTest",      "REMS",
                                    "Regression",      "SOI",         "Stats",
                                    "RecordFilter"};

//---------------------------------------------------------------------------
// Return true if the specified type is a valid one.
//---------------------------------------------------------------------------
bool DataProcessor::isValidType(const std::string& propertyType)
   {
   for (int i = 0; i != numTypes; i++)
      if (Str_i_Eq(validTypes[i], propertyType))
         return true;
   return false;
   }

//---------------------------------------------------------------------------
// Create a dataprocessor object based on the settings
// passed in. Caller is expected to free the object
// when finished with it.
//---------------------------------------------------------------------------
DataProcessor* DataProcessor::factory(const XMLNode& properties, TComponent* owner)
   {
   string componentName = properties.getName();
   if (Str_i_Eq(componentName, "ApsimFileReader"))
      return new ApsimFileReader("ApsimFileReader", owner);
   else if (Str_i_Eq(componentName, "Probability"))
      return new Probability("Probability", owner);
   else if (Str_i_Eq(componentName, "PredObs"))
      return new PredObs("PredObs", owner);
   else if (Str_i_Eq(componentName, "XmlFileReader"))
      return new XmlFileReader("XmlFileReader", owner);
   else if (Str_i_Eq(componentName, "Filter"))
      return new Filter("Filter", owner);
   else if (Str_i_Eq(componentName, "Cumulative"))
      return new Cumulative("Cumulative", owner);
   else if (Str_i_Eq(componentName, "Depth"))
      return new Depth("Depth", owner);
   else if (Str_i_Eq(componentName, "Diff"))
      return new Diff("Diff", owner);
//   else if (Str_i_Eq(componentName, "ExcelReader"))
//      return new ExcelReader("ExcelReader");
   else if (Str_i_Eq(componentName, "Frequency"))
      return new Frequency("Frequency", owner);
   else if (Str_i_Eq(componentName, "KWTest"))
      return new KWTest("KWTest", owner);
   else if (Str_i_Eq(componentName, "REMS"))
      return new REMS("REMS", owner);
   else if (Str_i_Eq(componentName, "Regression"))
      return new Regression("Regression", owner);
   else if (Str_i_Eq(componentName, "SOI"))
      return new SOI("SOI", owner);
   else if (Str_i_Eq(componentName, "Stats"))
      return new Stats("Stats", owner);
   else if (Str_i_Eq(componentName, "RecordFilter"))
      return new RecordFilter("RecordFilter", owner);
   else
      return NULL;
   }

//---------------------------------------------------------------------------
// Set the properties of this processor.
// Returns true if the state of the processor has been changed.
//---------------------------------------------------------------------------
bool DataProcessor::setProperties(const XMLNode& properties)
   {
   propertyXMLElements.erase(propertyXMLElements.begin(), propertyXMLElements.end());
   vector<string> localPropertyNames, localPropertyValues;
   for (XMLNode::iterator child = properties.begin();
                          child != properties.end();
                          child++)
      {
      if (!DataProcessor::isValidType(child->getName()))
         {
         propertyXMLElements.push_back(child->write());
         if (child->getValue() != "")
            {
            localPropertyNames.push_back(child->getName());
            localPropertyValues.push_back(child->getValue());
            }
         }
      }
   if (localPropertyNames.size() == 0 ||
       vectorsAreDifferent(propertyNames, localPropertyNames) ||
       vectorsAreDifferent(propertyValues, localPropertyValues))
      {
      propertyNames = localPropertyNames;
      propertyValues = localPropertyValues;
      return true;
      }
   return false;
   }

//---------------------------------------------------------------------------
// Refresh this dataset
//---------------------------------------------------------------------------
void DataProcessor::refresh(TDataSet* source, TDataSet* result)
   {
   errorMessage = "";
   try
      {
      result->Active = false;
      result->FieldDefs->Clear();
      if (source == NULL || source->Active)
         {
         addGroupByFieldDefs(source, result);
         createFields(source, result);
         result->Active = true;

         calcGroupByFilters(source);
         while (groupRecords(source))
            {
            int recNo = result->RecordCount;
            process(source, result);
            addGroupByFieldValues(source, result, recNo+1);
            }
         }
      }
   catch (const runtime_error& err)
      {
      errorMessage += err.what();
      if (source != NULL)
         {
         source->Filtered = false;
         source->Filter = "";
         }
      }
   catch (const Sysutils::Exception& err)
      {
      errorMessage += err.Message.c_str();
      if (source != NULL)
         {
         source->Filtered = false;
         source->Filter = "";
         }
      }
   }


//---------------------------------------------------------------------------
// Return a property to caller.
//---------------------------------------------------------------------------
string DataProcessor::getProperty(const std::string& name)
   {
   vector<string> values = getProperties(name);
   if (values.size() > 0)
      return values[0];
   return "";
   }

//---------------------------------------------------------------------------
// Return multiple properties to caller.
//---------------------------------------------------------------------------
vector<string> DataProcessor::getProperties(const std::string& name)
   {
   vector<string> values;
   for (unsigned i = 0; i != propertyNames.size(); i++)
      {
      if (Str_i_Eq(propertyNames[i], name))
         values.push_back(ReportMacros::resolve(owner, propertyValues[i]));
      }
   return values;
   }

//---------------------------------------------------------------------------
// Group records according to the group by field names. Use the TDataSet
// filtering mechanism to do this.
//---------------------------------------------------------------------------
bool DataProcessor::groupRecords(TDataSet* source)
   {
   if (currentGroupByFilter == groupByFilters.end())
      {
      if (source != NULL)
         {
         source->Filtered = false;
         source->Filter = "";
         }
      return false;
      }
   else
      {
      if (*currentGroupByFilter != "")
         {
         source->Filter = currentGroupByFilter->c_str();
         source->Filtered = true;
         }
      currentGroupByFilter++;
      return true;
      }
   }

// ------------------------------------------------------------------
// Calculate a groupby filter for current record.
// ------------------------------------------------------------------
string DataProcessor::calcGroupByFilter(TDataSet* data, const vector<string>& groupByFieldNames)
   {
   string filter;
   for (unsigned i = 0; i != groupByFieldNames.size(); i++)
      {
      if (filter != "")
         filter += " and ";
      filter = groupByFieldNames[i] + "=";
      string filterValue = AnsiString(data->FieldValues[groupByFieldNames[i].c_str()]).c_str();
      if (Is_numerical(filterValue.c_str()))
         filter += filterValue;
      else
         filter += singleQuoted(filterValue);
      }
   return filter;
   }
// ------------------------------------------------------------------
// Calculate all group by filters.
// ------------------------------------------------------------------
void DataProcessor::calcGroupByFilters(TDataSet* data)
   {
   groupByFilters.erase(groupByFilters.begin(), groupByFilters.end());

   vector<string> groupByFieldNames = getProperties("GroupByFieldName");
   if (groupByFieldNames.size() > 0)
      {
      data->First();
      while (!data->Eof)
         {
         string groupByFilter = calcGroupByFilter(data, groupByFieldNames);
         if (find(groupByFilters.begin(), groupByFilters.end(), groupByFilter)
             == groupByFilters.end())
             groupByFilters.push_back(groupByFilter);
         data->Next();
         }
      }
   else
      groupByFilters.push_back("");

   currentGroupByFilter = groupByFilters.begin();
   }

// ------------------------------------------------------------------
// Add all group by fields to result dataset
// ------------------------------------------------------------------
void DataProcessor::addGroupByFieldDefs(TDataSet* source, TDataSet* result)
   {
   if (source != NULL)
      {
      vector<string> groupByFieldNames = getProperties("GroupByFieldName");
      for (unsigned i = 0; i != groupByFieldNames.size(); i++)
         {
         int f = source->FieldDefs->IndexOf(groupByFieldNames[i].c_str());
         if (f != -1)
            result->FieldDefs->Add(groupByFieldNames[i].c_str(),
                                   source->FieldDefs->Items[f]->DataType,
                                   source->FieldDefs->Items[f]->Size,
                                   false);
         }
      }
   }

// ------------------------------------------------------------------
// Add all group by fields to result dataset starting with the
// specified record number.
// ------------------------------------------------------------------
void DataProcessor::addGroupByFieldValues(TDataSet* source, TDataSet* result, int startingRecNo)
   {
   if (source != NULL)
      {
      vector<string> groupByFieldNames = getProperties("GroupByFieldName");
      if (groupByFieldNames.size() > 0)
         {
         result->RecNo = startingRecNo;
         while (!result->Eof)
            {
            result->Edit();
            for (unsigned i = 0; i != groupByFieldNames.size(); i++)
               result->FieldValues[groupByFieldNames[i].c_str()]
                  = source->FieldValues[groupByFieldNames[i].c_str()];
            result->Post();
            result->Next();
            }
         }
      }
   }

// ------------------------------------------------------------------
// Save the current properties to the specified node.
// ------------------------------------------------------------------
void DataProcessor::save(string& st, int level)
   {
   for (unsigned i = 0; i != propertyXMLElements.size(); i++)
      st += indentString(level) + propertyXMLElements[i] + "\n";
   }

