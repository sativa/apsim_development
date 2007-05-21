//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DataContainer.h"
#include "DataProcessor.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\db_functions.h>
#include <kbmmemtable.hpp>
#include <DBAdvgrd.hpp>
#include "TGridForm.h"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
DataContainer::DataContainer(TComponent* _owner, DataContainer* parentContainer)
   : processor(NULL), data(NULL), owner(_owner), parent(parentContainer)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
DataContainer::~DataContainer()
   {
   delete processor;
   processor = NULL;
   delete data;
   data = NULL;
   for (unsigned i = 0; i != children.size(); i++)
      delete children[i];
   children.erase(children.begin(), children.end());
   }

//---------------------------------------------------------------------------
// Set the full XML for the system.
//---------------------------------------------------------------------------
void DataContainer::setXML(const string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   setProperties(doc.documentElement());
   }

//---------------------------------------------------------------------------
// Return the xml for the system.
//---------------------------------------------------------------------------
std::string DataContainer::getXML()
   {
   string xml;
   save(xml, 0);
   xml = "<Data>\n" + xml + "</Data>";
   return xml;
   }

//---------------------------------------------------------------------------
// Go parse the xml settings passed in and set ourselves up, refreshing
// all data as necessary.
//---------------------------------------------------------------------------
void DataContainer::setProperties(const XMLNode& properties)
   {
   name = properties.getAttribute("name");
   if (name == "")
      name = properties.getName();

   // create a processing object for this container.
   bool createNewProcessor = true;
   if (processor != NULL && Str_i_Eq(processor->type(), properties.getName()))
      createNewProcessor = false;
   if (createNewProcessor)
      {
      delete processor;
      processor = DataProcessor::factory(properties, owner);
      if (processor != NULL)
         {
         if (data == NULL)
            data = new TkbmMemTable(owner);
         }
      }
   if (data != NULL)
      data->Name = name.c_str();

   // give properties to processor and refresh ourselves if necessary.
   if (processor != NULL)
      if (processor->setProperties(properties))
         refresh();

   // remove unwanted children.
   for (unsigned i = 0; i != children.size(); i++)
      {
      bool found = false;
      for (XMLNode::iterator child = properties.begin();
                             child != properties.end() && !found;
                             child++)
         found = (child->getName() == children[i]->name ||
                  child->getAttribute("name") == children[i]->name);
      if (!found)
         {
         // we need to delete this child.
         delete children[i];
         children.erase(children.begin() + i);
         }
      }

   // setup all children
   unsigned childIndex = 0;
   for (XMLNode::iterator child = properties.begin();
                          child != properties.end();
                          child++)
      {
      if (DataProcessor::isValidType(child->getName()))
         {
         if (children.size() <= childIndex)
            children.push_back(new DataContainer(owner, this));
         children[childIndex]->setProperties(*child);
         childIndex++;
         }
      }
   }

//---------------------------------------------------------------------------
// Go find the DataContainer as specified by path.
// Path must be an absolute path including the name of the root node.
// e.g. data\apsimfilereader
//---------------------------------------------------------------------------
DataContainer* DataContainer::findContainer(const std::string& path)
   {
   unsigned pos = path.find('\\');
   if (pos == string::npos)
      {
      if (Str_i_Eq(path, name))
         return this;
      }
   else
      {
      if (Str_i_Eq(path.substr(0, pos), name))
         {
         for (unsigned c = 0; c != children.size(); c++)
            {
            DataContainer* ChildContainer = children[c]->findContainer(path.substr(pos+1));
            if (ChildContainer != NULL)
               return ChildContainer;
            }
         }
      }

   return NULL;
   }

//---------------------------------------------------------------------------
// Return a dataset for the object at the specified path.
//---------------------------------------------------------------------------
TDataSet* DataContainer::findData(const std::string& path)
   {
   DataContainer* container = findContainer(path);
   if (container != NULL)
      return container->data;
   else
      return NULL;
   }

//---------------------------------------------------------------------------
// Return an error message for the object as specified by the path.
//---------------------------------------------------------------------------
string DataContainer::findErrorMessage(const std::string& path)
   {
   DataContainer* container = findContainer(path);
   if (container != NULL && container->processor != NULL)
      return container->processor->getErrorMessage();
   else
      return "";
   }

//---------------------------------------------------------------------------
// Refresh all data
//---------------------------------------------------------------------------
void DataContainer::refresh()
   {
   TDataSet* source = NULL;
   if (parent != NULL)
      source = parent->data;

   if (data != NULL)
      {
      data->DisableControls();
      processor->refresh(source, data);
      }

   for (unsigned c = 0; c != children.size(); c++)
      children[c]->refresh();

   if (data != NULL)
      data->EnableControls();
   }

//---------------------------------------------------------------------------
// save all properties to the specified string
//---------------------------------------------------------------------------
void DataContainer::save(string& st, int level)
   {
   if (processor != NULL)
      {
      st += indentString(level) + "<" + processor->type() + " name=\"" + name + "\">\n";
      processor->save(st, level+1);
      for (unsigned i = 0; i != children.size(); i++)
         children[i]->save(st, level+1);
      st += indentString(level) + "</" + processor->type() + ">\n";
      }
   else
      {
      for (unsigned i = 0; i != children.size(); i++)
         children[i]->save(st, level+1);
      }
   }

//---------------------------------------------------------------------------
// Return all matching properties for the object at the
// specified path.
//---------------------------------------------------------------------------
std::vector<std::string> DataContainer::findProperties(const std::string& path,
                                                       const std::string& propertyName)
   {
   DataContainer* container = findContainer(path);
   if (container != NULL && container->processor != NULL)
      return container->processor->getProperties(propertyName);
   else
      return vector<string>();
   }



//---------------------------------------------------------------------------
//- INTERFACE CALLABLE FROM .NET --------------------------------------------
//---------------------------------------------------------------------------
extern "C" DataContainer* _export __stdcall CreateDataContainer()
   {
   return new DataContainer(NULL, NULL);
   }

extern "C" void _export __stdcall DeleteDataContainer(DataContainer* container)
   {
   delete container;
   }

extern "C" void _export __stdcall GetXml(DataContainer* container,
                                         char* returnString)
   {
   strcpy(returnString, "");
   if (container != NULL)
      strcpy(returnString, container->getXML().c_str());
   }


extern "C" void _export __stdcall SetXml(DataContainer* container,
                                         const char* xml)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   try
      {
      container->setXML(xml);
      }
   catch (const runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (Exception& err)
      {
      ::MessageBox(NULL, err.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (...)
      {
      }
   Screen->Cursor = savedCursor;
   }

extern "C" void _export __stdcall FindErrorMessage(DataContainer* container,
                                                   const char* path,
                                                   char* errorMessage)
   {
   strcpy(errorMessage, "");
   if (container != NULL)
      strcpy(errorMessage, container->findErrorMessage(path).c_str());
   }

extern "C" void _export __stdcall GetFieldNames(DataContainer* container,
                                                const char* path,
                                                char* returnString)
   {
   strcpy(returnString, "");
   if (container != NULL)
      {
      TDataSet* data = container->findData(path);
      if (data != NULL && data->FieldDefs->Count > 0)
         {
         vector<string> fieldNames;
         getDBFieldNames(data, fieldNames);
         strcpy(returnString, buildString(fieldNames, "\t").c_str());
         if (returnString != "" && returnString[strlen(returnString)-1] == '\t')
            returnString[strlen(returnString)-1] = '\0';
         }
      }
   }

extern "C" void _export __stdcall FindProperties(DataContainer* container,
                                                 const char* path,
                                                 const char* propertyName,
                                                 char* returnString)
   {
   strcpy(returnString, "");
   if (container != NULL)
      {
      vector<string> values = container->findProperties(path, propertyName);
      strcpy(returnString, buildString(values, "\t").c_str());
      }
   }

extern "C" unsigned _export __stdcall CreateDataForm(HWND parent)
   {
   TGridForm* form = new TGridForm((void*)parent);
   form->Show();
   return (unsigned) form;
   }
extern "C" void _export __stdcall DeleteDataForm(TForm* form)
   {
   delete form;
   }
extern "C" HWND _export __stdcall GetHandleOfDataForm(TForm* form)
   {
   return form->Handle;
   }
extern "C" void _export __stdcall FillDataFormWithData(TForm* form,
                                                       DataContainer* container,
                                                       const char* path)
   {
   TGridForm* GridForm = dynamic_cast<TGridForm*> (form);
   if (container != NULL && GridForm != NULL)
      {
      TDataSet* data = container->findData(path);
      if (data != NULL && data->Active)
         {
         GridForm->DataSource->DataSet = data;
         GridForm->DataSource->Enabled = true;
         }
      else
         GridForm->DataSource->DataSet = NULL;
      }
   }

//---------------------------------------------------------------------------
// A routine to store a field of data into the dataString - c# can then
// extract the data.
//---------------------------------------------------------------------------
char* StoreColumnInData(TDataSet* data, const char* fieldName, char* dataString)
   {
   int columnIndex = -1;
   for (int f = 0; f != data->FieldDefs->Count; f++)
      {
      if (Str_i_Eq(data->FieldDefs->Items[f]->Name.c_str(), fieldName))
         columnIndex = f;
      }
   char* pos = dataString;
   if (columnIndex != -1)
      {

      // store the type of data. 1=float, 2=string
      if (data->FieldDefs->Items[columnIndex]->DataType == ftFloat)
         *((int*)pos) = 1;
      else if (data->FieldDefs->Items[columnIndex]->DataType == ftDate)
         *((int*)pos) = 2;
      else
         *((int*)pos) = 3;
      pos += 4;

      // store the number of records.
      *((int*)pos) = data->RecordCount;
      pos += 4;

      data->First();
      while (!data->Eof)
         {
         if (data->Fields->Fields[columnIndex]->DataType == ftFloat)
            {
            *((float*) pos) = data->Fields->Fields[columnIndex]->AsFloat;
            pos += 4;
            }
         else if (data->Fields->Fields[columnIndex]->DataType == ftDate)
            {
            TDateTime d = data->Fields->Fields[columnIndex]->AsDateTime;
            unsigned short year, month, day;
            d.DecodeDate(&year, &month, &day);
            *((short*) pos) = year;
            pos += 2;
            *((short*) pos) = month;
            pos += 2;
            *((short*) pos) = day;
            pos += 2;
            }
         else
            {
            AnsiString st = data->Fields->Fields[columnIndex]->AsString.c_str();
            *((int*)pos) = (byte) st.Length();
            pos += 1;
            strcpy(pos, st.c_str());
            pos += st.Length();
            }

         data->Next();
         }
      }
   return pos;
   }


extern "C" void _export __stdcall GetXYData(DataContainer* container,
                                            const char* path,
                                            const char* x,
                                            const char* y,
                                            char* dataAsString)
   {
   strcpy(dataAsString, "");
   if (container != NULL)
      {
      TDataSet* data = container->findData(path);
      if (data != NULL && data->Active && data->FieldDefs->Count > 0)
         {
         dataAsString = StoreColumnInData(data, x, dataAsString);
         dataAsString = StoreColumnInData(data, y, dataAsString);
         }
      }
   }

