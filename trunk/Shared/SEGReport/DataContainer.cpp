//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DataContainer.h"
#include <general\xml.h>
#include <general\string_functions.h>
#include <kbmmemtable.hpp>
#include <DBAdvgrd.hpp>
#include "TGridForm.h"
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
void DataContainer::clear()
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
// Setup the data container using the specified properties. This method is
// called to completely initialise the system.
//---------------------------------------------------------------------------
void DataContainer::setup(const string& properties)
   {
   clear();
   XMLDocument doc(properties, XMLDocument::xmlContents);
   setProperties(doc.documentElement());
   TopLevelContainer = this;
   }

//---------------------------------------------------------------------------
// Set the properties for the specified data path.
// Path must be an absolute path including the name of the root node.
// e.g. data\apsimfilereader
// Return true if properties were actually changed ie. a refresh is needed.
//---------------------------------------------------------------------------
bool DataContainer::setProperties(const std::string& path, const std::string& properties)
   {
   DataContainer* container = findContainer(path);
   XMLDocument doc(properties, XMLDocument::xmlContents);
   if (container != NULL)
      return container->setProperties(doc.documentElement());
   return false;
   }

//---------------------------------------------------------------------------
// Go parse the xml settings passed in and set ourselves up.
// Return true if properties were actually changed ie. a refresh is needed.
//---------------------------------------------------------------------------
bool DataContainer::setProperties(const XMLNode& properties)
   {
   bool dataWasChanged = false;

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
      processor = DataProcessor::factory(properties);
      if (processor != NULL)
         {
         if (data == NULL)
            data = new TkbmMemTable(owner);
         data->Name = name.c_str();
         }
      }

   // give properties to processor.
   if (processor != NULL)
      dataWasChanged = processor->setProperties(properties);

   // setup all children
   unsigned childIndex = 0;
   for (XMLNode::iterator child = properties.begin();
                          child != properties.end();
                          child++)
      {
      if (DataProcessor::isValidType(child->getName()))
         {
         if (children.size() <= childIndex)
            children.push_back(new DataContainer(owner));
         dataWasChanged = (children[childIndex]->setProperties(*child)
                           || dataWasChanged);
         childIndex++;
         }
      }

   // remove unwanted children.
   for (unsigned i = childIndex; i < children.size(); i++)
      {
      delete children[i];
      children.erase(children.begin() + i);
      dataWasChanged = true;
      }
   return dataWasChanged;
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
// Return a result dataset for the absolute data path.
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
// Return a result dataset for the specified name.
// name will be searched for recursively through the
// tree of containers.
//---------------------------------------------------------------------------
TDataSet* DataContainer::searchForData(const std::string& nameToFind)
   {
   if (Str_i_Eq(name, nameToFind))
      return data;
   for (unsigned c = 0; c != children.size(); c++)
      {
      TDataSet* dataToReturn = children[c]->searchForData(nameToFind);
      if (dataToReturn != NULL)
         return dataToReturn;
      }
   return NULL;
   }

//---------------------------------------------------------------------------
// Return an error message for the specified data path.
//---------------------------------------------------------------------------
string DataContainer::getErrorMessage(const std::string& path)
   {
   DataContainer* container = findContainer(path);
   if (container != NULL && container->processor != NULL)
      return container->processor->getErrorMessage();
   else
      return "";
   }

//---------------------------------------------------------------------------
// Refresh the data container as specified by path.
//---------------------------------------------------------------------------
void DataContainer::refresh(const string& path)
   {
   // first we need to find the parent container (if any) so that we can
   // get the source data. Then we can find the container we're interested in
   // and then refresh it.
   TDataSet* parentData = NULL;
   unsigned pos = path.rfind('\\');
   if (pos != string::npos)
      parentData =  findData(path.substr(0, pos));

   DataContainer* container = findContainer(path);
   if (container == NULL)
      refresh(parentData);
   else
      container->refresh(parentData);
   }

//---------------------------------------------------------------------------
// refresh the result data for this container and all children using
// the specified source data.
//---------------------------------------------------------------------------
void DataContainer::refresh(TDataSet* source)
   {
   if (data != NULL)
      data->DisableControls();

   if (processor != NULL && data != NULL)
      processor->refresh(source, data);
   for (unsigned c = 0; c != children.size(); c++)
      children[c]->refresh(data);

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


#include "rems.h"
//---------------------------------------------------------------------------
//- INTERFACE CALLABLE FROM .NET --------------------------------------------
//---------------------------------------------------------------------------
extern "C" void _export __stdcall SetProperties(DataContainer* container,
                                                const char* path,
                                                const char* properties)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   try
      {
      if (container != NULL && container->setProperties(path, properties))
         container->refresh(path);
      }
   catch (...)
      {
      }
   Screen->Cursor = savedCursor;
   }

extern "C" void _export __stdcall GetErrorMessage(DataContainer* container,
                                                  const char* path,
                                                  char* errorMessage)
   {
   strcpy(errorMessage, "");
   if (container != NULL)
      strcpy(errorMessage, container->getErrorMessage(path).c_str());
   }

extern "C" void _export __stdcall GetFieldNames(DataContainer* container,
                                                const char* path,
                                                char* fieldNames)
   {
   strcpy(fieldNames, "");

   if (container != NULL)
      {
      TDataSet* data = container->findData(path);
      if (data != NULL && data->FieldDefs->Count > 0)
         {
         for (int f = 0; f != data->FieldDefs->Count; f++)
            {
            if (f > 0)
               strcat(fieldNames, "\t");
            strcat(fieldNames, data->FieldDefs->Items[f]->Name.c_str());
            }
         }
      }
   }

extern "C" void _export __stdcall GetREMSExperimentNames(DataContainer* container,
                                                         const char* path,
                                                         char* experimentNames)
   {
   strcpy(experimentNames, "");

   if (container != NULL)
      {
      DataContainer* REMSContainer = dynamic_cast<DataContainer*> (container->findContainer(path));
      if (REMSContainer != NULL)
         {
         REMS* REMSProcessor = dynamic_cast<REMS*> (REMSContainer->getProcessor());
         if (REMSProcessor != NULL)
            {
            vector<string> names = REMSProcessor->getExperimentNames();
            for (unsigned e = 0; e != names.size(); e++)
               {
               if (e > 0)
                  strcat(experimentNames, "\t");
               strcat(experimentNames, names[e].c_str());
               }
            }
         }
      }
   }

extern "C" void _export __stdcall GetREMSTreatmentNames(DataContainer* container,
                                                        const char* path,
                                                        char* treatmentNames)
   {
   strcpy(treatmentNames, "");

   if (container != NULL)
      {
      DataContainer* REMSContainer = dynamic_cast<DataContainer*> (container->findContainer(path));
      if (REMSContainer != NULL)
         {
         REMS* REMSProcessor = dynamic_cast<REMS*> (REMSContainer->getProcessor());
         if (REMSProcessor != NULL)
            {
            vector<string> names = REMSProcessor->getTreatmentNames();
            for (unsigned e = 0; e != names.size(); e++)
               {
               if (e > 0)
                  strcat(treatmentNames, "\t");
               strcat(treatmentNames, names[e].c_str());
               }
            }
         }
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
extern "C" HWND _export __stdcall GetHandleOfForm(TForm* form)
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

