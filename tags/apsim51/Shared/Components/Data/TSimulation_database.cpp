//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSimulation_database.h"
#include <fstream.h>
#include <vcl\oleauto.hpp>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\date_class.h>
#include <general\path.h>
#include <vector>
#include <set>
using namespace std;
#pragma package(smart_init)
#pragma resource "*.res"

static const char* DATA_TABLE_NAME = "Data";
static const char* SIMULATION_ID_FIELD_NAME = "SimulationID";
static const char* DICTIONARY_SECTION = "Dictionary";
static const char* TITLE_DELIMITER = ";";
//---------------------------------------------------------------------------
namespace Tsimulation_database
   {
   void __fastcall PACKAGE Register()
      {
      TComponentClass classes[1] = {__classid(TSimulation_database)};
      RegisterComponents("APSRU", classes, 0);
      }
   }
//---------------------------------------------------------------------------
__fastcall TSimulation_database::TSimulation_database(TComponent* Owner)
   : TADOConnection(Owner)
   {
   FSimulation_names = new TStringList;
   Index_table = NULL;
   Data_table = NULL;
   FCheckIfExists = true;

   // setup the dictionary.
   Path Dictionary_path (Application->ExeName.c_str());
   Dictionary_path.Set_name ("dictionary.ini");
   Dictionary_exists = (Dictionary_path.Exists());
   Dictionary.setFileName (Dictionary_path.Get_path());
   }
//---------------------------------------------------------------------------
__fastcall TSimulation_database::~TSimulation_database()
   {
   delete FSimulation_names;
   Clear();
   }
//---------------------------------------------------------------------------
void TSimulation_database::Clear(void)
   {
   delete Index_table;         // DAH & DPH 31/7/2000
   Index_table = NULL;
   delete Data_table;
   Data_table = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    if index table is null then create it.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    DAH & DPH 31/7/2000 - Pulled the initialization of Index_table
//                          out of the try statement, and set it's parent
//                          object to NULL because of a bug in TTable code.
//                          We now must delete Index_table ourselves (see
//                          the destructor).
//    DPH 11/7/2001 Fixed batch import D436
// ------------------------------------------------------------------
void TSimulation_database::Create_index_table(void)
   {
   if (Index_table == NULL)
      {
      Index_table = new TADOTable(this);
      try
         {
         Index_table->Connection = this;
         Index_table->TableName = "[Index]";
         Index_table->CursorLocation = clUseServer;
         Index_table->Active = true;
         }
      catch (...)
         {
         TADOQuery* createTableQuery = new TADOQuery(NULL);
         createTableQuery->SQL->Text =
            "CREATE TABLE [Index](SimulationID AutoIncrement TMyConstraint PRIMARY KEY,"
                                                 "Name text(255))";
         createTableQuery->Connection = this;
         createTableQuery->ExecSQL();
         delete createTableQuery;
         }
      }
   Index_table->Active = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    add the fields to the database.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    DPH 2/2/2001  renamed from Add_fields_to_database
//    DPH 11/7/2001 Fixed batch import D436

// ------------------------------------------------------------------
void TSimulation_database::Create_data_table (istream& In_stream,
                                              list<string>& Field_names)
   {
   // save current position in instream.
   unsigned long Saved_pos = In_stream.tellg();

   // get first record so that we can work out data types for each field.
   string Line;
   list<string> Field_values;
   getline (In_stream, Line);
   Split_string (Line, " ", Field_values);

   if (Data_table == NULL)
      {
      try
         {
         Data_table = new TADOTable(NULL);
         Data_table->Connection = this;
         Data_table->TableName = "Data";
         Data_table->CursorLocation = clUseServer;
         Data_table->Active = true;
         }
      catch (...)
         {
         string createSt =
            "CREATE TABLE [Data]([SimulationID] INTEGER";
         createSt += "," + Create_SQL_field_string(Field_names, Field_values) + ")";

         // go create query to create data table.
         TADOQuery* createTableQuery = new TADOQuery(NULL);
         createTableQuery->SQL->Text = createSt.c_str();
         createTableQuery->Connection = this;
         createTableQuery->ExecSQL();
         delete createTableQuery;
         }
      }
   else
      {
      Data_table->Active = true;
      vector<string> dataTableFieldNames;
      getDBFieldNames(Data_table, dataTableFieldNames);
      Data_table->Active = false;
      list<string> fieldNamesToAdd;
      list<string> fieldValuesToAdd;
      list<string>::iterator n = Field_names.begin();
      list<string>::iterator v = Field_values.begin();
      while (n != Field_names.end() && v != Field_values.end())
         {
         if (find(dataTableFieldNames.begin(), dataTableFieldNames.end(),
                  *n) == dataTableFieldNames.end())
            {
            fieldNamesToAdd.push_back(*n);
            fieldValuesToAdd.push_back(*v);
            }

         n++;
         v++;
         }

      if (fieldNamesToAdd.size() > 0)
         {
         string createSt =
            "ALTER TABLE [Data] Add Column ";
         createSt += Create_SQL_field_string(fieldNamesToAdd, fieldValuesToAdd);

         // go create query to create data table.
         TADOQuery* createTableQuery = new TADOQuery(NULL);
         createTableQuery->SQL->Text = createSt.c_str();
         createTableQuery->Connection = this;
         createTableQuery->ExecSQL();
         delete createTableQuery;
         copy(fieldNamesToAdd.begin(), fieldNamesToAdd.end(),
              inserter(dataTableFieldNames, dataTableFieldNames.begin()));
         }
      }
   Data_table->Active = true;

   // restore saved position.
   In_stream.seekg(Saved_pos);
   In_stream.clear();
   }


// ------------------------------------------------------------------
//  Short description:
//    create and return an SQL string for specified the fields in a database.
//    Used as part of a CREATE TABLE or ALTER TABLE SQL statement.

//  Changes:
//    DPH 3/12/2001
// ------------------------------------------------------------------
string TSimulation_database::Create_SQL_field_string (list<string>& Field_names,
                                                      list<string>& Field_values)
   {
   string createSt;


   // loop through all fields and add to our table.
   list<string>::iterator i = Field_names.begin();
   list<string>::iterator j = Field_values.begin();
   while (i != Field_names.end() && j != Field_values.end())
      {
      replaceAll(*i, ".", "");
      string msg = *i + "=" + *j;
      if (createSt != "")
         createSt += ",";
      createSt += "[" + *i + "]";
      if (Is_numerical((*j).c_str()))
         createSt += " DOUBLE";

      else if ( j->find("/") != string::npos)
         createSt += " DATE";

      else
         createSt += " TEXT(20)";

      i++;
      j++;
      }


   return createSt;
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the contents of the file.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void TSimulation_database::Read_file (istream& In_stream)
   {
   // make sure index is valid.
   Create_index_table();

   // read in field names and title.
   list<string> Field_names;
   string Title;
   Read_field_names (In_stream, Field_names, Title);

   // get the simulation id to use in the data table.
   int Simulation_id = Locate_simulation_in_index(Title.c_str());

   // only continue if we don't already have this data in the database.
   if (Simulation_id < 0)
      {
      // add this simulation to the index.
      int Simulation_id = Add_simulation_to_index (Title.c_str());

      // go create a ttable object for the data table.
      Create_data_table(In_stream, Field_names);

      // import all records.
      Read_and_store_records (In_stream, Data_table, Simulation_id, Field_names);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the field names from the stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void TSimulation_database::Read_field_names (istream& In_stream,
                                             list<string>& Field_names,
                                             string& Title)
   {
   // loop through all lines looking for heading line.
   string Line, Previous_line;
   bool Found = false;
   while (!In_stream.eof() && !Found)
      {
      // read next line
      getline (In_stream, Line);

      // look for title= on line
      string Key_value;
      Key_value = getKeyValue(Line, "title");
      if (Key_value != "")
         {
         Title = Key_value.c_str();
         Title_dictionary_substitution(Title);
         }

      // get first non blank character on line.
      char First_char = ' ';
      unsigned int Pos_first_char = Line.find_first_not_of (" ");
      if (Pos_first_char != string::npos)
         First_char = Line[Pos_first_char];

      // is this the units line?  If so then previous line must be headings.
      if (First_char == '(')
         {
         Found = true;
         Split_string (Previous_line, " ", Field_names);
         Dictionary_substitution (Field_names);

         if (!Dictionary_exists)
            {
            vector<string> units;
            Split_string (Line, " ", units);

            vector<string> temp;
            copy(Field_names.begin(), Field_names.end(), back_inserter(temp));

            addUnitsToFieldNames(units, temp);
            Field_names.erase(Field_names.begin(), Field_names.end());
            copy(temp.begin(), temp.end(), back_inserter(Field_names));
            }

         // check for duplicate fields
         vector<string> temp;
         copy(Field_names.begin(), Field_names.end(), back_inserter(temp));
         sort(temp.begin(), temp.end());
         if (unique(temp.begin(), temp.end()) != temp.end())
            ShowMessage("Non-unique column names encountered.  "
                        "An APSIM output file must not have two columns with "
                        "the same name.");
         }

      Previous_line = Line;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    read in and store all records

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void TSimulation_database::Read_and_store_records (istream& In_stream,
                                                   TDataSet* Table,
                                                   int Simulation_id,
                                                   list<string>& Field_names)
   {
   ShortDateFormat = "d/mm/yyyy";

   // loop through all records on the input stream.
   list<string> Field_values;
   string Line;
   while (!In_stream.eof())
      {
      // read next line and split it.
      getline(In_stream, Line);
      if (Line.length() > 0)
         {
         Split_string (Line, " ", Field_values);

         // append a new record and store the simulation id.
         Table->Append();
         Table->Fields->Fields[0]->AsInteger = Simulation_id;

         // loop through each value and store.
         for (list<string>::iterator i = Field_values.begin(), j=Field_names.begin();
                                     i != Field_values.end();
                                     i++, j++)
            {
            TField* field = Table->FindField( (*j).c_str() );
            if (field != NULL)
               {
               if (field->DataType == ftDateTime)
                  field->AsDateTime = TDateTime((*i).c_str(), TDateTime::Date);
               else if (field->DataType == ftFloat)
                  field->AsFloat = atof((*i).c_str());
               else
                  field->AsString = (*i).c_str();
               }
            }
         Table->Post();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return the record number of the specified simulation in the index.
//    returns -1 if not found.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int TSimulation_database::Locate_simulation_in_index (const char* Simulation_name)
   {
   if (FCheckIfExists &&
       Index_table->Locate("name", Simulation_name, TLocateOptions()))
      return Index_table->FieldValues[SIMULATION_ID_FIELD_NAME];
   else
      return -1;
   }

// ------------------------------------------------------------------
//  Short description:
//    add the specified simulation to the index and return it's ID.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int TSimulation_database::Add_simulation_to_index (const char* Simulation_name)
   {
   Index_table->Append();
   Index_table->FieldValues["Name"] = Simulation_name;
   Index_table->Post();
   return Index_table->FieldValues[SIMULATION_ID_FIELD_NAME];
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh this object.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void TSimulation_database::Refresh (void)
   {
   if (File_name.Length() > 0)
      {
      // make sure index is valid.
      Create_index_table();

      // clear current list of table names.
      FSimulation_names->Clear();

      if (Index_table->Active)
         {
         Index_table->Filtered = false;
         Index_table->First();
         while (!Index_table->Eof)
            {
            if (String(Index_table->FieldValues["Name"]) != "Index")
               FSimulation_names->Add (String(Index_table->FieldValues["Name"]));
            Index_table->Next();
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the contents of an APSIM file.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 20/3/01 added NotifyImportProgress C359

// ------------------------------------------------------------------
void __fastcall TSimulation_database::Import_APSIM_files (TStrings* Filenames,
                                                          TNotifyImportProgress NotifyEvent)
   {
   for (int i = 0; i < Filenames->Count; i++)
      {
      if (NotifyEvent != NULL)
         NotifyEvent(this, Filenames->Strings[i], i+1, Filenames->Count);

      ifstream In_stream (Filenames->Strings[i].c_str());
      if (In_stream)
         Read_file(In_stream);
      else
         {
         string msg ("Cannot find file : ");
         msg += Filenames->Strings[i].c_str();
         Application->MessageBox((char*) msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   Refresh();
   if (NotifyEvent != NULL)
      NotifyEvent(this, "", Filenames->Count, Filenames->Count);
   }

// ------------------------------------------------------------------
//  Short description:
//    create a new simulation database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void TSimulation_database::New (const char* Filename)
   {
/*   static const char* dbLangGeneral = ";LANGID=0x0409;CP=1252;COUNTRY=0";

//   static const int dbVersion10 =  1;
//   static const int dbEncrypt   = 2 ;
//   static const int dbDecrypt   = 4 ;         // only used when packing
//   static const int dbVersion11 = 8 ;
   static const int dbVersion20 = 16;
//   static const int dbVersion30 = 32;         // choose this for Access 95 database

   // go create the database.
   Variant DBEngine = Variant::CreateObject("DAO.DBEngine");
   Variant Workspace = DBEngine.OlePropertyGet("Workspaces", 0);
   Procedure CreateDatabase ("CreateDatabase");
   Workspace.Exec(CreateDatabase << Filename << dbLangGeneral << dbVersion20);
   Set_database_filename (Filename);
*/   }

// ------------------------------------------------------------------
//  Short description:
//    set the database to point to a new filename.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void __fastcall TSimulation_database::Set_database_filename (AnsiString Filename)
   {
   Filename += ".";

   // create a fully qualified database pathname
   Path Database_file;
   Database_file.Set_path(Filename.c_str());
   if (Database_file.Get_directory().length() == 0)
      {
      Database_file = Path::getCurrentFolder();
      Database_file.Set_path(Filename.c_str());
      }
   Database_file.Set_extension(".mdb");

   // point this object to the new file.
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=####;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "####", Database_file.Get_path().c_str());
   Connected = false;
   ConnectionString = connectionString.c_str();
   Connected = true;

   Create_index_table();
   delete Data_table;
   Data_table = NULL;
   Refresh();
   }

// ------------------------------------------------------------------
//  Short description:
//    perform a dictionary substitution on the list of words passed in.

//  Notes:

//  Changes:
//    DPH 21/7/98

// ------------------------------------------------------------------
void TSimulation_database::Dictionary_substitution (list<string>& Words)
   {
   if (Dictionary_exists)
      {
      list<string> Replacement_words;
      for (list<string>::iterator i = Words.begin();
                                  i != Words.end();
                                  i++)
         {
         string Replacement_word;
         Dictionary.read(DICTIONARY_SECTION, (*i).c_str(), Replacement_word);
         if (Replacement_word.length() == 0)
            Replacement_word = *i;
         Replacement_words.push_back (Replacement_word);
         }
      Words = Replacement_words;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    perform a dictionary substitution on the specified title.

//  Notes:

//  Changes:
//    DPH 21/7/98

// ------------------------------------------------------------------
void TSimulation_database::Title_dictionary_substitution(string& Title)
   {
   if (Dictionary_exists && Title.find(";") != string::npos)
      {
      list<string> Title_parts, Replacement_parts;
      string Replacement_part;
      Split_string (Title, TITLE_DELIMITER, Title_parts);
      for (list<string>::iterator i = Title_parts.begin();
                                  i != Title_parts.end();
                                  i++)
         {
         string Key_name, Value;
         getKeyNameAndValue(*i, Key_name, Value);
         list<string> Words;
         Words.push_back (Key_name);
         Words.push_back (Value);
         Dictionary_substitution (Words);
         Build_string (Words, "=", Replacement_part);
         Replacement_parts.push_back (Replacement_part);
         }
      Build_string (Replacement_parts, TITLE_DELIMITER, Title);
      }
   }

// ------------------------------------------------------------------
// Add the specified units to the specified field names.
// ------------------------------------------------------------------
void TSimulation_database::addUnitsToFieldNames(vector<string>& units,
                                                vector<string>& Field_names)
   {
   for (unsigned  fieldI = 0; fieldI < Field_names.size(); fieldI++)
      {
      if (units[fieldI] != "()")
         {
         string unit = units[fieldI];
         Replace_all(unit, "/", " per ");
         Field_names[fieldI] += unit;
         }
      }
   }

