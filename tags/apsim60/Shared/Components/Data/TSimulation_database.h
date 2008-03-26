//---------------------------------------------------------------------------
#ifndef TSimulation_databaseH
#define TSimulation_databaseH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include <vcl\DB.hpp>
#include <vcl\dbtables.hpp>
#include <ADODB.hpp>

#include <string>
#include <list>
#include <vector>
using std::string;
using std::list;
#include <general\inifile.h>

typedef void __fastcall (__closure *TNotifyImportProgress)
   (System::TObject* Sender, AnsiString& CurrentFileName,
    int CurrentFileNumber, int TotalFileNumber);
// ------------------------------------------------------------------
//  Short description:
//      This database class encapsulates a special simulation database.
//      It provides methods for importing simulations into the database
//      as well as routines to extract the data.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DPH 2/2/2001 converted to ADO

// ------------------------------------------------------------------
class PACKAGE TSimulation_database : public TADOConnection
{
private:
   IniFile Dictionary;
   bool Dictionary_exists;
   bool FCheckIfExists;

   void Dictionary_substitution (list<string>& Words);
   void Title_dictionary_substitution(string& Title);
   std::string Create_SQL_field_string (std::list<std::string>& Field_names,
                                        std::list<std::string>& Field_values);
   void addUnitsToFieldNames(std::vector<std::string>& units,
                             std::vector<std::string>& Field_names);

protected:
   TADOTable* Index_table;
   TADOTable* Data_table;
   TStringList* FSimulation_names;
   AnsiString FFile_name;

   void Create_index_table(void);
   void Read_file (std::istream& In_stream);
   void Read_field_names (std::istream& In_stream, list<string>& Field_names, string& Title);
   void Create_data_table (std::istream& In_stream, list<string>& Field_names);
   void Read_and_store_records (std::istream& In_stream, TDataSet* Table, int Simulation_id, list<string>& Field_names);
   void Import_APSIM_file (const char* filename);
   void Clear(void);

   int Locate_simulation_in_index (const char* Simulation_name);
   int Add_simulation_to_index (const char* Simulation_name);

   TStringList* __fastcall Get_simulation_names (void);
   void __fastcall Set_simulation_names (TStringList* Simulation_names);
   void __fastcall Set_database_filename (AnsiString File_name);

public:
   __fastcall TSimulation_database(TComponent* Owner);
   __fastcall ~TSimulation_database();

   void New (const char* Filename);
   void Refresh (void);

__published:
   __property AnsiString File_name = {read=FFile_name, write=Set_database_filename};
   __property TStringList* Simulation_names = {read=FSimulation_names};
   __property bool CheckIfExists = {read=FCheckIfExists, write=FCheckIfExists};

   void __fastcall Import_APSIM_files (TStrings* File_names, TNotifyImportProgress NotifyEvent);
   };
//---------------------------------------------------------------------------
#endif
