//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TDatasource_join.h"
#pragma link "MemTable"
#pragma package(smart_init)
#pragma resource "*.res"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TDatasource_join *)
{
  new TDatasource_join(NULL);
}
//---------------------------------------------------------------------------
namespace Tdatasource_join
{
  void __fastcall PACKAGE Register()
  {
     TComponentClass classes[1] = {__classid(TDatasource_join)};
     RegisterComponents("APSRU", classes, 0);
  }
}
//---------------------------------------------------------------------------
__fastcall TDatasource_join::TDatasource_join(TComponent* Owner)
  : TMemoryTable(Owner)
{
}
// ------------------------------------------------------------------
//  Short description:
//      the table is just about to be open.  Let derived class add all necessary
//      fields to table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TDatasource_join::DoBeforeOpen(void)
   {
   Calc_and_store_fields();
   }
// ------------------------------------------------------------------
//  Short description:
//      the table is now open.  Let derived class add all necessary
//      records to table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TDatasource_join::DoAfterOpen(void)
   {
   Calc_and_store_records();
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all field names in memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TDatasource_join::Calc_and_store_fields()
   {
   // clear all existing fields.
   FieldDefs->Clear();

   // loop through all selected datasets.
   if (FDatasource_selection != NULL)
      {
      TMultiStringList* Dataset_names = FDatasource_selection->Datasets;
      vector<TDataSet*> Datasets;
      for (int i = 0; i < Dataset_names->Items->Count; i++)
         {
         // get datasource name.
         AnsiString Data_source_name = Dataset_names->Items->Strings[i].c_str();

         // get source dataset.
         TDataSet* dataset = FDatasource_selection->Get_dataset(Data_source_name.c_str());

         // add dataset to list.
         Datasets.push_back (dataset);
         }

      // get all common fields across all datasets.
      Field_set Common_fields;
      Get_common_fields (Datasets, Common_fields);

      // loop through all common fields and create a field in
      // our memory table
      for (Field_set::iterator j = Common_fields.begin();
                               j != Common_fields.end();
                               j++)
         {
         // add field to our memory table
         TFieldDef* field = FieldDefs->AddFieldDef();
         field->Name = (*j).Name.c_str();
         field->DataType = (*j).Type;
         }

      // add a descriptor column.
      TFieldDef* field = FieldDefs->AddFieldDef();
      field->Name = "Descriptor";
      field->DataType = ftString;
      field->Size=100;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    return a list of field names common to all open tables

//  Notes:

//  Changes:
//    DPH 19/3/98

// ------------------------------------------------------------------
void TDatasource_join::Get_common_fields (vector<TDataSet*> Datasets,
                                          Field_set& Common_fields)
   {
   // loop through each table.
   bool Is_first_dataset = true;
   for (vector<TDataSet*>::iterator i = Datasets.begin();
                                    i != Datasets.end();
                                    i++)
      {
      Field_set This_datasets_fields;
      TDataSet* This_dataset = *i;
      for (int i = 0; i < This_dataset->FieldCount; i++)
         {
         Field field(This_dataset->Fields[i]->FieldName.c_str(),
                     This_dataset->Fields[i]->DataType);
         if (Is_first_dataset)
            Common_fields.insert (field);
         else
            This_datasets_fields.insert (field);
         }
      if (!Is_first_dataset)
         std::set_intersection (Common_fields.begin(), Common_fields.end(),
                                This_datasets_fields.begin(), This_datasets_fields.end(),
                                std::inserter(Common_fields, Common_fields.begin()));
      Is_first_dataset = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in our memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TDatasource_join::Calc_and_store_records()
   {
   if (FDatasource_selection != NULL)
      {
      TMultiStringList* Dataset_names = FDatasource_selection->Datasets;
      vector<TDataSet*> Datasets;
      for (int i = 0; i < Dataset_names->Items->Count; i++)
         {
         // get datasource name.
         AnsiString Data_source_name = Dataset_names->Items->Strings[i].c_str();

         // get source dataset.
         TDataSet* dataset = FDatasource_selection->Get_dataset(Data_source_name.c_str());

         // get a descriptor for this dataset.
         AnsiString descriptor = FDatasource_selection->Get_dataset_descriptor(Data_source_name.c_str());

         // move records from source dataset to our memory table.
         BatchMove (dataset, batAppend, 0);

         // fill in all descriptors.
         int Descriptor_field = FieldCount - 1;
         First();
         while (!Eof)
            {
            if (Fields[Descriptor_field]->AsString.Length() == 0)
               {
               Edit();
               Fields[Descriptor_field]->AsString = descriptor;
               Post();
               }
            Next();
            }
         }
      }
   }

