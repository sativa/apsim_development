//---------------------------------------------------------------------------
#ifndef TAPSTable_datasetH
#define TAPSTable_datasetH

#include "TAPSTable.h"
// ------------------------------------------------------------------
//  Short description:
//      class for bridging the gap between a TDataSet and our APSTable

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TDataSet_2_TAPSTable : public TAPSTable
   {
   private:
      TDataSet* Fdataset;

      void Read_all_records (void);

   protected:
      virtual void Get_source_field_names (std::vector<std::string>& field_names);

   public:
      __fastcall TDataSet_2_TAPSTable(TComponent* Owner);
      __fastcall ~TDataSet_2_TAPSTable();

      virtual void refresh (void);

   __published:
      __property TDataSet* Dataset = {read=Fdataset, write=Fdataset};

   };

#endif
