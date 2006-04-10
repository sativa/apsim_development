//---------------------------------------------------------------------------
#ifndef TDatasource_joinH
#define TDatasource_joinH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "MemTable.hpp"
#include <Db.hpp>
#include <DBTables.hpp>
#include <general\mystring.h>
#include <general\myvector.h>
#include <set>
#include "TDatasource_selection.h"
//---------------------------------------------------------------------------
class PACKAGE TDatasource_join : public TMemoryTable
{
private:
   class Field
      {
      public:
         string Name;
         enum TFieldType Type;
         Field (void) {};
         Field (const char* name, enum TFieldType type)
            : Name (name), Type(type) {}
         bool operator== (const Field& rhs) const
            {return (Name == rhs.Name);}
         bool operator< (const Field& rhs) const
            {return (Name < rhs.Name);}
      };

   typedef std::set<Field, std::less<Field> > Field_set;

   TDatasource_selection* FDatasource_selection;

   void __fastcall DoBeforeOpen(void);
   void __fastcall DoAfterOpen(void);

   void Calc_and_store_fields();
   void Calc_and_store_records();
   void Get_common_fields (vector<TDataSet*> Datasets, Field_set& Common_fields);

protected:
public:
  __fastcall TDatasource_join(TComponent* Owner);
__published:
   __property TDatasource_selection* Datasource_selection = {read=FDatasource_selection, write=FDatasource_selection};
};
//---------------------------------------------------------------------------
#endif
