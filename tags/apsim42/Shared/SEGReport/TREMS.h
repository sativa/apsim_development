//---------------------------------------------------------------------------

#ifndef TREMSH
#define TREMSH
#include "TSEGTable.h"
#include <stdexcept>
#include <ADOdb.hpp>
//---------------------------------------------------------------------------
// derived from TSEGTable, this class reads in experimental data from
// the REMS database.
//---------------------------------------------------------------------------
class TREMS : public TSEGTable
   {
   private:
      AnsiString reportDirectory;
      AnsiString mdbFilename;
      AnsiString experimentName;
      AnsiString treatmentName;
      AnsiString datasourceName;
      TStringList* experimentNames;
      TStringList* treatmentNames;
      TADOQuery* query;
      std::vector<int> experimentIDs;
      std::vector<int> treatmentIDs;

      void __fastcall setFilename(AnsiString filename);
      void __fastcall setExperimentName(AnsiString experimentName);
      void __fastcall setTreatmentName(AnsiString treatmentName);
      void __fastcall setDatasource(AnsiString dataSource);
      TStrings* __fastcall getExperimentNames(void);
      TStrings* __fastcall getTreatmentNames(void);

      virtual void __fastcall Loaded(void);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TREMS(TComponent* owner);
      __fastcall ~TREMS(void);

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property AnsiString filename = {read=mdbFilename, write=setFilename};
      __property AnsiString experiment = {read=experimentName, write=setExperimentName};
      __property AnsiString treatment = {read=treatmentName, write=setTreatmentName};
      __property AnsiString datasource = {read=datasourceName, write=setDatasource};
      __property TStrings* experiments = {read=getExperimentNames};
      __property TStrings* treatments = {read=getTreatmentNames};
   };
#endif
