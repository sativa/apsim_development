//---------------------------------------------------------------------------
#ifndef TSEGTableH
#define TSEGTableH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include <kbmMemTable.hpp>
#include <stdexcept.h>
#include <vector>
#include <string>
#include <set>
#include <map>
typedef std::set<std::string> Errors;
// ------------------------------------------------------------------
// base class for all our memory tables.  It is derived from a
// memory table to inherit support for storing records in memory.
// It provides aditional functionality for managing blocks of
// records ('series') that are identified by the values in a
// series field.
// e.g.   Series   Year Day  Yield
//         File1   1988   1  268.1
//         File1   1989   1  452.0
//         File1   1990   1    0.1
//         File2   1988   1   45.2
//         File2   1989   1 1275.0
//         File2   1990   1    0.1
// Here there are 2 'series' called 'File1' and 'File2'.

// ------------------------------------------------------------------
class PACKAGE TSEGTable : public TkbmMemTable
   {
   private:
      TSEGTable* sourceDataset;
      std::vector<std::string> seriesNames;
      std::vector<std::string>::iterator currentSeriesI;
      TStringList* subscriptionComponents;

      typedef std::vector<TDataSetNotifyEvent> SubscriptionEvents;
      SubscriptionEvents subscriptionEvents;

      void __fastcall setSourceDataset(TSEGTable* sourceDataset);
      void __fastcall beforeOpen(TDataSet* dataset);
      void __fastcall afterOpen(TDataSet* dataset);
      void __fastcall setSubComponentNames(TStringList* compNames);
      void fixupSubReferences(void);

   protected:
      virtual void createFields(void) throw(std::runtime_error) {}
      virtual void storeRecords(void) throw(std::runtime_error) {}
      virtual void __fastcall Loaded(void);

   public:
      __fastcall TSEGTable(TComponent* Owner);
      __fastcall ~TSEGTable(void);

      // return the year field name from the dataset.
      std::string getYearFieldName(void) const throw(std::runtime_error);

      // return a list of all unique series names.
      void getSeriesNames(std::vector<std::string>& seriesNames);

      // return the series name for the current record.
      std::string getSeriesName(void);

      // set the series name for the current record.
      void setSeriesName(const std::string& seriesName);

      // add a factor to the series name for the current record.
      void addFactorToSeriesName(const std::string& factorName);

      // The next 3 series functions limit the accessible records to a
      // particular series only.  The caller still uses the
      // First(), Next() and Eof methods but they only get access
      // to a particular series of records.
      bool firstSeries(void);
      bool nextSeries(void);
      void cancelSeries(void);
      void refresh (void);

      static Errors& errors(void)
         {
         static Errors errs;
         return errs;
         }
      // Called by SEGReport to give components a chance to know the current
      // report directory.  Used by ApsimFileReader to use relative paths.
      virtual void setReportDirectory(AnsiString reportDir) { };

      void addDataChangeSubscription(AnsiString name);
      void removeDataChangeSubscription(AnsiString name);


   __published:
      __property TSEGTable* source = {read=sourceDataset, write=setSourceDataset};
      __property TStringList* subscriptionComponentNames = {read=subscriptionComponents, write=setSubComponentNames};

      void __fastcall onSourceDataChanged(TDataSet* dataset);
   };
//---------------------------------------------------------------------------
#endif
