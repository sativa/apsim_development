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
      std::vector<std::string> groupByFilters;
      std::vector<std::string>::iterator groupByFiltersI;
      TStringList* subscriptionComponents;
      bool addToToolbar;
      bool AddToWizard;
      AnsiString sortFieldNames;
      AnsiString groupByFieldNames;
      bool inForceRefresh;

      typedef std::vector<TDataSetNotifyEvent> SubscriptionEvents;
      SubscriptionEvents subscriptionEvents;

      void __fastcall setSourceDataset(TSEGTable* sourceDataset);
      void __fastcall setSubComponentNames(TStringList* compNames);
      void __fastcall setSortFieldNames(AnsiString sortFieldNames);

      void fixupSubReferences(void);
      std::string calcGroupByFilter(void);
      void calcGroupByFilters(void);

   protected:
      virtual bool createFields(void) throw(std::runtime_error) {return false;}
      virtual void storeRecords(void) throw(std::runtime_error) { };
      virtual void __fastcall Loaded(void);

      void addGroupByFieldDefsFromSource(void);
      void addGroupByValuesFromSource(void);


   public:
      __fastcall TSEGTable(TComponent* Owner);
      __fastcall ~TSEGTable(void);

      // return the year field name from the dataset.
      std::string getYearFieldName(void) const throw(std::runtime_error);

      // The next 3 series functions limit the accessible records to a
      // particular series only.  The caller still uses the
      // First(), Next() and Eof methods but they only get access
      // to a particular series of records.
      bool firstSeries(void);
      bool nextSeries(void);
      void cancelSeries(void);

      void refresh (void);
      void refreshLinkedComponents();
      void forceRefresh(bool displayError = true);

      static AnsiString errorMessage;

      // Called by SEGReport to give components a chance to know the current
      // report directory.  Used by ApsimFileReader to use relative paths.
      virtual void setReportDirectory(AnsiString reportDir) { };

      void addDataChangeSubscription(AnsiString name);
      void removeDataChangeSubscription(AnsiString name);

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue) { };

   __published:
      __property TSEGTable* source = {read=sourceDataset, write=setSourceDataset};
      __property TStringList* subscriptionComponentNames = {read=subscriptionComponents, write=setSubComponentNames};
      __property bool addToToolBar = {read=addToToolbar, write=addToToolbar};
      __property AnsiString sortFields = {read=sortFieldNames, write=setSortFieldNames};
      __property AnsiString groupByFields = {read=groupByFieldNames, write=groupByFieldNames};
      __property bool addToWizard = {read=AddToWizard, write=AddToWizard};

      void __fastcall onSourceDataChanged(TDataSet* dataset);
   };
//---------------------------------------------------------------------------
#endif
