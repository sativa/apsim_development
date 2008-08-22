//---------------------------------------------------------------------------
#ifndef TAPSTableH
#define TAPSTableH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include "TAPSRecord.h"
#include <components\general\TMultiStringList.h>
class TAPSTable_form;

#define CHART_SETTINGS_KEY string("Outlook chart settings")

// ------------------------------------------------------------------
//  Short description:
//      property editor for a TAPSTable class.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
/*class PACKAGE TAPSTable_editor : public TComponentEditor
   {
   public:
      __fastcall TAPSTable_editor(TComponent* AComponent, _di_IFormDesigner* ADesigner);
      virtual void __fastcall ExecuteVerb(int Index);
      virtual int  __fastcall GetVerbCount(void);
      virtual AnsiString __fastcall GetVerb(int Index);
   };
*/
// ------------------------------------------------------------------
//  Short description:
//      base class for all our memory tables.  It includes methods for
//      retrieving data, storing data, pivoting data sorting the data.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 31/10/00: D-380 Added Get_year_field_name() to public methods.

// ------------------------------------------------------------------
class PACKAGE TAPSTable : public TComponent
   {
   private:
      TAPSTable* FSourceDataset;

      std::vector<TAPSRecord> allRecords;
      std::vector<TAPSRecord>::iterator startBlockIterator;
      std::vector<TAPSRecord>::iterator endBlockIterator;
      std::vector<std::string> fieldNames;
      std::vector<std::string> pivotFieldNames;
      std::vector<std::string> invisibleDataBlockNames;

      void findStartBlockIterator(void);
      void findEndBlockIterator(void);
      void addRecords(unsigned int numRecords);
      void checkInvisibleDataBlocks(void);

   protected:
      virtual void calcAndStoreRecords(void) {}
      virtual TAPSTable_form* createPropertiesForm();
      virtual void setupPropertiesForm(TAPSTable_form* form);
      virtual void load(void);   // called on an edit.
      virtual void save(void);

   public:
      __fastcall TAPSTable(TComponent* Owner);
      virtual __fastcall ~TAPSTable();

      // refresh the in memory data.
      virtual void refresh (void);

      // let user edit the properties.
      virtual bool edit(void);

      // data retrieval methods
      bool first(void);
      bool next (void);
      std::vector<TAPSRecord>::const_iterator begin() const {return startBlockIterator;}
      std::vector<TAPSRecord>::const_iterator end() const {return endBlockIterator;}

      // field methods
      void clearFields(void);
      void addField(const std::string& fieldName);
      void getFieldNames(std::vector<std::string>& fieldNames);
      void getPivotNames(std::vector<std::string>& pivotNames);
      void getFieldNamesMinusPivots(std::vector<std::string>& fieldNames);
      void getFielddefs(TFieldDefs* fieldDefs);
      std::string getYearFieldName(void) const;
      void copyFieldNamesFrom(const TAPSTable& from);

      // pivot methods.
      void clearPivots(void);
      void markFieldAsAPivot(const std::string& pivotFieldName);
      void copyPivotsFrom(const TAPSTable& from);
      void copyAndFillPivots(const TAPSTable& from);
      
      // sorting methods.
      void sortRecords(const std::string& fieldName);

      // data block name methods for current data block.
      std::string getDataBlockName(void) const;
      void setDataBlockName(const std::string& dataBlockName);
      void getDataBlockNames(std::vector<std::string>& dataBlockNames);
      void getAllDataBlockNames(std::vector<std::string>& dataBlockNames);
      bool isDataBlockVisible(const std::string& dataBlockName);
      void markDataBlockVisible(const std::string& dataBlockName, bool isVisible);

      // conversion routines for current data block.
      void fieldAsStringArray(const std::string& fieldName,
                              std::vector<std::string>& arrayStrings);
      void fieldAsNumericArray(const std::string& fieldName,
                               std::vector<double>& arrayNumbers);
      void storeNumericArray(const std::string& fieldName,
                             const std::vector<double>& arrayNumbers);
      void storeStringArray(const std::string& fieldName,
                            const std::vector<std::string>& arrayStrings);

      // data store methods.
      void beginStoringData(void);
      void clearRecords(void);
      void storeData(const std::vector<TAPSRecord>& values);
      void storeData(const TAPSTable& from);
      void storeRecord(const TAPSRecord& record);
      void endStoringData(void);

      __property TAPSTable* sourceDataset = {read=FSourceDataset, write=FSourceDataset};

   };
//---------------------------------------------------------------------------
#endif
