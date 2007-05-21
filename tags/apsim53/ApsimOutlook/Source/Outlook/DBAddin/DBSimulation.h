//---------------------------------------------------------------------------
#ifndef DBSimulationH
#define DBSimulationH

#include <string>
#include <vector>
#include <TAPSRecord.h>
#include <DbTables.hpp>
#include <ADODB.hpp>
#include <scenario.h>
#define MAX_DATABASE_FILENAME_LENGTH 200

// ------------------------------------------------------------------
//  Short description:
//    This class encapsulates a single simulation in a database.  A
//    simulation has Factors and it has Values.

//  Notes:
//      A Simulation 'Title' should look like this:
//         site=dalby;soil=120mm;crop=sorghum
//      This gives factors of 'site',  'soil',  'crop'
//              and values of 'dalby', '120mm', 'sorghum'

//  Changes:
//    DPH 18/6/98
//    dph  8/12/99 changed to use the simulation name in the descriptor field
//                 instead of a special descriptor field c227,d308

// ------------------------------------------------------------------
class DBSimulation
   {
   public:
      DBSimulation (void);
      DBSimulation (const std::string& name);
      ~DBSimulation (void);

      // return true if this simulation equals the specified scenario.
      bool operator== (const std::string& rhsTitle) const
         {
         return (stricmp(title, rhsTitle.c_str()) == 0);
         }

      bool partialTitleCompare(const char* partialTitle) const
         {
         return (strncmpi(partialTitle, title, strlen(partialTitle)) == 0);
         }
      // read in all simulation info. from the current record in the
      // specified indexTable.
      void readFromIndex(const std::string& databaseFilename,
                         TDataSet* indexTable);

      // return a container of factors that represents this simulation.
      void getFactorNames(std::vector<string>& factorNames) const;

      // return a value for a specified factor.
      std::string getFactorValue(const std::string& factorName) const;

      // calculate a rank based on how close the factors and values
      // passed in are to our factors and values.
      unsigned int calculateRank(const std::string& titleToMatch) const;

      // read all data from the open dataset and store in all_data
      virtual void readData(TAPSTable& data, const std::string& simulationName);

   protected:
      char* title;
      char databaseFilename[MAX_DATABASE_FILENAME_LENGTH];
      int simulationId;
      TADOQuery* dataset;
      TADOConnection* db;

      void getFieldNames (TADOConnection* db, std::vector<std::string>& fieldNames);
      std::string getOrderFieldName(std::vector<std::string>& fieldNames);
      void open (void);
      void close (void);
      void getFactors(std::vector<std::string>& names, std::vector<std::string>& values) const;
   };

class PartialTitleFindIf
   {
   private:
      const std::string& partialTitle;
   public:
      PartialTitleFindIf(const std::string& title)
         : partialTitle(title) { }
      bool operator() (const DBSimulation* simulation) const
         {
         return simulation->partialTitleCompare(partialTitle.c_str());
         }
   };



#endif
