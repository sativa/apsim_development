//---------------------------------------------------------------------------

#ifndef SpreadsheetImporterH
#define SpreadsheetImporterH
#include <string>
#include "Soils.h"
//---------------------------------------------------------------------------
// Go import all soils from specified spreadsheet.
//---------------------------------------------------------------------------
class __declspec(dllexport) SpreadsheetImporter
   {
   public:
      //---------------------------------------------------------------------------
      // go import all soils from the specified spreadsheet
      //---------------------------------------------------------------------------
      void importFromFile(std::string& fileName, Soils& soils);

   private:
      //---------------------------------------------------------------------------
      // create a soil. Parent must delete pointer to soil when finished.
      //---------------------------------------------------------------------------
      Soil* createSoil(TDataSet* dataset);

      //---------------------------------------------------------------------------
      // Get crop names from the specified dataset.
      //---------------------------------------------------------------------------
      std::vector<std::string> getCropNames(TDataSet* dataset);

      //---------------------------------------------------------------------------
      // Get crops from the specified dataset.
      //---------------------------------------------------------------------------
      void getCropsFromDataSet(TDataSet* dataset,
                               const vector<string>& cropNames,
                               vector< vector<double> >& ll,
                               vector< vector<double> >& kl,
                               vector< vector<double> >& xf,
                               vector< vector<double> >& kl0);

      //---------------------------------------------------------------------------
      // Get a value from current record and store in values.
      //---------------------------------------------------------------------------
      template <class T>
      void getValue(TDataSet* dataset, const std::string& fieldName, std::vector<T>& values)
         {
         try
            {
            values.push_back(dataset->FieldValues[fieldName.c_str()]);
            }
         catch (const Exception& err)
            {
            values.push_back(missingValue<T>());
            }
         }


   };


#endif
