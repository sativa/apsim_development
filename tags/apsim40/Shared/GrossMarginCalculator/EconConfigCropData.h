//---------------------------------------------------------------------------

#ifndef EconConfigCropDataH
#define EconConfigCropDataH
#include <vector>
//---------------------------------------------------------------------------
// The class encapsulates all economic configuration data for a particular crop.
//---------------------------------------------------------------------------
class EconConfigCropData
   {
   public:
      EconConfigCropData(unsigned int recno);

      // return true if this is a valid crop configuration data object.
      bool isOk(void) {return cropRecordNumber > 0;}

      // convert a DRY yield into a WET yield.
      float calculateWetYield(float dryYield);

      // Calculate and return a gross margin return($/ha) given the DRY
      // yield(kg/ha) and protein(%).  Protein is ignored for non-wheat crops.
      float calculateReturn(float yield, float protein) const;

      // Calculate and return a gross margin cost($/ha) given the
      // nitrogen rate(kg/ha) and planting rate(plants/m2).
      float calculateCost(float nitrogenRate, float plantingRate) const;

   private:
      unsigned cropRecordNumber;
      AnsiString cropName;
      std::vector<double> proteinValues;
      std::vector<double> incrementValues;
      double seedWeight;
      double areaCost;
      double cropCost;

      // Get the protein adjustment table from the database.
      void fetchData(void);

      // calculate and return a price($/ha) for the given protein.
      float calculatePrice(float Protein) const;

   };
#endif
