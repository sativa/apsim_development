//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "EconConfigCropData.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include "DataForm.h"
#pragma package(smart_init)

//---------------------------------------------------------------------------
//constructor
//---------------------------------------------------------------------------
EconConfigCropData::EconConfigCropData(unsigned int recno)
   : cropRecordNumber(recno)
   {
   if (cropRecordNumber > 0)
      fetchData();
   }

//---------------------------------------------------------------------------
// convert a DRY yield into a WET yield.
//---------------------------------------------------------------------------
float EconConfigCropData::calculateWetYield(float dryYield)
   {
   DATA->Crop->RecNo = cropRecordNumber;
   float moistureContent = 0.0;
   try
      {
      moistureContent = DATA->Crop->FieldValues["MoistureContent"];
      }
   catch (const Exception& error)
      {}
   return dryYield / (1 - moistureContent / 100);
   }

//---------------------------------------------------------------------------
// Calculate and return a gross margin return($/ha) given the DRY
// yield(kg/ha) and protein(%).  Protein is ignored for non-wheat crops.
//---------------------------------------------------------------------------
float EconConfigCropData::calculateReturn(float yield, float protein) const
   {
   DATA->Crop->RecNo = cropRecordNumber;

   // get a price, harvestloss and moisturecontent for the crop.
   float price = calculatePrice(protein);   // price in $/t

   if (cropName.AnsiCompareIC("Cotton") == 0)
      return price * yield;           // $/bale * bale/ha = $/ha
   else
      return price * yield / 1000.0;  // $/tonne * kg/ha / 1000 = $/ha
   }

//---------------------------------------------------------------------------
// Caclulate and return a gross margin cost($/ha) given the
// nitrogen rate(kg/ha) and planting rate(plants/m2).
//---------------------------------------------------------------------------
float EconConfigCropData::calculateCost(float nitrogenRate, float plantingRate) const
   {
   DATA->Crop->RecNo = cropRecordNumber;

   // calculate unit costs
   // for Sowing * use PlantingRate
   // for Nitrogen Application * use NRate
   // anything else use ProductRate
   DATA->UnitCosts->First();
   float unitCost = 0.0;
   while(!DATA->UnitCosts->Eof)
      {
      try
         {
         float operationCost = DATA->UnitCosts->FieldValues["OperationCost"];
         AnsiString operationName = DATA->UnitCosts->FieldValues["Operation"];
         operationName = operationName.LowerCase();
         AnsiString productUnits = DATA->UnitCosts->FieldValues["ProductUnits"];
         productUnits = productUnits.LowerCase();
         float productCost = DATA->UnitCosts->FieldValues["ProductCost"];

         float factor;
         if(operationName == "sowing *")
            {
            if (productUnits == "kg")
               factor = 10.0;
            else if (productUnits == "g")
               factor = 10000.0;
            else
               factor = 0;
            unitCost += plantingRate * factor * seedWeight * operationCost;
            }
         else if(operationName == "nitrogen application *")
            {
            if (productUnits == "tonne")
               factor = 0.001;
            else if (productUnits == "kg")
               factor = 1;
            else
               factor = 0;
            unitCost += productCost * factor * nitrogenRate;
            }
         else
            {
            float productRate = DATA->UnitCosts->FieldValues["ProductRate"];
            unitCost += productCost * productRate;
            }
         }
      catch (const Exception& error)
         {}
      DATA->UnitCosts->Next();
      }
   return unitCost + areaCost + cropCost;
   }

//---------------------------------------------------------------------------
// calculate and return a price($/t) for the given protein.
//---------------------------------------------------------------------------
float EconConfigCropData::calculatePrice(float protein) const
   {
   float downGrade = 0.0;
   float price = 0.0;
   float downGradeReturn = 0.0;
   float levy = 0.0;
   float freight = 0.0;

   try
      {
      downGrade = DATA->Crop->FieldValues["Downgrade%"];
      price = DATA->Crop->FieldValues["Price"];
      downGradeReturn = DATA->Crop->FieldValues["DowngradeReturn"];
      levy = DATA->Crop->FieldValues["Levy"];
      freight = DATA->Crop->FieldValues["Freight"];
      }
   catch (const Exception& error)
      {}

   // if crop is wheat, add protein adjustment
   if (cropName.AnsiCompareIC("Wheat") == 0)
      {
      bool didInterp;
      double increment = linear_interp_real(protein, proteinValues,
                                            incrementValues, didInterp);
      price += increment;
      }
   return ((100 - downGrade)/100.0 * price)
          + (downGrade/100.0 * downGradeReturn) - levy - freight;
   }

//---------------------------------------------------------------------------
// Go fetch all static data.
//---------------------------------------------------------------------------
void EconConfigCropData::fetchData(void)
   {
   // Get the name of this crop.
   DATA->Crop->RecNo = cropRecordNumber;
   cropName = DATA->Crop->FieldValues["CropName"];

   // get protein matrix.
   DATA->WheatMatrix->Open();
   getDBFieldValues(DATA->WheatMatrix, "Protein%", proteinValues);
   getDBFieldValues(DATA->WheatMatrix, "Increment", incrementValues);
   DATA->WheatMatrix->Close();

   // get the seed weight for this crop.
   Variant var = DATA->CropList->Lookup("CropName", cropName, "SeedWt");
   if(var.IsNull())
      seedWeight = 0.0;
   else
      seedWeight = var;

   // get the area costs for this crop.
   DATA->AreaCosts->First();
   areaCost = 0.0;
   while(!DATA->AreaCosts->Eof)
      {
      try
         {
         areaCost += (double) DATA->AreaCosts->FieldValues["OperationCost"];
         areaCost += (double) DATA->AreaCosts->FieldValues["ProductCost"];
         }
      catch (const Exception& error)
         {}
      DATA->AreaCosts->Next();
      }

   // get the crop costs for this crop.
   DATA->CropCosts->First();
   cropCost = 0.0;
   while(!DATA->CropCosts->Eof)
      {
      try
         {
         cropCost += (double) DATA->CropCosts->FieldValues["OperationCost"];
         }
      catch (const Exception& error)
         {}
      DATA->CropCosts->Next();
      }
   }

