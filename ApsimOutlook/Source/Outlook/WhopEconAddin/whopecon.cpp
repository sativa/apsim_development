//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WhopEcon.h"
#include "About.h"
#include "Crop.h"
#include "DataForm.h"
#include "Econ.h"
#include "WheatMatrix.h"
#include "TWEValueSelectionForm.h"
#include <general\path.h>
#include <general\ini_file.h>
#include <general\stl_functions.h>

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------

#define WHOPECON_SECTION "WhopEcon"
#define WHOPECON_FACTOR_NAME "Econ Config"
#define ECON_DB_NAME "Econ Database"
#define BITMAP_NAME_KEY "bitmap"
#define SIMULATION_FACTOR_NAME "Simulation"

// static member variable declarations:
int WhopEcon::numObjects;

// general function declarations:
float GetPrice(int ScenarioIndex, AnsiString Crop, float *HarvestLoss,float Protein);
int GetScenarioIndex(AnsiString Scenario);
int GetCropIndex(AnsiString Crop,int ScenarioIndex);
float CalcAreaCost(void);
float CalcCropCost(void);
float GetSeedWt(AnsiString Crop);
float CalcUnitCost(float NRate, float SeedWt, float PlantingRate);
float ProteinAdj(float Protein);

// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 29/8/01 created

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters)
   {
   // will be called with begin
   // and end years from the database
   return new WhopEcon(parameters);
   }


// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DAH 29/08/01   created

// ------------------------------------------------------------------
WhopEcon::WhopEcon(const string& parameters)
   {
   if (numObjects == 0)
   {
      DATA = new TDATA(Application->MainForm);
      EconForm = new TEconForm(Application->MainForm);
      CropForm = new TCropForm(Application->MainForm);
      AboutBox = new TAboutBox(Application->MainForm);
      WheatMatrixForm = new TWheatMatrixForm(Application->MainForm);
   }

   // handle parameter string

   // read settings from ini
   Read_inifile_settings();

   Path bitmap_path(Application->ExeName.c_str());
   bitmap_path.Set_name (Econ_bitmap_name.c_str());
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(bitmap_path.Get_path().c_str());

   // get a default econ config name
   string default_config_name;
   EconForm->OpenEconDB();
   DATA->Scenario->First();
   if (!DATA->Scenario->Eof)
      default_config_name =  String(DATA->Scenario->FieldValues["ScenarioName"]).c_str();
   else
      default_config_name = "Empty";
   EconForm->CloseEconDB();

   // create a factor with the default name
   Factor econ(bitmap,WHOPECON_FACTOR_NAME,default_config_name.c_str(),this);
   factors.push_back(econ);

   numObjects++;
   }

WhopEcon::~WhopEcon(void) {
   if (numObjects == 1)
   {
      delete DATA;
      delete EconForm;
      delete CropForm;
      delete AboutBox;
      delete WheatMatrixForm;
   }
   numObjects--;
}


void WhopEcon::makeScenarioValid(Scenario& scenario,
                                     const std::string factor_name) const {}


Scenario WhopEcon::getDefaultScenario(void) const {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
}


void WhopEcon::getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   TListItems* config_names;
   EconForm->OpenEconDB();
   DATA->Scenario->First();
   while (!DATA->Scenario->Eof)
   {
      string name = String(DATA->Scenario->FieldValues["ScenarioName"]).c_str();
      factorValues.push_back(name);
      DATA->Scenario->Next();
   }
   EconForm->CloseEconDB();
}


TValueSelectionForm*  WhopEcon::getUIForm(const string& factorName,
                                                             TComponent* Owner) const{
   if ( WEValueSelectionForm == NULL)
      WEValueSelectionForm = new TWEValueSelectionForm(Owner);

   return  WEValueSelectionForm;
}


// ------------------------------------------------------------------
//  Short description:
//      read all defaults from .ini file.

//  Notes:

//  Changes:
//    DAH 29/8/01 :   Created

// ------------------------------------------------------------------
void WhopEcon::Read_inifile_settings (void)
{
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   // read all defaults.
   string st;
   ini.Read (WHOPECON_SECTION, BITMAP_NAME_KEY, st);
   Econ_bitmap_name = st;
   ini.Read (WHOPECON_SECTION, ECON_DB_NAME, st);
   Econ_DB_name = p.Get_directory() +  "\\"  +  st;
   EconForm->DBFileName = Econ_DB_name.c_str();
}


// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Created:   DAH   7/9/01   Adapted from G. McLean's WhopEcon and Dameasy
//                            doCalculations routine
// ------------------------------------------------------------------

void WhopEcon::doCalculations(TAPSTable& data,
                                  const std::vector<Scenario*>& selectedScenarios)
// for each data record, calculate the gross margin
{
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   // go thru the table 'data' and add a new factor field to reflect the
   // economic configuration
   data.markFieldAsAPivot(WHOPECON_FACTOR_NAME);

   vector<string> econ_config_names;              
   bool ok = data.first();
   while (ok)
   {
      for (vector<TAPSRecord>::const_iterator record = data.begin();
                     record != data.end(); record++)
      {
         // for each record in data, find the corresponding Scenario to find which
         // econConfig is being used
         string rec_name = record->getFieldValue(SIMULATION_FACTOR_NAME);
         vector<Scenario*>::const_iterator find_pos =
               find_if(selectedScenarios.begin(),selectedScenarios.end(),
                       PEqualToName<Scenario>(rec_name));
         Graphics::TBitmap* temp;
         string econ_config_name;
         (*find_pos)->getFactorAttributes(WHOPECON_FACTOR_NAME, econ_config_name, temp);
         econ_config_names.push_back(econ_config_name);
      }
      ok = data.next();
   }

   // add the config names to the data table.
   data.first();
   data.storeStringArray(WHOPECON_FACTOR_NAME, econ_config_names);
   data.endStoringData();   // this sorts the data so we can sensibly access it
                            // for the next section of code.

   // Now go thru entire table calculating GMs.
   // result vectors:
   vector<double> vGM, vReturn, vCost;

   EconForm->OpenEconDB();
   DATA->CropList->Open();

   ok = data.first();
   while (ok)
   {
      // do calculations/operations that are common to the whole block:
      vector<TAPSRecord>::const_iterator record = data.begin();
      AnsiString Config = record->getFieldValue(WHOPECON_FACTOR_NAME).c_str();
      int ConfigIndex = GetScenarioIndex(Config);

      AnsiString LastCrop = "";
      float Price,HarvestLoss;
      float AreaCost,CropCost;
      float SeedWt;
      for (record = data.begin(); record != data.end(); record++)
      {
         AnsiString Crop = record->getFieldValue("Crop").c_str();
         // wheat price has to be recalculated for every simulation (protein)
         if(Crop == "Wheat"){
            float Protein = StrToFloat(record->getFieldValue("Protein").c_str());
            Price = GetPrice(ConfigIndex,Crop,&HarvestLoss,Protein);   // price in $/t
            // if price < 0 then no crop of this name in this scenario
            //if(Price < 0){
            //   SimData->Next();
            //   continue;
            //}
         }
         else if(Crop != LastCrop){
            // get factors from EconDB tables
            // first Price
            Price = GetPrice(ConfigIndex,Crop,&HarvestLoss,0.0);   // price in $/t
            // if price < 0 then no crop of this name in this scenario
            //if(Price < 0){
            //  SimData->Next();
            //   continue;
            //}
         }
            // now calculate cost in $/ha from 3 cost tables
            //cost per area
         AreaCost = CalcAreaCost();
         SeedWt =  GetSeedWt(Crop);
         CropCost = CalcCropCost();
         LastCrop = Crop;

         float Yield = StrToFloat(record->getFieldValue("Yield (kg/ha)").c_str()) * (1 - HarvestLoss/100);
         float Return = Price * Yield / 1000.0;   // in $/ha
         float NRate = 0; //StrToFloat(record->getFieldValue("Applied NO3").c_str());
         float PlantingRate = 0; //StrToFloat(record->getFieldValue("PlantingRate").c_str());
         float UnitCost = 0; //CalcUnitCost(NRate,SeedWt,PlantingRate);
         float Cost = AreaCost + UnitCost + CropCost;

         vCost.push_back(Cost);
         vReturn.push_back(Return);
         vGM.push_back(Return - Cost);
      }
      ok = data.next();
   }

   // Append result vectors to 'data':
   data.first();
   data.storeNumericArray("Return ($)", vReturn);
   data.storeNumericArray("Cost ($)", vCost);
   data.storeNumericArray("GM ($)", vGM);

   EconForm->CloseEconDB();
   Screen->Cursor = savedCursor;
}


//---------------------------------------------------------------------------
int GetScenarioIndex(AnsiString Scenario)
{
   return DATA->Scenario->Lookup("ScenarioName",Scenario,"ScenarioIndex");
}
//---------------------------------------------------------------------------

float GetPrice(int ScenarioIndex, AnsiString Crop, float *HarvestLoss, float Protein)
{
   // return the adjusted price in $/t
   // = [(100 - DownGrade%) * Price] + [(DownGrade% * DownGradePrice)]  - Levy - Freight


   Variant locvalues[2];
   locvalues[0] = Variant(ScenarioIndex);
   locvalues[1] = Variant(Crop);

   if(!DATA->Crop->Locate("ScenarioIndex;CropName",VarArrayOf(locvalues, 1),TLocateOptions()))
      return -1;

   *HarvestLoss = DATA->Crop->FieldValues["HarvestLoss"];

   float DnGrde = DATA->Crop->FieldValues["Downgrade%"];
   float Price = DATA->Crop->FieldValues["Price"];
   float DnGrdeRetn = DATA->Crop->FieldValues["DowngradeReturn"];
   float Levy = DATA->Crop->FieldValues["Levy"];
   float Freight = DATA->Crop->FieldValues["Freight"];

   // if crop is wheat, add protein adjustment
   if(Crop == "Wheat")Price += ProteinAdj(Protein);


   return ((100 - DnGrde)/100.0 * Price) + (DnGrde/100.0 * DnGrdeRetn) -
                                       Levy - Freight;
}
//---------------------------------------------------------------------------

float ProteinAdj(float Protein)
{
   // use the values in the WheatMatrix table to decide on the protein adjustment
   DATA->WheatMatrix->Open();
   DATA->WheatMatrix->First();
   float P0=0,P1,I0=100,I1;
   // round to 0.1
   Protein = ((int)(Protein*10+0.5))/10.0;

   while(!DATA->WheatMatrix->Eof){
      P1 = DATA->WheatMatrix->FieldValues["Protein%"];
      I1 = DATA->WheatMatrix->FieldValues["Increment"];
      if(Protein > P1){
         P0 = P1;
         I0 = I1;
      }
      else{
         DATA->WheatMatrix->Close();
         return I0 + (Protein - P0)/(P1 - P0) * (I1 - I0);
      }
      DATA->WheatMatrix->Next();
   }

}
//----------------------------------------------------------------------------

float GetSeedWt(AnsiString Crop)
{
   return DATA->CropList->Lookup("CropName",Crop,"SeedWt");
}
//---------------------------------------------------------------------------

float CalcAreaCost(void)
{
   DATA->AreaCosts->First();
   float AreaCost = 0.0;
   while(!DATA->AreaCosts->Eof){
      AreaCost += (float)DATA->AreaCosts->FieldValues["OperationCost"];
      AreaCost += (float)DATA->AreaCosts->FieldValues["ProductCost"];
      DATA->AreaCosts->Next();
   }
   return AreaCost;
}
//---------------------------------------------------------------------------

float CalcCropCost(void)
{
   DATA->CropCosts->First();
   float CropCost = 0.0;
   while(!DATA->CropCosts->Eof){
      CropCost += (float)DATA->CropCosts->FieldValues["OperationCost"];
      DATA->CropCosts->Next();
   }
   return CropCost;
}
//---------------------------------------------------------------------------

float CalcUnitCost(float NRate, float SeedWt, float PlantingRate)
{
   // calculate unit costs
   // for Sowing * use PlantingRate
   // for Nitrogen Application * use NRate
   // anything else use ProductRate
   DATA->UnitCosts->First();
   float UnitCost = 0.0;
   while(!DATA->UnitCosts->Eof){
      UnitCost += (float)DATA->UnitCosts->FieldValues["OperationCost"];
      AnsiString Operation = DATA->UnitCosts->FieldValues["Operation"];
      AnsiString Units = DATA->UnitCosts->FieldValues["ProductUnits"];
      float Cost = (float)DATA->UnitCosts->FieldValues["ProductCost"];
      float Factor;
      if(Operation == "Sowing *"){
         if(Units == "kg")Factor = 10.0;
         else if(Units == "g")Factor = 10000.0;
         else Factor = 0;
         UnitCost += PlantingRate * Factor * SeedWt * Cost;
      }
      else if(Operation == "Nitrogen Application *"){
         if(Units == "tonne")Factor = 0.001;
         else Factor = 0;
         UnitCost += Cost * Factor * NRate;
      }
      else{
         float Rate = (float)DATA->UnitCosts->FieldValues["ProductRate"];
         UnitCost += Cost * Rate;
      }
      DATA->UnitCosts->Next();
   }
   return UnitCost;

}                        



