//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPSetupForm.h"
#include "Data.h"
#include "TYPWebSession.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <soil\soil.h>
#include <soil\SoilSample.h>
#include "Dir.h"
#include "Utilities.h"
using namespace boost::gregorian;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWOutlookBar"
#pragma link "IWCompEdit"
#pragma link "IWCompListbox"
#pragma link "IWTMSCal"
#pragma link "IWCompButton"
#pragma link "IWTMSCtrls"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompRectangle"
#pragma link "IWAdvWebGrid"
#pragma link "IWWebGrid"
#pragma link "IWCompCheckbox"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TYPSetupForm::TYPSetupForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TYPSetupForm::setup(TYPWebSession* session,
                         Data* d,
                         const string& userN,
                         const string& paddockN,
                         bool fromGrowerMan)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   fromGrowerManagement = fromGrowerMan;

   UserLabel->Text = "Paddock setup for user: xxx and paddock: yyy.";
   UserLabel->Text = StringReplace(UserLabel->Text, "xxx", data->getNameOfUser(userName).c_str(), TReplaceFlags());
   UserLabel->Text = StringReplace(UserLabel->Text, "yyy",  paddockName.c_str(), TReplaceFlags());

   populateRegionCombo();
   populateStationNumberCombo(data, userName, paddockName, RegionCombo->Text.c_str(), WeatherStationCombo);
   populateSoilTypeCombo(data, userName, paddockName, RegionCombo->Text.c_str(), SoilTypeCombo);
   populateCombo(SubSoilCombo, data, userName, paddockName, "subsoilconstraints");
   populateDatePicker(ResetDate, data, userName, paddockName, "resetdate");
   populateResetGrid();
   populateResetGrid2();
   SaveButton->Enabled = (webSession->isSaveAllowed());
   }
//---------------------------------------------------------------------------
// populate the region combo
//---------------------------------------------------------------------------
void TYPSetupForm::populateRegionCombo()
   {
   vector<string> values;
   data->getRegions(values);
   Stl_2_tstrings(values, RegionCombo->Items);

   AnsiString value = data->getProperty(userName, paddockName, "region").c_str();
   RegionCombo->ItemIndex = RegionCombo->Items->IndexOf(value);
   }
//---------------------------------------------------------------------------
// Populate the ResetGrid.
//---------------------------------------------------------------------------
void TYPSetupForm::populateResetGrid()
   {
   ResetGrid->ClearCells();
   if (RegionCombo->Text != "")
      {
      try
         {
         string soilContents = data->getSoil(RegionCombo->Text.c_str(), SoilTypeCombo->Text.c_str());
         Soil soil(soilContents);
         vector<string> layerStrings = thicknessToLayerStrings(soil.thickness());
         ResetGrid->RowCount = max(layerStrings.size(), 8);
         for (unsigned l = 0; l != layerStrings.size(); l++)
            ResetGrid->Cells[0][l] = layerStrings[l].c_str();

         // New way of dealing with soil samples.
         // Populate the reset grid.
         string soilSampleString = data->getProperty(userName, paddockName, "soilsample1");
         if (soilSampleString != "")
            {
            SoilSample sample(soilSampleString);
            ResetGrid->ClearCells();
            setGridCol(ResetGrid, 0, 0, thicknessToLayerStrings(sample.thickness()));
            if (sample.hasData("Water", "sw"))
               setGridCol(ResetGrid, 1, 0, sample.swPercent(), 0);
            if (sample.hasData("Nitrogen", "no3"))
               setGridCol(ResetGrid, 2, 0, sample.no3(), 1);
            if (sample.hasData("Nitrogen", "nh4"))
               setGridCol(ResetGrid, 3, 0, sample.nh4(), 1);
            }
         }
      catch (const exception& err)
         {
         }
      }
   }
//---------------------------------------------------------------------------
// Populate the ResetGrid2.
//---------------------------------------------------------------------------
void TYPSetupForm::populateResetGrid2()
   {
   ResetGrid2->ClearCells();
   if (RegionCombo->Text != "")
      {
      try
         {
         string soilContents = data->getSoil(RegionCombo->Text.c_str(), SoilTypeCombo->Text.c_str());
         Soil soil(soilContents);
         vector<string> layerStrings = thicknessToLayerStrings(soil.thickness());
         ResetGrid2->RowCount = max(layerStrings.size(), 8);
         for (unsigned l = 0; l != layerStrings.size(); l++)
            ResetGrid2->Cells[0][l] = layerStrings[l].c_str();

         // New way of dealing with soil samples.
         // Populate the reset grid.
         string soilSampleString = data->getProperty(userName, paddockName, "soilsample2");
         if (soilSampleString != "")
            {
            SoilSample sample(soilSampleString);
            ResetGrid2->ClearCells();
            setGridCol(ResetGrid2, 0, 0, thicknessToLayerStrings(sample.thickness()));
            if (sample.hasData("Nitrogen", "oc"))
               setGridCol(ResetGrid2, 1, 0, sample.oc(), 1);
            if (sample.hasData("Other", "ec"))
               setGridCol(ResetGrid2, 2, 0, sample.ec(), 1);
            if (sample.hasData("Nitrogen", "ph"))
               setGridCol(ResetGrid2, 3, 0, sample.ph(), 1);
            if (sample.hasData("Other", "esp"))
               setGridCol(ResetGrid2, 4, 0, sample.esp(), 1);
            }
         }
      catch (const exception& err)
         {
         }
      }
   }
//---------------------------------------------------------------------------
// Save the ResetGrid.
//---------------------------------------------------------------------------
void TYPSetupForm::saveResetGrid()
   {
   unsigned numLayers = numValuesInCol(ResetGrid, 0);

   SoilSample sample;
   sample.setThickness(layerStringsToThickness(gridCol<string>(ResetGrid, 0, 0, numLayers)));
   sample.setSwPercent(gridCol<double>(ResetGrid, 1, 0, numLayers));
   sample.setNo3(gridCol<double>(ResetGrid, 2, 0, numLayers));
   sample.setNh4(gridCol<double>(ResetGrid, 3, 0, numLayers));
   ostringstream soilSampleString;
   sample.write(soilSampleString);
   data->setProperty(userName, paddockName, "soilsample1", soilSampleString.str());
   data->deleteProperty(userName, paddockName, "startsw");
   data->deleteProperty(userName, paddockName, "startno3");
   data->deleteProperty(userName, paddockName, "startnh4");
   }
//---------------------------------------------------------------------------
// Save the ResetGrid2.
//---------------------------------------------------------------------------
void TYPSetupForm::saveResetGrid2()
   {
   unsigned numLayers = numValuesInCol(ResetGrid2, 0);

   SoilSample sample;
   sample.setThickness(layerStringsToThickness(gridCol<string>(ResetGrid2, 0, 0, numLayers)));
   sample.setOc(gridCol<double>(ResetGrid2, 1, 0, numLayers));
   sample.setEc(gridCol<double>(ResetGrid2, 2, 0, numLayers));
   sample.setPh(gridCol<double>(ResetGrid2, 3, 0, numLayers));
   sample.setEsp(gridCol<double>(ResetGrid2, 4, 0, numLayers));
   ostringstream soilSampleString;
   sample.write(soilSampleString);
   data->setProperty(userName, paddockName, "soilsample2", soilSampleString.str());
   data->deleteProperty(userName, paddockName, "oc");
   data->deleteProperty(userName, paddockName, "ec");
   data->deleteProperty(userName, paddockName, "ph");
   data->deleteProperty(userName, paddockName, "esp");
   }
//---------------------------------------------------------------------------
// User has clicked the save button.
//---------------------------------------------------------------------------
void __fastcall TYPSetupForm::SaveButtonClick(TObject *Sender)
   {
   if (webSession->isSaveAllowed())
      {
      try
         {
         saveCombo(RegionCombo, data, userName, paddockName, "region");
         saveCombo(WeatherStationCombo, data, userName, paddockName, "metstation");
         saveCombo(SoilTypeCombo, data, userName, paddockName, "soiltype");
         saveCombo(SubSoilCombo, data, userName, paddockName, "subsoilconstraints");
         saveDatePicker(ResetDate, data, userName, paddockName, "resetdate");
         saveResetGrid();
         saveResetGrid2();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on back button
//---------------------------------------------------------------------------
void __fastcall TYPSetupForm::BackButtonClick(TObject *Sender)
   {
   webSession->showPaddockForm(userName, paddockName, false, fromGrowerManagement);
   }
//---------------------------------------------------------------------------
// User has changed the region - update the station numbers and soils.
//---------------------------------------------------------------------------
void __fastcall TYPSetupForm::RegionComboChange(TObject *Sender)
   {
   populateStationNumberCombo(data, userName, paddockName, RegionCombo->Text.c_str(), WeatherStationCombo);
   populateSoilTypeCombo(data, userName, paddockName, RegionCombo->Text.c_str(), SoilTypeCombo);
   }





