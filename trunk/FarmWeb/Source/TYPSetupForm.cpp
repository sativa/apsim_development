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
                         bool readOnly,
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
   populateStationNumberCombo();
   populateSoilTypeCombo();
   populateCombo(SubSoilCombo, data, userName, paddockName, "subsoilconstraints");
   populateDatePicker(ResetDate, data, userName, paddockName, "resetdate");
   populateResetGrid();
   populateResetGrid2();
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
// populate the station number combo
//---------------------------------------------------------------------------
void TYPSetupForm::populateStationNumberCombo()
   {
   if (RegionCombo->Text != "")
      {
      vector<string> names;
      data->getMetStations(RegionCombo->Text.c_str(), names);
      Stl_2_tstrings(names, WeatherStationCombo->Items);

      AnsiString value = data->getProperty(userName, paddockName, "metstation").c_str();
      WeatherStationCombo->ItemIndex = WeatherStationCombo->Items->IndexOf(value);
      }
   else
      WeatherStationCombo->Items->Clear();
   }
//---------------------------------------------------------------------------
// populate the soil type combo
//---------------------------------------------------------------------------
void removeGrowerSoils(vector<string>& soilTypes, const string& userN)
   {
   string userName = userN;
   stripLeadingTrailing(userName, " ");
   vector<string> returnSoils;
   for (unsigned i = 0; i != soilTypes.size(); i++)
      {
      bool addSoil;
      unsigned posGrower = findSubString(soilTypes[i], "Grower soil:");
      if (posGrower == string::npos)
         addSoil = true;
      else
         {
         // only include soil if user name matches grower name.
         string growerName = soilTypes[i].substr(posGrower + strlen("Grower soil:"));
         stripLeadingTrailing(growerName, " ");
         addSoil = Str_i_Eq(growerName, userName);
         }
      if (addSoil)
         returnSoils.push_back(soilTypes[i]);
      }
   soilTypes = returnSoils;
   }
//---------------------------------------------------------------------------
// populate the soil type combo
//---------------------------------------------------------------------------
void TYPSetupForm::populateSoilTypeCombo()
   {
   if (RegionCombo->Text != "")
      {
      vector<string> values;
      data->getSoils(RegionCombo->Text.c_str(), values);
      removeGrowerSoils(values, userName);
      Stl_2_tstrings(values, SoilTypeCombo->Items);

      AnsiString value = data->getProperty(userName, paddockName, "soiltype").c_str();
      SoilTypeCombo->ItemIndex = SoilTypeCombo->Items->IndexOf(value);
      }
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
         for (unsigned l = 0; l != layerStrings.size(); l++)
            ResetGrid->Cells[0][l] = layerStrings[l].c_str();

         // Kept for compatibility reasons - can get rid of eventually.
         // Replaced with SoilSample objects.
         string valuesString = data->getProperty(userName, paddockName, "startsw");
         vector<string> sw;
         splitIntoValues(valuesString, " ", sw);

         valuesString = data->getProperty(userName, paddockName, "startno3");
         vector<string> no3;
         splitIntoValues(valuesString, " ", no3);

         valuesString = data->getProperty(userName, paddockName, "startnh4");
         vector<string> nh4;
         splitIntoValues(valuesString, " ", nh4);

         for (unsigned i = 0; i != sw.size(); i++)
            ResetGrid->Cells[1][i] = sw[i].c_str();
         for (unsigned i = 0; i != no3.size(); i++)
            ResetGrid->Cells[2][i] = no3[i].c_str();
         for (unsigned i = 0; i != nh4.size(); i++)
            ResetGrid->Cells[3][i] = nh4[i].c_str();

         // New way of dealing with soil samples.
         // Populate the reset grid.
         string soilSampleString = data->getProperty(userName, paddockName, "soilsample1");
         if (soilSampleString != "")
            {
            SoilSample sample(soilSampleString);
            ResetGrid->ClearCells();
            setGridCol(ResetGrid, 0, 0, thicknessToLayerStrings(sample.thickness()));
            if (sample.hasData("sw"))
               setGridCol(ResetGrid, 1, 0, sample.swPercent(), 0);
            if (sample.hasData("no3"))
               setGridCol(ResetGrid, 2, 0, sample.no3(), 1);
            if (sample.hasData("nh4"))
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
         for (unsigned l = 0; l != layerStrings.size(); l++)
            ResetGrid2->Cells[0][l] = layerStrings[l].c_str();

         // Kept for compatibility reasons - can get rid of eventually.
         // Replaced with SoilSample objects.
         string valuesString = data->getProperty(userName, paddockName, "oc");
         vector<string> oc;
         splitIntoValues(valuesString, " ", oc);

         valuesString = data->getProperty(userName, paddockName, "ec");
         vector<string> ec;
         splitIntoValues(valuesString, " ", ec);

         valuesString = data->getProperty(userName, paddockName, "ph");
         vector<string> ph;
         splitIntoValues(valuesString, " ", ph);

         valuesString = data->getProperty(userName, paddockName, "esp");
         vector<string> esp;
         splitIntoValues(valuesString, " ", esp);

         for (unsigned i = 0; i != oc.size(); i++)
            ResetGrid2->Cells[1][i] = oc[i].c_str();
         for (unsigned i = 0; i != ec.size(); i++)
            ResetGrid2->Cells[2][i] = ec[i].c_str();
         for (unsigned i = 0; i != ph.size(); i++)
            ResetGrid2->Cells[3][i] = ph[i].c_str();
         for (unsigned i = 0; i != esp.size(); i++)
            ResetGrid2->Cells[4][i] = esp[i].c_str();

         // New way of dealing with soil samples.
         // Populate the reset grid.
         string soilSampleString = data->getProperty(userName, paddockName, "soilsample2");
         if (soilSampleString != "")
            {
            SoilSample sample(soilSampleString);
            ResetGrid2->ClearCells();
            setGridCol(ResetGrid2, 0, 0, thicknessToLayerStrings(sample.thickness()));
            if (sample.hasData("oc"))
               setGridCol(ResetGrid2, 1, 0, sample.oc(), 1);
            if (sample.hasData("ec"))
               setGridCol(ResetGrid2, 2, 0, sample.ec(), 1);
            if (sample.hasData("ph"))
               setGridCol(ResetGrid2, 3, 0, sample.ph(), 1);
            if (sample.hasData("esp"))
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
   populateStationNumberCombo();
   populateSoilTypeCombo();
   }





