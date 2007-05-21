//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include "GMCalculatorMDB.h"
#include <general\path.h>
#include <GrossMarginCalculator\GMCalculator.h>

using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   OkButton->Enabled = false;
   AnsiString fileName = _argv[1];
   PromptLabel->Caption = "The file " + fileName + " has been automatically " +
                          "converted to the new format. The new filename is " +
                          ExtractFileName(fileName);

   string newFileName = fileName.c_str();
   To_lower(newFileName);
   if (newFileName.find(".mdb") != string::npos)
      {
      replaceAll(newFileName, ".mdb", ".xml");
      doConversion(fileName.c_str(), newFileName);
      }
   else
      ShowMessage("You must specify an MDB file on the command line");
   OkButton->Enabled = true;
   }
//---------------------------------------------------------------------------
void TMainForm::doConversion(const string& mdbFileName, const std::string& xmlFileName)
   {
   ofstream out(xmlFileName.c_str());
   out << "<economics/>";
   out.close();
   GMCalculator xmlCalculator;
   xmlCalculator.open(xmlFileName);

   try
      {
      GMCalculatorMDB mdbCalculator;
      mdbCalculator.open(mdbFileName);

      // convert all crop / seed weights.
      vector<string> cropNames;
      vector<double> seedWeights;
      mdbCalculator.getSeedWeights(cropNames, seedWeights);
      xmlCalculator.setSeedWeights(cropNames, seedWeights);

      // convert all proteins.
      vector<double> proteinValues, proteinIncrements;
      mdbCalculator.getProteinIncrements(proteinValues, proteinIncrements);
      xmlCalculator.setProteinIncrements(proteinValues, proteinIncrements);

      // convert all scenarios.
      vector<string> scenarioNames;
      mdbCalculator.getScenarioNames(scenarioNames);
      for (unsigned s = 0; s != scenarioNames.size(); s++)
         {
         xmlCalculator.addScenario(scenarioNames[s]);
         vector<string> crops;
         mdbCalculator.getCropsInScenario(scenarioNames[s], crops);
         for (unsigned c = 0; c != crops.size(); c++)
            {
            xmlCalculator.addCrop(scenarioNames[s], crops[c]);
            GMCalculator::Price price;
            mdbCalculator.getPrice(scenarioNames[s], crops[c], price);
            xmlCalculator.setPrice(scenarioNames[s], crops[c], price);

            vector<GMCalculator::Costs> costs;
            mdbCalculator.getCosts(scenarioNames[s], crops[c], costs);
            xmlCalculator.setCosts(scenarioNames[s], crops[c], costs);
            }
         }
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   xmlCalculator.save();
   }
void __fastcall TMainForm::OkButtonClick(TObject *Sender)
   {
   Close();
   }
//---------------------------------------------------------------------------

