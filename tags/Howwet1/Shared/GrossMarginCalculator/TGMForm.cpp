//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include "TGMForm.h"
#include "TCropForm.h"
#include "TWheatMatrixForm.h"
#include "TSeedWeightForm.h"
#include <general\vcl_functions.h>
#include <general\path.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "AdvCGrid"
#pragma link "AdvEdit"
#pragma resource "*.dfm"
TGMForm *GMForm;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TGMForm::TGMForm(TComponent* Owner)
   : TForm(Owner)
   {
   dataNeedsSaving = false;
   data = new GMCalculator;
   }
//---------------------------------------------------------------------------
// destructor.
//---------------------------------------------------------------------------
__fastcall TGMForm::~TGMForm()
   {
   delete data;
   }
//---------------------------------------------------------------------------
// Form has been shown - set everything up.
//---------------------------------------------------------------------------
void __fastcall TGMForm::FormShow(TObject *Sender)
   {
   openDatabase(fileName);

    // initialise
   GMInfo->ActivePage = ScenarioPage;

   // populate the ScenarioTree
   populateTree();
   }
//---------------------------------------------------------------------------
// Form has been closed - save data.
//---------------------------------------------------------------------------
void __fastcall TGMForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      saveDataIfNecessary();
      data->save();
      }
   data->close();
   }
//---------------------------------------------------------------------------
// Open the specified database.
//---------------------------------------------------------------------------
void TGMForm::openDatabase(const std::string& filename)
   {
   saveDataIfNecessary();
   fileName = filename;
   data->open(fileName);
   Caption = fileName.c_str();
   }
//---------------------------------------------------------------------------
// populate the scenario tree.
//---------------------------------------------------------------------------
void TGMForm::populateTree(void)
   {
   ScenarioTree->Items->Clear();

   // add each scenario in the scenario table, and for each scenario
   // add all crops.
   vector<string> scenarioNames;
   data->getScenarioNames(scenarioNames);
   for (unsigned i = 0; i != scenarioNames.size(); i++)
      {
      TTreeNode* scenarioNode = ScenarioTree->Items->AddChild(NULL, scenarioNames[i].c_str());
      vector<string> cropNames;
      data->getCropsInScenario(scenarioNames[i], cropNames);
      for (unsigned c = 0; c != cropNames.size(); c++)
         ScenarioTree->Items->AddChild(scenarioNode, cropNames[c].c_str());
      }
   if (ScenarioTree->Items->Count > 0)
      ScenarioTree->Items->Item[0]->Selected = true;
   }
//---------------------------------------------------------------------------
// when a node is selected set the proper page, popup menu and caption
// and set the record to the scenario/crop index
//---------------------------------------------------------------------------
void __fastcall TGMForm::ScenarioTreeChange(TObject *Sender, TTreeNode *node)
   {
   Variant index = (int) node->Data;

   if(!node)return;  // when no node is selected
   if (node->Level == 0)
      {
      CropCostPage->TabVisible = false;
      CropPricePage->TabVisible = false;
      ScenarioLabel->Caption = node->Text;
      }
   else
      {
      CropCostPage->TabVisible = true;
      CropPricePage->TabVisible = true;
      }
   updateLabels();

   populateCostPage();
   populatePricePage();
   }
//---------------------------------------------------------------------------
// update labels.
//---------------------------------------------------------------------------
void TGMForm::updateLabels(void)
   {
   if (ScenarioTree->Selected != NULL)
      {
      RenameButton->Enabled = (ScenarioTree->Selected->Level == 0);

      AddButton->Hint = "Add a new scenario or crop";
      DeleteButton->Hint = "Delete " + ScenarioTree->Selected->Text;
      RenameButton->Hint = "Rename " + ScenarioTree->Selected->Text;
      CopyButton->Hint = "Copy " + ScenarioTree->Selected->Text;
      }
   if (copiedCrops.size() == 1)
      {
      PasteButton->Hint = "Paste " + AnsiString(copiedCrops[0].cropName.c_str());
      PasteButton->Enabled = true;
      }
   else if (copiedCrops.size() > 0)
      {
      PasteButton->Hint = "Paste " + AnsiString(copiedScenarioName.c_str());
      PasteButton->Enabled = true;
      }
   else
      PasteButton->Enabled = false;
   }
//---------------------------------------------------------------------------
// add new Scenario
//---------------------------------------------------------------------------
void __fastcall TGMForm::addScenario(TObject *Sender)
   {
   AnsiString newScen = newName("New Scenario");
   data->addScenario(newScen.c_str());
   TTreeNode* newNode = ScenarioTree->Items->AddChild(NULL, newScen);
   newNode->Selected = true;
   renameSelected(NULL);
   }
//---------------------------------------------------------------------------
// add new crop
//---------------------------------------------------------------------------
void __fastcall TGMForm::addCrop(TObject *Sender)
   {
   TCropForm* cropForm = new TCropForm(this);

   vector<string> cropNames;
   data->getPossibleCrops(cropNames);
   Stl_2_tstrings(cropNames, cropForm->CropList->Items);
   if (cropForm->ShowModal() == mrOk)
      {
      // check through the list for any selected crops and add them to the Tree
      AnsiString scenarioName = ScenarioTree->Selected->Text;
      for(int i=0;i < cropForm->CropList->Items->Count;i++)
         {
         if(cropForm->CropList->Checked[i])
            {
            AnsiString cropName = cropForm->CropList->Items->Strings[i];
            data->addCrop(scenarioName.c_str(), cropName.c_str());
            ScenarioTree->Items->AddChild(ScenarioTree->Selected, cropName);
            }
         }
      }
   delete cropForm;
   }
//---------------------------------------------------------------------------
// used by paste, rename and add scenario.
// while the name exists
// take oldname and add (1) to it (or change (1) to (2) or whatever)
//---------------------------------------------------------------------------
AnsiString TGMForm::newName(AnsiString oldName)
   {
   while(scenarioExists(oldName))
      {
      int i = oldName.LastDelimiter("(");
      AnsiString Tail = oldName.SubString(i,oldName.Length());
      int n = 0;
      sscanf(Tail.c_str(),"(%d)",&n);
      if (n)
         oldName.SetLength(i-1);
      oldName = oldName.Trim() + " (" + String(n+1) + ")";
      }
   return oldName;
   }
//---------------------------------------------------------------------------
// test to see if the scenarioname exists
//---------------------------------------------------------------------------
bool TGMForm::scenarioExists(AnsiString oldName)
   {
   for(int i = 0; i < ScenarioTree->Items->Count; i++)
      if(ScenarioTree->Items->Item[i]->Text == oldName)
         return true;
   return false;
   }
//---------------------------------------------------------------------------
// delete crop or Scenario depending on what is currently selected.
//---------------------------------------------------------------------------
void __fastcall TGMForm::deleteSelected(TObject* sender)
   {
   TTreeNode* node = ScenarioTree->Selected;

   // delete crop or Scenario
   // first work out which one
   if(node->Level == 1)
      {
      // delete crop
      AnsiString message = "This will delete the crop " + ScenarioTree->Selected->Text
                            + " and all economic data associated with it!  Do you wish to proceed?";
      if(Application->MessageBox(message.c_str(), "WARNING - Deleting Crop.",
                                                         MB_YESNO) == IDYES)
         {
         data->deleteCrop(node->Parent->Text.c_str(), node->Text.c_str());
         node->Delete();
         }
      }
   else
      {
      // delete scenario
      AnsiString message = "This will delete the scenario " + ScenarioTree->Selected->Text
                           + ", all crops in this Scenario and all economic data "
                           " associated with the crops!  Do you wish to proceed?";
      if(Application->MessageBox(message.c_str(), "WARNING - Deleting Scenario.",
                                                         MB_YESNO) == IDYES)
         {
         data->deleteScenario(node->Text.c_str());
         node->Delete();
         }
      }
   updateLabels();
   }
//---------------------------------------------------------------------------
// rename a scenario.
//---------------------------------------------------------------------------
void __fastcall TGMForm::renameSelected(TObject* sender)
   {
   ScenarioTree->Selected->EditText();
   }
//---------------------------------------------------------------------------
// Do we allow renaming?  Only for scenarios.
//---------------------------------------------------------------------------
void __fastcall TGMForm::ScenarioTreeEditing(TObject *Sender,
      TTreeNode *Node, bool &AllowEdit)
   {
   AllowEdit = (Node->Level == 0);
   }
//---------------------------------------------------------------------------
// User has finished renaming a node.
//---------------------------------------------------------------------------
void __fastcall TGMForm::ScenarioTreeEdited(TObject *Sender,
      TTreeNode *Node, AnsiString &S)
   {
   data->renameScenario(ScenarioTree->Selected->Text.c_str(), S.c_str());
   updateLabels();
   }
//---------------------------------------------------------------------------
// Copy the current tree item - crop or Scenario
//---------------------------------------------------------------------------
void __fastcall TGMForm::copySelected(TObject *Sender)
   {
   copiedCrops.erase(copiedCrops.begin(), copiedCrops.end());

   string scenarioName;
   vector<string> cropNames;
   if (ScenarioTree->Selected->Level == 0)
      {
      scenarioName = ScenarioTree->Selected->Text.c_str();
      data->getCropsInScenario(ScenarioTree->Selected->Text.c_str(), cropNames);
      copiedScenarioName = scenarioName;
      }
   else
      {
      scenarioName = ScenarioTree->Selected->Parent->Text.c_str();
      cropNames.push_back(ScenarioTree->Selected->Text.c_str());
      copiedScenarioName = "";
      }
   for (unsigned i = 0; i != cropNames.size(); i++)
      {
      CropData cropData;
      cropData.cropName = cropNames[i];
      data->getCosts(scenarioName, cropNames[i], cropData.costs);
      data->getPrice(scenarioName, cropNames[i], cropData.price);
      copiedCrops.push_back(cropData);
      }
   updateLabels();
   }
//---------------------------------------------------------------------------
// Paste crop or Scenario
//---------------------------------------------------------------------------
void __fastcall TGMForm::pasteSelected(TObject* sender)
   {
   TTreeNode* scenarioNode;
   if(copiedScenarioName != "")
      {
      string newScenarioName = newName(copiedScenarioName.c_str()).c_str();
      data->addScenario(newScenarioName);
      scenarioNode = ScenarioTree->Items->AddChild(NULL, newScenarioName.c_str());
      }
   else
      {
      if (ScenarioTree->Selected->Level == 0)
         scenarioNode = ScenarioTree->Selected;
      else
         scenarioNode = ScenarioTree->Selected->Parent;
      }

   // add all the child crops
   string newScenarioName = scenarioNode->Text.c_str();
   for (unsigned i = 0; i != copiedCrops.size(); i++)
      {
      if (data->addCrop(newScenarioName, copiedCrops[i].cropName))
         {
         ScenarioTree->Items->AddChild(scenarioNode, copiedCrops[i].cropName.c_str());
         data->setCosts(newScenarioName, copiedCrops[i].cropName, copiedCrops[i].costs);
         data->setPrice(newScenarioName, copiedCrops[i].cropName, copiedCrops[i].price);
         }
      else
         {
         ShowMessage(AnsiString("This Scenario already has ")
                     + copiedCrops[i].cropName.c_str()
                     + ".  It cannot be added again.");
         }
      }
   copiedCrops.erase(copiedCrops.begin(), copiedCrops.end());
   updateLabels();
   }
//---------------------------------------------------------------------------
// user has clicked ph button show form.
//---------------------------------------------------------------------------
void __fastcall TGMForm::WheatProteinButtonClick(TObject *Sender)
   {
   WheatMatrixForm = new TWheatMatrixForm(this);
   vector<double> protein, increment;
   data->getProteinIncrements(protein, increment);
   for (unsigned p = 0; p != protein.size(); p++)
      {
      WheatMatrixForm->ProteinIncrementGrid->Cells[0][p+1] = FloatToStrF(protein[p], ffFixed, 6, 1);
      WheatMatrixForm->ProteinIncrementGrid->Cells[1][p+1] = FloatToStrF(increment[p], ffFixed, 6, 2);
      }

   if (WheatMatrixForm->ShowModal() == mrOk)
      {
      protein.erase(protein.begin(), protein.end());
      increment.erase(increment.begin(), increment.end());
      for (int row = 1; row != WheatMatrixForm->ProteinIncrementGrid->RowCount; row++)
         {
         if (WheatMatrixForm->ProteinIncrementGrid->Cells[0][row] != "")
            {
            protein.push_back(StrToFloat(WheatMatrixForm->ProteinIncrementGrid->Cells[0][row]));
            increment.push_back(StrToFloat(WheatMatrixForm->ProteinIncrementGrid->Cells[1][row]));
            }
         }
      data->setProteinIncrements(protein, increment);
      }
   delete WheatMatrixForm;
   }
//---------------------------------------------------------------------------
// user has clicked seed weights button - display form
//---------------------------------------------------------------------------
void __fastcall TGMForm::SeedWeightButtonClick(TObject *Sender)
   {
   SeedWeightsForm = new TSeedWeightsForm(this);

   vector<string> cropNames;
   vector<double> seedWeights;
   data->getSeedWeights(cropNames, seedWeights);
   for (unsigned c = 0; c!= cropNames.size(); c++)
      {
      SeedWeightsForm->SeedWeightGrid->Cells[0][c+1] = cropNames[c].c_str();
      SeedWeightsForm->SeedWeightGrid->Cells[1][c+1] = FloatToStrF(seedWeights[c], ffFixed, 6, 3);
      }
   if (SeedWeightsForm->ShowModal() == mrOk)
      {
      cropNames.erase(cropNames.begin(), cropNames.end());
      seedWeights.erase(seedWeights.begin(), seedWeights.end());
      for (int row = 1; row != SeedWeightsForm->SeedWeightGrid->RowCount; row++)
         {
         if (SeedWeightsForm->SeedWeightGrid->Cells[0][row] != "")
            {
            cropNames.push_back(SeedWeightsForm->SeedWeightGrid->Cells[0][row].c_str());
            seedWeights.push_back(StrToFloat(SeedWeightsForm->SeedWeightGrid->Cells[1][row]));
            }
         }
      data->setSeedWeights(cropNames, seedWeights);
      }

   delete SeedWeightsForm;
   }
//---------------------------------------------------------------------------
// populate costs page.
//---------------------------------------------------------------------------
void __fastcall TGMForm::CropCostPageShow(TObject *Sender)
   {
   populateCostPage();
   }
//---------------------------------------------------------------------------
// populate price page.
//---------------------------------------------------------------------------
void __fastcall TGMForm::CropPricePageShow(TObject *Sender)
   {
   populatePricePage();
   }
//---------------------------------------------------------------------------
// Specify units for the 3rd column.
//---------------------------------------------------------------------------
void __fastcall TGMForm::CostGridGetEditorType(TObject *Sender, int ACol,
      int ARow, TEditorType &AEditor)
   {
   if (ACol == 0)
      {
      vector<string> operationNames;
      data->getPreviousOperations(operationNames);
      Stl_2_tstrings(operationNames, CostGrid->Columns->Items[0]->ComboItems);
      }
   else if (ACol == 2)
      {
      CostGrid->BtnUnitEdit->Units->Clear();
      CostGrid->BtnUnitEdit->Units->Add("/kg");
      CostGrid->BtnUnitEdit->Units->Add("/g");
      CostGrid->BtnUnitEdit->Units->Add("/tonne");
      CostGrid->BtnUnitEdit->Units->Add("/litre");
      }
   }
//---------------------------------------------------------------------------
// save data if necessary.
//---------------------------------------------------------------------------
void TGMForm::saveDataIfNecessary(void)
   {
   if (dataNeedsSaving)
      {
      if (GMInfo->ActivePage == CropCostPage)
         saveCostPage();
      else
         savePricePage();
      dataNeedsSaving = false;
      }
   }
//---------------------------------------------------------------------------
// populate the cost page.
//---------------------------------------------------------------------------
void TGMForm::populateCostPage(void)
   {
   if (ScenarioTree->Selected != NULL && ScenarioTree->Selected->Level == 1)
      {
      CostGrid->ClearRows(1, CostGrid->RowCount-1);

      vector<GMCalculator::Costs> costs;
      data->getCosts(ScenarioTree->Selected->Parent->Text.c_str(),
                     ScenarioTree->Selected->Text.c_str(),
                     costs);
      for (unsigned c = 0; c != costs.size(); c++)
         {
         CostGrid->Cells[0][c+1] = costs[c].operationName.c_str();
         CostGrid->Cells[1][c+1] = FloatToStrF(costs[c].operationCost, ffFixed, 8, 2);
         if (costs[c].productCost > 0
             && (costs[c].costType != GMCalculator::fixedCost || costs[c].productRate > 0))
            {
            CostGrid->Cells[2][c+1] = FloatToStrF(costs[c].productCost, ffFixed, 8, 2)
                                    + "/" + costs[c].productUnits.c_str();
            if (costs[c].costType == GMCalculator::fixedCost)
               CostGrid->Cells[3][c+1] = FloatToStrF(costs[c].productRate, ffFixed, 8, 0);
            else if (costs[c].costType == GMCalculator::sowingCost)
               CostGrid->Cells[3][c+1] = "density from sim.";
            else if (costs[c].costType == GMCalculator::nitrogenCost)
               CostGrid->Cells[3][c+1] = "nit. from sim.";
            else if (costs[c].costType == GMCalculator::yieldCost)
               CostGrid->Cells[3][c+1] = "yield from sim.";
            }
         }
      dataNeedsSaving = false;
      }
   }
//---------------------------------------------------------------------------
// save the cost page.
//---------------------------------------------------------------------------
void TGMForm::saveCostPage(void)
   {
   if (ScenarioTree->Selected != NULL && ScenarioTree->Selected->Level == 1)
      {
      vector<GMCalculator::Costs> costs;
      for (int row = 1; row != CostGrid->RowCount; row++)
         {
         GMCalculator::Costs cost;
         cost.operationName = CostGrid->Cells[0][row].c_str();
         if (cost.operationName != "")
            {
            cost.operationCost = StrToFloat(CostGrid->Cells[1][row]);
            if (CostGrid->Cells[2][row] != "")
               {
               char* endPos;
               cost.productCost = strtod(CostGrid->Cells[2][row].c_str(), &endPos);
               cost.productUnits = endPos + 1;
               }
            else
               cost.productCost = 0;

            cost.productRate = 0;
            if (CostGrid->Cells[3][row] == "density from sim.")
               cost.costType = GMCalculator::sowingCost;
            else if (CostGrid->Cells[3][row] == "nit. from sim.")
               cost.costType = GMCalculator::nitrogenCost;
            else if (CostGrid->Cells[3][row] == "yield from sim.")
               cost.costType = GMCalculator::yieldCost;
            else
               {
               cost.costType = GMCalculator::fixedCost;
               cost.productRate = StrToFloatDef(CostGrid->Cells[3][row], 0.0);
               }
            costs.push_back(cost);
            }
         }
      data->setCosts(ScenarioTree->Selected->Parent->Text.c_str(),
                     ScenarioTree->Selected->Text.c_str(),
                     costs);
      }
   }
//---------------------------------------------------------------------------
// populate the price page.
//---------------------------------------------------------------------------
void TGMForm::populatePricePage(void)
   {
   if (ScenarioTree->Selected != NULL && ScenarioTree->Selected->Level == 1)
      {
      GMCalculator::Price price;
      data->getPrice(ScenarioTree->Selected->Parent->Text.c_str(),
                     ScenarioTree->Selected->Text.c_str(),
                     price);
      PriceEdit->Text = FloatToStrF(price.price, ffFixed, 8, 0);
      DowngradeReturnEdit->Text = FloatToStrF(price.downgradeReturn, ffFixed, 8, 0);
      DowngradePercentEdit->Text = FloatToStrF(price.downgradePercent, ffFixed, 8, 0);
      HarvestLossEdit->Text = FloatToStrF(price.harvestLoss, ffFixed, 8, 0);
      MoistureContentEdit->Text = FloatToStrF(price.moistureContent, ffFixed, 8, 1);
      dataNeedsSaving = false;
      }
   }
//---------------------------------------------------------------------------
// save the price page.
//---------------------------------------------------------------------------
void TGMForm::savePricePage(void)
   {
   if (ScenarioTree->Selected != NULL && ScenarioTree->Selected->Level == 1)
      {
      GMCalculator::Price price;
      price.price = StrToFloatDef(PriceEdit->Text, 0);
      price.downgradeReturn = StrToFloatDef(DowngradeReturnEdit->Text, 0);
      price.downgradePercent = StrToFloatDef(DowngradePercentEdit->Text, 0);
      price.harvestLoss = StrToFloatDef(HarvestLossEdit->Text, 0);
      price.moistureContent = StrToFloatDef(MoistureContentEdit->Text, 0);
      data->setPrice(ScenarioTree->Selected->Parent->Text.c_str(),
                     ScenarioTree->Selected->Text.c_str(),
                     price);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TGMForm::DowngradePercentEditChange(TObject *Sender)
   {
   float downgradePercent = StrToFloatDef(DowngradePercentEdit->Text, 0);
   PriceLabel->Caption = "for %" + FloatToStrF(100 - downgradePercent, ffFixed, 3, 0)
                       + " of yield";
   dataNeedsSaving = true;
   }
//---------------------------------------------------------------------------
// A cost grid cell has changed, flag that we need to save data later.
//---------------------------------------------------------------------------
void __fastcall TGMForm::CostGridCellsChanged(TObject *Sender, TRect &R)
   {
   dataNeedsSaving = true;
   }
//---------------------------------------------------------------------------
// User has changed a price edit box - flag that we need to save data later.
//---------------------------------------------------------------------------
void __fastcall TGMForm::PriceEditBoxesChanged(TObject *Sender)
   {
   dataNeedsSaving = true;
   }
//---------------------------------------------------------------------------
// User has changed selected node in tree - save data if necessary.
//---------------------------------------------------------------------------
void __fastcall TGMForm::ScenarioTreeChanging(TObject *Sender, TTreeNode *Node, bool &AllowChange)
   {
   saveDataIfNecessary();
   }
//---------------------------------------------------------------------------
// User has changed to a different tab in tab control - save data if necessary.
//---------------------------------------------------------------------------
void __fastcall TGMForm::GMInfoChanging(TObject *Sender, bool &AllowChange)
   {
   saveDataIfNecessary();
   }
//---------------------------------------------------------------------------
// User has clicked open - let them choose an economics database.
//---------------------------------------------------------------------------
void __fastcall TGMForm::OpenButtonClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      openDatabase(OpenDialog->FileName.c_str());
      populateTree();
      }
   }
//---------------------------------------------------------------------------
// User has clicked the new button - create a new economics database.
//---------------------------------------------------------------------------
void __fastcall TGMForm::NewButtonClick(TObject *Sender)
   {
   if (SaveDialog->Execute())
      {
      ofstream out(SaveDialog->FileName.c_str());
      out << " ";
      out.close();
      openDatabase(SaveDialog->FileName.c_str());
      populateTree();
      }
   }
//---------------------------------------------------------------------------
// User has clicked the DeleteRow menu item on the costs grid.
//---------------------------------------------------------------------------
void __fastcall TGMForm::Deleterow1Click(TObject *Sender)
   {
   // The next 2 lines are necessary to get out of edit mode.  Can't
   // delete a line properly in edit mode.
   CostGrid->Row = CostGrid->Row + 1;
   CostGrid->Row = CostGrid->Row - 1;

   CostGrid->ClearRows(CostGrid->Row, 1);
   for (int row = CostGrid->Row+1; row != CostGrid->RowCount-1; row++)
      CostGrid->MoveRow(row, row-1);
   }
//---------------------------------------------------------------------------

