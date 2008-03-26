//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TCharacterisationForm.h"
#include "TSoilChartForm.h"
#include "Soil.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\vcl_functions.h>
#include <general\db_functions.h>
#include <numeric>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma link "AdvCGrid"
#pragma resource "*.dfm"
TCharacterisationForm *CharacterisationForm;
static const unsigned DEPTH_COL = 0;
static const unsigned BD_COL = 1;
static const unsigned SAT_COL = 2;
static const unsigned DUL_COL = 3;
static const unsigned LL15_COL = 4;
static const unsigned PAWC_COL = 5;
static const unsigned AIRDRY_COL = 6;
static const unsigned FIRST_CROP_COL = 7;

static const unsigned SWCON_COL = 1;
static const unsigned NO3_COL = 2;
static const unsigned NH4_COL = 3;
//static const unsigned UREA_COL = 4;
static const unsigned FBIOM_COL = 4;
static const unsigned FINERT_COL = 5;
static const unsigned OC_COL = 6;
static const unsigned EC_COL = 7;
static const unsigned PH_COL = 8;
static const unsigned CL_COL = 9;
static const unsigned CEC_COL = 10;
static const unsigned CA_COL = 11;
static const unsigned MG_COL = 12;
static const unsigned NA_COL = 13;
static const unsigned K_COL = 14;
static const unsigned EXCHG_SODIUM_COL = 15;
static const unsigned PARTICLE_SIZE_SAND_COL = 16;
static const unsigned PARTICLE_SIZE_SILT_COL = 17;
static const unsigned PARTICLE_SIZE_CLAY_COL = 18;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TCharacterisationForm::TCharacterisationForm(HWND Owner)
   : TForm((HWND)Owner)
   {
   dirtyData = false;
   dirtyDataOutside = false;
   SoilChartForm = new TSoilChartForm(this);
   soil = NULL;
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TCharacterisationForm::TCharacterisationForm(TComponent* Owner)
   : TForm(Owner)
   {
   dirtyData = false;
   dirtyDataOutside = false;
   SoilChartForm = new TSoilChartForm(this);
   soil = NULL;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TCharacterisationForm::~TCharacterisationForm()
   {
   delete soil;
   }



//---------------------------------------------------------------------------
// Setup this form.
//---------------------------------------------------------------------------
void TCharacterisationForm::setup(const std::string& soilXML, bool readonly)
   {
   soil = new Soil(soilXML);
   readOnly = readonly;
   loadGrids();
   }
void TCharacterisationForm::setup(Soil* soilNode, bool readonly)
   {
   soil = soilNode;
   readOnly = readonly;
   loadGrids();
   }
//---------------------------------------------------------------------------
// populate a single column of a grid with the specified values.
//---------------------------------------------------------------------------
int findLastNonBlankRow(TStringGrid* grid, int col)
   {
   for (int row = grid->RowCount-1; row > 0; row--)
      {
      if (grid->Cells[col][row] != "TOTAL:" &&
          grid->Cells[col][row] != "")
         return row;
      }
   return 0;
   }
//---------------------------------------------------------------------------
// get the values from a single column of a grid into the specified vector
//---------------------------------------------------------------------------
bool vectorAllBlanks(const vector<string>& container)
   {
   for (vector<string>::const_iterator i = container.begin();
                                       i != container.end();
                                       i++)
      {
      if (*i != "")
         return false;
      }
   return true;
   }
//---------------------------------------------------------------------------
// populate a single column of a grid with the specified values.
//---------------------------------------------------------------------------
template <class T>
void setGridCol(TStringGrid* grid, int col, const vector<T>& data, unsigned numDecPlaces)
   {
   for (unsigned i = 0; i != data.size(); i++)
      {
      if (!isMissingValue(data[i]))
         grid->Cells[col][i+1] = ftoa(data[i], numDecPlaces).c_str();
      }
   }
//---------------------------------------------------------------------------
// get the values from a single column of a grid into the specified vector
//---------------------------------------------------------------------------
template <class T>
vector<T> gridCol(TAdvStringGrid* grid, unsigned col)
   {
   vector<T> values;
   int lastRow = findLastNonBlankRow(grid, col);
   for (int row = 1; row <= lastRow; row++)
      values.push_back(atof(grid->Cells[col][row].c_str()));
   return values;
   }
//---------------------------------------------------------------------------
// populate the water grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadGeneralGrid(void)
   {
   GeneralGrid->Cells[1][0] = soil->region().c_str();
   GeneralGrid->Cells[1][1] = soil->site().c_str();
   GeneralGrid->Cells[1][2] = soil->name().c_str();
   GeneralGrid->Cells[1][3] = soil->localName().c_str();
   GeneralGrid->Cells[1][4] = soil->soilType().c_str();
   GeneralGrid->Cells[1][5] = soil->nearestTown().c_str();
   GeneralGrid->Cells[1][6] = soil->comment().c_str();
   GeneralGrid->Cells[1][7] = soil->gps().c_str();
   GeneralGrid->Cells[1][8] = soil->gpsType().c_str();
   GeneralGrid->Cells[1][9] = soil->mapId().c_str();
   GeneralGrid->Cells[1][10]= soil->naturalVegetation().c_str();
   }
//---------------------------------------------------------------------------
// save the general grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveGeneralGrid(void)
   {
   soil->setRegion(GeneralGrid->Cells[1][0].c_str());
   soil->setSite(GeneralGrid->Cells[1][1].c_str());
   soil->setName(GeneralGrid->Cells[1][2].c_str());
   soil->setLocalName(GeneralGrid->Cells[1][3].c_str());
   soil->setSoilType(GeneralGrid->Cells[1][4].c_str());
   soil->setNearestTown(GeneralGrid->Cells[1][5].c_str());
   soil->setComment(GeneralGrid->Cells[1][6].c_str());
   soil->setGps(GeneralGrid->Cells[1][7].c_str());
   soil->setGpsType(GeneralGrid->Cells[1][8].c_str());
   soil->setMapId(GeneralGrid->Cells[1][9].c_str());
   soil->setNaturalVegetation(GeneralGrid->Cells[1][10].c_str());
  }
//---------------------------------------------------------------------------
// populate the water grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadWaterGrid(void)
   {
   WaterGrid->ClearRows(1, WaterGrid->RowCount);
   WaterGrid->ColCount = 7;

   setGridCol(WaterGrid, DEPTH_COL, soil->depth(), 0);
   setGridCol(WaterGrid, BD_COL, soil->bd(),2);
   setGridCol(WaterGrid, SAT_COL, soil->sat(),2);
   setGridCol(WaterGrid, DUL_COL, soil->dul(),2);
   setGridCol(WaterGrid, LL15_COL, soil->ll15(),2);
   setGridCol(WaterGrid, AIRDRY_COL, soil->airdry(),2);
   calcAndDisplayPAWC(PAWC_COL);

   vector<string> crops = soil->crops();
   for (unsigned crop = 0; crop != crops.size(); crop++)
      {
      WaterGrid->ColCount = WaterGrid->ColCount + 4;
      int col = WaterGrid->ColCount - 4;
      WaterGrid->Cells[col][0] = string(crops[crop] + "\rLL(%vol)").c_str();
      setGridCol(WaterGrid, col, soil->ll(crops[crop]), 2);
      calcAndDisplayPAWC(col+1);
      try {
         WaterGrid->Cells[col+2][0] = string(crops[crop] + "\rKL").c_str();
         setGridCol(WaterGrid, col+2, soil->kl(crops[crop]), 2);
      } catch (const exception& error){
         if(crops[crop] != "ozcot")
            Application->MessageBox(error.what(), "Error", MB_ICONSTOP | MB_OK);
      }
      try {
         WaterGrid->Cells[col+3][0] = string(crops[crop] + "\rXF").c_str();
         setGridCol(WaterGrid, col+3, soil->xf(crops[crop]), 1);
      } catch (const exception& error){
         if(crops[crop] != "ozcot")
            Application->MessageBox(error.what(), "Error", MB_ICONSTOP | MB_OK);
      }
      }
   }

//---------------------------------------------------------------------------
// returns true if data has not been saved
//---------------------------------------------------------------------------
bool TCharacterisationForm::isDirty(){
     return dirtyData;
}
//---------------------------------------------------------------------------
// save the water grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveWaterGrid(void)
   {
   vector<unsigned> depth = gridCol<unsigned>(WaterGrid, DEPTH_COL);
   vector<double> bd = gridCol<double>(WaterGrid, BD_COL);
   vector<double> sat = gridCol<double>(WaterGrid, SAT_COL);
   vector<double> dul = gridCol<double>(WaterGrid, DUL_COL);
   vector<double> ll15 = gridCol<double>(WaterGrid, LL15_COL);
   vector<double> airdry = gridCol<double>(WaterGrid, AIRDRY_COL);
   soil->setDepth(depth);
   soil->setBd(bd);
   soil->setSat(sat);
   soil->setDul(dul);
   soil->setLl15(ll15);
   soil->setAirdry(airdry);

   // save crop data
   for (int col = FIRST_CROP_COL; col < WaterGrid->ColCount; col+=4)
      {
      string cropName = WaterGrid->Cells[col][0].c_str();
      cropName.erase(cropName.find_last_of("\r"));

      vector<double> ll = gridCol<double>(WaterGrid, col);
      vector<double> kl = gridCol<double>(WaterGrid, col+2);
      vector<double> xf = gridCol<double>(WaterGrid, col+3);
      soil->setll(cropName, ll);
      soil->setkl(cropName, kl);
      soil->setxf(cropName, xf);
      }
   }
//---------------------------------------------------------------------------
// populate the water predicted grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadWaterPredictedGrid(void)
   {
   WaterPredictedGrid->ClearRows(1, WaterPredictedGrid->RowCount);
   WaterPredictedGrid->FixedCols = 0;
   WaterPredictedGrid->ColCount = 1;
   WaterPredictedGrid->RowCount = 100;

   setGridCol(WaterPredictedGrid, DEPTH_COL, soil->depth(), 0);

   vector<string> crops;
   soil->predictedCrops(crops);
   for (unsigned crop = 0; crop != crops.size(); crop++)
      {
      WaterPredictedGrid->ColCount = WaterPredictedGrid->ColCount + 4;
      int col = WaterPredictedGrid->ColCount - 4;

      WaterPredictedGrid->Cells[col][0] = string(crops[crop] + "\rLL(%vol)").c_str();
      WaterPredictedGrid->Cells[col+1][0] = string(crops[crop] + "\rPAWC(mm)").c_str();
      WaterPredictedGrid->Cells[col+2][0] = string(crops[crop] + "\rKL").c_str();
      WaterPredictedGrid->Cells[col+3][0] = string(crops[crop] + "\rXF").c_str();

      vector<double> ll, kl, xf;
      soil->calcPredictedCropData(crops[crop], ll, kl, xf);

      setGridCol(WaterPredictedGrid, col, ll, 0);

      vector<double> pawcByLayer;
      calcPAWC(soil->depth(), ll, soil->dul(), pawcByLayer);
      setGridCol(WaterPredictedGrid, col+1, pawcByLayer, 0);
      setGridCol(WaterPredictedGrid, col+2, kl, 2);
      setGridCol(WaterPredictedGrid, col+3, xf, 1);

      WaterPredictedGrid->Cells[col+1][WaterPredictedGrid->RowCount-1]
         = ftoa(accumulate(pawcByLayer.begin(), pawcByLayer.end(), 0.0), 0).c_str();
      WaterPredictedGrid->Cells[0][WaterPredictedGrid->RowCount-1] = "TOTAL:";
      }

   if (WaterPredictedGrid->ColCount > 1)
      WaterPredictedGrid->FixedCols = 1;
   }
//---------------------------------------------------------------------------
// populate the 'profile' grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadProfileGrid(void)
   {
   ProfileGrid->ClearRows(1, ProfileGrid->RowCount);

   setGridCol(ProfileGrid, DEPTH_COL, soil->depth(), 0);
   ProfileGrid->RowCount = soil->depth().size() + 1;

   setGridCol(ProfileGrid, DEPTH_COL, soil->depth(), 0);
   try {setGridCol(ProfileGrid, SWCON_COL, soil->swcon(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, NO3_COL, soil->no3(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, NH4_COL, soil->nh4(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, FBIOM_COL, soil->fbiom(), 2);} catch (...) { }
   try {setGridCol(ProfileGrid, FINERT_COL, soil->finert(), 2);} catch (...) { }
   try {setGridCol(ProfileGrid, OC_COL, soil->oc(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, EC_COL, soil->ec(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, PH_COL, soil->ph(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, CL_COL, soil->cl(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, CEC_COL, soil->cec(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, CA_COL, soil->ca(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, MG_COL, soil->mg(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, NA_COL, soil->na(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, K_COL, soil->k(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, EXCHG_SODIUM_COL, soil->exchangableSodium(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, PARTICLE_SIZE_SAND_COL, soil->particleSizeSand(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, PARTICLE_SIZE_SILT_COL, soil->particleSizeSilt(), 1);} catch (...) { }
   try {setGridCol(ProfileGrid, PARTICLE_SIZE_CLAY_COL, soil->particleSizeClay(), 1);} catch (...) { }
   }
//---------------------------------------------------------------------------
// save the other grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveProfileGrid(void)
   {
   vector<double> swcon = gridCol<double>(ProfileGrid, SWCON_COL);
   vector<double> no3 = gridCol<double>(ProfileGrid, NO3_COL);
   vector<double> nh4 = gridCol<double>(ProfileGrid, NH4_COL);
   vector<double> fbiom = gridCol<double>(ProfileGrid, FBIOM_COL);
   vector<double> finert = gridCol<double>(ProfileGrid, FINERT_COL);
   vector<double> oc = gridCol<double>(ProfileGrid, OC_COL);
   vector<double> ec = gridCol<double>(ProfileGrid, EC_COL);
   vector<double> ph = gridCol<double>(ProfileGrid, PH_COL);
   vector<double> cl = gridCol<double>(ProfileGrid, CL_COL);
   vector<double> cec = gridCol<double>(ProfileGrid, CEC_COL);
   vector<double> ca = gridCol<double>(ProfileGrid, CA_COL);
   vector<double> mg = gridCol<double>(ProfileGrid, MG_COL);
   vector<double> na = gridCol<double>(ProfileGrid, NA_COL);
   vector<double> k = gridCol<double>(ProfileGrid, K_COL);
   vector<double> exchangableSodium = gridCol<double>(ProfileGrid, EXCHG_SODIUM_COL);
   vector<double> particleSizeSand = gridCol<double>(ProfileGrid, PARTICLE_SIZE_SAND_COL);
   vector<double> particleSizeSilt = gridCol<double>(ProfileGrid, PARTICLE_SIZE_SILT_COL);
   vector<double> particleSizeClay = gridCol<double>(ProfileGrid, PARTICLE_SIZE_CLAY_COL);

   soil->setSwcon(swcon);
   soil->setNo3(no3);
   soil->setNh4(nh4);
   soil->setFbiom(fbiom);
   soil->setFinert(finert);
   soil->setOc(oc);
   soil->setEc(ec);
   soil->setPh(ph);
   soil->setCl(cl);
   soil->setCec(cec);
   soil->setCa(ca);
   soil->setMg(mg);
   soil->setNa(na);
   soil->setK(k);
   soil->setExchangableSodium(exchangableSodium);
   soil->setParticleSizeSand(particleSizeSand);
   soil->setParticleSizeSilt(particleSizeSilt);
   soil->setParticleSizeClay(particleSizeClay);
   }
//---------------------------------------------------------------------------
// populate the apsim grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadApsimGrid(void)
   {
   ApsimGrid->ClearCols(1, 1);

   ApsimGrid->Cells[1][1] = ftoa(soil->u(), 0).c_str();
   ApsimGrid->Cells[1][2] = ftoa(soil->cona(), 1).c_str();
   ApsimGrid->Cells[1][3] = ftoa(soil->salb(), 2).c_str();
   ApsimGrid->Cells[1][5] = ftoa(soil->diffusConst(), 0).c_str();
   ApsimGrid->Cells[1][6] = ftoa(soil->diffusSlope(), 0).c_str();
   ApsimGrid->Cells[1][8] = ftoa(soil->cn2Bare(), 0).c_str();
   ApsimGrid->Cells[1][9] = ftoa(soil->cnRed(), 0).c_str();
   ApsimGrid->Cells[1][10] = ftoa(soil->cnCov(), 1).c_str();
   ApsimGrid->Cells[1][12] = ftoa(soil->rootCN(), 0).c_str();
   ApsimGrid->Cells[1][13] = ftoa(soil->rootWt(), 0).c_str();
   ApsimGrid->Cells[1][14] = ftoa(soil->soilCN(), 1).c_str();
   ApsimGrid->Cells[1][16]= ftoa(soil->enrAcoeff(), 1).c_str();
   ApsimGrid->Cells[1][17] = ftoa(soil->enrBcoeff(), 1).c_str();
   ApsimGrid->AddNode(0, 4);
   ApsimGrid->AddNode(4, 3);
   ApsimGrid->AddNode(7, 4);
   ApsimGrid->AddNode(11, 4);
   ApsimGrid->AddNode(15, 3);
   }
//---------------------------------------------------------------------------
// save the Apsim grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveApsimGrid(void)
   {
   soil->setU(StrToFloat(ApsimGrid->Cells[1][1]));
   soil->setCona(StrToFloat(ApsimGrid->Cells[1][2]));
   soil->setSalb(StrToFloat(ApsimGrid->Cells[1][3]));
   soil->setDiffusConst(StrToFloat(ApsimGrid->Cells[1][5]));
   soil->setDiffusSlope(StrToFloat(ApsimGrid->Cells[1][6]));
   soil->setCn2Bare(StrToFloat(ApsimGrid->Cells[1][8]));
   soil->setCnRed(StrToFloat(ApsimGrid->Cells[1][9]));
   soil->setCnCov(StrToFloat(ApsimGrid->Cells[1][10]));
   soil->setRootCN(StrToFloat(ApsimGrid->Cells[1][12]));
   soil->setRootWt(StrToFloat(ApsimGrid->Cells[1][13]));
   soil->setSoilCN(StrToFloat(ApsimGrid->Cells[1][14]));
   soil->setEnrAcoeff(StrToFloat(ApsimGrid->Cells[1][16]));
   soil->setEnrBcoeff(StrToFloat(ApsimGrid->Cells[1][17]));
   }
//---------------------------------------------------------------------------
// Load the grid currently being displayed.
//---------------------------------------------------------------------------
void TCharacterisationForm::loadGrids(void)
   {
   try
      {
      switch (PageControl->TabIndex)
         {
         case 0 : loadGeneralGrid();         break;
         case 1 : loadWaterGrid();           break;
         case 2 : loadWaterPredictedGrid();  break;
         case 3 : loadProfileGrid();           break;
         case 4 : loadApsimGrid();           break;
         }
      }
   catch (const exception& error)
      {
      Application->MessageBox(error.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (const Exception& error)
      {
      Application->MessageBox(error.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Save the data in the current grid if necessary.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveGrids(void)
   {
   if (dirtyData)
      {
      try
         {
         switch (PageControl->TabIndex)
            {
            case 0 : saveGeneralGrid();      break;
            case 1 : saveWaterGrid();        break;
            case 3 : saveProfileGrid();        break;
            case 4 : saveApsimGrid();        break;
            }
         }
      catch (const exception& error)
         {
         string msg = error.what();
         msg += ".  Data not saved!";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      catch (const Exception& error)
         {
         string msg = error.Message.c_str();
         msg += ".  Data not saved!";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }

      dirtyData = false;
      }
   }
//---------------------------------------------------------------------------
// Save soil into a file soil file.
//---------------------------------------------------------------------------
void TCharacterisationForm::saveSoil(std::string filename)
   {
   saveGrids();
   Soil &XMLFile = getSoil();
   ofstream file(filename.c_str());

   XMLFile.write(file);
   file.close();
   }
//---------------------------------------------------------------------------
// Re-calculate a PAWC value for the specified LL column in the grid and
// put it's value at the top of the next column.
//---------------------------------------------------------------------------
void TCharacterisationForm::calcAndDisplayPAWC(int col)
   {
   // get the depths, ll and dul values.
   vector<unsigned> depths = gridCol<unsigned>(WaterGrid, DEPTH_COL);
   vector<double> dul = gridCol<double>(WaterGrid, DUL_COL);
   vector<double> ll = gridCol<double>(WaterGrid, col-1);

   try
      {
      string cropName = WaterGrid->Cells[col-1][0].c_str();
      unsigned posCR = cropName.find("\rLL");
      if (posCR == string::npos)
         cropName = "";
      else
         cropName = cropName.erase(posCR);

      vector<double> pawcByLayer;
      calcPAWC(depths, ll, dul, pawcByLayer);

      if (cropName != "")
         WaterGrid->Cells[col][0] = AnsiString(cropName.c_str()) + "\rPAWC(mm)";
      else
         WaterGrid->Cells[col][0] = "PAWC\r(mm)";

      setGridCol(WaterGrid, col, pawcByLayer, 0);

      // update the floating footer.
      double pawc = calcPAWC(depths, ll, dul);
      WaterGrid->Cells[col][WaterGrid->RowCount-1] = ftoa(pawc, 0).c_str();
      WaterGrid->Cells[0][WaterGrid->RowCount-1] = "TOTAL:";

      }
   catch (const exception& error)
      {
      Application->MessageBox(error.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
//---------------------------------------------------------------------------
// Return true if the specified col is a LL column in the water grid.
//---------------------------------------------------------------------------
bool TCharacterisationForm::colIsLLCol(int col)
   {
   if(LL15_COL == col){
     return true;
   }
   string cropName = WaterGrid->Cells[col][0].c_str();
   unsigned posCR = cropName.find("\rLL");
   return (col >= FIRST_CROP_COL && posCR != string::npos);
   }
//---------------------------------------------------------------------------
// Return true if the specified col is a PAWC column in the water grid.
//---------------------------------------------------------------------------
bool TCharacterisationForm::colIsPAWCCol(int col)
   {
   return (WaterGrid->Cells[col][0].Pos("PAWC") > 0);
   }
//---------------------------------------------------------------------------
// User has just changed the page (tab) control - save grid data before
// tab actually changes.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::PageControlChanging(TObject *Sender,
      bool &AllowChange)
   {
   saveGrids();
   }
//---------------------------------------------------------------------------
// page control has now changed its tab - load up grids.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::PageControlChange(TObject *Sender)
   {
   loadGrids();
   }
//---------------------------------------------------------------------------
// Form is about to be closed - save if necessary.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   saveGrids();
   }
//---------------------------------------------------------------------------
// User has changed a cell - mark data as dirty and update PAWC values if
// necessary.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridSetEditText(
      TObject *Sendver, int col, int ARow, const AnsiString Value)
   {
   dirtyDataOutside = true;
   dirtyData = true;
   if (PageControl->TabIndex == 1 && colIsLLCol(col))
      calcAndDisplayPAWC(col+1);
   }
//---------------------------------------------------------------------------
// Colour the crop columns of the water grid alternating colours.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::WaterGridGetCellColor(
      TObject *Sender, int ARow, int ACol, TGridDrawState AState,
      TBrush *ABrush, TFont *AFont)
   {
   static TColor cropColours[2] = {(TColor)RGB(255,255,200), (TColor)RGB(255,255,100)};
   if (ACol >= FIRST_CROP_COL && ARow > 0)
      {
      ACol = ACol - FIRST_CROP_COL;            // 0,1,2,3     4,5,6,7     8,9,10,11    12,13,14,15
      ACol = ACol / 4 + 1;                     // 1,1,1,1     2,2,2,2     3,3,3,3      4,4,4,4
      int colour = (ACol / 2.0 != ACol / 2);   // 0,0,0,0     1,1,1,1     0,0,0,0      1,1,1,1
      ABrush->Color = cropColours[colour];
      }
   }
//---------------------------------------------------------------------------
// Only let user edit non pawc columns
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridCanEditCell(
      TObject *Sender, int ARow, int ACol, bool &CanEdit)
   {
   CanEdit = (!readOnly && !colIsPAWCCol(ACol));
   }
//---------------------------------------------------------------------------
// Only let user edit certain cells
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridProfileCanEdit(TObject *Sender,
      int ARow, int ACol, bool &CanEdit)
{
   CanEdit = (!readOnly);
}
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::LockLabelClick(TObject *Sender)
   {
   if (LockButton->Down)
      WaterGrid->FixedCols = FIRST_CROP_COL;
   else
      WaterGrid->FixedCols = 0;
   }
//---------------------------------------------------------------------------
// Colour the crop columns of the water predicted grid alternating colours.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::WaterPredictedGridGetCellColor(
      TObject *Sender, int ARow, int ACol, TGridDrawState AState,
      TBrush *ABrush, TFont *AFont)
   {
   static TColor cropColours[2] = {(TColor)RGB(255,255,200), (TColor)RGB(255,255,100)};
   if (ACol > 0 && ARow > 0)
      {
      ACol = ACol - 1;                         // 0,1,2,3     4,5,6,7     8,9,10,11    12,13,14,15
      ACol = ACol / 4 + 1;                     // 1,1,1,1     2,2,2,2     3,3,3,3      4,4,4,4
      int colour = (ACol / 2.0 != ACol / 2);   // 0,0,0,0     1,1,1,1     0,0,0,0      1,1,1,1
      ABrush->Color = cropColours[colour];
      }
   }
//---------------------------------------------------------------------------
// This event handler makes the DEL key clear the contents of all
// selected cells.
// Need to have MouseActions|DisjuctCellSelect property = true
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridKeyDown(TObject *Sender,
      WORD &Key, TShiftState Shift)
   {
   TAdvStringGrid* grid = dynamic_cast<TAdvStringGrid*> (Sender);
   if (grid != NULL &&
         (Key == VK_UP || Key == VK_DOWN || Key == VK_LEFT || Key == VK_RIGHT))
      grid->ClearSelectedCells();

/*   if (grid != NULL && Key == VK_DELETE)
      {
      for (int col = 0; col != grid->ColCount; col++)
         for (int row = 0; row != grid->RowCount; row++)
            {
            if (grid->IsSelected(col, row))
               grid->Cells[col][row] = "";
            }
      dirtyDataOutside = true;
      dirtyData = true;
      }
*/   }
//---------------------------------------------------------------------------
// This event handler disables the single click selection mechanism.
// Normally when a user clicks a cell (without the CTRL key down) the
// cell becomes selected and is quite difficult to become unselected.
// NB The user can still select multiple cells if they hold the CTRL key
// down - provided MouseActions|DisjuctCellSelect=true
// and             MouseActions|RangeSelectAndEdit=true
// and             Options|goEditing=false
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
   {
//   TAdvStringGrid* grid = dynamic_cast<TAdvStringGrid*> (Sender);
//   if (grid != NULL && !Shift.Contains(ssCtrl))
//      grid->SelectedCells[grid->Col][grid->Row] = false;
   }
//---------------------------------------------------------------------------
// Make the total line at the bottom of the grids bold and red.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GridGetDisplText(TObject *Sender,
      int ACol, int ARow, AnsiString &value)
   {
   TAdvStringGrid* grid = dynamic_cast<TAdvStringGrid*> (Sender);
   if (ARow == grid->RowCount-1)
      value = "<font face=\"Arial\" size=\"8\" color=\"clred\"><b>" + value + "</b></font>";
   }
//---------------------------------------------------------------------------
// User wants to copy cells from grid to clipboard.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::CopyMenuClick(TObject *Sender)
   {
   switch (PageControl->TabIndex)
      {
      case 0 : GeneralGrid->CopySelectionToClipboard(); break;
      case 1 : WaterGrid->CopySelectionToClipboard();   break;
      case 2 : WaterPredictedGrid->CopySelectionToClipboard(); break;
      case 3 : ProfileGrid->CopySelectionToClipboard(); break;
      case 4 : ApsimGrid->CopySelectionToClipboard(); break;
      }
   dirtyDataOutside = true;
   dirtyData = true;
   }

//---------------------------------------------------------------------------
// User wants to paste cells from clipboard to grid.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::PasteMenuClick(TObject *Sender)
   {
   switch (PageControl->TabIndex)
      {
      case 0 : GeneralGrid->PasteSelectionFromClipboard(); break;
      case 1 : WaterGrid->PasteSelectionFromClipboard();   break;
      case 2 : WaterPredictedGrid->PasteSelectionFromClipboard(); break;
      case 3 : ProfileGrid->PasteSelectionFromClipboard(); break;
      case 4 : ApsimGrid->PasteSelectionFromClipboard(); break;
      }
   dirtyDataOutside = true;
   dirtyData = true;
   }

//---------------------------------------------------------------------------
// User has asked to see the graph.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::GraphButtonClick(TObject *Sender)
   {
   SoilChartForm->Show();
   populateGraph();
   }
//---------------------------------------------------------------------------
// populate the graph.
//---------------------------------------------------------------------------
void TCharacterisationForm::populateGraph()
   {
   SoilChartForm->populateFromGrid(WaterGrid, soil->name());
   }
Soil* soil;
TCharacterisationForm* form = NULL;
//---------------------------------------------------------------------------
// Return a point to an instance of a characterisation form.
//---------------------------------------------------------------------------
extern "C" TCharacterisationForm* _export _stdcall CreateSoilForm(HWND parent, const char* soilXml)
   {
   try
      {
      form = new TCharacterisationForm((HWND)parent);
      form->ParentWindow = parent;
      form->Show();
      form->setup(soilXml, false);
      return form;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   }
//---------------------------------------------------------------------------
// Return a point to an instance of a characterisation form.
//---------------------------------------------------------------------------
extern "C" void _export _stdcall ResizeSoilForm(unsigned width, unsigned height)
   {
   if (form != NULL)   // make sure form is NULLed in TMainForm constructor.
      {
      form->Width = width;
      form->Height = height;
      }
   }
//---------------------------------------------------------------------------
// Return a point to an instance of a characterisation form.
//---------------------------------------------------------------------------
extern "C" TCharacterisationForm* _export _stdcall CreateSoilSForm(HWND parent, Soil* soilNode)
   {
   try
      {
      static TCharacterisationForm* form = new TCharacterisationForm((HWND)parent);
      form->ParentWindow = parent;
      form->Show();
      form->setup(soilNode, false);
      form->Align = alNone;
      return form;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   }
//---------------------------------------------------------------------------
// Return the current XML for the soil form.
//---------------------------------------------------------------------------
extern "C" void _export _stdcall GetXML(char* xml)
   {
   form->saveGrids();
   ostringstream out;
   form->getSoil().write(out);
   strcpy(xml, out.str().c_str());
   }
//---------------------------------------------------------------------------
// Return a point to an instance of a characterisation form.
//---------------------------------------------------------------------------
extern "C" void _export _stdcall DeleteSoilForm()
   {
   delete form;
   form = NULL;
   }

//---------------------------------------------------------------------------
// Fixes cells to a set size
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::ApsimGridIsFixedCell(
      TObject *Sender, int row, int col, bool &isFixed)
   {
   isFixed = (row == 0 || row == 4 || row == 7 || row == 11 ||
              row == 15);
   }
//---------------------------------------------------------------------------
// Inserts the crop typed in the Dialog box
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::NewCropButtonClick(TObject *Sender)
{
   AnsiString newcrop = InputBox("Input Crop Name"
                                 , "Please enter the crop name:"
                                 , "");
   soil->addCrop(newcrop.c_str());
   WaterGrid->ClearNormalCells();
   dirtyDataOutside = true;
   dirtyData = true;
   loadGrids();
}                                                                            \

//---------------------------------------------------------------------------
// Deletes the crop typed in the Dialog box
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::DeleteCropButtonClick(TObject *Sender)
{
   AnsiString deletecrop = InputBox("Input Crop Name"
                                 , "Please enter the crop name:"
                                 , "");
   if(deletecrop != NULL){
     try {
       soil->deleteCrop(deletecrop.c_str());
       dirtyDataOutside = true;          
       dirtyData = true;

       loadGrids();
     }  catch (const exception& err)
     {
       ShowMessage(err.what());
     }
   }
}


