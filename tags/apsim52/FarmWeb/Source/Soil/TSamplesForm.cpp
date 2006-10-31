//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\vcl_functions.h>
#include <general\db_functions.h>
#pragma hdrstop

#include "TSamplesForm.h"
#include "SoilSample.h"
#include "Soil.h"
#include "Soils.h"
#include "ConsToSoil.h"
#include "Clipboardtester.h"
#include <ApsimShared\ApsimSettings.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma resource "*.dfm"

//Samples Grid
/*static const unsigned SAMPLE_DATE = 0;
static const unsigned SAMPLE_NAME = 1;
static const unsigned SOIL_NAME = 2;

//Sample Grid
static const unsigned DEPTH = 0;
static const unsigned WET = 1;
static const unsigned DRY = 2;
static const unsigned NO3 = 3;
static const unsigned SW = 4;*/
TSamplesForm *SamplesForm;

//---------------------------------------------------------------------------
// Constructors
//---------------------------------------------------------------------------
__fastcall TSamplesForm::TSamplesForm(TComponent* Owner)
    : TForm(Owner)
{
   dirtyData = false;
   readOnly = false;
   deleting = false;
   soils = NULL;
   soil = NULL;
   thesample = NULL;
   soil = NULL;
   numbersamples = 0;
}
__fastcall TSamplesForm::TSamplesForm(HWND Owner)
   : TForm((HWND)Owner)
   {
   deleting = false;
   dirtyData = false;
   readOnly = false;
   soils = NULL;
   soil = NULL;
   thesample = NULL;
   soil = NULL;
   numbersamples = 0;
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
// Load the samples grid
//---------------------------------------------------------------------------
void TSamplesForm::loadGrids()
   {
     vector<std::string> samplenames;
     vector<std::string> soilsnames;
     vector<std::string> dates;

     soils->allSamples(samplenames, soilsnames, dates);
   SelectorGrid->ClearNormalCells();
   SelectorGrid->RowCount = max(samplenames.size()+1, 2);
   numbersamples = samplenames.size();
   for (unsigned r = 0; r < samplenames.size(); r++)
      {
      SelectorGrid->Cells[SAMPLE_DATE][r+1] = dates[r].c_str();
      SelectorGrid->Cells[SAMPLE_NAME][r+1] = samplenames[r].c_str();
      SelectorGrid->Cells[SOIL_NAME][r+1] = soilsnames[r].c_str();
      }

   //Can't edit sample that has not been created yet
   if(soils->zeroSamples())
     disableSampleDisplay();
   SelectorGrid->SortByColumn(1);
   SelectorGrid->SortByColumn(SelectorGrid->SortSettings->Column);
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
// Setup this form.
//---------------------------------------------------------------------------
void TSamplesForm::setup(const std::string& soilXML)
   {
   soils = new Soils();
   soils->open(soilXML);
   loadGrids();
   if(numbersamples > 0){
      loadSample(string(SelectorGrid->Cells[SOIL_NAME][SelectorGrid->Row].c_str())
                ,string(SelectorGrid->Cells[SAMPLE_NAME][SelectorGrid->Row].c_str()));
      }
   }
void TSamplesForm::setup(Soils* soilsObject)
   {
   soils = soilsObject;
   loadGrids();
   if(numbersamples > 0){
      loadSample(string(SelectorGrid->Cells[SOIL_NAME][SelectorGrid->Row].c_str())
                ,string(SelectorGrid->Cells[SAMPLE_NAME][SelectorGrid->Row].c_str()));
      }
   }


//---------------------------------------------------------------------------
// Displays Samples information on the grid below
//---------------------------------------------------------------------------
void TSamplesForm::loadSample(const std::string& Soilname,const std::string& Samplename)
   {
   //There are no samples to show
   if(soils->zeroSamples()){
     SoilComboBox->Clear();
     SampleNameEditBox->Text = "";
     SampleGrid->ClearNormalCells();
     disableSampleDisplay();
   //Show the sample
   } else {
     enableSampleDisplay();
     Soil* thesoil = soils->get(Soilname);
     vector<std::string> soilsnames;
     soilsnames = soils->names();
//   if(thesample != NULL)
//      delete thesample;
     thesample = new SoilSample(thesoil->getSample(Samplename));
     TDate test = StrToDate(thesample->date().c_str());
     currentSoilName = Soilname.c_str();
     SoilComboBox->Clear();
     for(int i = 0; i < soilsnames.size(); i++){
        SoilComboBox->AddItem(soilsnames[i].c_str(),NULL);
        if(0 == stricmp(currentSoilName.c_str(),soilsnames[i].c_str())){
           SoilComboBox->ItemIndex = i;
        }
     }

     currentSampleName = Samplename.c_str();
     SampleDateTimePicker->Date = test;
     SampleNameEditBox->Text = Samplename.c_str();
     SampleGrid->ClearNormalCells();
     setGridCol(SampleGrid, DEPTH, thesample->depth(), 0);
     thesample->doNotThrowExceptions();
     if(thesample->depth().size() > 0){
       ImportDepths->Enabled = false;
       setGridCol(SampleGrid, WET, thesample->wet(), 2);
       setGridCol(SampleGrid, DRY, thesample->dry(), 2);
       setGridCol(SampleGrid, NO3, thesample->no3(), 2);
       setGridCol(SampleGrid, NH4_COL, thesample->nh4(), 2);
       setGridCol(SampleGrid, OC_COL, thesample->oc(), 2);
       setGridCol(SampleGrid, EC_COL, thesample->ec(), 2);
       setGridCol(SampleGrid, PH_COL, thesample->ph(), 2);
       setGridCol(SampleGrid, ESP_COL, thesample->esp(), 2);

       if(depthEqual(thesample->depth(), thesoil->depth())){
          vector<double> sw;
          calcSW(thesample->wet(), thesample->dry(), thesoil->bd(), sw);
          setGridCol(SampleGrid, SW, sw, 2);
       }
     } else {
       ImportDepths->Enabled = true;
     }
   }

   }
//---------------------------------------------------------------------------
// Returns true if there is text in clipboard that represents a sample
//---------------------------------------------------------------------------
bool TSamplesForm::isSampleInClipboard()
   {
     TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
     std::string test(pClip->AsText.c_str());
     Clipboardtester fromClipboard(test);
     return fromClipboard.isSample();
   }

//---------------------------------------------------------------------------
// Save sample being displayed
//---------------------------------------------------------------------------
void TSamplesForm::saveSample()
   {
///   Sample the = new Sample();
   if(numbersamples > 0){
     thesample->setName(SampleNameEditBox->Text.c_str());
     thesample->setDate(SampleDateTimePicker->Date.DateString().c_str());
     vector<unsigned> depth = gridCol<unsigned>(SampleGrid, DEPTH);
     if(depth.size() > 0){
       vector<double> wet = gridCol<double>(SampleGrid, WET);
       vector<double> dry = gridCol<double>(SampleGrid, DRY);
       vector<double> no3 = gridCol<double>(SampleGrid, NO3);
       vector<double> nh4 = gridCol<double>(SampleGrid, NH4_COL);
       vector<double> oc = gridCol<double>(SampleGrid, OC_COL);
       vector<double> ec = gridCol<double>(SampleGrid, EC_COL);
       vector<double> ph = gridCol<double>(SampleGrid, PH_COL);
       vector<double> esp = gridCol<double>(SampleGrid, ESP_COL);


       thesample->setDepth(depth);
       thesample->setWet(wet);
       thesample->setDry(dry);
       thesample->setNo3(no3);
       thesample->setNh4(nh4);
       thesample->setOc(oc);
       thesample->setEc(ec);
       thesample->setPh(ph);
       thesample->setEsp(esp);


     }
     soils->replace(string(currentSoilName.c_str()), string(currentSampleName.c_str()), *thesample);
   }
   }
//---------------------------------------------------------------------------
// Makes sure form uses all the space available
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::FormResize(TObject *Sender)
   {
//   if (form != NULL)
//      {
//      form->Width = SoilDetails->Width;
//      form->Height = SoilDetails->Height;
//      }
   }
//---------------------------------------------------------------------------
TSamplesForm* formsample = NULL;
//---------------------------------------------------------------------------
// Used to create a form in the main form
//---------------------------------------------------------------------------
extern "C" TSamplesForm* _export _stdcall CreateSamplesForm(HWND parent, const char* soilXml)
   {
   try
      {
      formsample = new TSamplesForm((HWND)parent);
      formsample->ParentWindow = parent;
      formsample->Show();
      formsample->setup(soilXml);
      return formsample;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   catch (const Exception& error)
      {
         string msg = error.Message.c_str();
//         msg += ".  Data not saved!";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

extern "C" TSamplesForm* _export _stdcall CreateSamplesOForm(HWND parent, Soils* thesoils)
   {
   try
      {
      formsample = new TSamplesForm((HWND)parent);
      formsample->ParentWindow = parent;
      formsample->Show();
      formsample->setup(thesoils);
      return formsample;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   catch (const Exception& error)
      {
         string msg = error.Message.c_str();
//         msg += ".  Data not saved!";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

//---------------------------------------------------------------------------
// Executed when a different sample is selected.
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::OnRowChange(TObject *Sender, int OldRow,
      int NewRow, bool &Allow)
{
  //NOTE: if last sample is deleted then row is changed
  //but this sample should not be saved since it was deleted
  if(!deleting)
  {
    saveSample();
    loadGrids();
    if(numbersamples > 0){
      String name(SelectorGrid->Cells[SAMPLE_NAME][NewRow].c_str());
      String soilname(SelectorGrid->Cells[SOIL_NAME][NewRow].c_str());
      loadSample(soilname.c_str(), name.c_str());
    }
  }
}

//---------------------------------------------------------------------------
// Used to determine if cell can be edited
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::CanEditCell(TObject *Sender, int ARow,
      int ACol, bool &CanEdit)
{
   CanEdit = (!readOnly && !colIsSWCol(ACol));
}

//---------------------------------------------------------------------------
// Return true if the specified col is a SW column in the sample grid.
//---------------------------------------------------------------------------
bool TSamplesForm::colIsSWCol(int col)
   {
   return col == SW;
   }

   
void __fastcall TSamplesForm::GridSetEditText(TObject *Sender, int ACol,
      int ARow, const AnsiString Value)
{
   dirtyData = true;
   if(ACol == WET || ACol == DRY)
      calcAndDisplaySW();
}
void TSamplesForm::calcAndDisplaySW()
{
   vector<unsigned> depth = gridCol<unsigned>(SampleGrid, DEPTH);
   vector<double> wet = gridCol<double>(SampleGrid, WET);
   vector<double> dry = gridCol<double>(SampleGrid, DRY);
   Soil* thesoil = soils->get(currentSoilName.c_str());
//   SampleGrid->ClearNormalCols(SW,SW);
   if(depthEqual(depth, thesoil->depth())){
      vector<double> sw;
      calcSW(wet, dry, thesoil->bd(), sw);
      setGridCol(SampleGrid, SW, sw, 2);
   }
}

//---------------------------------------------------------------------------
// returns true if data has not been saved
//---------------------------------------------------------------------------
bool TSamplesForm::isDirty(){
   return dirtyData;
}

//---------------------------------------------------------------------------
// Execute before Save is excuted.
//---------------------------------------------------------------------------
void TSamplesForm::save()
   {
   saveSample();
   dirtyData = false;
   }

//---------------------------------------------------------------------------
// Execute when soil a sample belongs to is changed
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::OnSamplesSoilChange(TObject *Sender)
{
   vector<std::string> soilsnames;
   soilsnames = soils->names();
   soils->replace(string(currentSoilName.c_str())
                 ,soilsnames[SoilComboBox->ItemIndex], string(currentSampleName.c_str()), *thesample);
   currentSoilName = soilsnames[SoilComboBox->ItemIndex].c_str();
   calcAndDisplaySW();
   dirtyData = true;
}

//---------------------------------------------------------------------------
// Execute when a new sample is inserted
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::NewSampleExecute(TObject *Sender)
{
   //Construct sample
   SoilSample *currentsample;
   currentsample = new SoilSample();
   vector<std::string> soilsnames;
   soilsnames = soils->names();

   //call it New Sample, Assigned wiht current date, Assigned to a soil
   currentsample->setName("New Sample");
   currentsample->setDate(TDateTime::CurrentDate().FormatString("dd/MM/yyyy").c_str());
   thesample = currentsample;
   soils->add(soilsnames[0], *currentsample);
   loadGrids();
   focusRowOfSoilNameandSample(soilsnames[0], currentsample->name());
   dirtyData = true;
}

//---------------------------------------------------------------------------
// Select the sample given its name and the soil it belongs to.
//---------------------------------------------------------------------------
void TSamplesForm::focusRowOfSoilNameandSample(const std::string& Soilname,const std::string& Samplename)
   {
     for(int i = 1; i < SelectorGrid->RowCount; i++){
        if (Str_i_Eq(SelectorGrid->Cells[SOIL_NAME][i].c_str(), Soilname)
          &&Str_i_Eq(SelectorGrid->Cells[SAMPLE_NAME][i].c_str(), Samplename)){
          if(SelectorGrid->Row == i){
            saveSample();
            loadSample(Soilname.c_str(), Samplename.c_str());
          } else {
            SelectorGrid->Row = i;
          }
        }
     }
   }

//---------------------------------------------------------------------------
// Get depths from soil sample belongs to
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::ImportDepthsExecute(TObject *Sender)
{
   Soil* thesoil = soils->get(currentSoilName.c_str());
   setGridCol(SampleGrid, DEPTH, thesoil->depth(), 0);
   dirtyData = true;
}

//---------------------------------------------------------------------------
// Executed to cut a sample from the samples table
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::CutSampleExecute(TObject *Sender)
{
    SoilSample *cutSample = soils->get(currentSoilName.c_str(), currentSampleName.c_str());
    ostringstream sampleXml;
    cutSample->write(sampleXml);
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    pClip->SetTextBuf(sampleXml.str().begin());
    DeleteSampleExecute(NULL);
}

//---------------------------------------------------------------------------
// Executed to delete a sample from the samples table
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::DeleteSampleExecute(TObject *Sender)
{
   string message = "Are you sure you want to delete " + getCurrentSampleNameSelected()
                  + " Sample belonging to "+ getCurrentSoilNameSelected() +" Soil?";
   if(IDYES == Application->MessageBox(message.c_str(), "Apsoil", MB_YESNO|MB_ICONSTOP)){
      deleting = true;
      dirtyData = true;
      soils->eraseSample(currentSoilName.c_str(), currentSampleName.c_str());
      loadGrids();
      loadSample(getCurrentSoilNameSelected(), getCurrentSampleNameSelected());
      deleting = false;
   };
}

//---------------------------------------------------------------------------
// Executed to Copy a sample from the samples table
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::CopySampleExecute(TObject *Sender)
{
    SoilSample *cutSample = soils->get(currentSoilName.c_str(), currentSampleName.c_str());
    ostringstream sampleXml;
    cutSample->write(sampleXml);
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    pClip->SetTextBuf(sampleXml.str().begin());
}

//---------------------------------------------------------------------------
// Executed to Paste a sample from the samples table
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::PasteSampleExecute(TObject *Sender)
{
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    std::string test(pClip->AsText.c_str());
    SoilSample fromClipboard(test);
    vector<std::string> soilsnames;
    soilsnames = soils->names();
    soils->add(soilsnames[0], fromClipboard);
    loadGrids();
    focusRowOfSoilNameandSample(soilsnames[0], fromClipboard.name());
    dirtyData = true;
}

//---------------------------------------------------------------------------
// Used to work out if paste should be enabled in popup menu
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::PopupGridMenuPopup(TObject *Sender)
{
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    std::string test(pClip->AsText.c_str());
    Clipboardtester fromClipboard(test);
    if(fromClipboard.isSample()){
       PasteSample->Enabled = true;
    } else {
       PasteSample->Enabled = false;
    }
}

//---------------------------------------------------------------------------
// Executed when delete key is pressed
//---------------------------------------------------------------------------
void __fastcall TSamplesForm::SelectorGridKeyDown(TObject *Sender,
      WORD &Key, TShiftState Shift)
{
   if(Key == VK_DELETE){
      DeleteSampleExecute(NULL);
   }
}


