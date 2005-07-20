//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSoilPickerForm.h"
#include "TCharacterisationForm.h"
#include "Soil.h"
#include "Soils.h"
#include "ConsToSoil.h"
#include "Clipboardtester.h"
#include "SpreadsheetImporter.h"
#include <ApsimShared\ApsimSettings.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvCGrid"
#pragma link "AdvGrid"
#pragma link "BaseGrid"
#pragma resource "*.dfm"

static const unsigned REGION = 0;
static const unsigned SITE = 1;
static const unsigned SOIL_NAME = 2;
static const unsigned LOCAL_NAME = 3;
static const unsigned ORDER = 4;
//static const unsigned REFNO = 5;

TSoilPickerForm *SoilPickerForm;
//---------------------------------------------------------------------------
// Constructors
//---------------------------------------------------------------------------
__fastcall TSoilPickerForm::TSoilPickerForm(TComponent* Owner)
   : TForm(Owner)
   {
    soil = NULL;
    dirtyData = false;
   }
__fastcall TSoilPickerForm::TSoilPickerForm(HWND Owner)
   : TForm((HWND)Owner)
   {
   dirtyData = false;
//   SoilChartForm = new TSoilChartForm(this);
   soil = NULL;
   }

extern "C" TCharacterisationForm* _export _stdcall CreateSoilSForm(HWND parent, Soil* soilNode);
//---------------------------------------------------------------------------
//void TSoilPickerForm::setup(Soils& soils, bool readOnly, unsigned refno)
//---------------------------------------------------------------------------
// Setup up this form
//---------------------------------------------------------------------------
void TSoilPickerForm::setup(const std::string& fileName,
                            const std::string& defaultSoilN, bool readOnly,
                            bool showOkCancel)
   {
   defaultSoilName = defaultSoilN;

   if (!showOkCancel)
      ButtonPanel->Visible = false;
   else
      BorderStyle = bsSizeable;

   soils = new Soils();
   soils->open(fileName);
   readonly = readOnly;
   loadGrids();
   delete soil;

   soil = soils->get(SelectorGrid->Cells[SOIL_NAME][1].c_str());
   form = CreateSoilSForm(SoilDetails->Handle, soil);

   // select default soil name
   for (int r = 0; r < SelectorGrid->RowCount; r++)
      {
      if (Str_i_Eq(SelectorGrid->Cells[SOIL_NAME][r].c_str(), defaultSoilName))
         SelectorGrid->Row = r;
      }
   }
void TSoilPickerForm::setup(Soils* thesoils,
                            const std::string& defaultSoilN, bool readOnly,
                            bool showOkCancel)
   {
   defaultSoilName = defaultSoilN;

   if (!showOkCancel)
      ButtonPanel->Visible = false;
   else
      BorderStyle = bsSizeable;

   soils = thesoils;
   readonly = readOnly;
   loadGrids();
   delete soil;

   soil = soils->get(SelectorGrid->Cells[SOIL_NAME][1].c_str());
   form = CreateSoilSForm(SoilDetails->Handle, soil);

   // select default soil name
   for (int r = 0; r < SelectorGrid->RowCount; r++)
      {
      if (Str_i_Eq(SelectorGrid->Cells[SOIL_NAME][r].c_str(), defaultSoilName))
         SelectorGrid->Row = r;
      }
   }


//---------------------------------------------------------------------------
// Save the current soil viewed in characterisation form
//---------------------------------------------------------------------------
void TSoilPickerForm::saveCurrentSoil()
   {
   if(form->isDirtyOutside())
      {
      dirtyData = true;
      form->saveGrids();
      form->OutsideSave();
      soils->replace(CurrentSoilName,form->getSoil());
      }
   }

//---------------------------------------------------------------------------
// Get the current soil that is selected
//---------------------------------------------------------------------------
std::string TSoilPickerForm::getCurrentSoilNameSelected()
   {
     return string(SelectorGrid->Cells[SOIL_NAME][SelectorGrid->Row].c_str());
   }
//---------------------------------------------------------------------------
// Get the current soil that is selected
//---------------------------------------------------------------------------
std::string TSoilPickerForm::getCurrentSoilNameDisplayed()
   {
     return CurrentSoilName;
   }

//---------------------------------------------------------------------------
// Select the soil given its name
//---------------------------------------------------------------------------
void TSoilPickerForm::focusRowOfSoilName(const std::string& name)
   {
     for(int i = 1; i < SelectorGrid->RowCount; i++){
        if (Str_i_Eq(SelectorGrid->Cells[SOIL_NAME][i].c_str(), name)){
        if(SelectorGrid->Row == i){

        } else {
          SelectorGrid->Row = i;
        }
        }
     }
   }

//---------------------------------------------------------------------------
// Load the soils grid
//---------------------------------------------------------------------------
void TSoilPickerForm::loadGrids()
   {
     vector<std::string> regions;
     vector<std::string> sites;
     vector<std::string> soilTypes;
     vector<std::string> names;
     names = soils->names();

     soils->allRegions(regions);
     soils->allSites(sites);
     soils->allSoilTypes(soilTypes);

   SelectorGrid->RowCount = max(names.size()+1, 2);
   for (unsigned r = 0; r < names.size(); r++)
      {
      SelectorGrid->Cells[SOIL_NAME][r+1] = names[r].c_str();
      SelectorGrid->Cells[REGION][r+1] = regions[r].c_str();
      SelectorGrid->Cells[SITE][r+1] = sites[r].c_str();
      SelectorGrid->Cells[ORDER][r+1] = soilTypes[r].c_str();
      /*
      SelectorGrid->Cells[LOCAL_NAME][r+1] = info.localName.c_str();*/
      }
   SelectorGrid->SortByColumn(SelectorGrid->SortSettings->Column);

   }
//---------------------------------------------------------------------------
// Paste contents of clipboard into table
//---------------------------------------------------------------------------
void TSoilPickerForm::pasteSoil(){
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    std::string test(pClip->AsText.c_str());
    Soil fromClipboard(test);
    soils->add(fromClipboard);
    loadGrids();
    focusRowOfSoilName(fromClipboard.name());
}

//---------------------------------------------------------------------------
// return true if there is soil infomation in clipboard
//---------------------------------------------------------------------------
bool TSoilPickerForm::isSoilInClipboard(){
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    std::string test(pClip->AsText.c_str());
    Clipboardtester fromClipboard(test);
    return fromClipboard.isSoil();
}

//---------------------------------------------------------------------------
// copy current soil selected into clipboard
//---------------------------------------------------------------------------
void TSoilPickerForm::copySoil(){
    Soil *toClipboard = soils->get(getCurrentSoilNameSelected());
    ostringstream soilXml;
    toClipboard->write(soilXml);
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    pClip->SetTextBuf(soilXml.str().begin());
}

//---------------------------------------------------------------------------
// cut current soil selected into clipboard
//---------------------------------------------------------------------------
void TSoilPickerForm::cutSoil(){
    Soil *toClipboard = soils->get(getCurrentSoilNameSelected());
    ostringstream soilXml;
    toClipboard->write(soilXml);
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    pClip->SetTextBuf(soilXml.str().begin());
    deleteSoil();

}

//---------------------------------------------------------------------------
// displays the soil in characterisation form
//---------------------------------------------------------------------------
void TSoilPickerForm::setSoil(const std::string& name)
   {
      delete soil;
      CurrentSoilName = name;
      soil = soils->get(name);
      form->setup(soil, false);

   }

//---------------------------------------------------------------------------
// Delete the current soil selected
//---------------------------------------------------------------------------
void TSoilPickerForm::deleteSoil()
   {
     string message = "Are you sure you want to delete " + getCurrentSoilNameSelected()
                    + " Soil?\n(Note: Any samples belonging to this soil will also be deleted.)";
     if(IDYES == Application->MessageBox(message.c_str(), "Apsoil", MB_YESNO|MB_ICONSTOP)){
      dirtyData = true;
      soils->erase(getCurrentSoilNameSelected());
      loadGrids();
      setSoil(getCurrentSoilNameSelected());
     };
   }

//---------------------------------------------------------------------------
// Imports a soils file into the database
//---------------------------------------------------------------------------
void TSoilPickerForm::importSoils(const std::string& name)
   {
       Soils addsoils;
       addsoils.open(name.c_str());
       vector<std::string> names;
       names = addsoils.names();
       for(unsigned i = 0; i < names.size(); i++){
          soils->add(*addsoils.get(names[i]));
       }
       dirtyData = true;
       loadGrids();
   }
//---------------------------------------------------------------------------
// Imports a soil file into the database
//---------------------------------------------------------------------------
void TSoilPickerForm::importSoil(const std::string& name)
   {
      Soil addsoil(name.c_str());
      soils->add(addsoil);
      dirtyData = true;
      loadGrids();
      focusRowOfSoilName(addsoil.name());
   }
//---------------------------------------------------------------------------
// Imports a con file into the database
//---------------------------------------------------------------------------
void TSoilPickerForm::importPar(const std::string& name)
   {
   ConsToSoil conSoil;
   conSoil.importFromFile(name, *soils);
   dirtyData = true;
   loadGrids();
   }
//---------------------------------------------------------------------------
// Imports a xls file into the database
//---------------------------------------------------------------------------
void TSoilPickerForm::importXls(const std::string& name)
   {
   SpreadsheetImporter importer;
   importer.importFromFile(name, *soils);
   dirtyData = true;
   loadGrids();
   }
//---------------------------------------------------------------------------
// Imports a W2N2 file into the database
// String parsed to it points to w2 file
//---------------------------------------------------------------------------
void TSoilPickerForm::importW2N2(const std::string& W2file)
   {
/*      Soil addsoil;
      ConsToSoil *contestSoil = new ConsToSoil(W2file,"ok.xml");
      contestSoil->convertW2N2(addsoil);
      addsoil.setRegion(" ");
      addsoil.setSite(" ");
      addsoil.setSoilType(" ");
      soils->add(addsoil);
      dirtyData = true;
      loadGrids();
      focusRowOfSoilName(addsoil.name());
*/   }

//---------------------------------------------------------------------------
// returns true if data has not been saved
//---------------------------------------------------------------------------
bool TSoilPickerForm::isDirty(){
   return (dirtyData || form->isDirty());
}
//---------------------------------------------------------------------------
// populate the selector grid.
//---------------------------------------------------------------------------
/*void TCharacterisationForm::loadSelectorGrid(void)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   vector<unsigned> refNos;
   soils->getSoils(refNos);
   SelectorGrid->RowCount = max(refNos.size(), 2);
   for (unsigned r = 0; r != refNos.size(); r++)
      {
      Soils::Info info;
      soils->getInfo(refNos[r], info);
      SelectorGrid->Cells[0][r+1] = info.region.c_str();
      SelectorGrid->Cells[1][r+1] = info.site.c_str();
      SelectorGrid->Cells[2][r+1] = info.name.c_str();
      SelectorGrid->Cells[3][r+1] = info.localName.c_str();
      SelectorGrid->Cells[4][r+1] = info.soilType.c_str();
      SelectorGrid->Cells[5][r+1] = IntToStr(refNos[r]);

      // select this row?
      if (refNos[r] == selectedRefNo)
         SelectorGrid->Row = r+1;
      }

   // select a soil in grid if none selected so far.
   if (SelectorGrid->Row == 0)
      SelectorGrid->Row = 1;
   selectedRefNo = StrToInt(SelectorGrid->Cells[5][SelectorGrid->Row]);

   fillFilters();
   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
// User has clicked on a selector grid cell - change soil selection.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::SelectorGridSelectCell(
      TObject *Sender, int ACol, int ARow, bool &CanSelect)
   {
   static unsigned previousRefNo = 0;
   if (ARow > 0 && !filtering)
      {
      unsigned refNo = StrToIntDef(SelectorGrid->Cells[5][ARow], 0);
      if (refNo > 0)
         {
         onSoilChanged(previousRefNo, refNo);
         populateGraph(ARow);
         previousRefNo = refNo;
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on a selector grid cell - change soil selection.
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::SelectorGridFilterSelect(
      TObject *Sender, int Column, int ItemIndex, AnsiString FriendlyName,
      AnsiString &FilterCondition)
   {
   filtering = true;
   }
//---------------------------------------------------------------------------
void __fastcall TCharacterisationForm::SelectorGridFilterDone(
      TObject *Sender)
   {
   filtering = false;
   bool canSelect = true;
   SelectorGridSelectCell(NULL, 0, SelectorGrid->Row, canSelect);
   fillFilters();
   }
//---------------------------------------------------------------------------
// Fill the filters from what is currently in the grid.
//---------------------------------------------------------------------------
void TCharacterisationForm::fillFilters()
   {
   // populate column filters as well.
   for (unsigned col = 0; col != 6; col++)
      {
      SelectorGrid->Columns->Items[col]->Filter->Clear();
      SelectorGrid->Columns->Items[col]->Filter->Add("*");
      for (int row = 1; row != SelectorGrid->RowCount; row++)
         {
         AnsiString value = SelectorGrid->Cells[col][row];
         if (SelectorGrid->Columns->Items[col]->Filter->IndexOf(value) == -1)
            SelectorGrid->Columns->Items[col]->Filter->Add(value);
         }
      }
   // sort the column filters as well.
   for (unsigned col = 0; col != 5; col++)
      SelectorGrid->Columns->Items[col]->Filter->Sort();
   }
*/
//---------------------------------------------------------------------------
// Imports a soils file into the database
//---------------------------------------------------------------------------
extern "C" TSoilPickerForm* _export _stdcall CreateSoilPForm(HWND parent, const char* soilXml)
   {
   try
      {
      static TSoilPickerForm* formpick = new TSoilPickerForm((HWND)parent);
      formpick->ParentWindow = parent;
      formpick->Show();
      formpick->setup(soilXml, "", false, false);
      return formpick;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   }
extern "C" TSoilPickerForm* _export _stdcall CreateSoilPOForm(HWND parent, Soils* soilsObject)
   {
   try
      {
      static TSoilPickerForm* formpick = new TSoilPickerForm((HWND)parent);
      formpick->ParentWindow = parent;
      formpick->Show();
      formpick->setup(soilsObject, "", false, false);
      return formpick;
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      return NULL;
      }
   }

//---------------------------------------------------------------------------
// Save data into a file.
//---------------------------------------------------------------------------
void TSoilPickerForm::saveSoils(std::string filename)
   {
   saveCurrentSoil();
   ofstream file(filename.c_str());
   soils->write(file);
   file.close();
   dirtyData = false;
   }

//---------------------------------------------------------------------------
// Change Row reloads the soil information Grid
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::OnRowChange(TObject *Sender, int OldRow,
      int NewRow, bool &Allow)
  {
  saveCurrentSoil();
  loadGrids();

  String name(SelectorGrid->Cells[SOIL_NAME][NewRow].c_str());
  setSoil(name.c_str());
  }


//---------------------------------------------------------------------------
// User has pressed the delete key to delete a soil
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::SelectorGridKeyDown(TObject *Sender,
      WORD &Key, TShiftState Shift)
{
   if(Key == VK_DELETE){
      deleteSoil();
   }
}
//---------------------------------------------------------------------------
// Makes sure form uses all the space available
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::FormResize(TObject *Sender)
   {
   if (form != NULL)
      {
      form->Width = SoilDetails->Width;
      form->Height = SoilDetails->Height;
      }
   }
//---------------------------------------------------------------------------
// Allows user to browser for a .soils file
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::BrowseButtonClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      setup(OpenDialog->FileName.c_str(), defaultSoilName, false, true);
      }
   }

//---------------------------------------------------------------------------
// Allows user to insert a blank soil into the database
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::InsertNewSoilExecute(TObject *Sender)
{
   ApsimSettings settings;
   string newTemplateFile;
   settings.read("Soil|New template file", newTemplateFile, true);
   importSoil(newTemplateFile.c_str());
}

//---------------------------------------------------------------------------
// Cut Action for popup menu
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::CutActionExecute(TObject *Sender)
{
   cutSoil();
}


//---------------------------------------------------------------------------
// Copy Action for popup menu
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::CopyActionExecute(TObject *Sender)
{
   copySoil();
}
//---------------------------------------------------------------------------
// Paste Action for popup menu
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::PasteActionExecute(TObject *Sender)
{
   pasteSoil();
}
//---------------------------------------------------------------------------
// Delete Action for popup menu
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::DeleteActionExecute(TObject *Sender)
{
   deleteSoil();
}

//---------------------------------------------------------------------------
// This function determines if paste should be enabled in popup menu
//---------------------------------------------------------------------------
void __fastcall TSoilPickerForm::PopupGridMenuPopup(TObject *Sender)
{
    TClipboard *pClip = Clipboard(); // don’t clear the clipboard!
    std::string test(pClip->AsText.c_str());
    Clipboardtester fromClipboard(test);
    if(fromClipboard.isSoil()){
       PasteAction->Enabled = true;
    } else {
       PasteAction->Enabled = false;
    }
}


