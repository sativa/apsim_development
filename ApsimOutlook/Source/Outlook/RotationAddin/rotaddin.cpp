//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "RotAddIn.h"
#include "TRotationForm.h"
#include "CropFields.h"
#include "RotationValues.h"
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\stristr.h>
#include <map>
#include <values.h>
#pragma resource "Rotation.res"
#pragma link "TAPSTable"
#pragma link "TAPSTable_Form"
#pragma link "TAPSRecord"
#pragma link "TMultiSTringListForm"


using namespace std;
// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DPH 5/4/01
// ------------------------------------------------------------------
extern "C" ToolBarAddInBase* _export __stdcall createToolBarAddIn(const string& parameters)
   {
   return new RotationAddIn(parameters);
   }
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
RotationAddIn::RotationAddIn(const string& parameters)
   {
   needsUpdating = false;
   rotationAnalysisOn = false;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
RotationAddIn::~RotationAddIn(void)
   {
   int pos = rotationButton->ImageIndex;
   Toolbar->RemoveControl(rotationButton);
   delete rotationButton;
   Toolbar->Images->Delete(pos);
   delete glyph;
   }

// ------------------------------------------------------------------
//  Short description:
//    Add buttons etc. to the ToolBar passed in.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
void RotationAddIn::decorateToolBar(TToolBar* toolbar)
   {
   Toolbar = toolbar;
   rotationButton = new TToolButton(toolbar);
   rotationButton->Left = toolbar->Width; // ensures button goes at right end of row
   rotationButton->Parent = toolbar;

   glyph = new Graphics::TBitmap;
   glyph->LoadFromResourceName((unsigned int) HInstance, "ROTATION_BITMAP");
   glyph->Transparent = true;

   rotationButton->ImageIndex = toolbar->Images->Add(glyph, NULL);
   rotationButton->Hint = "Turn crop rotations on/off";
   rotationButton->OnClick = buttonClick;
   }

// ------------------------------------------------------------------
//  Short description:
//    Add buttons etc. to the ToolBar passed in.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
void __fastcall RotationAddIn::buttonClick(TObject* Sender)
   {
   RotationForm = new TRotationForm(Application->MainForm);
   RotationForm->rotationAddIn = this;
   needsUpdating = (RotationForm->ShowModal() == mrOk);
   delete RotationForm;
   }
// ------------------------------------------------------------------
// Return the number of rotations.
// ------------------------------------------------------------------
int RotationAddIn::getNumRotations(void)
   {
   ensureRotationMappingStillValid();
   return rotations.size();
   }

// ------------------------------------------------------------------
// Return a list of rotation names to caller
// ------------------------------------------------------------------
void RotationAddIn::getRotationNames(vector<string>& rotationNames)
   {
   ensureRotationMappingStillValid();
   for (Rotations::const_iterator rotationI = rotations.begin();
                                  rotationI != rotations.end();
                                  rotationI++)
      rotationNames.push_back(rotationI->first);
   }

// ------------------------------------------------------------------
// Return the data block names for a given rotation.  Returns true
// if ok.
// ------------------------------------------------------------------
bool RotationAddIn::getRotation(const std::string& name, DataBlockNames& dataBlockNames) const
   {
   Rotations::const_iterator rotI = rotations.find(name);
   if (rotI != rotations.end())
      {
      copy(rotI->second.begin(), rotI->second.end(),
           inserter(dataBlockNames, dataBlockNames.begin()));
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// Return the data block names for a given rotation.
// ------------------------------------------------------------------
void RotationAddIn::addRotation(const std::string& name, const DataBlockNames& dataBlockNames)
   {
   rotations.insert(Rotations::value_type(name, dataBlockNames));
   }
// ------------------------------------------------------------------
// The class responsible for calculating a 'score' by comparing a base
// data block name to another datablock name.
// ------------------------------------------------------------------
class ScoreNameAgainst
   {
   private:
      const string baseName;
   public:
      ScoreNameAgainst(const string& basename) : baseName(basename) { }
      unsigned scoreAgainst(const string& name)
         {
         unsigned index = 0;
         while (baseName[index] != '\0' &&
                name[index] != '\0' &&
                baseName[index] == name[index])
            index++;

         // see if 2 strings are identical
         if (baseName[index] == '\0' && name[index] == '\0')
            return index;
         return index;
         }
      bool operator()(const string& dataBlockName1, const string& dataBlockName2)
         {
         return (scoreAgainst(dataBlockName1) < scoreAgainst(dataBlockName2));
         }
   };
// ------------------------------------------------------------------
// Partition all datablock names into rotations by using a scoring
// algorithm that compares datablock names to each other.
// ------------------------------------------------------------------
void RotationAddIn::partitionFilesIntoRotations(void)
   {
   clearRotations();
   if (working != NULL)
      {
      vector<string> unknownDataBlocks;
      vector<string> dataBlockNames;
      working->getAllDataBlockNames(dataBlockNames);

      vector<string>::iterator dataBlockI = dataBlockNames.begin();
      while (dataBlockI != dataBlockNames.end())
         {
         string rotationName = *dataBlockI;
         dataBlockI = dataBlockNames.erase(dataBlockI);

         // work out which data blocks belong to this rotation by finding
         // the highest scoring datablock name and then including it and all
         // other datablock names with that score.

         vector<string> rotationDataBlocks;
         rotationDataBlocks.push_back(rotationName);

         ScoreNameAgainst baseName(rotationName);
         vector<string>::iterator rotationBlockI
            = max_element(dataBlockNames.begin(), dataBlockNames.end(), baseName);
         unsigned score = baseName.scoreAgainst(*rotationBlockI);

         // only assume we have a match if the score is greater than
         // an arbitary value of 5.  Otherwise add this base data block
         // to an 'unknown files' rotation.
         if (score >= 5)
            {
            // go find all datablock names with our score.  When found, remove
            // them from the datablocknames list and include them in the
            // rotationdatablocks list.
            rotationBlockI = dataBlockNames.begin();
            while (rotationBlockI != dataBlockNames.end())
               {
               if (baseName.scoreAgainst(*rotationBlockI) == score)
                  {
                  rotationDataBlocks.push_back(*rotationBlockI);
                  rotationBlockI = dataBlockNames.erase(rotationBlockI);
                  }
               else
                  rotationBlockI++;
               }
            // Remove all characters after the score position.  Assumes the
            // score is actually a character position within the string.
            rotationName.erase(score);

            // store this rotation
            addRotation(rotationName, rotationDataBlocks);
            }
         else
            unknownDataBlocks.push_back(rotationName);
         }

      // if we have unknown data blocks then create an unknown files rotation.
      if (unknownDataBlocks.size() > 0)
         addRotation("Unknown files", unknownDataBlocks);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    given the source data object, and the list of user selected
//    scenarios, perform all calculations and store all new data
//    in the returned TAPSTable.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
void RotationAddIn::doCalculations(TAPSTable& data)
   {
   if (needsUpdating && rotationAnalysisOn)
      {
      ensureRotationMappingStillValid();

      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      TAPSTable* destData = new TAPSTable(NULL);
      destData->beginStoringData();

      for (Rotations::iterator rotationI = rotations.begin();
                               rotationI != rotations.end();
                               rotationI++)
         processRotation(data, *destData, rotationI);

      destData->endStoringData();
      data.storeData(*destData);
      delete destData;

      needsUpdating = false;
      Screen->Cursor = savedCursor;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Process a single crop rotation ie. multiple datablocks that
//    have the same title as the data block passed in.  Return
//    true if more data blocks to process.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
bool RotationAddIn::processRotation(TAPSTable& data,
                                    TAPSTable& destData,
                                    Rotations::iterator rotationI)
   {
   // get a list of all fieldnames.
   vector<string> fieldNames;
   data.getFieldNamesMinusPivots(fieldNames);

   // get the name of the year field.
   string yearFieldName = data.getYearFieldName();

   // get the rotation name
   string rotationName = rotationI->first;

   // setup some storage for our values indexed by year.
   RotationValues values(fieldNames);

   int firstYear = 0;
   int lastYear = 10000;

   // loop through all data blocks, all records within a datablock
   // and all fields in each record.
   unsigned numDataBlocks = 0;
   bool ok = data.first();
   CropFields cropFields(data.begin());
   while (ok)
      {

      if (find(rotationI->second.begin(),
               rotationI->second.end(),
               data.begin()->getFieldValue("Simulation")) != rotationI->second.end())
         {
         int firstDataBlockYear = StrToInt(data.begin()->getFieldValue(yearFieldName).c_str());
         int lastDataBlockYear = 0;

         for (vector<TAPSRecord>::const_iterator recordI = data.begin();
                                                 recordI != data.end();
                                                 recordI++)
            {
            int year = StrToInt(recordI->getFieldValue(yearFieldName).c_str());
            lastDataBlockYear = max(lastDataBlockYear, year);

            for (vector<string>::iterator fieldI = fieldNames.begin();
                                          fieldI != fieldNames.end();
                                          fieldI++)
               {
               string value = recordI->getFieldValue(*fieldI);
               bool addToRecords = false;

               // For crop variables where the crop was sown in the
               // current year, add the value to our Values container.
               // For non-crop variables always add the value to our Values
               // container.
               if (cropFields.isCropField(*fieldI))
                  addToRecords = cropFields.cropWasSown(*recordI, *fieldI);
               else
                  addToRecords = true;

               if (addToRecords)
                  {
                  unsigned fieldNum = fieldI - fieldNames.begin();
                  values.addValue(year, fieldNum, numDataBlocks, value);
                  }
               }
            }
         firstYear = max(firstYear, firstDataBlockYear);
         lastYear = min(lastYear, lastDataBlockYear);
         numDataBlocks++;
         }

      ok = data.next();
      }

   // Tell values to do its output.
   if (numDataBlocks > 0)
      {
      values.writeToDataset(rotationName, destData, firstYear, lastYear, numDataBlocks);

      // Give all field names to destination dataset.
      for (vector<string>::iterator fieldI = fieldNames.begin();
                                    fieldI != fieldNames.end();
                                    fieldI++)

         destData.addField(addPerYearToFieldName(*fieldI));
      destData.markFieldAsAPivot("Simulation");
      }

   return ok;
   }

// ------------------------------------------------------------------
// Make sure all field names have a per yr on the end.
// ------------------------------------------------------------------
string addPerYearToFieldName(string& fieldName)
   {
   if (fieldName[fieldName.length()-1] == ')')
      return fieldName.substr(0, fieldName.length()-1) + " per yr)";
   else
      return fieldName;
   }
// ------------------------------------------------------------------
// Ensure the previous rotation mapping is still valid.
// ------------------------------------------------------------------
void RotationAddIn::ensureRotationMappingStillValid(void)
   {
   vector<string> dataBlockNames;
   working->getAllDataBlockNames(dataBlockNames);

   unsigned numDataBlocks = 0;
   bool ok = true;
   for (Rotations::const_iterator rotationI = rotations.begin();
                                  rotationI != rotations.end();
                                  rotationI++)
      {
      const DataBlockNames& rotationDataBlockNames = rotationI->second;
      for (DataBlockNames::const_iterator j = rotationDataBlockNames.begin();
                                          j != rotationDataBlockNames.end();
                                          j++)
         {
         ok = (ok && find(dataBlockNames.begin(), dataBlockNames.end(), *j)
                     != dataBlockNames.end());
         numDataBlocks++;
         }
      }
   if (!ok || numDataBlocks != dataBlockNames.size())
      partitionFilesIntoRotations();
   }

