//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "RotAddIn.h"
#include "TRotationForm.h"
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\ini_file.h>
#include <general\path.h>
#include <map>
#pragma resource "Rotation.res"

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
//  Short description:
//    This class is a helper class to keep track of all the field
//    values for a given year and then to calculate an average when
//    asked to do so.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
class YearValues
   {
   private:

      class FieldValue
         {
         private:
            string stringValue;
            double numericalValue;
            unsigned int count;
            enum FieldValueType {str, numerical, unknown};
            FieldValueType type;
         public:
            FieldValue(void) : type(unknown), count(0) { }

            unsigned int getCount(void) {return count;}
            void addValue(const string& value)
               {
               if (type == unknown)
                  {
                  char* endptr;
                  numericalValue = strtod(value.c_str(), &endptr);
                  if (*endptr == 0)
                     type = numerical;
                  else
                     {
                     type = str;
                     stringValue = value;
                     }
                  }
               else if (type == numerical)
                  {
                  char* endptr;
                  numericalValue += strtod(value.c_str(), &endptr);
                  }
               count++;
               }
            string getValue(void)
               {
               if (type == numerical)
                  {
                  if (count == 0)
                     return "Divide by zero";
                  else
                     {
                     char buffer[20];
                     sprintf(buffer, "%10.3f", numericalValue / count);
                     return buffer;
                     }
                  }
               else
                  return stringValue;
               }
         };

      vector<FieldValue> values;
   public:
      YearValues(void) { }

      void addValue(unsigned int valueIndex, const string& value)
         {
         for (unsigned i = values.size(); i <= valueIndex; i++)
            values.push_back(FieldValue());
         values[valueIndex].addValue(value);
         }
      unsigned int getCount(void)
         {
         if (values.size() == 0)
            return 0;
         else
            return values[0].getCount();
         }
      string getValue(unsigned int valueIndex)
         {
         return values[valueIndex].getValue();
         }

   };

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
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      TAPSTable* destData = new TAPSTable(NULL);

      bool ok = data.first();
      while (ok)
         ok = processRotation(data, *destData);

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
bool RotationAddIn::processRotation(TAPSTable& data, TAPSTable& destData)
   {
   // For the provide a concrete example consider this block of data.
   // sow_year  residue_wt   nw_sowdate   nw_yield  nw_tops_biom  so_sowdate   so_biom
   //   1957        18.856       0            0         0         01/11/1957   9925.227
   //   1958      5404.098   01/06/1958    2645.035   762.648         0           0
   //   1959      2656.253       0            0         0             0           0
   //   1960        16.922       0            0         0         01/11/1959  11647.982
   //   1961      5573.266   01/06/1961    2211.77    778.457         0           0
   //   1962        16.833       0            0         0             0           0
   //   1963         0.244       0            0         0         01/11/1962   9187.556
   //   1964      8461.154   01/06/1964    2135.836   1059.699        0           0

   // get a list of all fieldnames.
   vector<string> fieldNames;
   data.getFieldNamesMinusPivots(fieldNames);

   // get the name of the year field.
   string yearFieldName = data.getYearFieldName();

   // get rotation name we're to process.
   string rotationName = data.getDataBlockName();

   // go get a list of all crops (e.g. nw and so)
   getCropNames(fieldNames);

   // setup some storage for our values indexed by year.
   typedef map<int, YearValues> Values;
   Values values;

   int firstYear = 0;
   int lastYear = 10000;

   // loop through all data blocks, all records within a datablock
   // and all fields in each record.
   unsigned int numDataBlocks = 0;
   bool ok = true;
   while (ok)
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
            if (isCropVariable(*fieldI))
               addToRecords = cropWasSown(*recordI, *fieldI);
            else
               addToRecords = true;

            if (addToRecords)
               {
               if (values.find(year) == values.end())
                  values.insert(Values::value_type(year, YearValues()));
               values[year].addValue(fieldI - fieldNames.begin(), value);
               }
            }
         }

      firstYear = max(firstYear, firstDataBlockYear);
      lastYear = min(lastYear, lastDataBlockYear);

      numDataBlocks++;
      ok = data.next();
      }

   // output a single data block containing all years and all averaged
   // numerical field values.  Only consider years that are covered by
   // all datablocks (remember, each data block is offset by a year).
   destData.beginStoringData();
   destData.clearRecords();
   for (Values::iterator valueI = values.begin();
                         valueI != values.end();
                         valueI++)
      {
      if (valueI->first >= firstYear && valueI->first <= lastYear)
         {
         TAPSRecord newRecord;
         for (vector<string>::iterator fieldI = fieldNames.begin();
                                       fieldI != fieldNames.end();
                                       fieldI++)
            {
            unsigned int fieldIndex = fieldI - fieldNames.begin();
            if (fieldIndex == 0)
               newRecord.setFieldValue("Simulation", rotationName);
            else
               newRecord.setFieldValue(*fieldI, valueI->second.getValue(fieldIndex));
            }
         destData.storeRecord(newRecord);
         }
      }
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      destData.addField(*fieldI);
   destData.markFieldAsAPivot("Simulation");

   destData.endStoringData();
   return ok;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return a list of crop names by looking through the field names
//    passed in.  The algorithm uses a list of crop acronyms read in
//    from the rotation .ini file.  If it finds one of these at the
//    start of a field name then it is added to the list of crops.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
void RotationAddIn::getCropNames(std::vector<std::string>& fieldNames)
   {
   // get a list of all recognised crop acronyms
   Path iniPath(Application->ExeName.c_str());
   iniPath.Append_path("rotationaddin");
   iniPath.Set_name("rotationaddin.ini");
   Ini_file ini(iniPath.Get_path().c_str());
   string crop_acronym_string;
   ini.Read("crops", "crop_acronyms", crop_acronym_string);
   vector<string> crop_acronyms;
   Split_string(crop_acronym_string, " ", crop_acronyms);

   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      {
      for (vector<string>::iterator acronymI = crop_acronyms.begin();
                                  acronymI != crop_acronyms.end();
                                  acronymI++)
         {
         string acronym_ = *acronymI + "_";
         if (fieldI->find(acronym_) == 0
             && find(cropNames.begin(), cropNames.end(), *acronymI) == cropNames.end())
            {
            // found one - store the acronym.
            cropNames.push_back(*acronymI);
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Return true if the specified field is a crop field.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
bool RotationAddIn::isCropVariable(const string& fieldName) const
   {
   unsigned posUnderscore = fieldName.find("_");
   if (posUnderscore != string::npos)
      {
      return (find(cropNames.begin(), cropNames.end(),
                   fieldName.substr(0, posUnderscore)) != cropNames.end());
      }
   else
      return false;
   }

// ------------------------------------------------------------------
//  Short description:
//    Return true if the specified crop was actually sown
//    for the current record.  A crop is sown if:
//       there is a least 1 non zero value in the fields for this crop OR
//       the crop_fail field (if it exists) has a 'yes' in it.

//  Changes:
//    DPH 9/8/2001
// ------------------------------------------------------------------
bool RotationAddIn::cropWasSown(const TAPSRecord& recordI, const std::string& fieldName) const
   {
   unsigned posUnderscore = fieldName.find("_");
   if (posUnderscore != string::npos)
      {
      string crop_acronym = fieldName.substr(0, posUnderscore);
      string failedFieldName = fieldName.substr(0, posUnderscore) + "_fail";
      string failedValue = recordI.getFieldValue(failedFieldName);
      return (Str_i_Eq(failedValue, "yes") || CropHasNonZeroValue(recordI, crop_acronym));
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return true if there is any field for the specified crop, in the
// specified record that has a non zero value.
// ------------------------------------------------------------------
bool RotationAddIn::CropHasNonZeroValue(const TAPSRecord& recordI,
                                        const string& crop_acronym) const
   {
   vector<string> fieldNames = recordI.getFieldNames();
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      {
      string acronym_ = crop_acronym + "_";
      if (fieldI->find(acronym_) == 0)
         {
         string Value = recordI.getFieldValue(*fieldI);
         if (Is_numerical(Value.c_str()))
            return (StrToFloat(Value.c_str()) != 0.0);
         }
      }
   return false;
   }

