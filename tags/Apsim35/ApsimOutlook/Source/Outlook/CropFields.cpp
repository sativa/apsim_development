//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "CropFields.h"
#include <general\path.h>
using namespace std;
//---------------------------------------------------------------------------

#pragma package(smart_init)

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
CropFields::CropFields(const TAPSRecord* record)
   {
   // get a list of all recognised crop acronyms
   string st;
   settings.read("Outlook Crops|crop_acronyms", st);
   Split_string(st, ",", recogCropAcronyms);
   settings.read("Outlook Crops|crop_names", st);
   Split_string(st, ",", recogCropNames);

   for (vector<string>::iterator i = recogCropAcronyms.begin();
                                 i != recogCropAcronyms.end();
                                 i++)
      stripLeadingTrailing(*i, " ");
   for (vector<string>::iterator i = recogCropNames.begin();
                                 i != recogCropNames.end();
                                 i++)
      stripLeadingTrailing(*i, " ");

   // get a list of field names
   if (record != NULL)
      {
      string cropName = record->getFieldValue("Crop");
      cropFieldExists = (cropName != "");

      fieldNames = record->getFieldNames();

      // now create a list of field names minus the units.
      for (vector<string>::iterator i = fieldNames.begin();
                                    i != fieldNames.end();
                                    i++)
         {
         unsigned posUnits = i->find("(");
         if (posUnits == string::npos)
            fieldNamesMinusUnits.push_back(*i);
         else
            {
            string fieldNameMinusUnits = i->substr(0, posUnits);
            stripLeadingTrailing(fieldNameMinusUnits, " ");
            fieldNamesMinusUnits.push_back(fieldNameMinusUnits);
            }
         }

      calcCropAcronyms();
      }
   }

// ------------------------------------------------------------------
// Function object for converting a crop acronym to a crop name.
// ------------------------------------------------------------------
class AcronymToName
   {
   private:
      const vector<string>& recogCropAcronyms;
      const vector<string>& recogCropNames;
   public:
      AcronymToName(const vector<string>& recognisedCropAcronyms,
                    const vector<string>& recognisedCropNames)
         : recogCropAcronyms(recognisedCropAcronyms),
           recogCropNames(recognisedCropNames)
         { }

      string operator() (const string& cropAcronym) const
         {
         vector<string>::const_iterator acronymI =
            find(recogCropAcronyms.begin(), recogCropAcronyms.end(),
                 cropAcronym);
         if (acronymI != recogCropAcronyms.end())
            return recogCropNames[acronymI - recogCropAcronyms.begin()];
         else
            return cropAcronym;
         }
   };
// ------------------------------------------------------------------
// convert the crop acronym to a real crop name.
// ------------------------------------------------------------------
std::string CropFields::realCropName(const std::string& cropAcronym)
   {
   return AcronymToName(recogCropAcronyms, recogCropNames)(cropAcronym);
   }

// ------------------------------------------------------------------
// Calculate a list of crop names by looking through the field names.
// The algorithm uses a list of crop acronyms read in the applications
// .ini file. If it finds one of these at the start of a field name then it
// is added to the list of crops.
// ------------------------------------------------------------------
void CropFields::calcCropAcronyms(void)
   {
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end();
                                 fieldI++)
      {
      for (vector<string>::const_iterator acronymI = recogCropAcronyms.begin();
                                          acronymI != recogCropAcronyms.end();
                                          acronymI++)
         {
         string acronym_ = *acronymI + "_";
         if (fieldI->find(acronym_) == 0
             && find(cropAcronyms.begin(), cropAcronyms.end(), *acronymI) == cropAcronyms.end())
            {
            // found one - store the acronym.
            cropAcronyms.push_back(*acronymI);
            }
         }
      }
   }

// ------------------------------------------------------------------
// Return a list of crop names by looking through the field names
// passed in on the record passed in.  The algorithm uses a list
// of crop acronyms read in the applications .ini file.
// If it finds one of these at the start of a field name then it
// is added to the list of crops.
// ------------------------------------------------------------------
void CropFields::getCropAcronyms(const TAPSRecord& record,
                                 std::vector<std::string>& crops) const
   {
   if (!cropFieldExists)
      copy(cropAcronyms.begin(), cropAcronyms.end(), back_inserter(crops));

   else
      {
      string cropName = record.getFieldValue("Crop");
      crops.push_back(cropName);
      }
   }


// ------------------------------------------------------------------
// Return true if the crop for the specified field was actually sown
// for the current record.  A crop is sown if:
//    there is a least 1 non zero value in the fields for this crop OR
//    the crop_fail field (if it exists) has a 'yes' in it.
// ------------------------------------------------------------------
bool CropFields::cropWasSown(const TAPSRecord& recordI, const string& fieldName) const
   {
   unsigned posUnderscore = fieldName.find("_");
   if (posUnderscore != string::npos)
      {
      string cropAcronym = fieldName.substr(0, posUnderscore);
      return cropWasSownByAcronym(recordI, cropAcronym);
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return true if the crop for the specified acronym was actually sown
// for the current record.  A crop is sown if:
//    there is a least 1 non zero value in the fields for this crop OR
//    the crop_fail field (if it exists) has a 'yes' in it.
// ------------------------------------------------------------------
bool CropFields::cropWasSownByAcronym(const TAPSRecord& recordI, const string& cropAcronym) const
   {
   string failedFieldName = cropAcronym + "_fail";
   string failedValue = recordI.getFieldValue(failedFieldName);
   return (Str_i_Eq(failedValue, "yes") || cropHasNonZeroValue(recordI, cropAcronym));
   }

// ------------------------------------------------------------------
// Return true if the specified field is a crop field.
// ------------------------------------------------------------------
bool CropFields::isCropField(const string& fieldName) const
   {
   unsigned posUnderscore = fieldName.find("_");
   if (posUnderscore != string::npos)
      {
      return (find(recogCropAcronyms.begin(), recogCropAcronyms.end(),
                   fieldName.substr(0, posUnderscore)) != recogCropAcronyms.end());
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return true if there is any field for the specified crop, in the
// specified record that has a non zero value.
// ------------------------------------------------------------------
bool CropFields::cropHasNonZeroValue(const TAPSRecord& recordI,
                                     const string& crop_acronym) const
   {
   if (cropFieldExists)
      {
      string yieldValue;
      getCropValue(recordI, "yield", crop_acronym, yieldValue);
      if (yieldValue != "" && Is_numerical(yieldValue.c_str()))
         return (StrToFloat(yieldValue.c_str()) != 0.0);
      }
   else
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
            if (Is_numerical(Value.c_str()) && StrToFloat(Value.c_str()) != 0.0)
               return true;
            }
         }
      }
   return false;
   }

// ------------------------------------------------------------------
// Returns the value of a specific crop field for the specified record.
// Returns true if all went ok.
// ------------------------------------------------------------------
bool CropFields::getCropValue(const TAPSRecord& record,
                              const string& fieldIdentifier,
                              const string& cropAcronym,
                              std::string& value) const
   {
   string fieldName = getCropFieldName(record, fieldIdentifier, cropAcronym);
   if (fieldName == "")
      return false;
   else
      {
      value = record.getFieldValue(fieldName);
      return true;
      }
   }
// ------------------------------------------------------------------
// Returns the value of a specific crop field for the specified record
// AS A FLOAT.  Returns true if all went ok.
// ------------------------------------------------------------------
bool CropFields::getCropValue(const TAPSRecord& record,
                              const string& fieldIdentifier,
                              const string& cropName,
                              float& value) const
   {
   string stringValue;
   if (getCropValue(record, fieldIdentifier, cropName, stringValue))
      {
      try
         {
         value = StrToFloat(stringValue.c_str());
         return true;
         }
      catch (Exception& error)
         {
         value = 0.0;
         return true;
         }
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Return a field name from the specified record,
// for the specified crop and field identifier.
// ------------------------------------------------------------------
std::string CropFields::getCropFieldName(const TAPSRecord& recordI,
                                         const std::string& fieldIdentifier,
                                         const std::string& cropAcronym) const
   {
   // get a list of all recognised crop acronyms
   string st;
   settings.read("Outlook Crops|" + fieldIdentifier, st);
   vector<string> recognisedFieldNames;
   Split_string(st, ",", recognisedFieldNames);

   vector<string> fieldNames = recordI.getFieldNames();

   for (vector<string>::const_iterator recognisedFieldI = recognisedFieldNames.begin();
                                       recognisedFieldI != recognisedFieldNames.end();
                                       recognisedFieldI++)
      {
      vector<string>::const_iterator fieldI = find_if
         (fieldNamesMinusUnits.begin(), fieldNamesMinusUnits.end(),
          CaseInsensitiveStringComparison(*recognisedFieldI));
      if (fieldI == fieldNamesMinusUnits.end())
         fieldI = find_if
            (fieldNamesMinusUnits.begin(), fieldNamesMinusUnits.end(),
             CaseInsensitiveStringComparison(cropAcronym + "_" + *recognisedFieldI));
      if (fieldI != fieldNamesMinusUnits.end())
         return fieldNames[fieldI-fieldNamesMinusUnits.begin()];
      }
   return "";
   }

