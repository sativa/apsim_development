//---------------------------------------------------------------------------

#ifndef CropFieldsH
#define CropFieldsH
#include <vector>
#include <string>
#include <TAPSRecord.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimSettings.h>
// ---------------------------------------------------------------------------
// This class handles all the mess to do with crop fields
// ie. it handles the crop acronyms on field names like wht_yield, can_yield etc.
// ---------------------------------------------------------------------------
class CropFields
   {
   public:
      CropFields(const TAPSRecord* record);

      // Return a list of crop names by looking through the field names
      // passed in on the record passed in.  The algorithm uses a list
      // of crop acronyms read in the applications .ini file.
      // If it finds one of these at the start of a field name then it
      // is added to the list of crops.
      void getCropAcronyms(const TAPSRecord& record,
                           std::vector<std::string>& cropAcronyms) const;

      // convert the crop acronym to a real crop name.
      std::string realCropName(const std::string& cropAcronym);

      // Returns the value of a specific crop field for the specified record.
      // Returns true if all went ok.
      bool getCropValue(const TAPSRecord& record,
                        const std::string& fieldIdentifier,
                        const std::string& cropAcronym,
                        std::string& value) const;

      // Returns the value of a specific crop field for the specified record
      // AS A FLOAT.  Returns true if all went ok.
      bool getCropValue(const TAPSRecord& record,
                        const std::string& fieldIdentifier,
                        const std::string& cropAcronym,
                        float& value) const;

      // Return true if the specified field is a crop field.
      bool isCropField(const std::string& fieldName) const;

      // Return true if the crop for the specified field was actually sown
      // for the current record.  A crop is sown if:
      //    there is a least 1 non zero value in the fields for this crop OR
      //    the crop_fail field (if it exists) has a 'yes' in it.
      bool cropWasSown(const TAPSRecord& recordI, const std::string& fieldName) const;

      // Return true if the crop for the specified acronym was actually sown
      // for the current record.  A crop is sown if:
      //    there is a least 1 non zero value in the fields for this crop OR
      //    the crop_fail field (if it exists) has a 'yes' in it.
      bool cropWasSownByAcronym(const TAPSRecord& recordI, const string& cropAcronym) const;

      // Return a field name for the specified crop and the specified
      // field identifier.
      std::string getCropFieldName(const TAPSRecord& recordI,
                                   const std::string& fieldIdentifier,
                                   const std::string& cropAcronym) const;

   private:
      std::vector<std::string> recogCropAcronyms;
      std::vector<std::string> recogCropNames;
      std::vector<std::string> fieldNames;
      std::vector<std::string> fieldNamesMinusUnits;
      std::vector<std::string> cropAcronyms;
      ApsimSettings settings;
      bool cropFieldExists;

      // Return true if there is any field for the specified crop, in the
      // specified record that has a non zero value.
      bool cropHasNonZeroValue(const TAPSRecord& recordI,
                               const std::string& crop_acronym) const;

      void calcCropAcronyms(void);


   };
#endif
