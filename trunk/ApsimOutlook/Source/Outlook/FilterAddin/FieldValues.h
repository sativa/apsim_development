//---------------------------------------------------------------------------
#ifndef FieldValuesH
#define FieldValuesH
#include <ParseExpr.hpp>
#include <TAPSRecord.h>
#include "Flt_Box.hpp"
// ------------------------------------------------------------------
//  Short description:
//    This class encapsulates all field values stored in a single
//    record.  This is then fed into the Expression Parser.

//  Changes:
//    DPH 1/7/2001

// ------------------------------------------------------------------
class FieldValues
   {
   public:
      FieldValues(void) { }
      void clearFields(void);
      bool hasBeenSetup(void) {return (fieldInfo.size() > 0);}
      void setupFields(const TAPSRecord& record);
      void giveFieldsToParser(TExpressionParser& parser);
      void giveFieldsToFilterBox(TPSCFltBox& filterBox);

      void updateFieldValues(const TAPSRecord& record);

   private:

      struct FieldInfo
         {
         FieldInfo(const AnsiString& n, bool isn, unsigned ndx)
            : name(n), isNumeric(isn), vectorIndex(ndx)
            { }
         AnsiString name;
         bool isNumeric;
         unsigned vectorIndex;
         };
      std::vector<FieldInfo> fieldInfo;
      std::vector<double> numericValues;
      std::vector<AnsiString> stringValues;

   };
#endif
