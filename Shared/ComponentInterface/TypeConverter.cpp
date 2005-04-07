#include <windows.h>
#pragma hdrstop

#include "TypeConverter.h"
#include "Component.h"
#include <limits.h>
#include <stdio.h>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

using namespace std;
using namespace protocol;
char buffer[10000];
MessageData bufferMD(buffer, sizeof(buffer));


TypeConverter::TypeConverter(void)
   : bufferMessageData(bufferMD)
   {
   }

class Int4FromInt4 : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         int value;
         messageData >> value;
         bufferMessageData << value;
         }
      virtual TypeConverter* clone(void)
         {
         return new Int4FromInt4;
         }
   } int4FromInt4;
class SingleFromSingle : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         float value;
         messageData >> value;
         bufferMessageData << value;
         }
      virtual TypeConverter* clone(void)
         {
         return new SingleFromSingle;
         }
   } singleFromSingle;
class DoubleFromDouble : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         double value;
         messageData >> value;
         bufferMessageData << value;
         }
      virtual TypeConverter* clone(void)
         {
         return new DoubleFromDouble;
         }
   } doubleFromDouble;

class SingleFromInt4 : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         int value;
         messageData >> value;
         float result = value;

         bufferMessageData << result;
         }
      virtual TypeConverter* clone(void)
         {
         return new SingleFromInt4;
         }
   } singleFromInt4;

class Int4FromSingle : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         float value;
         messageData >> value;
         int result = value;

         bufferMessageData << result;
         }
      virtual TypeConverter* clone(void)
         {
         return new Int4FromSingle;
         }
   } int4FromSingle;

class StringFromInt4 : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         int value;
         messageData >> value;
         char result[100];
         itoa(value, result, 10);
         bufferMessageData << FString(result, strlen(result), CString);
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromInt4;
         }
   } stringFromInt4;
class StringFromSingle : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         float value;
         messageData >> value;
         char st[100];
         sprintf(st, "%f", value);
         bufferMessageData << FString(st);
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromSingle;
         }
   } stringFromSingle;
class StringFromDouble : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         double value;
         messageData >> value;
         char st[100];
         sprintf(st, "%f", value);
         bufferMessageData << FString(st);
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromDouble;
         }
   } stringFromDouble;

class StringFromBoolean : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         bool value;
         messageData >> value;
         if (value)
            bufferMessageData << FString("1");
         else
            bufferMessageData << FString("0");
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromBoolean;
         }
   } stringFromBoolean;

class StringFromChar : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         char value;
         messageData >> value;
         bufferMessageData << FString(&value, 1, CString);
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromChar;
         }
   } stringFromChar;
class Int4FromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         char buffer[50];
         strncpy(buffer, value.f_str(), value.length());
         buffer[value.length()] = 0;
         bufferMessageData << atoi(buffer);
         }
      virtual TypeConverter* clone(void)
         {
         return new Int4FromString;
         }
   } int4FromString;
class SingleFromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         char buffer[50];
         strncpy(buffer, value.f_str(), value.length());
         buffer[value.length()] = 0;
         float f = atof(buffer);
         bufferMessageData << f;
         }
      virtual TypeConverter* clone(void)
         {
         return new SingleFromString;
         }
   } singleFromString;
class DoubleFromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         char buffer[50];
         strncpy(buffer, value.f_str(), value.length());
         buffer[value.length()] = 0;
         double f = atof(buffer);
         bufferMessageData << f;
         }
      virtual TypeConverter* clone(void)
         {
         return new DoubleFromString;
         }
   } doubleFromString;
class BooleanFromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         bufferMessageData << (bool) (value == "1");
         }
      virtual TypeConverter* clone(void)
         {
         return new BooleanFromString;
         }
   } booleanFromString;
class SingleFromDouble : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         double value;
         messageData >> value;
         float result = value;

         bufferMessageData << result;
         }
      virtual TypeConverter* clone(void)
         {
         return new SingleFromDouble;
         }
   } singleFromDouble;
class DoubleFromSingle : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         float value;
         messageData >> value;
         double result = value;

         bufferMessageData << result;
         }
      virtual TypeConverter* clone(void)
         {
         return new DoubleFromSingle;
         }
   } doubleFromSingle;

class StringFromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         bufferMessageData << value;
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromString;
         }
   } stringFromString;

class ArrayFromScalar : public TypeConverter
   {
   public:
      ArrayFromScalar(void) : doDelete(false) {}
      ~ArrayFromScalar(void) {if (doDelete) delete baseConverter;}
      TypeConverter* baseConverter;
      bool doDelete;
      void doConvert(MessageData& messageData)
         {
         bufferMessageData << 1;
         baseConverter->doConvert(messageData);
         }
      virtual TypeConverter* clone(void)
         {
         ArrayFromScalar* newConverter = new ArrayFromScalar;
         newConverter->baseConverter = baseConverter->clone();
         newConverter->doDelete = true;
         return newConverter;
         }
   } arrayFromScalar;
class ScalarFromArray : public TypeConverter
   {
   public:
      ScalarFromArray(void) : doDelete(false) {}
      ~ScalarFromArray(void) {if (doDelete) delete baseConverter;}
      TypeConverter* baseConverter;
      bool doDelete;
      void doConvert(MessageData& messageData)
         {
         unsigned numvals;
         messageData >> numvals;
         if (numvals > 0)
            baseConverter->doConvert(messageData);
         }
      virtual TypeConverter* clone(void)
         {
         ScalarFromArray* newConverter = new ScalarFromArray;
         newConverter->baseConverter = baseConverter->clone();
         newConverter->doDelete = true;
         return newConverter;
         }
   } scalarFromArray;
class ArrayFromArray : public TypeConverter
   {
   public:
      TypeConverter* baseConverter;
      bool doDelete;
      ArrayFromArray(void) : doDelete(false) {}
      ~ArrayFromArray(void) {if (doDelete) delete baseConverter;}

      void doConvert(MessageData& messageData)
         {
         int numValues;
         messageData >> numValues;
         bufferMessageData << numValues;
         for (int v = 0; v < numValues; v++)
            baseConverter->doConvert(messageData);
         }
      virtual TypeConverter* clone(void)
         {
         ArrayFromArray* newConverter = new ArrayFromArray;
         newConverter->baseConverter = baseConverter->clone();
         newConverter->doDelete = true;
         return newConverter;
         }
   } arrayFromArray;

static TypeConverter* scalarConversionMatrix[9][9] =  {
//                                             SOURCE (FROM)
//                int1      int2    int4              int8   single                double              boolean             char             string
/*D   int1*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*E   int2*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*S   int4*/    {  NULL,    NULL,  &int4FromInt4,     NULL, &int4FromSingle,        NULL,              NULL,               NULL,        &int4FromString},
/*T   int8*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*    single*/  {  NULL,    NULL,  &singleFromInt4,   NULL, &singleFromSingle,   &singleFromDouble,    NULL,               NULL,        &singleFromString},
/*    double*/  {  NULL,    NULL,    NULL,            NULL, &doubleFromSingle,   &doubleFromDouble,    NULL,               NULL,        &doubleFromString},
/*    boolean*/ {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,        &booleanFromString},
/*    char*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*    string*/  {  NULL,    NULL,  &stringFromInt4,   NULL, &stringFromSingle,   &stringFromDouble, &stringFromBoolean, &stringFromChar,&stringFromString},
     };

// ------------------------------------------------------------------
//  Short description:
//     Return a data type converter if possible or NULL if none
//     available.

//  Changes:
//    DPH 7/6/2001
// ------------------------------------------------------------------
bool _export protocol::getTypeConverter(Component* parent,
                                const FString& name,
                                const Type& sourceType,
                                const Type& destType,
                                TypeConverter*& converter)
   {
   return getTypeConverter(parent, name,
                           sourceType.getCode(), destType.getCode(),
                           sourceType.isArray(), destType.isArray(),
                           converter);
   }

// ------------------------------------------------------------------
// Return a data type converter if possible or NULL if none
// available.
// ------------------------------------------------------------------
bool _export protocol::getTypeConverter(Component* parent,
                                const FString& name,
                                protocol::DataTypeCode sourceTypeCode,
                                protocol::DataTypeCode destTypeCode,
                                bool isSourceArray,
                                bool isDestArray,
                                TypeConverter*& converter)
   {
   converter = NULL;
   if (sourceTypeCode == DTunknown && destTypeCode == DTunknown)
      converter = NULL;
   else
      {
      if (sourceTypeCode == destTypeCode &&
          isSourceArray == isDestArray &&
          sourceTypeCode != DTunknown)
         converter = NULL;
      else
         {
         if (sourceTypeCode == DTunknown || destTypeCode == DTunknown)
            converter = NULL;

         else if (isSourceArray)
            {
            // source is an array - we support only array to array converters.
            // or array to string converters.
            if (isDestArray)
               {
               ArrayFromArray* Aconverter = &arrayFromArray;
               Aconverter->baseConverter = scalarConversionMatrix[destTypeCode][sourceTypeCode];
               if (Aconverter->baseConverter != NULL)
                  converter = Aconverter;
               }
            else
               {
               ScalarFromArray* Aconverter = &scalarFromArray;
               Aconverter->baseConverter = scalarConversionMatrix[destTypeCode][sourceTypeCode];
               if (Aconverter->baseConverter != NULL)
                  converter = Aconverter;
               }
            }
         else
            {
            // source is a scalar - we support scalar to arrays and normal
            // scalar to scalar.
            if (isDestArray)
               {
               ArrayFromScalar* Aconverter = &arrayFromScalar;
               Aconverter->baseConverter = scalarConversionMatrix[destTypeCode][sourceTypeCode];
               if (Aconverter->baseConverter != NULL)
                  converter = Aconverter;
               }
            else
               converter = scalarConversionMatrix[destTypeCode][sourceTypeCode];
            }

         if (converter == NULL)
            {
            FString sourceTypeString = Type::codeToString(sourceTypeCode);
            FString destTypeString = Type::codeToString(destTypeCode);

            char msg[300];
            strcpy(msg, "Cannot create a type converter.");
            strcat(msg, "\nSource type: ");
            strncat(msg, sourceTypeString.f_str(), sourceTypeString.length());
            if (isSourceArray)
               strcat(msg, " array");
            strcat(msg, "\nDestination type: ");
            strncat(msg, destTypeString.f_str(), destTypeString.length());
            if (isDestArray)
               strcat(msg, " array");
            strcat(msg, "\nVariable name: ");
            strncat(msg, name.f_str(), name.length());
            parent->error(msg, true);
            return false;
            }
         else
            converter = converter->clone();
         }
      }
   return true;
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

