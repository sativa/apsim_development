#include <general\pch.h>
#pragma hdrstop

#include "TypeConverter.h"
#include "Component.h"
#include <limits.h>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

using namespace std;
using namespace protocol;
char buffer[1000];
MessageData TypeConverter::bufferMessageData(buffer, sizeof(buffer));

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
         bufferMessageData << FString(result, strlen(result));
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
         sprintf(st, "%10.3f", value);
         bufferMessageData << FString(st, strlen(st));
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
         sprintf(st, "%10.3f", value);
         bufferMessageData << FString(st, strlen(st));
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
            bufferMessageData << FString("yes", 3);
         else
            bufferMessageData << FString("no", 2);
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
         bufferMessageData << FString(&value, 1);
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
class BooleanFromString : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         FString value;
         messageData >> value;
         bufferMessageData << (bool) (value == "yes");
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
class StringFromDoubleA : public TypeConverter
   {
   public:
      void doConvert(MessageData& messageData)
         {
         unsigned int numValues;
         messageData >> numValues;
         unsigned pos = 4;
         for (unsigned int v = 0; v < numValues; v++)
            {
            double value;
            messageData >> value;
            sprintf(&buffer[pos], "%10.3f", value);
            pos += 10;
            }
         bufferMessageData << numValues;
         }
      virtual TypeConverter* clone(void)
         {
         return new StringFromDoubleA;
         }
   } stringFromDoubleA;

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
/*S   int4*/    {  NULL,    NULL,    NULL,            NULL, &int4FromSingle,        NULL,              NULL,               NULL,        &int4FromString},
/*T   int8*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*    single*/  {  NULL,    NULL,    NULL,            NULL,   NULL,              &singleFromDouble,    NULL,               NULL,        &singleFromString},
/*    double*/  {  NULL,    NULL,    NULL,            NULL, &doubleFromSingle,      NULL,              NULL,               NULL,            NULL},
/*    boolean*/ {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,        &booleanFromString},
/*    char*/    {  NULL,    NULL,    NULL,            NULL,   NULL,                 NULL,              NULL,               NULL,            NULL},
/*    string*/  {  NULL,    NULL,  &stringFromInt4,   NULL, &stringFromSingle,   &stringFromDouble, &stringFromBoolean, &stringFromChar,&stringFromString},
     };

static TypeConverter* arrayToStringConversionMatrix[9] =  {
//                                             SOURCE (FROM)
//                 int1      int2    int4    int8   single  double              boolean     char    string
/*    string*/     NULL,    NULL,   NULL,   NULL,  NULL,   &stringFromDoubleA,   NULL,      NULL,   NULL
     };


// ------------------------------------------------------------------
//  Short description:
//     Return a data type converter if possible or NULL if none
//     available.

//  Changes:
//    DPH 7/6/2001
// ------------------------------------------------------------------
bool protocol::getTypeConverter(Component* parent,
                                const FString& name,
                                Type& sourceType,
                                Type& destType,
                                TypeConverter*& converter)
   {
   converter = NULL;
   if (sourceType.getCode() == DTunknown && destType.getCode() == DTunknown)
      converter = NULL;
   else
      {
      if (sourceType.getCode() == destType.getCode() &&
          sourceType.isArray() == destType.isArray() &&
          sourceType.getCode() != DTunknown)
         converter = NULL;
      else
         {
         if (sourceType.getCode() == DTunknown || destType.getCode() == DTunknown)
            converter = NULL;

         else if (sourceType.isArray())
            {
            // source is an array - we support only array to array converters.
            // or array to string converters.
            if (destType.isArray())
               {
               ArrayFromArray* Aconverter = &arrayFromArray;
               Aconverter->baseConverter = scalarConversionMatrix[destType.getCode()][sourceType.getCode()];
               if (Aconverter->baseConverter != NULL)
                  converter = Aconverter;
               }
            else if (destType.getCode() == DTstring)
               converter = arrayToStringConversionMatrix[sourceType.getCode()];
            }
         else
            {
            // source is a scalar - we support scalar to arrays and normal
            // scalar to scalar.
            if (destType.isArray())
               {
               ArrayFromScalar* Aconverter = &arrayFromScalar;
               Aconverter->baseConverter = scalarConversionMatrix[destType.getCode()][sourceType.getCode()];
               if (Aconverter->baseConverter != NULL)
                  converter = Aconverter;
               }
            else
               converter = scalarConversionMatrix[destType.getCode()][sourceType.getCode()];
            }

         if (converter == NULL)
            {
            char msg[300];
            strcpy(msg, "Cannot create a type converter.");
            strcat(msg, "\nSource type: ");
            strncat(msg, sourceType.getTypeString().f_str(), sourceType.getTypeString().length());
            strcat(msg, "\nDestination type: ");
            strncat(msg, destType.getTypeString().f_str(), destType.getTypeString().length());
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

