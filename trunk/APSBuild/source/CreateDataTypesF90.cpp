//---------------------------------------------------------------------------
#include <stdlib.h>

#include <fstream>
#include <algorithm>
#include <set>
#include <vector>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/ApsimDataTypeData.h>
#include <ApsimShared/ApsimDataTypesFile.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimRegistrationData.h>

#include "CreateDataTypesF90.h"

using namespace std;

set<string> routinesDoneSoFar;
ApsimDataTypesFile* dataTypes;
ApsimComponentData* component;

// ------------------------------------------------------------------
// Clean-up the name passed in, removing unwanted characters.
// ------------------------------------------------------------------
std::string getCleanName(const string& name)
   {
   std::string cleanName = name;
   Replace_all(cleanName, ".", "_");
   Replace_all(cleanName, "/", "");
   return cleanName;
   }
// ------------------------------------------------------------------
// convert a DDML 'kind' string to a intrinsic FORTRAN data type.
// ------------------------------------------------------------------
std::string KindToFORTRANType(const std::string& kind, bool isRoutineParameter = false)
   {
   if (Str_i_Eq(kind, "integer1"))
      return "integer*1";
   else if (Str_i_Eq(kind, "integer2"))
      return "integer*2";
   else if (Str_i_Eq(kind, "integer4"))
      return "integer";
   else if (Str_i_Eq(kind, "integer8"))
      return "integer*8";
   else if (Str_i_Eq(kind, "single"))
      return "real";
   else if (Str_i_Eq(kind, "double"))
      return "double precision";
   else if (Str_i_Eq(kind, "boolean"))
      return "logical";
   else if (Str_i_Eq(kind, "char"))
      return "character(len=1)";
   else if (Str_i_Eq(kind, "string"))
      {
      if (isRoutineParameter)
         return "character(len=*)";
      else
         return "character(len=max_char_size)";
      }
   else
      {
      string msg = "Unknown DDML kind: " + kind;
      cout << msg.c_str() << endl;
      return "????";
      }
   }

// ------------------------------------------------------------------
// convert a DDML 'kind' string to a intrinsic FORTRAN data type.
// ------------------------------------------------------------------
std::string getIntrinsicSizeOf(const ApsimDataTypeData& dataType)
   {
   string kind = dataType.getKind();
   if (Str_i_Eq(kind, "integer1"))
      return "1";
   else if (Str_i_Eq(kind, "integer2"))
      return "2";
   else if (Str_i_Eq(kind, "integer4"))
      return "4";
   else if (Str_i_Eq(kind, "integer8"))
      return "8";
   else if (Str_i_Eq(kind, "single"))
      return "4";
   else if (Str_i_Eq(kind, "double"))
      return "8";
   else if (Str_i_Eq(kind, "boolean"))
      return "1";
   else if (Str_i_Eq(kind, "char"))
      return "1";
   else if (Str_i_Eq(kind, "string"))
      {
      if (dataType.isArray())
         return "4+len_trim(value(i))";
      else
         return "4+len_trim(value)";
      }
   else
      throw runtime_error("Unknown kind: " + kind);
   }

// ------------------------------------------------------------------
// write a call to bound check for given datatype.
// ------------------------------------------------------------------
void writeBoundCheck(const ApsimDataTypeData& dataType, ostream& out,
                     const char* ref,
                     const string& variableName, const string& numValuesName)
   {
   out << "call bound_check_" << dataType.getKind();
   if (dataType.isArray())
      out << "_array";

   out << '('
       << variableName
       << ',' << dataType.getLowerBound()
       << ',' << dataType.getUpperBound()
       << ",'" << ref << dataType.getName() << "'";
   if (dataType.isArray())
      out  << ',' << numValuesName;
   out << ')' << endl;
   }

// ------------------------------------------------------------------
// write a declaration for the specified type
// ------------------------------------------------------------------
void writeDeclaration(const ApsimDataTypeData& dataType, ostream& out, const char* intent,
                      const string& variableName)
   {
   if (dataType.getKind() != "" || dataType.isStructure())
      {
      if (dataType.isStructure())
         out << "   type(" << getCleanName(dataType.getName())
             << "Type)";
      else
         out << "   " << KindToFORTRANType(dataType.getKind(), true);
      if (dataType.isArray())
         out << ", dimension(:)";
      out << ",intent(" << intent << ") :: " << variableName << endl;
      }
   }

// ------------------------------------------------------------------
// go find a data type for the specified registration.
// ------------------------------------------------------------------
ApsimDataTypeData getDataType(const ApsimRegistrationData& registration)
   {
   try
      {
      return component->getDataType(registration.getDataTypeName());
      }
   catch (const runtime_error& error)
      {
      }
   try
      {
      return dataTypes->getDataType(registration.getDataTypeName());
      }
   catch (const runtime_error& error)
      {
      throw runtime_error("Cannot find a definition for datatype: "
                          + registration.getDataTypeName()
                          + "\nRegistration name: " + registration.getName());
      }
   }

// ------------------------------------------------------------------
// Predicates for filtering Registrations
// ------------------------------------------------------------------
class IsNotOfType
   {
   private:
      const char* type;
   public:
      IsNotOfType(const char* t) : type(t) { }

      bool operator() (const ApsimRegistrationData& registration)
        {return !registration.isOfType(type);}
   };
class IsOfType
   {
   private:
      const char* type;
   public:
      IsOfType(const char* t) : type(t) { }

      bool operator() (const ApsimRegistrationData& registration)
        {return registration.isOfType(type);}
   };
bool IsStructure(const ApsimRegistrationData& registration)
   {
   if (!registration.isOfType("read"))
      {
      ApsimDataTypeData dataType = getDataType(registration);
      return dataType.isStructure();
      }
   return false;
   };
bool IsGetButNotBuiltIn(const ApsimRegistrationData& registration)
   {
   if (registration.isOfType("getVariable"))
      {
      ApsimDataTypeData dataType = getDataType(registration);
      return !dataType.isBuiltIn();
      }
   return false;
   };
bool NeedsSendRoutine(const ApsimRegistrationData& registration)
   {
   return registration.isOfType("respondToGet") ||
          registration.isOfType("event") ||
          registration.isOfType("methodCall");
   };
bool IsARespondTo(const ApsimRegistrationData& registration)
   {
   return registration.isOfType("respondToSet") ||
          registration.isOfType("respondToEvent") ||
          registration.isOfType("respondToMethodCall");
   };
bool IsAutoRegister(const ApsimRegistrationData& registration)
   {
   return !registration.isOfType("read") && registration.doAutoRegister();
   };

// ------------------------------------------------------------------
// for_each function to write a add_registration call.
// ------------------------------------------------------------------
class WriteRegistrationID
   {
   private:
      ostream& out;
   public:
      WriteRegistrationID(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         string idString = "      integer :: " + getCleanName(registration.getName());
         if (routinesDoneSoFar.find(idString) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(idString);
            out << idString << endl;
            }
         }
   };

// ------------------------------------------------------------------
// for_each function to write FORTRAN typestring for a given type.
// ------------------------------------------------------------------
class WriteTypeString
   {
   private:
      ostream& out;
      unsigned level;
   public:
      WriteTypeString(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         string parameterName = getCleanName(dataType.getName()) + "Ddml";
         if (routinesDoneSoFar.find(parameterName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(parameterName);
            level = 1;
            out << "   character (len=*), parameter :: ";
            out << parameterName;
            out << " =    &" << endl;
            writeFieldDefinition("type", dataType);
            out << endl;
            }
         }
      void writeFieldDefinition(const char* tag, const ApsimDataTypeData& dataType)
         {
         level++;
         out.width(level*3+3);
         out << ' ' << "'<" << tag;
         writeDefinition(dataType);

         if (dataType.isStructure())
            {
            out << ">'";
            for (ApsimDataTypeData::iterator i = dataType.begin();
                                             i != dataType.end();
                                             i++)
               {
               out << "   // &" << endl;
               if ((*i).isArrayElement())
                  writeFieldDefinition("element", *i);
               else
                  writeFieldDefinition("field", *i);
               }
            out << "   // &" << endl;
            out.width(level*3+3);
            out << ' ' << "'</" << tag << ">'";
            }
         else
            out << "/>'";
         level--;
         }
      void writeDefinition(const ApsimDataTypeData& dataType)
         {
         if (dataType.getName() != "")
            out << " name=\"" << dataType.getName() << '\"';
         if (dataType.getKind() != "")
            out << " kind=\"" << dataType.getKind() << '\"';
         if (dataType.getUnit() != "")
            out << " unit=\"" << dataType.getUnit() << '\"';
         if (dataType.isArray())
            out << " array=\"T\"";
         }

   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN structure for a given type.
// ------------------------------------------------------------------
class WriteFortranStructure
   {
   private:
      ostream& out;
   public:
      WriteFortranStructure(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         if (routinesDoneSoFar.find(dataType.getName()) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(dataType.getName());
            out << "   ! ----------" << endl;
            ostringstream typestream;
            writeStructure(typestream, dataType, getCleanName(dataType.getName()));
            }
         }
      void writeStructure(ostringstream& typestream, const ApsimDataTypeData& dataType,
                          const string& parentName)
         {
         if (dataType.isArrayElement())
            for (ApsimDataTypeData::iterator i = dataType.begin();
                                             i != dataType.end();
                                             i++)
               {
               string nestedTypeName = parentName + getCleanName((*i).getName());
               writeStructure(typestream, *i, nestedTypeName);   // recursion
               }

         else if (dataType.isStructure())
            {
            ostringstream newTypestream;
            string typeName = parentName + "Type";
            newTypestream << "   type " << typeName << endl;
            newTypestream << "      sequence" << endl;
            for (ApsimDataTypeData::iterator i = dataType.begin();
                                             i != dataType.end();
                                             i++)
               {
               string nestedTypeName = parentName + getCleanName((*i).getName());
               writeStructure(newTypestream, *i, nestedTypeName);   // recursion
               }
            newTypestream << "   end type " << typeName << endl;
            out << newTypestream.str();
            writeDefinition(typestream, dataType, typeName);
            }
         else
            writeDefinition(typestream, dataType, getCleanName(dataType.getName()));
         }
      void writeDefinition(ostream& typestream, const ApsimDataTypeData& dataType,
                           const string& typeName)
         {
         if (dataType.isArray())
            typestream << "      integer :: Num" << getCleanName(dataType.getName()) << 's' << endl;
         if (dataType.isStructure())
            typestream << "      type (" << typeName << ")";
         else
            typestream << "      " << KindToFORTRANType(dataType.getKind());
         if (dataType.isArray())
            typestream << ", dimension(max_array_size)";
         typestream << " :: " << getCleanName(dataType.getName()) << endl;
         }
   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN pack routine for a given type.
// ------------------------------------------------------------------
class WritePack
   {
   private:
      ostream& out;
      const char* pack;
      unsigned numArraysSoFar;

   public:
      WritePack(const char* p, ostream& o) : pack(p), out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         numArraysSoFar = 0;
         string variableName = getCleanName(dataType.getName());
         string routineName = pack + string("_") + variableName;
         if (variableName != "null" &&
             routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            ostringstream tempout;
            out << "   ! ----------" << endl;
            out << "   subroutine " << routineName
                                    << "(variant, " << variableName;
            if (dataType.isArray())
               out << ", Num" << variableName << 's';
            out << ')' << endl;
            out << "   use DataModule" << endl;
            out << "   implicit none" << endl;
            if (Str_i_Eq(pack, "pack"))
               writeDeclaration(dataType, out, "in", variableName);
            else
               writeDeclaration(dataType, out, "out", variableName);
            out << "   integer, intent(in out) :: variant" << endl;
            if (dataType.isArray())
               {
               out << "   integer, intent(";
               if (Str_i_Eq(pack, "pack"))
                  out << "in";
               else
                  out << "out";
               out << ") :: Num" << variableName << 's' << endl;
               }
/*            if (dataType.isStructure())
               {
               for (ApsimDataTypeData::iterator i = dataType.fieldsBegin();
                                       i != dataType.end();
                                       i++)
                  writeField(0, variableName + "%", *i, tempout);    // recursion
               }
            else
*/               writeField(0, "", dataType, tempout);

            out << tempout.str();
            out << "   end subroutine " << routineName << endl;
            }
         }
      void writeField(unsigned arrayLevel, const string& ref,
                      const ApsimDataTypeData& dataType, ostream& tempout)
         {
         static char arrayIndex;
         if (dataType.isArray() && !dataType.isStructure())
            {
            writeCallToPack(arrayLevel, ref, dataType, tempout);
            return;
            }
         if (dataType.isArray())
            {
            string numItemsSt = ref + "Num" + getCleanName(dataType.getName()) + "s";
            tempout.width(arrayLevel*3+3);
            tempout << ' ' << "call " << pack << "_integer4(variant, "
                    << numItemsSt << ")" << endl;
            arrayIndex = 'i';
            arrayIndex += arrayLevel;
            arrayLevel++;
            if (arrayLevel > numArraysSoFar)
               {
               out << "   integer :: " << arrayIndex << endl;
               numArraysSoFar = arrayLevel;
               }
            tempout.width(arrayLevel*3);
            tempout << ' ' << "do " << arrayIndex << " = 1, " << numItemsSt << endl;
            }
         if (dataType.isArrayElement())
            for (ApsimDataTypeData::iterator i = dataType.begin();
                                    i != dataType.end();
                                    i++)
               writeField(arrayLevel, ref, *i, tempout);    // recursion

         else if (dataType.isStructure())
            for (ApsimDataTypeData::iterator i = dataType.begin();
                                    i != dataType.end();
                                    i++)
               {
               string newRef = ref + getCleanName(dataType.getName());
               if (dataType.isArray())
                  newRef += string("(") + arrayIndex + ")" + "%";
               else
                  newRef += "%";
               writeField(arrayLevel, newRef, *i, tempout);    // recursion
               }
         else
            writeCallToPack(arrayLevel, ref, dataType, tempout, arrayIndex);

         if (dataType.isArray() && dataType.isStructure())
            {
            tempout.width(arrayLevel*3);
            tempout << ' ' << "enddo" << endl;
            arrayLevel--;
            arrayIndex--;            
            }
         }
      void writeCallToPack(unsigned arrayLevel, const string& ref,
                           const ApsimDataTypeData& dataType, ostream& tempout,
                           char arrayIndex = ' ')
         {
         bool builtInArrayType = dataType.isArray() && !dataType.isStructure();
//         if (builtInArrayType && strcmpi(pack, "pack") == 0)
//            {
//            tempout.width(arrayLevel*3+3);
//            tempout << ' ' << "call " << pack << "_integer4(variant, " << ref
//                    << "num_" << getCleanName(dataType.getName()) << ')' << endl;
//            }
         tempout.width(arrayLevel*3+3);
         tempout << ' ' << "call " << pack << "_" << dataType.getKind();
         if (builtInArrayType)
            tempout << "_array";
         tempout << "(variant, " << ref << getCleanName(dataType.getName());
         if (dataType.isArray() && arrayIndex != ' ')
            tempout << '(' << arrayIndex << ')';
         if (builtInArrayType)
            {
            if (Str_i_Eq(pack, "unpack"))
               tempout << ", max_array_size";
            tempout  << ", " << ref << "Num" << getCleanName(dataType.getName()) << 's';
            }
         tempout << ')' << endl;
         if (Str_i_Eq(pack, "pack") &&
             dataType.getLowerBound() != "" && dataType.getUpperBound() != "")
            {
            tempout.width(arrayLevel*3+3);
            tempout << ' ';
            writeBoundCheck(dataType, tempout, ref.c_str(),
                            ref + getCleanName(dataType.getName()),
                            ref + "Num" + getCleanName(dataType.getName()) + "s");
            }
         }

   };
// ------------------------------------------------------------------
// function to write a FORTRAN call to pack a variable
// ------------------------------------------------------------------
void WriteCallPack(const ApsimDataTypeData& dataType, ostream& out)
   {
   if (dataType.isStructure() || dataType.getKind() != "")
      {
      if (dataType.isStructure())
         out << "   call pack_" << getCleanName(dataType.getName());
      else
         {
         out << "   call pack_" << dataType.getKind();
         if (dataType.isArray())
            out << "_array";
         }
      out << "(variant, value";
      if (dataType.isArray())
         out << ", num_values";
      out << ')' << endl;
      }
   }

// ------------------------------------------------------------------
// for_each function to write FORTRAN sizeof routine for a given type.
// ------------------------------------------------------------------
class WriteSizeOf
   {
   private:
      ostream& out;
      unsigned numArraysSoFar;
   public:
      WriteSizeOf(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         string variableName = getCleanName(dataType.getName());
         string routineName = "sizeof_" + variableName;
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            numArraysSoFar = 0;
            out << "   ! ----------" << endl;
            ostringstream tempout;
            out << "   function " << routineName << '(' << variableName;
            if (dataType.isArray())
               out << ", Num" << variableName << 's';
            out << ')' << endl;
            out << "   implicit none" << endl;
            writeDeclaration(dataType, out, "in", variableName);
            if (dataType.isArray())
               out << "   integer, intent(in) :: Num" << variableName << 's' << endl;
            out << "   integer sizeof_" << getCleanName(dataType.getName()) << endl;
            out << "   integer :: s" << endl;
            tempout << "   s = 0" << endl;
            writeField(0, "", dataType, tempout);

            out << tempout.str();
            out << "   " << routineName << " = s" << endl;
            out << "   end function " << routineName << endl;
            }
         }

      void writeField(unsigned arrayLevel, const string& ref,
                      const ApsimDataTypeData& dataType, ostream& tempout)
         {
         static char arrayIndex;
         if (dataType.isArray() && dataType.getKind() != "")
            {
            tempout.width(arrayLevel*3+3);
            tempout << ' ' << "s = s + 4";
            if (dataType.getKind() == "string")
               {
               out << "   integer :: i" << endl;
               tempout << endl;
               tempout << "   do i = 1, " << ref << "Num" << getCleanName(dataType.getName()) << 's' << endl;
                  tempout << "      s = s + 4 + len_trim(" << ref << getCleanName(dataType.getName())
                          << "(i))" << endl;
               tempout << "   enddo" << endl;
               }
            else
               {
               tempout << " + " << ref << "Num" << getCleanName(dataType.getName()) << "s * "
                       << getIntrinsicSizeOf(dataType) << endl;
               }
            }
         else
            {
            if (dataType.isArray())
               {
               arrayIndex = 'i';
               arrayIndex += arrayLevel;
               arrayLevel++;
               if (arrayLevel > numArraysSoFar)
                  {
                  out << "   integer :: " << arrayIndex << endl;
                  numArraysSoFar = arrayLevel;
                  }
               tempout.width(arrayLevel*3);
               tempout << ' ' << "s = s + 4" << endl;
               tempout.width(arrayLevel*3);
               tempout << ' ' << "do " << arrayIndex << " = 1, " << ref << "Num" << getCleanName(dataType.getName()) << 's' << endl;
               }

            if (dataType.isArrayElement())
               for (ApsimDataTypeData::iterator i = dataType.begin();
                                       i != dataType.end();
                                       i++)
                  writeField(arrayLevel, ref, *i, tempout);    // recursion

            else if (dataType.isStructure())
               for (ApsimDataTypeData::iterator i = dataType.begin();
                                       i != dataType.end();
                                       i++)
                  {
                  if (ref == "value")
                     writeField(arrayLevel, "value%", *i, tempout);
                  else
                     {
                     string newRef = ref + getCleanName(dataType.getName());
                     if (dataType.isArray())
                        newRef += string("(") + arrayIndex + ")" + "%";
                     else
                        newRef += "%";
                     writeField(arrayLevel, newRef, *i, tempout);    // recursion
                     }
                  }
            else
               {
               if (Str_i_Eq(dataType.getKind(), "string"))
                  {
                  tempout.width(arrayLevel*3+3);
                  tempout << ' ' << "s = s + 4 + len_trim(" << ref << getCleanName(dataType.getName()) << ')' << endl;
                  }
               else
                  {
                  tempout.width(arrayLevel*3+3);
                  tempout << ' ' << "s = s + " << getIntrinsicSizeOf(dataType) << endl;
                  }
               }
            if (dataType.isArray() && dataType.isStructure())
               {
               tempout.width(arrayLevel*3);
               tempout << ' ' << "enddo" << endl;
               }
            }
         }
   };
// ------------------------------------------------------------------
// function to write a FORTRAN call to sizeof routine
// ------------------------------------------------------------------
void WriteCallSizeOf(const ApsimDataTypeData& dataType, ostream& out)
   {
   bool closeEndDo = false;
   if (dataType.isArray())
      {
      if (dataType.isStructure() || dataType.getKind() == "string")
         {
         out << "   integer :: i" << endl;
         out << "   s = 4" << endl;
         out << "   do i = 1, num_values" << endl;
         out << "      s = s + ";
         closeEndDo = true;
         }
      else
         {
         out << "   s = 4 + num_values * ";
         }
      }
   else
      out << "   s = ";
   if (dataType.isStructure())
      {
      out << "sizeof_" << getCleanName(dataType.getName()) << "(value";
      if (dataType.isArray())
         out << ", num_values";
      out << ')';
      }
   else if (dataType.getKind() != "")
      out << getIntrinsicSizeOf(dataType);
   else
      out << '0';
   out << endl;
   if (closeEndDo)
      out << "   enddo" << endl;
   }

// ------------------------------------------------------------------
// for_each function to write a FORTRAN variable set routine for a given type.
// ------------------------------------------------------------------
class WriteSend
   {
   private:
      ostream& out;
   public:
      WriteSend(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         ostringstream tempout;
         string routineName = "send_";
         if (dataType.isStructure())
            routineName += getCleanName(dataType.getName());
         else if (dataType.getKind() != "")
            routineName += dataType.getKind();
         else
            routineName += "null";
         if (dataType.isArray())
            routineName += "_array";
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            out << "   ! ----------" << endl;
            string typeString = getCleanName(dataType.getName()) + "Ddml";
            out << "   subroutine " << routineName
                                         << "(regID, ";
            if (dataType.isStructure() || dataType.getKind() != "")
               out << "value, ";
            if (dataType.isArray())
               out << "num_values, ";
            out << "to, messageType, ddml, ack)" << endl;
            out << "   implicit none" << endl;
            out << "   integer,intent(in) :: regID" << endl;
            writeDeclaration(dataType, out, "in", "value");
            if (dataType.isArray())
               out << "   integer, intent(in) :: num_values" << endl;
            out << "   integer, intent(in) :: to" << endl;
            out << "   integer, intent(in) :: messageType" << endl;
            out << "   character(len=*), intent(in) :: ddml" << endl;
            out << "   logical, intent(in) :: ack" << endl;
            out << "   integer :: variant" << endl;
            out << "   type(Message),pointer :: amessage" << endl;
            out << "   integer :: s" << endl;
            WriteCallSizeOf(dataType, out);
            out << "   s = s + 4 + 4 + len_trim(ddml)" <<endl;
            out << "   amessage => construct_message(messageType,       &" << endl;
            out << "                                 to,                &" << endl;
            out << "                                 ack,               &" << endl;
            out << "                                 s)" << endl;
            out << "   variant = new_variant(amessage)" << endl;
            out << "   call pack_integer4(variant, regID)" << endl;
            out << "   call pack_string(variant, ddml)" << endl;
            WriteCallPack(dataType, out);
            out << "   call delete_variant(variant)" << endl;
            out << "   call send_message(amessage)" << endl;
            out << "   end subroutine " << routineName << endl;
            }
         }
   };

// ------------------------------------------------------------------
// function to write a FORTRAN call to send a variable
// ------------------------------------------------------------------
void WriteCallSend(const ApsimDataTypeData& dataType, ostream& out,
                   const char* regID, const char* to, const char* messageType,
                   const char* ack)
   {
   if (dataType.getLowerBound() != "")
      {
      out << "   ";
      writeBoundCheck(dataType, out, "", "value", "num_values");
      }

   out << "   call send_";
   if (dataType.isStructure())
      out << getCleanName(dataType.getName());
   else if (dataType.getKind() != "")
      out << dataType.getKind();
   else
      out << "null";
   if (dataType.isArray())
      out << "_array";

   out << "(" << regID;
   if (dataType.isStructure() || dataType.getKind() != "")
      out << ", value";
   if (dataType.isArray())
      out << ", num_values";
   out << ", " << to << ", " << messageType;
   out << ", " << getCleanName(dataType.getName()) << "Ddml";
   out << ", " << ack << ')' << endl;
   }

// ------------------------------------------------------------------
// for_each function to write a FORTRAN variable receive routine for a given type.
// ------------------------------------------------------------------
class WriteGet
   {
   private:
      ostream& out;
   public:
      WriteGet(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         string routineName = "get_";
         if (dataType.isStructure())
            routineName += getCleanName(dataType.getName());
         else
            routineName += dataType.getKind();
         if (dataType.isArray())
            routineName += "_array";
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            out << "   ! ----------" << endl;
            ostringstream tempout;
            string typeString = getCleanName(dataType.getName()) + "Ddml";
            out << "   function " << routineName
                                         << "(regID, value";
            if (dataType.isArray())
               out << ", num_values";

            out << ", optionalGet)" << endl;
            out << "   implicit none" << endl;
            out << "   integer, intent(in) :: regID" << endl;
            writeDeclaration(dataType, out, "out", "value");
            if (dataType.isArray())
               out << "   integer, intent(out) :: num_values" << endl;
            out << "   logical, intent(in), optional :: optionalGet" << endl;
            out << "   logical :: " << routineName << endl;
            out << "   logical :: getIsOptional" << endl;
            out << "   integer :: variant" << endl;
            out << "   getIsOptional = .false." << endl;
            out << "   if (present(optionalGet)) then" << endl;
            out << "      getIsOptional = optionalGet" << endl;
            out << "   endif" << endl;
            out << "   if (get_variable_internal(regID, variant, getIsOptional)) then" << endl;
            out << "      call unpack_";
            if (dataType.isStructure())
               out << getCleanName(dataType.getName());
            else
               out << dataType.getKind();
            if (dataType.isArray())
               out << "_array(variant, value, ubound(value, 1), num_values)" << endl;
            else
               out << "(variant, value)" << endl;
            out << "      " << routineName << " = .true." << endl;
            out << "   else" << endl;
            out << "      " << routineName << " = .false." << endl;
            out << "   endif" << endl;
            out << "   end function " << routineName << endl;
            }
         }
   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN interface bit.
// ------------------------------------------------------------------
class WriteGetInterface
   {
   private:
      ostream& out;
   public:
      WriteGetInterface(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         out << endl;
         out << "   interface get_" << getCleanName(registration.getName()) << endl;
         out << "      module procedure get_";
         if (dataType.isStructure())
            out << getCleanName(dataType.getName());
         else
            out << dataType.getKind();
         if (dataType.isArray())
            out << "_array";
         out << endl;
         out << "   end interface get_" << getCleanName(registration.getName()) << endl;
         }
   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN variable return routine for a given type.
// ------------------------------------------------------------------
class WriteReturn
   {
   private:
      ostream& out;
   public:
      WriteReturn(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         ostringstream tempout;
         string variableName = "value";
         string routineName = "return_" + getCleanName(dataType.getName());
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            out << "   ! ----------" << endl;
            out << "   subroutine " << routineName
                                         << "(variableInfo, " << variableName;
            if (dataType.isArray())
               out << ", num_values";
            out << ')' << endl;
            out << "   implicit none" << endl;
            out << "   type(QueryData),intent(in) :: variableInfo" << endl;
            writeDeclaration(dataType, out, "in", "value");
            if (dataType.isArray())
               out << "   integer,intent(in) :: num_values" << endl;
            WriteCallSend(dataType, out, "variableInfo%replyid", "variableInfo%replyto",
                          "MESSAGE_ReturnValue", ".false.");
            out << "   end subroutine " << routineName << endl;
            }
         }
   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN variable set routine for a given type.
// ------------------------------------------------------------------
class WriteSet
   {
   private:
      ostream& out;
   public:
      WriteSet(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         ostringstream tempout;
         string routineName = "set_" + getCleanName(registration.getName());
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);

            string typeString = getCleanName(dataType.getName()) + "Ddml";
            out << "   ! ----------" << endl;
            out << "   function " << routineName
                                         << "(regID, value";
            if (dataType.isArray())
               out << ", num_values";
            out << ')' << endl;
            out << "   implicit none" << endl;
            out << "   integer,intent(in) :: regID" << endl;
            writeDeclaration(dataType, out, "in", "value");
            if (dataType.isArray())
               out << "   integer,intent(in) :: num_values" << endl;
            out << "   logical :: " << routineName << endl;
            out << "   logical :: ok" << endl;
            WriteCallSend(dataType, out, "regID", "get_ParentID()", "MESSAGE_RequestSetValue", ".false.");
            out << "   ok = get_setVariableSuccess()" << endl;
            out << "   if (.not. ok) then" << endl;
            out << "      call set_variable_error(regID)" << endl;
            out << "   endif" << endl;
            out << "   " << routineName << " = ok" << endl;
            out << "   end function " << routineName << endl;
            }
         }
   };

// ------------------------------------------------------------------
// for_each function to write a FORTRAN variable publish routine for a given type.
// ------------------------------------------------------------------
class WritePublish
   {
   private:
      ostream& out;
      const char* publish;
   public:
      WritePublish(ostream& o, const char* p) : out(o), publish(p) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         ApsimDataTypeData dataType = getDataType(registration);
         ostringstream tempout;
         string routineName = publish;
         if (Str_i_Eq(publish, "event") == 0)
            routineName = "publish";
         routineName += "_" + getCleanName(dataType.getName());
         if (routinesDoneSoFar.find(routineName) == routinesDoneSoFar.end())
            {
            routinesDoneSoFar.insert(routineName);
            out << "   ! ----------" << endl;
            out << "   subroutine " << routineName
                                         << "(regID";
            if (dataType.isStructure() || dataType.getKind() != "")
               out << ", value";
            if (dataType.isArray())
               out << ", num_values";
            out << ", ack)" << endl;

            out << "   implicit none" << endl;
            out << "   integer,intent(in) :: regID" << endl;
            writeDeclaration(dataType, out, "in", "value");
            if (dataType.isArray())
               out << "   integer, intent(in) :: num_values" << endl;
            out << "   logical,intent(in) :: ack" << endl;
            WriteCallSend(dataType, out, "regID", "get_ParentID()", "MESSAGE_PublishEvent", "ack");
            out << "   end subroutine " << routineName << endl;
            }
         }
   };

// ------------------------------------------------------------------
// for_each function to write a add_registration call.
// ------------------------------------------------------------------
class WriteRegistration
   {
   private:
      ostream& out;
   public:
      WriteRegistration(ostream& o) : out(o) { }

      void operator() (const ApsimRegistrationData& registration)
         {
         out << "   id%" << getCleanName(registration.getName())
             << " = add_registration(" << registration.getType()
             << ", '" << registration.getName()
             << "', " << getCleanName(registration.getDataTypeName()) << "Ddml)" << endl;
         }
   };

// ------------------------------------------------------------------
// converts the specified .interface filename to FORTRAN90
// ------------------------------------------------------------------
void CreateDataTypesF90::doConvert(const std::string& contents,
                                   const std::string& destFilename,
                                   ostream& console)
   {
   dataTypes = NULL;
   component = NULL;
   routinesDoneSoFar.erase(routinesDoneSoFar.begin(), routinesDoneSoFar.end());
   try
      {

      dataTypes = new ApsimDataTypesFile;
      component = new ApsimComponentData(contents.c_str());

      console << "Creating: " << destFilename.c_str() << endl;
      ofstream out(destFilename.c_str());
      out << "module DataTypesModule" << endl;
      out << "   use ComponentCInterfaceModule" << endl;
      out << "   use DataModule" << endl;
      out << "   implicit none" << endl;
      out << "   integer, parameter :: max_char_size = 100" << endl;
      out << "   integer, parameter :: max_array_size = 100" << endl;
      out << endl;
      out << "   type IDsType" << endl;
      out << "      sequence" << endl;
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteRegistrationID(out), IsAutoRegister);
      out << "   end type IDsType" << endl;
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteTypeString(out), IsNotOfType("read"));
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteFortranStructure(out), IsStructure);
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteGetInterface(out), IsGetButNotBuiltIn);

      out << endl;
      out << "   contains" << endl;
      out << endl;

      for_each_if(component->regBegin(), component->regEnd(),
                  WritePack("pack", out), IsStructure);
      for_each_if(component->regBegin(), component->regEnd(),
                  WritePack("unpack", out), IsARespondTo);
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteSizeOf(out), IsStructure);
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteSend(out), NeedsSendRoutine);
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteGet (out), IsOfType("getVariable"));
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteReturn(out), IsOfType("respondToGet"));
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteSet(out), IsOfType("setVariable"));
      for_each_if(component->regBegin(), component->regEnd(),
                  WritePublish(out, "event"), IsOfType("event"));
      for_each_if(component->regBegin(), component->regEnd(),
                  WritePublish(out, "methodCall"), IsOfType("methodCall"));

      out << "   ! ----------" << endl;
      out << "   subroutine do_registrations(id)" << endl;
      out << "   type(IDSType) :: id" << endl;
      for_each_if(component->regBegin(), component->regEnd(),
                  WriteRegistration(out), IsAutoRegister);
      out << "   end subroutine do_registrations" << endl;

      out << "end module DataTypesModule" << endl;
      delete component;
      delete dataTypes;
      }
   catch (const exception& error)
      {
      delete component;
      delete dataTypes;
      throw;
      }
   }

// ------------------------------------------------------------------
// converts the specified .interface filename to FORTRAN90
// ------------------------------------------------------------------
void CreateDataTypesF90::convert(const std::string& ddml, ostream& console)
   {
   try
      {
      doConvert(ddml, "../apsim/infra/source/datatypesmodule.f90", console);
      }
   catch (const exception& error)
      {
      console << error.what() << endl;
      }
   }

