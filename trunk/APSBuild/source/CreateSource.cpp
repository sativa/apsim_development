//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "CreateSource.h"
#include <ApsimShared\ApsimDataTypesFile.h>

#pragma package(smart_init)
using namespace std;
// ------------------------------------------------------------------
// convert a DDML 'kind' string to a CPP built in type.
// ------------------------------------------------------------------
std::string ddmlKindToCPP(const std::string& kind)
   {
   if (Str_i_Eq(kind, "integer4"))
      return "int";
   else if (Str_i_Eq(kind, "single"))
      return "float";
   else if (Str_i_Eq(kind, "double"))
      return "double";
   else if (Str_i_Eq(kind, "boolean"))
      return "bool";
   else if (Str_i_Eq(kind, "char"))
      return "char";
   else if (Str_i_Eq(kind, "string"))
      return "FString";
   else
      return "????";
   }
// ------------------------------------------------------------------
// convert a DDML 'kind' string to a FOR built in type.
// ------------------------------------------------------------------
std::string ddmlKindToFOR(const std::string& kind)
   {
   if (Str_i_Eq(kind, "integer4"))
      return "integer";
   else if (Str_i_Eq(kind, "single"))
      return "real";
   else if (Str_i_Eq(kind, "double"))
      return "double precision";
   else if (Str_i_Eq(kind, "boolean"))
      return "logical";
   else if (Str_i_Eq(kind, "char"))
      return "character(len=1)";
   else if (Str_i_Eq(kind, "string"))
      return "character(len=100)";
   else
      return "????";
   }
//---------------------------------------------------------------------------
// write a type definition in C for the specified data type.
//---------------------------------------------------------------------------
void writeTypeInC(const ApsimDataTypeData& dataType, bool fortranFriendly, ostream& out)
   {
   // write declaration
   string typeName = dataType.getName() + "Type";
   if (fortranFriendly)
      typeName = "F" + typeName;
   out << "struct " << typeName << endl;
   out << "   {" << endl;

   for (ApsimDataTypeData::iterator field = dataType.begin();
                                    field != dataType.end();
                                    field++)
      {
      string cDataType = ddmlKindToCPP(field->getKind());
      if (cDataType != "????")
         {
         if (field->isArray())
            {
            if (fortranFriendly)
               {
               out << "   " << cDataType << ' ' << field->getName();
               if (field->isArray())
                  out << "[max_array_size]";
               out << ';' << endl;
               out << "   unsigned num_" << field->getName() << ";" << endl;
               }
            else
               out << "   protocol::vector<" << cDataType << "> " << field->getName() << ';' << endl;
            }
         else
            out << "   " << cDataType << ' ' << field->getName() << ';' << endl;
         }
      else
         out << "   // unknown data type: " << field->getKind() << endl;
      }
   out << "   };" << endl;
   }
//---------------------------------------------------------------------------
// write insertion and extraction operators and memorySize functions for type
//---------------------------------------------------------------------------
void writeOperators(const ApsimDataTypeData& dataType, bool fortranFriendly, ostream& out)
   {
   string typeName = dataType.getName() + "Type";
   if (fortranFriendly)
      typeName = "F" + typeName;

   out << "inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const "
       << typeName << "& data)" << endl;
   out << "   {" << endl;
   for (ApsimDataTypeData::iterator field = dataType.begin();
                                    field != dataType.end();
                                    field++)
      {
      string fieldName = field->getName();
      if (fieldName != "" && ddmlKindToCPP(field->getKind()) != "????")
         {
         if (field->isArray() && fortranFriendly)
            {
            out << "   messageData << data.num_" << fieldName << ';' << endl;
            out << "   for (unsigned i = 0; i != data.num_" << fieldName << "; i++)" << endl;
            out << "      messageData << data." << fieldName << "[i];" << endl;
            }
         else
            out << "   messageData << data." << fieldName << ';' << endl;
         }
      }
   out << "   return messageData;" << endl;
   out << "   }" << endl;

   // write extraction operator for type
   out << "inline protocol::MessageData& operator>>(protocol::MessageData& messageData, "
       << typeName << "& data)" << endl;
   out << "   {" << endl;
   for (ApsimDataTypeData::iterator field = dataType.begin();
                                    field != dataType.end();
                                    field++)
      {
      string fieldName = field->getName();
      if (fieldName != "" && ddmlKindToCPP(field->getKind()) != "????")
         {
         if (field->isArray() && fortranFriendly)
            {
            out << "   messageData >> data.num_" << fieldName << ';' << endl;
            out << "   for (unsigned i = 0; i != data.num_" << fieldName << "; i++)" << endl;
            out << "      messageData >> data." << fieldName << "[i];" << endl;
            }
         else
            out << "   messageData >> data." << fieldName << ';' << endl;
         }
      }
   out << "   return messageData;" << endl;
   out << "   }" << endl;

   // write a memorySize routine
   out << "inline unsigned int memorySize(const "
       << typeName << "& data)" << endl;
   out << "   {" << endl;
   out << "   return ";
   bool first = true;
   for (ApsimDataTypeData::iterator field = dataType.begin();
                                    field != dataType.end();
                                    field++)
      {
      string fieldName = field->getName();
      if (fieldName != "" && ddmlKindToCPP(field->getKind()) != "????")
         {
         if (!first)
            out << "\n          + ";
         if (field->isArray() && fortranFriendly)
            out << "4 + data.num_" << fieldName << " * protocol::memorySize(data." + fieldName + "[0])";
         else
            out << "protocol::memorySize(data." + fieldName + ")";
         first = false;
         }
      }
   if (first)
      out << '0';
   out <<";\n   }" << endl;
   }
//---------------------------------------------------------------------------
// Performs the conversion using the specified ddml.  Writes source to
// cppSource, hppSource (in c++) and forDataTypes and forDataTypesInterface
// (in FORTRAN)
//---------------------------------------------------------------------------
void CreateSource::go(const std::string& ddml,
                      std::ostream& cpp,
                      std::ostream& hpp,
                      std::ostream& forDataTypes,
                      std::ostream& forDataTypesInterface)
   {
   ApsimDataTypesFile dataTypes(ddml);
   hpp << "#ifndef DataTypesH\n";
   hpp << "#define DataTypesH\n";
   hpp << "#include <ComponentInterface\\MessageData.h>\n";
   hpp << "#include \"ProtocolVector.h\"\n";
   hpp << "namespace protocol {\n";
   hpp << endl;
   cpp << "#include \"DataTypes.h\"\n";
   cpp << "#include \"FortranComponent.h\"\n";
   cpp << "const unsigned max_array_size = 100;\n";
   cpp << endl;
   forDataTypes << "module dataTypes\n";
   forDataTypes << "   character(len=*), parameter :: nullTypeDDML = '<type/>'\n";
   forDataTypes << "   integer, parameter :: max_array_size = 100\n";
   forDataTypesInterface << "module dataTypesInterface\n";
   forDataTypesInterface << "   interface\n";

   for (ApsimDataTypesFile::iterator dataType = dataTypes.begin();
                                     dataType != dataTypes.end();
                                     dataType++)
      {
      if (dataType->isStructure())
         {
         bool needForToCMapping = false;

         string typeName = dataType->getName() + "Type";
         // write DDML
         hpp << "//-------------------- " << typeName << endl;
         hpp << "#define " << typeName << "DDML \\" << endl;
         hpp << "   \"<type name=\\\"" << dataType->getName() << "\\\">\" \\" << endl;
         for (ApsimDataTypeData::iterator field = dataType->begin();
                                          field != dataType->end();
                                          field++)
            {
            hpp << "   \"   <field name=\\\"" << field->getName()
                << "\\\" kind=\\\"" << field->getKind() << "\\\"";
            if (field->isArray())
               {
               hpp << " array=\"T\"";
               needForToCMapping = true;
               }
            hpp << "/>\" \\" << endl;
            }
         hpp << "   \"</type>\"" << endl;

         // write struct
         writeTypeInC(*dataType, false, hpp);

         // write insertion operator for type
         writeOperators(*dataType, false, hpp);

         // write a FORTRAN compatible type
         if (needForToCMapping)
            {
            cpp << "namespace protocol {" << endl;
            writeTypeInC(*dataType, true, cpp);
            writeOperators(*dataType, true, cpp);
            cpp << "};" << endl;
            }

         cpp << "extern \"C\" void __stdcall publish_" << dataType->getName()
             << "(unsigned* id, const protocol::";
         if (needForToCMapping)
            cpp << "F";
         cpp << typeName << "* data)\n";
         cpp << "   {" << endl;
         cpp << "   FortranProxyComponent::currentInstance->publish(*id, *data);" << endl;
         cpp << "   }" << endl;

         // write a  stub to unpack the structure
         cpp << "extern \"C\" void __stdcall unpack_" << dataType->getName()
             << "(protocol::Variant* variant, protocol::";
         if (needForToCMapping)
            cpp << "F";
         cpp << typeName << "* data)\n";
         cpp << "   {" << endl;
         cpp << "   variant->unpack(*data);" << endl;
         cpp << "   }" << endl;

         // write the FORTRAN code now.
         // write DDML
         forDataTypes << "!-------------------- " << typeName << endl;

         forDataTypes << "   character(len=*), parameter :: " << typeName << "DDML = &" << endl;
         forDataTypes << "      '<type name=\"" << dataType->getName() << "\">' // &" << endl;
         for (ApsimDataTypeData::iterator field = dataType->begin();
                                          field != dataType->end();
                                          field++)
            {
            forDataTypes << "      '   <field name=\"" << field->getName()
                << "\" kind=\"" << field->getKind();
            if (field->isArray())
               forDataTypes << " array=\"T\"";
            forDataTypes << "\"/>' // &" << endl;

            }
         forDataTypes << "      '</type>'" << endl;

         // write FORTRAN derived type
         forDataTypes << "   type " << typeName << endl;
         forDataTypes << "      sequence" << endl;

         int fieldNumber = 1;
         for (ApsimDataTypeData::iterator field = dataType->begin();
                                          field != dataType->end();
                                          field++)
            {
            string fDataType = ddmlKindToFOR(field->getKind());
            if (fDataType != "????")
               {
               forDataTypes << "      " << fDataType << " :: " << field->getName();
               if (field->isArray())
                  {
                  forDataTypes << "(max_array_size)\n";
                  forDataTypes << "      integer :: num_" << field->getName();
                  }

               forDataTypes << endl;
               }
            else
               {
               string fieldName = field->getName();
               if (fieldName == "")
                  fieldName = string(fieldNumber++, 'z');
               forDataTypes << "      integer " << fieldName << " ! unknown data type: " << field->getKind() << endl;
               }
            }
         forDataTypes << "   end type " << typeName << endl;

         // write FORTRAN publish routine
         forDataTypesInterface << "!-------------------- " << typeName << endl;
         forDataTypesInterface << "   subroutine publish_" << dataType->getName() << "(id, data)" << endl;
         forDataTypesInterface << "      use dataTypes" << endl;
         forDataTypesInterface << "      ml_external publish_" << dataType->getName() << endl;
         forDataTypesInterface << "      integer, intent(in) :: id" << endl;
         forDataTypesInterface << "      type(" << typeName << "), intent(in) :: data" << endl;
         forDataTypesInterface << "   end subroutine publish_" << dataType->getName() << endl;

         // write FORTRAN unpack routine
         forDataTypesInterface << "   subroutine unpack_" << dataType->getName() << "(variant, data)" << endl;
         forDataTypesInterface << "      use dataTypes" << endl;
         forDataTypesInterface << "      ml_external unpack_" << dataType->getName() << endl;
         forDataTypesInterface << "      integer, intent(in) :: variant" << endl;
         forDataTypesInterface << "      type(" << typeName << "), intent(in out) :: data" << endl;
         forDataTypesInterface << "   end subroutine unpack_" << dataType->getName() << endl;
         }
      }
   hpp << "} // protocol\n";
   hpp << "#endif\n";
   forDataTypes << "end module dataTypes\n";
   forDataTypesInterface << "   end interface\n";
   forDataTypesInterface << "end module dataTypesInterface\n";
   }

