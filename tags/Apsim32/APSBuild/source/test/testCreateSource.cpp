//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "testCreateSource.h"

#include "..\CreateSource.h"
#include <test\framework\testsuite.h>
#include <test\framework\testcaller.h>
#include <general\string_functions.h>
#include <fstream>
#include <sstream>
using namespace std;
#pragma package(smart_init)

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
Test* TestCreateSource::suite(void)
   {
   TestSuite *testSuite = new TestSuite ("CreateSource");
   testSuite->addTest(new TestCaller <TestCreateSource>
      ("convertStructure", &TestCreateSource::convertStructure));
   return testSuite;
   }
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void TestCreateSource::setUp(void)
   {
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void TestCreateSource::tearDown(void)
   {
   }
//---------------------------------------------------------------------------
// test that a structure converts ok.
//---------------------------------------------------------------------------
void TestCreateSource::convertStructure(void)
   {
   static const char* ddml =
      "<types>\n"
      "   <type name=\"time_string\" kind=\"string\" unit=\"\" description=\"Time of day as a string\"/>\n"
      "   <type name=\"time\" description=\"Change in the simulation system time and the duration of the new time step\">\n"
      "      <field name=\"startday\" kind=\"integer4\" description=\"Day number of the start of the timestep\"/>\n"
      "      <field name=\"startsec\" kind=\"integer4\" description=\"Seconds past midnight of the start of the timestep (0-86399)\"/>\n"
      "      <field name=\"startsecpart\" kind=\"double\" description=\"Fraction of a second of the start of the timestep (0-1)\"/>\n"
      "   </type>\n"
      "</types>";
   static const char* goodHPP =
      "#ifndef DataTypesH\n"
      "#define DataTypesH\n"
      "#include <ComponentInterface\\MessageData.h>\n"
      "namespace protocol {\n"
      "\n"
      "//-------------------- timeType\n"
      "#define timeTypeDDML \\\n"
      "   \"<type name=\\\"time\\\">\" \\\n"
      "   \"   <field name=\\\"startday\\\" kind=\\\"integer4\\\"/>\" \\\n"
      "   \"   <field name=\\\"startsec\\\" kind=\\\"integer4\\\"/>\" \\\n"
      "   \"   <field name=\\\"startsecpart\\\" kind=\\\"double\\\"/>\" \\\n"
      "   \"</type>\"\n"
      "struct timeType\n"
      "   {\n"
      "   int startday;\n"
      "   int startsec;\n"
      "   double startsecpart;\n"
      "   };\n"
      "inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const timeType& data)\n"
      "   {\n"
      "   messageData << data.startday;\n"
      "   messageData << data.startsec;\n"
      "   messageData << data.startsecpart;\n"
      "   return messageData;\n"
      "   }\n"
      "inline protocol::MessageData& operator>>(protocol::MessageData& messageData, timeType& data)\n"
      "   {\n"
      "   messageData >> data.startday;\n"
      "   messageData >> data.startsec;\n"
      "   messageData >> data.startsecpart;\n"
      "   return messageData;\n"
      "   }\n"
      "inline unsigned int memorySize(const timeType& data)\n"
      "   {\n"
      "   return protocol::memorySize(data.startday)\n"
      "          + protocol::memorySize(data.startsec)\n"
      "          + protocol::memorySize(data.startsecpart);\n"
      "   }\n"
      "} // protocol\n"
      "#endif\n";
   static const char* goodForDataTypes =
         "module dataTypes\n"
         "   character(len=*), parameter :: nullTypeDDML = '<type/>'\n"
         "   character(len=*), parameter :: timeTypeDDML = &\n"
         "      '<type name=\"time\">' // &\n"
         "      '   <field name=\"startday\" kind=\"integer4\"/>' // &\n"
         "      '   <field name=\"startsec\" kind=\"integer4\"/>' // &\n"
         "      '   <field name=\"startsecpart\" kind=\"double\"/>' // &\n"
         "      '</type>'\n"
         "   type timeType\n"
         "      sequence\n"
         "      integer :: startday\n"
         "      integer :: startsec\n"
         "      double precision :: startsecpart\n"
         "   end type timeType\n"
         "end module dataTypes\n";

   try
      {
      ostringstream cpp, hpp, forDataTypes, forDataTypesInterface;
      CreateSource converter;
      converter.go(ddml, cpp, hpp, forDataTypes, forDataTypesInterface);

      ofstream file1("file1");
      file1 << forDataTypes.str();
      file1.close();

      ofstream file2("file2");
      file2 << goodForDataTypes;
      file2.close();

      test(hpp.str() == goodHPP);
      test (cpp.str() ==
         "#include \"DataTypes.h\"\n"
         "#include \"FortranComponent.h\"\n\n"
         "extern \"C\" void __stdcall publish_time(unsigned* id, const protocol::timeType* data)\n"
         "   {\n"
         "   FortranProxyComponent::currentInstance->publish(*id, *data);\n"
         "   }\n"
         "extern \"C\" void __stdcall unpack_time(protocol::Variant* variant, protocol::timeType* data)\n"
         "   {\n"
         "   variant->unpack(*data);\n"
         "   }\n"
         );
      test(forDataTypes.str() == goodForDataTypes);
      test(forDataTypesInterface.str() ==
         "module dataTypesInterface\n"
         "   interface\n"
         "   subroutine publish_time(id, data)\n"
         "      use dataTypes\n"
         "      ml_external publish_time\n"
         "      integer, intent(in) :: id\n"
         "      type(timeType), intent(in) :: data\n"
         "   end subroutine publish_time\n"
         "   subroutine unpack_time(variant, data)\n"
         "      use dataTypes\n"
         "      ml_external unpack_time\n"
         "      integer, intent(in) :: variant\n"
         "      type(timeType), intent(in out) :: data\n"
         "   end subroutine unpack_time\n"
         "   end interface\n"
         "end module dataTypesInterface\n"
         );

      }
   catch (const runtime_error& )
      {
      test(false);
      }
   }

