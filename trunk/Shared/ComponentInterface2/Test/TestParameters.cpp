//---------------------------------------------------------------------------
#pragma hdrstop

#include "TestParameters.h"
#include "../DataTypes.h"
#include "../CMPScienceAPI.h"
#include "../CMPComponentInterface.h"
#include "../MessageData.h"
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test_framework;

extern CMPComponentInterface* componentInterface;
extern ScienceAPI* scienceAPI;
extern unsigned messageArg;
extern int parentID;
extern int componentID;
void STDCALL PMCallback(const unsigned* arg, Message& message);
void setup();
void teardown();

static const char* simScript = "<component name=\"ABC\" executable=\"abc.dll\">"
                               "   <initdata>"
                               "      <a>1.0</a>"
                               "      <b>2.0</b>"
                               "      <c>Three</c>"
                               "      <parameters2>"
                               "         <a>4.0</a>"
                               "         <b>5.0</b>"
                               "         <c>Six</c>"
                               "      </parameters2>"
                               "   </initdata>"
                               "</component>";

//---------------------------------------------------------------------------
// setup the component/PM testbed.
//---------------------------------------------------------------------------
void sendInit1()
   {
   Init1 init1;
   init1.sdml = simScript;
   init1.fqn = "parent.abc";
   init1.inStartup = true;
   componentInterface->messageToLogic(newMessage(Message::Init1, parentID, componentID, false, init1));
   }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// UseCase: read some numeric values from parameter set.
//---------------------------------------------------------------------------
void ReadParameter()
   {
   setup();
   sendInit1();

   BOOST_ASSERT(scienceAPI->name() == "abc");
   BOOST_ASSERT(scienceAPI->parent() == "parent");

   float a;
   scienceAPI->read("a", a, false);
   BOOST_ASSERT(a == 1);

   int b;
   scienceAPI->read("b", b, false);
   BOOST_ASSERT(b == 2);

   string c;
   scienceAPI->read("c", c, false);
   BOOST_ASSERT(c == "Three");

   BOOST_ASSERT(!scienceAPI->read("notfound", c, true));

   teardown();
   }

//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
void TestParameters(void)
   {
   ReadParameter();
   }




