//---------------------------------------------------------------------------
#pragma hdrstop

#include "TestMisc.h"
#include "../DataTypes.h"
#include "../CMPScienceAPI.h"
#include "../CMPComponentInterface.h"
#include "../MessageData.h"
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test_framework;

extern ScienceAPI* scienceAPI;
extern vector<MessageData> messagesSent;

void setup();
void teardown();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// UseCase: Test to make sure query sends out QueryInfo messages and
//          returns matches to caller.
//---------------------------------------------------------------------------
void Query()
   {
   setup();

   vector<QueryMatch> matches;
   scienceAPI->query("*", matches);

   // make sure a queryInfo message happened.
   QueryInfo queryInfo;
   unpack(messagesSent[0], queryInfo);
   BOOST_ASSERT(queryInfo.name == "*");
   BOOST_ASSERT(queryInfo.kind == 7 /* component */);

   scienceAPI->query("*.sw", matches);
   unpack(messagesSent[1], queryInfo);
   BOOST_ASSERT(queryInfo.name == "*.sw");
   BOOST_ASSERT(queryInfo.kind == 2 /* variable */);
   BOOST_ASSERT(matches.size() == 2);
   BOOST_ASSERT(matches[0].name == "comp1.sw");
   BOOST_ASSERT(matches[1].name == "comp2.sw");

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Test the write method and make sure a summary event is published.
//---------------------------------------------------------------------------
void Write()
   {
   setup();

   scienceAPI->write("This is a message");

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "SummaryFileWrite");
   BOOST_ASSERT(registerData.kind == 5 /*event*/);

   PublishEvent publishEvent;
   Summary summary;

   unpack(messagesSent[1], publishEvent);
   BOOST_ASSERT(publishEvent.ID == registerData.ID);
   BOOST_ASSERT(publishEvent.ddml == DDML(summary));

   unpack(messagesSent[1], summary);
   BOOST_ASSERT(summary.name == ""); // an init1 hasn't been sent yet so no name.
   BOOST_ASSERT(summary.msg == "This is a message");

   teardown();
   }


//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
void TestMisc(void)
   {
   Query();
   Write();
   }





