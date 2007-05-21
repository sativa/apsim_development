//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestSoilSample.h"
#include "..\SoilSample.h"
#include "..\soil.h"
#pragma package(smart_init)
using namespace std;
using namespace boost::unit_test_framework;


SoilSample* sample;

//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpSample(void)
   {
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDownSample(void)
   {
   delete sample;
   }
//---------------------------------------------------------------------------
// Test that we can convert the sample layer structure to less number of layers.
//---------------------------------------------------------------------------
void testMapSampleToSoil1(void)
   {
   // setup a soil.
   static const unsigned soilThickness[5] = {100, 400, 200, 300, 300};
   static const double   soilSw[5]        = {0.1, 0.1, 0.1, 0.1, 0.1};
   static const double   soilBd[5]        = {1.1, 1.1, 1.1, 1.1, 1.1};
   Soil soil;
   soil.setThickness(vector<unsigned>(soilThickness, soilThickness+5));
   soil.setSw(vector<double>(soilSw, soilSw+5));
   soil.setBd(vector<double>(soilBd, soilBd+5));

   // setup a soil sample.
   static const unsigned sampleThickness[2] = {100, 500};
   static const double   sampleSw[5]        = {0.5, 0.3};
   SoilSample sample;
   sample.setThickness(vector<unsigned>(sampleThickness, sampleThickness+2));
   sample.setSw(vector<double>(sampleSw, sampleSw+2));

   sample.mapSampleToSoil(soil);

   vector<double> sw = sample.sw();
   BOOST_CHECK(sw.size() == 5);
   BOOST_CHECK(sw[0] == 0.5);
   BOOST_CHECK(sw[1] == 0.3);
   BOOST_CHECK(sw[2] == 0.3);
   BOOST_CHECK(sw[3] == 0.1);
   BOOST_CHECK(sw[4] == 0.1);
   }
//---------------------------------------------------------------------------
// Test that we can convert the sample layer structure to less number of layers.
//---------------------------------------------------------------------------
void testMapSampleToSoil2(void)
   {
   // setup a soil.
   static const unsigned soilThickness[5] = {100, 400, 200, 300, 300};
   static const double   soilOc[5]        = {1.0, 0.5, 0.5, 0.5, 0.5};
   static const double   soilBd[5]        = {1.1, 1.1, 1.1, 1.1, 1.1};
   Soil soil;
   soil.setThickness(vector<unsigned>(soilThickness, soilThickness+5));
   soil.setOc(vector<double>(soilOc, soilOc+5));
   soil.setBd(vector<double>(soilBd, soilBd+5));

   // setup a soil sample.
   static const unsigned sampleThickness[2] = {100, 500};
   static const double   sampleOc[2]        = {1.5, 0.3};
   SoilSample sample;
   sample.setThickness(vector<unsigned>(sampleThickness, sampleThickness+2));
   sample.setOc(vector<double>(sampleOc, sampleOc+2));

   sample.mapSampleToSoil(soil);

   vector<double> oc = sample.oc();
   BOOST_CHECK(oc.size() == 5);
   BOOST_CHECK(oc[0] == 1.5);
   BOOST_CHECK(oc[1] == 0.3);
   BOOST_CHECK(oc[2] == 0.3);
   BOOST_CHECK(oc[3] == 0.5);
   BOOST_CHECK(oc[4] == 0.5);
   }
//---------------------------------------------------------------------------
// Test that we can convert the sample layer structure to less number of layers.
//---------------------------------------------------------------------------
void testMapSampleToSoil3(void)
   {
   // setup a soil.
   static const unsigned soilThickness[5] = {100, 150, 150, 150, 150};
   static const double   soilNo3[5]       = {20, 15, 10, 5, 4};
   static const double   soilBd[5]        = {1.36, 1.374, 1.293, 1.374, 1.293};
   Soil soil;
   soil.setThickness(vector<unsigned>(soilThickness, soilThickness+5));
   soil.setNo3(vector<double>(soilNo3, soilNo3+5));
   soil.setBd(vector<double>(soilBd, soilBd+5));

   // setup a soil sample.
   SoilSample sample;
   static const double   sampleNo3[5]        = {5,    4,   2,   1, 0.5};
   sample.setThickness(vector<unsigned>(soilThickness, soilThickness+5));
   sample.setNo3(vector<double>(sampleNo3, sampleNo3+5));

   sample.mapSampleToSoil(soil);

   vector<double> no3 = sample.no3();
   BOOST_CHECK(no3.size() == 5);
   BOOST_CHECK(no3[0] == 5);
   BOOST_CHECK(no3[1] == 4);
   BOOST_CHECK(no3[2] == 2);
   BOOST_CHECK(no3[3] == 1);
   BOOST_CHECK(no3[4] == 0.5);
   }
//---------------------------------------------------------------------------
// Test that we can convert the sample layer structure to less number of layers.
//---------------------------------------------------------------------------
void testMapSampleToSoil4(void)
   {
   // setup a soil.
   static const unsigned soilThickness[5] = {100, 150, 150, 150, 150};
   static const double   soilOc[5]        = {1.0, 0.5, 0.5, 0.5, 0.5};
   static const double   soilNo3[5]       = {20, 15, 10, 5, 4};
   static const double   soilBd[5]        = {1.36, 1.374, 1.293, 1.374, 1.293};
   Soil soil;
   soil.setThickness(vector<unsigned>(soilThickness, soilThickness+5));
   soil.setNo3(vector<double>(soilNo3, soilNo3+5));
   soil.setOc(vector<double>(soilOc, soilOc+5));
   soil.setBd(vector<double>(soilBd, soilBd+5));

   // setup a soil sample.
   static const unsigned sampleThickness[2] = {100, 500};
   static const double   sampleNo3[2]       = {5, 4};
   static const double   sampleOc[1]        = {1.5};
   SoilSample sample;
   sample.setThickness(vector<unsigned>(sampleThickness, sampleThickness+2));
   sample.setNo3(vector<double>(sampleNo3, sampleNo3+2));
   sample.setOc(vector<double>(sampleOc, sampleOc+1));

   sample.mapSampleToSoil(soil);

   vector<double> no3 = sample.no3();
   BOOST_CHECK(no3.size() == 5);
   BOOST_CHECK(no3[0] == 5);
   BOOST_CHECK(no3[1] == 3.90568);
   BOOST_CHECK(no3[2] == 4.15035);
   BOOST_CHECK(no3[3] == 3.90568);
   BOOST_CHECK(no3[4] == 4.05012);

   vector<double> oc = sample.oc();
   BOOST_CHECK(oc.size() == 5);
   BOOST_CHECK(oc[0] == 1.5);
   BOOST_CHECK(oc[1] == 0.5);
   BOOST_CHECK(oc[2] == 0.5);
   BOOST_CHECK(oc[3] == 0.5);
   BOOST_CHECK(oc[4] == 0.5);
   }
//---------------------------------------------------------------------------
// register all tests
//---------------------------------------------------------------------------
test_suite* testSoilSample(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestSoilSample");
   test->add(BOOST_TEST_CASE(&testMapSampleToSoil1));
   test->add(BOOST_TEST_CASE(&testMapSampleToSoil2));
   test->add(BOOST_TEST_CASE(&testMapSampleToSoil3));
   test->add(BOOST_TEST_CASE(&testMapSampleToSoil4));

   return test;
   }

