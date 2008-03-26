//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestSoils.h"
#include <boost\filesystem\path.hpp>
#include <boost\filesystem\operations.hpp>
#include <general\string_functions.h>
#include "..\Soils.h"

#pragma package(smart_init)
using namespace std;
using namespace boost::unit_test_framework;
using namespace boost::filesystem;

extern const char* SOIL_DATA;

Soils soils;
string soilName1;
string soilName2;
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpSoils(void)
   {
   Soils::newSoilsFile("test.soils");
   soils.open("test.soils");
   soilName1 = soils.add(Soil(SOIL_DATA));
   soilName2 = soils.add(Soil(SOIL_DATA));


   Soils::Info info;
   info.region = "Test region";
   info.site = "Test site";
   info.name = "Test name";
   info.localName = "Test local name";
   info.soilType = "Black Vertosol";
   info.nearestTown = "Test nearest town";
   info.comment = "Test comment";
   info.gps = "Test gps";
   info.gpsType = "Test gps type";
   info.mapId = "Test map id";
   info.naturalVegetation = "Test natural vegetation";
   data.setInfo(refno, info);

   Soils::LayeredData layered;
   layered.depth.assign(depth, depth+numLayers);
   layered.bd.assign(bd, bd+numLayers);
   layered.ll15.assign(ll15, ll15+numLayers);
   layered.airdry.assign(airdry, airdry+numLayers);
   layered.dul.assign(dul, dul+numLayers);
   layered.sat.assign(sat, sat+numLayers);
   layered.oc.assign(oc, oc+numLayers);
   layered.ec.assign(ec, ec+numLayers);
   layered.ph.assign(ph, ph+numLayers);
   layered.cl.assign(cl, cl+numLayers);
   layered.cec.assign(cec, cec+numLayers);
   layered.ca.assign(ca, ca+numLayers);
   layered.mg.assign(mg, mg+numLayers);
   layered.na.assign(na, na+numLayers);
   layered.k.assign(k, k+numLayers);
   layered.exchangableSodium.assign(exchangableSodium, exchangableSodium+numLayers);
   layered.particleSizeSand.assign(particleSizeSand, particleSizeSand+numLayers);
   layered.particleSizeSilt.assign(particleSizeSilt, particleSizeSilt+numLayers);
   layered.particleSizeClay.assign(particleSizeClay, particleSizeClay+numLayers);
   layered.swcon.assign(swcon, swcon+numLayers);
   layered.fbiom.assign(fbiom, fbiom+numLayers);
   layered.finert.assign(finert, finert+numLayers);
   layered.no3ppm.assign(no3ppm, no3ppm+numLayers);
   layered.nh4ppm.assign(nh4ppm, nh4ppm+numLayers);
   layered.ureappm.assign(ureappm, ureappm+numLayers);
   data.setLayeredData(1, layered);
   }
//---------------------------------------------------------------------------
// Setup crop data.
//---------------------------------------------------------------------------
void setupCropData(void)
   {
   Soils::CropData sorgData;
   sorgData.depth.assign(depth, depth+numLayers);
   sorgData.ll.assign(sorgLL, sorgLL+numLayers);
   sorgData.kl.assign(sorgKL, sorgKL+numLayers);
   sorgData.xf.assign(sorgXF, sorgXF+numLayers);
   data.setCropData(1, "Sorghum", sorgData);

   Soils::CropData wheatData;
   wheatData.depth.assign(depth, depth+numLayers);
   wheatData.ll.assign(wheatLL, wheatLL+numLayers);
   wheatData.kl.assign(wheatKL, wheatKL+numLayers);
   wheatData.xf.assign(wheatXF, wheatXF+numLayers);
   data.setCropData(1, "Wheat", wheatData);
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDown(void)
   {
   data.close();
   DeleteFile("test.mdb");
   }
//---------------------------------------------------------------------------
// test the adding and retrieval of soil information.
//---------------------------------------------------------------------------
void testGetInfo(void)
   {
   setUp();
   BOOST_CHECK(refno == 1);
   BOOST_CHECK(data.isDirty());
   Soils::Info info;
   data.getInfo(refno, info);
   BOOST_CHECK(info.region == "Test region");
   BOOST_CHECK(info.site == "Test site");
   BOOST_CHECK(info.name == "Test name");
   BOOST_CHECK(info.localName == "Test local name");
   BOOST_CHECK(info.soilType == "Black Vertosol");
   BOOST_CHECK(info.nearestTown == "Test nearest town");
   BOOST_CHECK(info.comment == "Test comment");
   BOOST_CHECK(info.gps == "Test gps");
   BOOST_CHECK(info.gpsType == "Test gps type");
   BOOST_CHECK(info.mapId == "Test map id");
   BOOST_CHECK(info.naturalVegetation == "Test natural vegetation");

   BOOST_CHECK_THROW(data.getInfo(1111, info), std::runtime_error);
   tearDown();
   }                 
//---------------------------------------------------------------------------
// Test that when we close the database we are not saving anything.
// Method should throw.
//---------------------------------------------------------------------------
void testNoSave(void)
   {
   setUp();
   data.close();
   data.open("test.mdb");
   Soils::Info info;
   data.getInfo(refno, info);    // should throw
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the saveAs method.
//---------------------------------------------------------------------------
void testSaveAs(void)
   {
   setUp();
   data.saveAs("saved.mdb");
   data.open("saved.mdb");
   Soils::Info info;
   data.getInfo(refno, info);
   BOOST_CHECK(info.site == "Test site");
   BOOST_CHECK(data.isDirty() == false);
   DeleteFile("saved.mdb");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the save method.
//---------------------------------------------------------------------------
void testSave(void)
   {
   setUp();
   data.save();
   data.close();
   data.open("test.mdb");
   Soils::Info info;
   data.getInfo(refno, info);
   BOOST_CHECK(info.site == "Test site");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the deleteSoil method. Test should throw because the soil
// isn't found.
//---------------------------------------------------------------------------
void testDeleteSoil(void)
   {
   setUp();
   data.deleteSoil(1);
   Soils::Info info;
   data.getInfo(refno, info);  // should throw
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the getSoils method.
//---------------------------------------------------------------------------
void testGetSoils(void)
   {
   setUp();
   Soils::Info info;
   data.getInfo(1, info);
   unsigned refno = data.addSoil();
   data.setInfo(refno, info);
   vector<unsigned> refnos;
   data.getSoils(refnos);
   BOOST_CHECK(refnos.size() == 2);
   BOOST_CHECK(refnos[0] == 1);
   BOOST_CHECK(refnos[1] == 2);
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the getSoils method.
//---------------------------------------------------------------------------
void testGetLists(void)
   {
   setUp();
   Soils::Info info;
   data.getInfo(1, info);
   info.region = "Test region 2";
   info.site = "Test site 2";
   info.soilType = "Test soil type 2";
   unsigned refno = data.addSoil();
   data.setInfo(refno, info);

   vector<string> regions;
   data.getPossibleRegions(regions);
   BOOST_CHECK(regions.size() == 3);
   BOOST_CHECK(regions[0] == "Test region");
   BOOST_CHECK(regions[1] == "Test region 2");
   BOOST_CHECK(regions[2] == "Unknown");

   vector<string> sites;
   data.getPossibleSites(sites);
   BOOST_CHECK(sites.size() == 3);
   BOOST_CHECK(sites[0] == "Test site");
   BOOST_CHECK(sites[1] == "Test site 2");
   BOOST_CHECK(sites[2] == "Unknown");

   vector<string> soilTypes;
   data.getPossibleSoilTypes(soilTypes);
   BOOST_CHECK(soilTypes[0] == "Black Vertosol");
   BOOST_CHECK(soilTypes[1] == "Black/ brown Sodosol");
   tearDown();
   }
//---------------------------------------------------------------------------
// test the adding and retrieval of soil data.
//---------------------------------------------------------------------------
void testGetData(void)
   {
   setUp();
   Soils::Data data1;
   data1.maxEvap = 1;
   data1.cona = 2;
   data1.diffusConst = 3;
   data1.diffusSlope = 4;
   data1.u = 5;
   data1.salb = 6;
   data1.cn2Bare = 7;
   data1.cnRed = 8;
   data1.cnCov = 9;
   data1.cnCanopyFact = 10;
   data1.rootCN = 11;
   data1.rootWT = 12;
   data1.soilCN = 13;
   data1.enrAcoeff = 14;
   data1.enrBcoeff = 15;

   data.setData(1, data1);
   Soils::Data data2;
   data.getData(1, data2);
   BOOST_CHECK(data2.maxEvap == 1);
   BOOST_CHECK(data2.cona == 2);
   BOOST_CHECK(data2.diffusConst == 3);
   BOOST_CHECK(data2.diffusSlope == 4);
   BOOST_CHECK(data2.u == 5);
   BOOST_CHECK(data2.salb == 6);
   BOOST_CHECK(data2.cn2Bare == 7);
   BOOST_CHECK(data2.cnRed == 8);
   BOOST_CHECK(data2.cnCov == 9);
   BOOST_CHECK(data2.cnCanopyFact == 10);
   BOOST_CHECK(data2.rootCN == 11);
   BOOST_CHECK(data2.rootWT == 12);
   BOOST_CHECK(data2.soilCN == 13);
   BOOST_CHECK(data2.enrAcoeff == 14);
   BOOST_CHECK(data2.enrBcoeff == 15);

   BOOST_CHECK_THROW(data.getData(111, data1), std::runtime_error);
   tearDown();
   }
//---------------------------------------------------------------------------
// test that the setData method throws.
//---------------------------------------------------------------------------
void testSetData(void)
   {
   setUp();
   Soils::Data data1;
   BOOST_CHECK_THROW(data.setData(111, data1), std::runtime_error);
   tearDown();
   }
//---------------------------------------------------------------------------
// test the adding and retrieval of soil layered data.
//---------------------------------------------------------------------------
void testGetLayeredData(void)
   {
   setUp();
   Soils::LayeredData layered;
   data.getLayeredData(1, layered);
   BOOST_CHECK(layered.depth   == vector<unsigned>(depth, depth+numLayers));
   BOOST_CHECK(doubleVectorsAreEqual(layered.bd, vector<double>(bd, bd+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.ll15, vector<double>(ll15, ll15+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.airdry, vector<double>(airdry, airdry+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.dul, vector<double>(dul, dul+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.sat, vector<double>(sat, sat+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.oc, vector<double>(oc, oc+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.ec, vector<double>(ec, ec+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.ph, vector<double>(ph, ph+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.cl, vector<double>(cl, cl+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.cec, vector<double>(cec, cec+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.ca, vector<double>(ca, ca+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.mg, vector<double>(mg, mg+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.na, vector<double>(na, na+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.k, vector<double>(k, k+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.exchangableSodium, vector<double>(exchangableSodium, exchangableSodium+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.particleSizeSand, vector<double>(particleSizeSand, particleSizeSand+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.particleSizeSilt, vector<double>(particleSizeSilt, particleSizeSilt+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.particleSizeClay, vector<double>(particleSizeClay, particleSizeClay+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.swcon, vector<double>(swcon, swcon+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.fbiom, vector<double>(fbiom, fbiom+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.finert, vector<double>(finert, finert+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.no3ppm, vector<double>(no3ppm, no3ppm+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.nh4ppm, vector<double>(nh4ppm, nh4ppm+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(layered.ureappm, vector<double>(ureappm, ureappm+numLayers)));

   BOOST_CHECK_THROW(data.getLayeredData(1111, layered), std::runtime_error);
   tearDown();
   }
//---------------------------------------------------------------------------
// test the setlayeredData throws.
//---------------------------------------------------------------------------
void testSetLayeredData(void)
   {
   setUp();
   Soils::LayeredData layered;
   BOOST_CHECK_THROW(data.setLayeredData(1111, layered), std::runtime_error);

   data.getLayeredData(1, layered);
   layered.no3ppm.push_back(111.0);
   BOOST_CHECK_THROW(data.setLayeredData(1, layered), std::runtime_error);
   tearDown();
   }
//---------------------------------------------------------------------------
// test that we can get a list of all crops ok.
//---------------------------------------------------------------------------
void testGetCrops(void)
   {
   setUp();
   setupCropData();
   vector<string> cropNames;
   data.getCrops(1, cropNames);
   BOOST_CHECK(cropNames.size() == 2);
   BOOST_CHECK(cropNames[0] == "Sorghum");
   BOOST_CHECK(cropNames[1] == "Wheat");

   BOOST_CHECK_THROW(data.getCrops(1111, cropNames), std::runtime_error);
   tearDown();
   }
//---------------------------------------------------------------------------
// test that we can get crop data.
//---------------------------------------------------------------------------
void testGetCropData(void)
   {
   setUp();
   setupCropData();
   Soils::CropData wheat;
   data.getCropData(1, "Wheat", wheat);
   BOOST_CHECK(wheat.depth   == vector<unsigned>(depth, depth+numLayers));
   BOOST_CHECK(doubleVectorsAreEqual(wheat.ll, vector<double>(wheatLL, wheatLL+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(wheat.kl, vector<double>(wheatKL, wheatKL+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(wheat.xf, vector<double>(wheatXF, wheatXF+numLayers)));

   Soils::CropData sorg;
   data.getCropData(1, "Sorghum", sorg);
   BOOST_CHECK(sorg.depth   == vector<unsigned>(depth, depth+numLayers));
   BOOST_CHECK(doubleVectorsAreEqual(sorg.ll, vector<double>(sorgLL, sorgLL+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(sorg.kl, vector<double>(sorgKL, sorgKL+numLayers)));
   BOOST_CHECK(doubleVectorsAreEqual(sorg.xf, vector<double>(sorgXF, sorgXF+numLayers)));

   BOOST_CHECK_THROW(data.getCropData(1111, "Wheat", wheat), runtime_error);
   BOOST_CHECK_THROW(data.getCropData(1, "XXXX", wheat), runtime_error);

   tearDown();
   }
//---------------------------------------------------------------------------
// test that deleteCrop works.
//---------------------------------------------------------------------------
void testDeleteCrop(void)
   {
   setUp();
   setupCropData();
   data.deleteCrop(1, "Sorghum");
   Soils::CropData wheat;
   data.getCropData(1, "Wheat", wheat);
   BOOST_CHECK(wheat.depth   == vector<unsigned>(depth, depth+numLayers));
   tearDown();
   }
//---------------------------------------------------------------------------
// test that GetPredictedCrops works ok.
//---------------------------------------------------------------------------
void testGetPredictedCrops(void)
   {
   setUp();
   setupCropData();
   vector<string> cropNames;
   data.getPredictedCrops(1, cropNames);
   BOOST_CHECK(cropNames.size() == 2);
   BOOST_CHECK(cropNames[0] == "Cotton");
   BOOST_CHECK(cropNames[1] == "Sorghum");
   tearDown();
   }
//---------------------------------------------------------------------------
// test that GetPredictedCropData works ok.
//---------------------------------------------------------------------------
void testGetPredictedCropData(void)
   {
   setUp();
   setupCropData();
   Soils::CropData cottonData;
   data.getPredictedCropData(1, "Cotton", cottonData);

   BOOST_CHECK(cottonData.depth   == vector<unsigned>(depth, depth+numLayers));
   BOOST_CHECK(doubleVectorsAreEqual(cottonData.ll, vector<double>(cottonPredLL, cottonPredLL+numLayers)));

   Soils::CropData sorgData;
   data.getPredictedCropData(1, "Sorghum", sorgData);
   BOOST_CHECK(doubleVectorsAreEqual(sorgData.ll, vector<double>(sorgPredLL, sorgPredLL+numLayers)));
   tearDown();
   }
//---------------------------------------------------------------------------
// register all tests
//---------------------------------------------------------------------------
test_suite* testSoils(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestSoils");
   test->add(BOOST_TEST_CASE(&testGetInfo));
   test->add(BOOST_TEST_CASE(&testNoSave));
   test->add(BOOST_TEST_CASE(&testSaveAs));
   test->add(BOOST_TEST_CASE(&testSave));
   test->add(BOOST_TEST_CASE(&testDeleteSoil));
   test->add(BOOST_TEST_CASE(&testGetSoils));
   test->add(BOOST_TEST_CASE(&testGetLists));
   return test;
   }

