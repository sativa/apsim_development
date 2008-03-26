//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <SegReport\TExcel.h>
#include "SpreadsheetImporter.h"
#include <soil\soil.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\math_functions.h>
#include <general\xml.h>
#include <general\macro.h>
#include <boost\lexical_cast.hpp>
#include <iomanip>
using namespace boost;

#pragma package(smart_init)

//---------------------------------------------------------------------------
// go import all soils from the specified spreadsheet
//---------------------------------------------------------------------------
void SpreadsheetImporter::importFromFile(string& fileName, Soils& soils)
   {
   string pageName = "SoilData";
   TExcel* excelDataSet = new TExcel(NULL);
   excelDataSet->fileName = fileName.c_str();
   if (excelDataSet->pageNames->IndexOf(pageName.c_str()) == -1)
      {
      delete excelDataSet;
      throw runtime_error("Cannot find any soil data on spreadsheet page: " + pageName);
      }

   excelDataSet->pageName = pageName.c_str();
   excelDataSet->sortFields = "Name;depth";
   excelDataSet->Active = true;

   // Loop through all records and add to soils
   excelDataSet->First();
   while (!excelDataSet->Eof)
      {
      Soil* soil = createSoil(excelDataSet);
      soils.add(*soil);
      //delete soil;
      }
   delete excelDataSet;
   }

string getName(TDataSet* dataset)
   {
   ostringstream name;
   if (dataset->Fields->FindField("refno") != NULL)
      {
      unsigned thisRefNo = getDBUnsigned(dataset, "refno");

      name << '#';
      name.fill('0');
      name.width(3);
      name << thisRefNo;
      name << " - ";
      }
   string soilName = getDBValue(dataset, "Name");
   if (soilName == "")
      soilName = getDBValue(dataset, "Soil.Name");
   string site = getDBValue(dataset, "Site.Name");
   if (site != "" && site != soilName)
      soilName = site + "," + soilName;
   name << soilName;
   return name.str();
   }
//---------------------------------------------------------------------------
// create a soil. Parent must delete pointer to soil when finished.
//---------------------------------------------------------------------------
Soil* SpreadsheetImporter::createSoil(TDataSet* dataset)
   {
   bool measurementsInPercent = false;
   bool cropsPartOfDataSet = true;

   vector<string> cropNames;
   if (cropsPartOfDataSet)
      cropNames = getCropNames(dataset);

   //unsigned thisRefNo;
   string thisName = getName(dataset);
   Soil* soil = new Soil;
   try
      {
      //unsigned thisRefNo = getDBUnsigned(dataset, "refno");
      soil->setName(thisName);
      soil->setSite(getDBValue(dataset, "Site.Name"));
      soil->setRegion(getDBValue(dataset, "Region.Name"));
      soil->setLocalName(getDBValue(dataset, "LocalName"));
      soil->setSoilType(getDBValue(dataset, "SoilType.Name"));
      soil->setNearestTown(getDBValue(dataset, "NearestTown"));
      soil->setComment(getDBValue(dataset, "Comment"));
      soil->setGps(getDBValue(dataset, "GPS"));
      soil->setGpsType(getDBValue(dataset, "GPSType"));
      soil->setMapId(getDBValue(dataset, "MapId"));
      soil->setNaturalVegetation(getDBValue(dataset, "NaturalVegetation"));

      soil->setCona(getDBDouble(dataset, "Cona"));
      soil->setDiffusConst(getDBDouble(dataset, "DiffusConst"));
      soil->setDiffusSlope(getDBDouble(dataset, "DiffusSlope"));
      soil->setU(getDBDouble(dataset, "u"));
      soil->setSalb(getDBDouble(dataset, "salb"));
      soil->setCn2Bare(getDBDouble(dataset, "cn2bare"));
      soil->setCnRed(getDBDouble(dataset, "cnred"));
      soil->setCnCov(getDBDouble(dataset, "cncov"));
      soil->setCnCanopyFact(getDBDouble(dataset, "cncanopyfact"));
      soil->setRootCN(getDBDouble(dataset, "rootcn"));
      soil->setRootWt(getDBDouble(dataset, "rootwt"));
      soil->setSoilCN(getDBDouble(dataset, "soilcn"));
      soil->setEnrAcoeff(getDBDouble(dataset, "enrAcoeff"));
      soil->setEnrBcoeff(getDBDouble(dataset, "enrBCoeff"));

      vector<unsigned> thickness;
      vector<double> bd, ll15, airdry, dul, sat, sw, oc, ec, ph,
         cl, cec, ca, mg, na, k, exchangableSodium,
         particleSizeSand, particleSizeSilt, particleSizeClay,
         swcon, fbiom, finert, no3, nh4;

      vector< vector<double> > ll;
      vector< vector<double> > kl;
      vector< vector<double> > xf;
      vector< vector<double> > kl0;

      int accDepthSoFar = 0;
      while (!dataset->Eof && getName(dataset) == thisName)
         {
         if (dataset->Fields->FindField("dlayer") != NULL)
            getValue(dataset, "dlayer", thickness);
         else
            {
            getValue(dataset, "depth", thickness);
            unsigned layer = thickness.size()-1;
            thickness[layer] *= 10;
            if (thickness.size() > 1)
               thickness[layer] -= accDepthSoFar;

            accDepthSoFar += thickness[layer];
            }

         getValue(dataset, "bd", bd);
         getValue(dataset, "ll15", ll15);
         getValue(dataset, "airdry", airdry);
         getValue(dataset, "dul", dul);
         getValue(dataset, "sat", sat);
         getValue(dataset, "sw", sw);
         getValue(dataset, "oc", oc);
         getValue(dataset, "ec", ec);
         getValue(dataset, "ph", ph);
         getValue(dataset, "cl", cl);
         getValue(dataset, "cec", cec);
         getValue(dataset, "ca", ca);
         getValue(dataset, "mg", mg);
         getValue(dataset, "na", na);
         getValue(dataset, "k", k);
         getValue(dataset, "exchangableSodium", exchangableSodium);
         getValue(dataset, "particleSizeSand", particleSizeSand);
         getValue(dataset, "particleSizeSilt", particleSizeSilt);
         getValue(dataset, "particleSizeClay", particleSizeClay);
         getValue(dataset, "swcon", swcon);
         getValue(dataset, "fbiom", fbiom);
         getValue(dataset, "finert", finert);
         getValue(dataset, "no3ppm", no3);
         getValue(dataset, "nh4ppm", nh4);

         if (cropsPartOfDataSet)
            getCropsFromDataSet(dataset, cropNames, ll, kl, xf, kl0);

         dataset->Next();
         }

      if (measurementsInPercent)
         {
         ll15 = divide_value(ll15, 100.0);
         airdry = divide_value(airdry, 100.0);
         dul = divide_value(dul, 100.0);
         sat = divide_value(sat, 100.0);
         sw = divide_value(sw, 100.0);
         }

      soil->setThickness(thickness);
      soil->setBd(bd);
      soil->setLl15(ll15);
      soil->setAirdry(airdry);
      soil->setDul(dul);
      soil->setSat(sat);
      soil->setSw(sw);
      soil->setOc(oc);
      soil->setEc(ec);
      soil->setPh(ph);
      soil->setCl(cl);
      soil->setCec(cec);
      soil->setCa(ca);
      soil->setMg(mg);
      soil->setNa(na);
      soil->setK(k);
      soil->setExchangableSodium(exchangableSodium);
      soil->setParticleSizeSand(particleSizeSand);
      soil->setParticleSizeSilt(particleSizeSilt);
      soil->setParticleSizeClay(particleSizeClay);
      soil->setSwcon(swcon);
      soil->setFbiom(fbiom);
      soil->setFinert(finert);
      soil->setNo3(no3);
      soil->setNh4(nh4);

      if (cropsPartOfDataSet)
         {
         for (unsigned c = 0; c != cropNames.size(); c++)
            {
            soil->setll(cropNames[c], ll[c]);
            soil->setkl(cropNames[c], kl[c]);
            soil->setxf(cropNames[c], xf[c]);
            soil->setkl0(cropNames[c], kl0[c]);
            }
         }
      }
   catch (const exception& err)
      {
      string msg = string(err.what()) + ". Soil name = " + thisName;
      ShowMessage(msg.c_str());
      }

   return soil;
   }
//---------------------------------------------------------------------------
// Get crop names from the specified dataset.
//---------------------------------------------------------------------------
vector<string> SpreadsheetImporter::getCropNames(TDataSet* dataset)
   {
   vector<string> cropNames;

   vector<string> fieldNames;
   getDBFieldNames(dataset, fieldNames);
   for (unsigned f = 0; f != fieldNames.size(); f++)
      {
      if (fieldNames[f].substr(0, 2) == "ll")
         cropNames.push_back(splitOffBracketedValue(fieldNames[f], '(', ')'));
      }
   return cropNames;
   }
//---------------------------------------------------------------------------
// Get crops from the specified dataset.
//---------------------------------------------------------------------------
void SpreadsheetImporter::getCropsFromDataSet(TDataSet* dataset,
                                              const vector<string>& cropNames,
                                              vector< vector<double> >& ll,
                                              vector< vector<double> >& kl,
                                              vector< vector<double> >& xf,
                                              vector< vector<double> >& kl0)
   {
   for (unsigned c = 0; c != cropNames.size(); c++)
      {
      if (ll.size() <= c)
         {
         ll.push_back(vector<double>());
         kl.push_back(vector<double>());
         xf.push_back(vector<double>());
         kl0.push_back(vector<double>());
         }
      string fieldName = "ll(" + cropNames[c] + ")";
      getValue(dataset, fieldName, ll[c]);

      fieldName = "kl(" + cropNames[c] + ")";
      getValue(dataset, fieldName, kl[c]);

      fieldName = "xf(" + cropNames[c] + ")";
      getValue(dataset, fieldName, xf[c]);

      fieldName = "kl0(" + cropNames[c] + ")";
      getValue(dataset, fieldName, kl0[c]);
      }
   }
//---------------------------------------------------------------------------
// Get crop information from the database.
//---------------------------------------------------------------------------
/*void TForm1::getCropsFromDb(Soil* soil, unsigned thisRefNo)
   {
   // get crop info.
   ostringstream sql;

   sql << "SELECT CropData.*, Crop.Name FROM CropData, Crop, SoilLayeredData, Soil "
       << "WHERE CropID = Crop.ID "
       << " AND SoilLayeredDataID = SoilLayeredData.ID "
       << " AND SoilLayeredData.SoilID = Soil.ID "
       << " AND refno = " << thisRefNo
       << " ORDER BY Crop.Name, depth";
   TDataSet* cropQuery = runQuery(Connection, sql.str());

   while (!cropQuery->Eof)
      {
      string previousCropName = getDBValue(cropQuery, "name");
      vector<double> ll, kl, xf;
      while (!cropQuery->Eof && getDBValue(cropQuery, "name") == previousCropName)
         {
         getValue(cropQuery, "ll", ll);
         getValue(cropQuery, "kl", kl);
         getValue(cropQuery, "xf", xf);
         cropQuery->Next();
         }

      ll = divide_value(ll, 100.0);
      soil->setll(previousCropName, ll);
      soil->setkl(previousCropName, kl);
      soil->setxf(previousCropName, xf);
      }

   delete cropQuery;
   }
*/
//---------------------------------------------------------------------------
// write the specified soil as a text file.
//---------------------------------------------------------------------------
/*void TForm1::convertSoilsFile()
   {
   static const char* macroTemplate =
      "#for_each apsoil\n"
      "#for_each apsoil.layer "
      "apsoil.name|apsoil.cona|apsoil.diffusconst|apsoil.diffusslope|apsoil.u|apsoil.salb|apsoil.cn2bare|apsoil.cnred|apsoil.cncov|apsoil.cncanopyfact|apsoil.rootcn|apsoil.rootwt|apsoil.soilcn|apsoil.enracoeff|apsoil.enrbcoeff|layer.thickness|layer.dul|layer.sat|layer.bd|layer.ll15|layer.airdry|layer.oc|layer.ph|layer.swcon|layer.fbiom|layer.finert|layer.no3|layer.nh4|"
      "#for_each apsoil.layer.crop "
      "crop.name|crop.ll|crop.kl|crop.xf|"
      "#endfor\n\n"
      "#endfor\n"
      "#endfor\n";
   try
      {
      ostringstream soilContents;
      ifstream in("soils.xml");
      soilContents << in.rdbuf();
      string contents = soilContents.str();
      replaceAll(contents, "<soil ", "<apsoil ");
      replaceAll(contents, "</soil>", "</apsoil>");
      XMLDocument xml(contents, XMLDocument::xmlContents);

      ofstream out("soils.txt");
      out << "name|cona|diffusconst|diffusslope|u|salb|cn2bare|cnred|cncov|cncanopyfact|rootcn|rootwt|soilcn|enracoeff|enrbcoeff";
      out << "|depth|dul|sat|bd|ll15|airdry|oc|ph|swcon|fbiom|finert|no3|nh4";
      out << "|cropname|ll|kl|xf\n";

      Macro macro;
      macro.go(xml.documentElement(), macroTemplate, out);
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }
*/
