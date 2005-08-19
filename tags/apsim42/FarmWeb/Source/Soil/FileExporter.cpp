//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "FileExporter.h"
#include "Soils.h"
#include <general\Macro.h>
#include <general\string_functions.h>
#include <general\math_functions.h>
#include <general\path.h>
#include <general\xml.h>
#include <iomanip>
#include <general\db_functions.h>

#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
// Format and return a space separated string for the given vector of numbers.
//---------------------------------------------------------------------------
template <class T>
string formatString(vector<T>& values)
   {
   ostringstream out;
   for (unsigned l = 0; l != values.size(); l++)
      {
      if (isMissingValue(values[l]))
         break;

      out << setw(8);
      out << setprecision(3);
      out.setf(ios::fixed, ios::floatfield);
      out << values[l];
      }
   return out.str();
   }
//---------------------------------------------------------------------------
// Return a wr string for the given number of depths.
//---------------------------------------------------------------------------
string getWrString(unsigned numLayers)
   {
   string returnString = "1.00    0.86    0.64    0.40";
   for (unsigned i = 4; i < numLayers; i++)
      returnString += "    0.10";
   return returnString;
   }
//---------------------------------------------------------------------------
// Return a nem string for the given number of depths.
//---------------------------------------------------------------------------
string getNemString(unsigned numLayers)
   {
   string returnString;
   for (unsigned i = 0; i != numLayers; i++)
      returnString += "    0.60";
   return returnString;
   }
//---------------------------------------------------------------------------
// Do export.
//---------------------------------------------------------------------------
void FileExporter::doExport(Soils& soils,
                            unsigned refno,
                            const std::string& templateFile,
                            vector<string>& filesGenerated)
   {
   // remember the directory so that we can reinstate it later.
   AnsiString cwd = GetCurrentDir();
   Path::getTempFolder().Change_directory();

   try
      {
      XMLDocument xml("data", XMLDocument::rootName);
      XMLNode soilNode = xml.documentElement().appendChild("soil", true);

      // Store all soil info into attributes.
      Soils::Info info;
      soils.getInfo(refno, info);

      soilNode.setAttribute("name", info.name);
      soilNode.appendChild("refNo", true).setValue(IntToStr(refno).c_str());
      soilNode.appendChild("region", true).setValue(info.region);
      soilNode.appendChild("site", true).setValue(info.site);
      soilNode.appendChild("localName", true).setValue(info.localName);
      soilNode.appendChild("soilType", true).setValue(info.soilType);
      soilNode.appendChild("nearestTown", true).setValue(info.nearestTown);
      soilNode.appendChild("comment", true).setValue(info.comment);
      soilNode.appendChild("gps", true).setValue(info.gps);
      soilNode.appendChild("gpsType", true).setValue(info.gpsType);
      soilNode.appendChild("mapId", true).setValue(info.mapId);
      soilNode.appendChild("naturalVegetation", true).setValue(info.naturalVegetation);

      // Store all layered data into attributes.
      Soils::LayeredData layeredData;
      soils.getLayeredData(refno, layeredData);

      // calculate thickness in mm.
      vector<unsigned> thickness;
      for (unsigned l = 0; l != layeredData.depth.size(); l++)
         {
         if (l == 0)
            thickness.push_back(layeredData.depth[l] * 10);
         else
            thickness.push_back((layeredData.depth[l]-layeredData.depth[l-1])*10);
         }

      // convert to %vol
      convertToPercentVol(layeredData.ll15);
      convertToPercentVol(layeredData.airdry);
      convertToPercentVol(layeredData.dul);
      convertToPercentVol(layeredData.sat);

      soilNode.appendChild("thickness", true).setValue(formatString(thickness));
      soilNode.appendChild("depth", true).setValue(formatString(layeredData.depth));
      soilNode.appendChild("bd", true).setValue(formatString(layeredData.bd));
      soilNode.appendChild("ll15", true).setValue(formatString(layeredData.ll15));
      soilNode.appendChild("airdry", true).setValue(formatString(layeredData.airdry));
      soilNode.appendChild("dul", true).setValue(formatString(layeredData.dul));
      soilNode.appendChild("sat", true).setValue(formatString(layeredData.sat));
      soilNode.appendChild("oc", true).setValue(formatString(layeredData.oc));
      soilNode.appendChild("ec", true).setValue(formatString(layeredData.ec));
      soilNode.appendChild("ph", true).setValue(formatString(layeredData.ph));
      soilNode.appendChild("cl", true).setValue(formatString(layeredData.cl));
      soilNode.appendChild("cec", true).setValue(formatString(layeredData.cec));
      soilNode.appendChild("ca", true).setValue(formatString(layeredData.ca));
      soilNode.appendChild("mg", true).setValue(formatString(layeredData.mg));
      soilNode.appendChild("na", true).setValue(formatString(layeredData.na));
      soilNode.appendChild("k", true).setValue(formatString(layeredData.k));
      soilNode.appendChild("exchangableSodium", true).setValue(formatString(layeredData.exchangableSodium));
      soilNode.appendChild("particleSizeSand", true).setValue(formatString(layeredData.particleSizeSand));
      soilNode.appendChild("particleSizeSilt", true).setValue(formatString(layeredData.particleSizeSilt));
      soilNode.appendChild("particleSizeClay", true).setValue(formatString(layeredData.particleSizeClay));
      soilNode.appendChild("swcon", true).setValue(formatString(layeredData.swcon));
      soilNode.appendChild("fbiom", true).setValue(formatString(layeredData.fbiom));
      soilNode.appendChild("finert", true).setValue(formatString(layeredData.finert));
      soilNode.appendChild("no3ppm", true).setValue(formatString(layeredData.no3ppm));
      soilNode.appendChild("nh4ppm", true).setValue(formatString(layeredData.nh4ppm));
      soilNode.appendChild("ureappm", true).setValue(formatString(layeredData.ureappm));

      // Store all soil data into attributes.
      Soils::Data soilData;
      soils.getData(refno, soilData);
      soilNode.appendChild("maxEvap", true).setValue(ftoa(soilData.maxEvap, 3));
      soilNode.appendChild("cona", true).setValue(ftoa(soilData.cona, 3));
      soilNode.appendChild("diffusConst", true).setValue(ftoa(soilData.diffusConst, 3));
      soilNode.appendChild("diffusSlope", true).setValue(ftoa(soilData.diffusSlope, 3));
      soilNode.appendChild("u", true).setValue(ftoa(soilData.u, 3));
      soilNode.appendChild("salb", true).setValue(ftoa(soilData.salb, 3));
      soilNode.appendChild("cn2Bare", true).setValue(ftoa(soilData.cn2Bare, 3));
      soilNode.appendChild("cnRed", true).setValue(ftoa(soilData.cnRed, 3));
      soilNode.appendChild("cnCov", true).setValue(ftoa(soilData.cnCov, 3));
      soilNode.appendChild("cnCanopyFact", true).setValue(ftoa(soilData.cnCanopyFact, 3));
      soilNode.appendChild("rootCN", true).setValue(ftoa(soilData.rootCN, 3));
      soilNode.appendChild("rootWT", true).setValue(ftoa(soilData.rootWT, 3));
      soilNode.appendChild("soilCN", true).setValue(ftoa(soilData.soilCN, 3));
      soilNode.appendChild("enrAcoeff", true).setValue(ftoa(soilData.enrAcoeff, 3));
      soilNode.appendChild("enrBcoeff", true).setValue(ftoa(soilData.enrBcoeff, 3));

      // add a crop macro.
      vector<string> crops;
      soils.getCrops(refno, crops);
      for (unsigned c = 0; c != crops.size(); c++)
         {
         XMLNode cropNode = soilNode.appendChild("Crop", true);
         cropNode.setAttribute("name", crops[c]);
         Soils::CropData cropData;
         soils.getCropData(refno, crops[c], cropData);

         convertToPercentVol(cropData.ll);

         cropNode.appendChild("depth", true).setValue(formatString(cropData.depth));
         cropNode.appendChild("ll", true).setValue(formatString(cropData.ll));
         cropNode.appendChild("kl", true).setValue(formatString(cropData.kl));
         cropNode.appendChild("xf", true).setValue(formatString(cropData.xf));
         cropNode.appendChild("wr", true).setValue(getWrString(cropData.depth.size()));
         cropNode.appendChild("nem", true).setValue(getNemString(cropData.depth.size()));
         cropNode.appendChild("predicted", true).setValue("false");
         }

      // add predicted crops.
/*      vector<string> predictedCrops;
      data.getPredictedCrops(refno, predictedCrops);
      for (unsigned c = 0; c != predictedCrops.size(); c++)
         {
         if (find(crops.begin(), crops.end(), predictedCrops[c]) == crops.end())
            {
            MacroSubstFile::ValueAttributes cropAttributes;

            vector<unsigned> depth;
            vector<double> ll, dul, kl, xf;
            data.calcCropLL(refno, predictedCrops[c], depth, ll, dul, kl, xf);
            if (ll.size() > 0)
               {

               // convert LL from %vol to mm/mm
               for (unsigned l = 0; l != depth.size(); l++)
                  {
                  ll[l] = ll[l] / 100.0;
                  }

               cropAttributes.push_back(make_pair("name", predictedCrops[c]));
               cropAttributes.push_back(make_pair("ll", formatString(ll)));
               cropAttributes.push_back(make_pair("kl", formatString(kl)));
               cropAttributes.push_back(make_pair("xf", formatString(xf)));
               cropAttributes.push_back(make_pair("wr", getWrString(depth.size())));
               cropAttributes.push_back(make_pair("nem", getNemString(depth.size())));
               cropAttributes.push_back(make_pair("predicted", "true"));

               macro.addValue("crop", cropAttributes);
               }
            }
         }
*/
      if (!FileExists(templateFile.c_str()))
         throw runtime_error("Cannot find template file: " + templateFile);

      ifstream templateIn(templateFile.c_str());
      ostringstream contents;
      contents << templateIn.rdbuf();
      Macro macro;
      macro.go(xml.documentElement(), contents.str(), filesGenerated);
      }
   catch (...)
      {
      SetCurrentDir(cwd);
      throw;
      }

   SetCurrentDir(cwd);
   }
