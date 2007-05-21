//---------------------------------------------------------------------------
#include <general\pch.h>
#pragma hdrstop

#include "SoilBase.h"
#include <general\xml.h>
#include <boost\lexical_cast.hpp>
#include <general\string_functions.h>
#include <general\math_functions.h>
#include <numeric>

using namespace std;
using namespace boost;

// ------------------------------------------------------------------
// Get a layer property from a specified layer
// ------------------------------------------------------------------
template<class T>
struct GetLayerValue
   {
   string name;
   vector<T>& values;
   GetLayerValue(const string& n, vector<T>& v)
      : name(n), values(v)
      {}

   void operator() (XMLNode& node)
      {
      XMLNode::iterator i = find_if(node.begin(), node.end(),
                                    EqualToName<XMLNode>(name));
      if (i == node.end())
         values.push_back(missingValue<T>());
      else
         {
         string value = i->getValue();
         stripLeadingTrailing(value, " ");
         values.push_back(lexical_cast<T>(value));
         }
      }
   };
// ------------------------------------------------------------------
// Return a specific layered data
// ------------------------------------------------------------------
vector<double> SoilBase::getLayered(const char* propertyType, const char* layeredType)
   {
  vector<double> values;
   if (hasData(propertyType, layeredType))
      {
      XMLNode propertyNode = findNode(doc->documentElement(),propertyType);

      GetLayerValue<double> getLayerValue(layeredType, values);
      for_each_if(propertyNode.begin(),
                  propertyNode.end(),
                  getLayerValue,
                  EqualToName<XMLNode>("layer"));
      return values;
      }
   else if (0 == stricmp(layeredType, "Wet")|| 0 == stricmp(layeredType, "Dry"))
      return values;
   else if (0 == stricmp(propertyType, "Nitrogen")|| 0 == stricmp(propertyType, "Water"))
      {
      if(throwexceptions)
         throw runtime_error("Cannot find soil property " + string(layeredType));
      else
         return values;
      }
   else
      return values;
   }

// ------------------------------------------------------------------
// Return true if sample as the specified data.
// ------------------------------------------------------------------
bool SoilBase::hasData(const std::string& propertyType, const std::string& dataType)
   {
   XMLNode propertyNode = findNode(doc->documentElement(),propertyType);
   if(propertyNode.isValid())
      {
      XMLNode layerNode = findNode(propertyNode, "layer");
      if (layerNode.isValid())
         return findNode(layerNode, dataType).isValid();
      else
         return false;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// Set the soil profile depths.
// ------------------------------------------------------------------
template <class T>
void setLayeredValues(XMLNode soilNode, const string& dataName, const vector<T>& values)
   {
   for (unsigned l = 0; l != values.size(); l++)
      {
      if (!isMissingValue(values[l]))
         {
         XMLNode layerNode = appendChildIfNotExist(soilNode,
                                                   "layer",
                                                   lexical_cast<string>(l+1));
         XMLNode childNode = layerNode.appendChild(dataName);
         childNode.setValue(lexical_cast<string>(values[l]));
         }
      }
   }
// ------------------------------------------------------------------
// Set a layered data property
// ------------------------------------------------------------------
void SoilBase::setLayered(const char* propertyType, const char* layeredType, const std::vector<double>& values)
   {
   if (values.size() > 0)
      {
      if (dlayer().size() >= values.size())
         {
         XMLNode propertyNode = appendChildIfNotExist(doc->documentElement(),propertyType,"");
         setLayeredValues(propertyNode, layeredType, values);
         }
      else
         {
         string msg = "Invalid number of values for variable: ";
         msg += layeredType;
         msg += ". The number of values does not equal the number of layers.";
         throw runtime_error(msg);
         }
      }
   }

// ------------------------------------------------------------------
// Return the soil profile depths.
// ------------------------------------------------------------------
vector<unsigned> SoilBase::dlayer(void)
   {
   vector<unsigned> dlayer;
   GetLayerValue<unsigned> getDepth("thickness", dlayer);
   XMLNode waterNode = appendChildIfNotExist(doc->documentElement(),"Water","");
   for_each_if(waterNode.begin(),
               waterNode.end(),
               getDepth,
               EqualToName<XMLNode>("layer"));
   return dlayer;
   }
// ------------------------------------------------------------------
// Set the soil profile depths.
// ------------------------------------------------------------------
void SoilBase::setDlayer(const vector<unsigned>& dlayer)
   {
   XMLNode waterNode = appendChildIfNotExist(doc->documentElement(),"Water","");
   setLayeredValues(waterNode, "thickness", dlayer);

   // Get rid of unwanted layers.
   unsigned layersSoFar = 0;
   XMLNode::iterator child = waterNode.begin();
   while (child != waterNode.end())
      {
      if (child->getName() == "layer")
         {
         layersSoFar++;
         if (layersSoFar > dlayer.size())
            {
            XMLNode::iterator temp = child;
            temp++;
            waterNode.erase(child);
            child = temp;
            }
         else
            child++;
         }
      else
         child++;
      }
   }
// ------------------------------------------------------------------
// Convert the values from ppm to kg/ha
// ------------------------------------------------------------------
vector<double> SoilBase::ppmToKgHa(const vector<double>& ppm, const vector<double>& bd)
   {
   vector<unsigned> thickness = dlayer();
   if (ppm.size() != thickness.size() || ppm.size() != bd.size())
      throw runtime_error("Cannot convert ppm to kg/ha in soil sample as the number of layers doesn't match.");
   vector<double> kgHa;
   for (unsigned l = 0; l != ppm.size(); l++)
      kgHa.push_back(ppm[l] * bd[l] * thickness[l] / 100);
   return kgHa;
   }
// ------------------------------------------------------------------
// Convert kg/ha to ppm
// ------------------------------------------------------------------
vector<double> SoilBase::kgHaToPpm(const vector<double>& kgHa, const vector<double>& bd)
   {
   vector<unsigned> thickness = dlayer();
   if (kgHa.size() != thickness.size() || kgHa.size() != bd.size())
      throw runtime_error("Cannot convert kg/ha to ppm in soil sample as the number of layers doesn't match.");
   vector<double> ppm;
   for (unsigned l = 0; l != kgHa.size(); l++)
      ppm.push_back(kgHa[l] / bd[l] / thickness[l] * 100);
   return ppm;
   }
// ------------------------------------------------------------------
// convert a vector of soil thicknesses(mm) to a vector of layer strings(cm)
// e.g. "0-10", "10-30" ...
// ------------------------------------------------------------------
vector<string> thicknessToLayerStrings(const std::vector<unsigned>& thickness)
   {
   vector<string> layerStrings;
   unsigned depthSoFar = 0;
   for (unsigned l = 0; l != thickness.size(); l++)
      {
      unsigned thisThickness = thickness[l] / 10;   // to cm
      layerStrings.push_back(lexical_cast<string>(depthSoFar)
                             + "-"
                             + lexical_cast<string>(depthSoFar + thisThickness));
      depthSoFar = depthSoFar + thisThickness;
      }
   return layerStrings;
   }
// ------------------------------------------------------------------
// convert a vector of soil layer strings e.g. "0-10", "10-30" ...
// to a vector of layer strings(cm) e.g. 10, 20 ...
// ------------------------------------------------------------------
std::vector<unsigned> layerStringsToThickness(const std::vector<std::string>& layerStrings)
   {
   vector<unsigned> thickness;
   for (unsigned l = 0; l != layerStrings.size(); l++)
      {
      unsigned posDash = layerStrings[l].find('-');
      if (posDash == string::npos)
         throw runtime_error("Invalid layer string: " + layerStrings[l] +
                             ". String must be of the form: 10-30");

      string topDepthString = layerStrings[l].substr(0, posDash);
      stripLeadingTrailing(topDepthString, " ");
      unsigned topDepth = lexical_cast<unsigned>(topDepthString);

      string bottomDepthString = layerStrings[l].substr(posDash+1);
      stripLeadingTrailing(bottomDepthString, " ");
      unsigned bottomDepth = lexical_cast<unsigned>(bottomDepthString);

      thickness.push_back((bottomDepth - topDepth) * 10);
      }
   return thickness;
   }

// ------------------------------------------------------------------
//  Return a calculated layer mid point (mm) ie depth center
//  eg.  Depths
//         15
//         30
//         60
//         90
//        120
//  gives
//       depthCenter
//         7.5
//        22.5
//          45
//          75
//         105
// ------------------------------------------------------------------
void calcDepthCenter(const vector<unsigned>& depths,
                     vector<double>& depthCenter)
   {
   for (unsigned layer = 0; layer != depths.size(); layer++)
      {
      if (layer == 0)
         depthCenter.push_back(depths[layer] / 2.0);
      else
         depthCenter.push_back( (depths[layer] + depths[layer-1]) / 2.0);
      }
   }
// ------------------------------------------------------------------
// Return a calculated depth thicknesses (mm)
//     eg.  Depths
//            15
//            30
//            60
//            90
//           120
//     gives:
//          Thickness
//             15
//             15
//             30
//             30
//             30
// ------------------------------------------------------------------
void calcThickness (const vector<unsigned>& depths,
                    vector<unsigned>& thickness)
   {
   for (unsigned layer = 0; layer != depths.size(); layer++)
      {
      if (layer == 0)
         thickness.push_back(depths[layer]);
      else
         thickness.push_back( (depths[layer] - depths[layer-1]) );
      }
   }


