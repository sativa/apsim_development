//---------------------------------------------------------------------------
#include <general\pch.h>
#pragma hdrstop

#include "SoilSample.h"
#include <general\xml.h>
#include <general\math_functions.h>
#include <boost\lexical_cast.hpp>
#include <numeric>
#include "Soil.h"
using namespace std;
using namespace boost;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
SoilSample::SoilSample(void)
   {
   doc = new XMLDocument("SoilSample", XMLDocument::rootName);
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
SoilSample::SoilSample(const string& xml)
   {
   doc = new XMLDocument(xml, XMLDocument::xmlContents);
   }

SoilSample::SoilSample(const XMLNode& xml)
   {
      doc = new XMLDocument(xml.write(), XMLDocument::xmlContents);

//      XMLNode childNode = doc->documentElement();
//      doc = new XMLDocument(
//      xml;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
SoilSample::~SoilSample(void)
   {
   delete doc;
   }
// ------------------------------------------------------------------
// Write the soil sample contents to the specified stream.
// ------------------------------------------------------------------
void SoilSample::write(std::ostream& out)
   {
   out << doc->documentElement().write();
   }
// ------------------------------------------------------------------
// Return soil water as mm
// ------------------------------------------------------------------
vector<double> SoilSample::swMm()
   {
   return multiply(sw(), thickness());
   }
// ------------------------------------------------------------------
// Return soil water as percent
// ------------------------------------------------------------------
vector<double> SoilSample::swPercent()
   {
   return multiply_value(sw(), 100.0);
   }
// ------------------------------------------------------------------
// Set soil water with the specified units.
// ------------------------------------------------------------------
void SoilSample::setSwMm(const vector<double>& swMm)
   {
   setSw(divide(swMm, thickness()));
   }
// ------------------------------------------------------------------
// Set soil water with the specified units.
// ------------------------------------------------------------------
void SoilSample::setSwPercent(const vector<double>& swPercent)
   {
   setSw(divide_value(swPercent, 100.0));
   }
// ------------------------------------------------------------------
// Return no3 as kg/ha
// ------------------------------------------------------------------
vector<double> SoilSample::no3KgHa(const vector<double>& bd)
   {
   return ppmToKgHa(no3(), bd);
   }
// ------------------------------------------------------------------
// Set the no3 values
// ------------------------------------------------------------------
void SoilSample::setNo3KgHa(const std::vector<double>& no3KgHa, const std::vector<double>& bd)
   {
   setNo3(kgHaToPpm(no3KgHa, bd));
   }
// ------------------------------------------------------------------
// Return nh4 as kg/ha
// ------------------------------------------------------------------
vector<double> SoilSample::nh4KgHa(const vector<double>& bd)
   {
   return ppmToKgHa(nh4(), bd);
   }
// ------------------------------------------------------------------
// Set the nh4 values
// ------------------------------------------------------------------
void SoilSample::setNh4KgHa(const std::vector<double>& nh4KgHa, const std::vector<double>& bd)
   {
   setNh4(kgHaToPpm(nh4KgHa, bd));
   }
// ------------------------------------------------------------------
// Return oc as a ppm.
// ------------------------------------------------------------------
vector<double> SoilSample::ocPpm()
   {
   return divide_value(oc(), 10000.0);
   }
// ------------------------------------------------------------------
// Set oc as a ppm.
// ------------------------------------------------------------------
void SoilSample::setOcPpm(const std::vector<double>& ocPpm)
   {
   setOc(multiply_value(ocPpm, 10000.0));
   }
// ------------------------------------------------------------------
// Map the sample to the specified soil. Takes care of mapping soil
// sample layer structure to soil layer structure.
// ------------------------------------------------------------------
void SoilSample::mapSampleToSoil(Soil& soil)
   {
   vector<double> sw, no3, nh4, oc, ec, ph, esp;
   if (hasData("water", "sw"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("water", "sw", soil, sampleThickness, sw);
      sw = multiply(sw, sampleThickness); // to mm
      sw = spatialRedistribute(sw, sampleThickness, soil.thickness());
      }
   if (hasData("nitrogen", "no3"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("nitrogen", "no3", soil, sampleThickness, no3);
      no3 = massRedistribute(no3, sampleThickness, soil.thickness(), soil.bd());
      }
   if (hasData("nitrogen", "nh4"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("nitrogen", "nh4", soil, sampleThickness, nh4);
      nh4 = massRedistribute(nh4, sampleThickness, soil.thickness(), soil.bd());
      }
   if (hasData("nitrogen", "oc"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("nitrogen", "oc", soil, sampleThickness, oc);
      oc = divide_value(oc, 10000.0); // to ppm
      oc = massRedistribute(oc, sampleThickness, soil.thickness(), soil.bd());
      }
   if (hasData("other", "ec"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("other", "ec", soil, sampleThickness, ec);
      ec = multiply(ec, sampleThickness);
      ec = spatialRedistribute(ec, sampleThickness, soil.thickness());
      ec = divide(ec, soil.thickness());
      }
   if (hasData("nitrogen", "ph"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("nitrogen", "ph", soil, sampleThickness, ph);
      ph = multiply(ph, sampleThickness);
      ph = spatialRedistribute(ph, sampleThickness, soil.thickness());
      ph = divide(ph, soil.thickness());
      }
   if (hasData("other", "esp"))
      {
      vector<unsigned> sampleThickness;
      createVariableForMapping("other", "esp", soil, sampleThickness, esp);
      esp = multiply(esp, sampleThickness);
      esp = spatialRedistribute(esp, sampleThickness, soil.thickness());
      esp = divide(esp, soil.thickness());
      }

   setThickness(soil.thickness());
   if (hasData("water", "sw"))
      setSwMm(sw);
   if (hasData("nitrogen", "no3"))
      setNo3(no3);
   if (hasData("nitrogen", "nh4"))
      setNh4(nh4);
   if (hasData("nitrogen", "oc"))
      setOcPpm(oc);
   if (hasData("other", "ec"))
      setEc(ec);
   if (hasData("nitrogen", "ph"))
      setPh(ph);
   if (hasData("other", "esp"))
      setEsp(esp);
   }
// ------------------------------------------------------------------
// Spatial mass redistribution algorithm.
// ------------------------------------------------------------------
vector<double> SoilSample::spatialRedistribute(const vector<double>& fromMass,
                                               const vector<unsigned>& fromThickness,
                                               const vector<unsigned>& toThickness)
   {
   if (fromMass.size() != fromThickness.size())
      throw runtime_error ("Cannot redistribute soil sample layer structure to soil layer structure. "
                           "The number of values in the sample doesn't match the number of layers in the sample.");

   // Remapping is achieved by first constructing a map of
   // cumulative mass vs depth
   // The new values of mass per layer can be linearly
   // interpolated back from this shape taking into account
   // the rescaling of the profile.
   vector<double> toMass;

   // build interpolation pairs based on 'squashed' original root profile
   vector<double> cumDepth, cumMass;
   cumDepth.push_back(0.0);
   cumMass.push_back(0.0);

   for (unsigned layer = 1; layer <= fromThickness.size(); layer++)
      {
      cumDepth.push_back(cumDepth[layer-1] + fromThickness[layer-1]);
      cumMass.push_back(cumMass[layer-1] + fromMass[layer-1]);
      }

   // look up new mass from interpolation pairs
   for (unsigned layer = 1; layer <= toThickness.size(); layer++)
      {
      unsigned layerBottom = accumulate(toThickness.begin(),
                                        toThickness.begin()+layer, 0.0);
      unsigned layerTop = layerBottom - toThickness[layer-1];
      bool didInterpolate;
      double cumMassTop = linear_interp_real(layerTop,
                                             cumDepth, cumMass,
                                             didInterpolate);
      double cumMassBottom = linear_interp_real(layerBottom,
                                                cumDepth, cumMass,
                                                didInterpolate);
      toMass.push_back(cumMassBottom - cumMassTop);
      }
   return toMass;
   }
// ------------------------------------------------------------------
// Uses bulk density to do a spatial mass redistribution algorithm.
// ------------------------------------------------------------------
vector<double> SoilSample::massRedistribute(const vector<double>& fromMass,
                                            const vector<unsigned>& fromThickness,
                                            const vector<unsigned>& toThickness,
                                            const vector<double>& toBd)
   {
   // need to work out a bd for this sample from the characterisation bd passed in.
   vector<double> bdMass = multiply(toBd, toThickness);
   vector<double> fromBd = spatialRedistribute(bdMass, toThickness, fromThickness);
   fromBd = divide(fromBd, fromThickness);

   // now get mass as kg/ha.
   vector<double> newValuesKgHa;
   for (unsigned l = 0; l != fromMass.size(); l++)
      newValuesKgHa.push_back(fromMass[l] * fromBd[l] * fromThickness[l] / 100);

   newValuesKgHa = spatialRedistribute(newValuesKgHa, fromThickness, toThickness);

   // now convert mass back to ppm.
   vector<double> newValuesPpm;
   for (unsigned l = 0; l != newValuesKgHa.size(); l++)
      newValuesPpm.push_back(newValuesKgHa[l] * 100.0 / toBd[l] / toThickness[l]);
   return newValuesPpm;
   }
// ------------------------------------------------------------------
// Create a vector of values ready for interpolation. Values come from
// this sample as well as the specified soil. A dummy layer will optionally
// be inserted so that layer thicknesses line up with the soil.
// ------------------------------------------------------------------
void SoilSample::createVariableForMapping(const std::string& propertyType,
                                          const string& dataType,
                                          Soil& soil,
                                          vector<unsigned>& sampleThickness,
                                          vector<double>& sampleValues)
   {
   sampleThickness = thickness();
   sampleValues = getLayered(propertyType.c_str(), dataType.c_str());

   // look for missing values and remove unwanted depths.
   unsigned cumSampleDepth = 0;
   unsigned layer;
   for (layer = 0;
                 layer != sampleThickness.size() && !isMissingValue(sampleValues[layer]);
                 layer++)
      cumSampleDepth += sampleThickness[layer];
   sampleThickness.erase(sampleThickness.begin() + layer, sampleThickness.end());
   sampleValues.erase(sampleValues.begin() + layer, sampleValues.end());

   // work out if we need to create a dummy layer so that the sample depths line up
   // with the soil depths - makes interpolation easier.
   vector<unsigned> soilThickness = soil.thickness();
   unsigned dummyLayerThickness = 0;
   unsigned cumSoilDepth = 0;
   for (layer = 0; layer != soilThickness.size(); layer++)
      {
      cumSoilDepth += soilThickness[layer];
      if (cumSoilDepth == cumSampleDepth)
         break;
      if (cumSoilDepth > cumSampleDepth)
         {
         dummyLayerThickness = cumSoilDepth-cumSampleDepth;
         break;
         }
      }
   if (dummyLayerThickness > 0)
      {
      sampleThickness.push_back(dummyLayerThickness);
      double lastValue = sampleValues[sampleValues.size()-1];
      sampleValues.push_back(lastValue);
      }

   copyValuesBelowDepth(soilThickness, soilThickness, sampleThickness,
                        cumSoilDepth);

   // copy values from the base soil. For ec and esp allow base soil to have
   // no default values. In that situation, supply default values manually.
   if (soil.hasData(propertyType, dataType))
      {
      vector<double> soilValues = soil.getLayered(propertyType.c_str(), dataType.c_str());
      copyValuesBelowDepth(soilThickness, soilValues, sampleValues,
                           cumSoilDepth);
      }
   else if (dataType == "ec")
      {
      while (sampleValues.size() != sampleThickness.size())
         sampleValues.push_back(0.5); // default value that produces xf = 1
      }
   else if (dataType == "esp")
      {
      while (sampleValues.size() != sampleThickness.size())
         sampleValues.push_back(10); // default value that produces xf = 1
      }
   }
// ------------------------------------------------------------------
// Set a specific property of the sample.
// ------------------------------------------------------------------
void SoilSample::setInfo(const char* infoType, const std::string& value)
   {
   if (value != "")
      {
      XMLNode childNode = doc->documentElement().appendChild(infoType);
      childNode.setValue(value);
      }
   }

// ------------------------------------------------------------------
// Return depths in sample
// ------------------------------------------------------------------
vector<unsigned> SoilSample::depth()
   {
   vector<unsigned> depth;
   vector<unsigned> thick = thickness();
   if (thick.size() > 0)
      {
      depth.push_back(thick[0]);
      for (unsigned l = 1; l != thick.size(); l++)
         depth.push_back(thick[l] + depth[l-1]);
      }
   return depth;
   }

// ------------------------------------------------------------------
// Return a specific property of the sample
// ------------------------------------------------------------------
std::string SoilSample::getInfo(const char* typeName)
   {
   XMLNode::iterator i = find_if(doc->documentElement().begin(), doc->documentElement().end(),
                                 EqualToName<XMLNode>(typeName));
   if (i == doc->documentElement().end())
      return "";
   else
      {
      string value = i->getValue();
      stripLeadingTrailing(value, " ");
      return value;
      }
   }

// ------------------------------------------------------------------
// Set depths in sample
// ------------------------------------------------------------------
void SoilSample::setDepth(const vector<unsigned>& depth)
   {
   vector<unsigned> thickness;
   if (depth.size() > 0)
      thickness.push_back(depth[0]);
   for (unsigned l = 1; l != depth.size(); l++)
      thickness.push_back(depth[l] - depth[l-1]);

   setThickness(thickness);
   }

// ------------------------------------------------------------------
// Return name of soil
// ------------------------------------------------------------------
void SoilSample::setName(const std::string& newName)
   {
   doc->documentElement().setAttribute("name", newName);
   }

// ------------------------------------------------------------------
// return plant available water content by layer (mm) given
// depth, lower limit and dul all in (mm).
// ------------------------------------------------------------------
void _export calcSW(const vector<double>& wet,
                    const vector<double>& dry,
                     const vector<double>& bd,
                      vector<double>& sw)
   {
   sw.erase(sw.begin(), sw.end());

   // calculate depth increments.
   if(wet.size() == dry.size()){
     if (wet.size() > 0)
        {
        for (unsigned layer = 0; layer != wet.size(); layer++)
           if(dry[layer]!=0){
             sw.push_back(((wet[layer]-dry[layer])/dry[layer])*bd[layer]);
           } else {
             sw.push_back(0);
           }
        }
     }
   }
// ------------------------------------------------------------------
// return true if two depth vectors given contain same information
// ------------------------------------------------------------------
bool _export depthEqual(const vector<unsigned>& depth1,
                        const vector<unsigned>& depth2)
   {
   if(depth1.size()!=depth2.size()){
      return false;
   } else {
      for(int i = 0; i < depth1.size();i++){
         if(depth1[i] != depth2[i]){
            return false;
         }
      }
      return true;
   }
   }

// ------------------------------------------------------------------
// Return name of soil
// ------------------------------------------------------------------
std::string SoilSample::name(void)
   {
   return doc->documentElement().getAttribute("name");
   }
