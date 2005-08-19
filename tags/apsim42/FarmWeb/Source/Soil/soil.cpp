//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Soil.h"
#include <general\math_functions.h>
#include <general\xml.h>
#include <general\macro.h>
#include <general\stl_functions.h>
#include <boost\lexical_cast.hpp>
#include <boost\bind.hpp>
#include <numeric>
#include <iterator>
#include <iomanip>
#include <math.h>

using namespace std;
using namespace boost;
//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------
// Format and return a space separated string for the given vector of numbers.
//---------------------------------------------------------------------------
template <class T>
string formatString(const vector<T>& values)
   {
   ostringstream out;
   for (unsigned l = 0; l != values.size(); l++)
      {
      out << setw(8);
      out << setprecision(3);
      out.setf(ios::fixed, ios::floatfield);
      out << values[l];
      }
   return out.str();
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Soil::Soil(void)
   {
      doc = new XMLDocument("Soil", XMLDocument::rootName);
      XMLNode initWater = doc->documentElement().appendChild("InitWater", false);
      XMLNode percentMethod = initWater.appendChild("percentmethod", false);
      XMLNode percent = percentMethod.appendChild("percent", false);
      percent.setValue("1");
      XMLNode distributed = percentMethod.appendChild("distributed", false);
      distributed.setValue("filled from top");
   }

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Soil::Soil(const string& xml)
   {
      doc = new XMLDocument(xml, XMLDocument::xmlContents);
   }

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Soil::Soil(const char* filename)
   {
      ostringstream soilXml;
      ifstream in(filename);
      soilXml << in.rdbuf();
      doc = new XMLDocument(soilXml.str(), XMLDocument::xmlContents);
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Soil::~Soil(void)
   {
      delete doc;
   }

// ------------------------------------------------------------------
// Write the soil contents to the specified stream.
// ------------------------------------------------------------------
void Soil::write(std::ostream& out)
   {
      out << doc->documentElement().write();
   }

// ------------------------------------------------------------------
// Export the soil using the specified template.
// ------------------------------------------------------------------
void Soil::exportSoil(std::ostream& out, const std::string& macroTemplate, bool modifyKlXf)
   {
   doc->documentElement().appendChild("thickness").setValue(formatString(thickness()));
   doc->documentElement().appendChild("airdry").setValue(formatString(airdry()));
   doc->documentElement().appendChild("ll15").setValue(formatString(ll15()));
   doc->documentElement().appendChild("dul").setValue(formatString(dul()));
   doc->documentElement().appendChild("sat").setValue(formatString(sat()));
   doc->documentElement().appendChild("swcon").setValue(formatString(swcon()));
   if (hasData("water", "sw"))
      doc->documentElement().appendChild("sw").setValue(formatString(sw()));
   doc->documentElement().appendChild("bd").setValue(formatString(bd()));
   if (hasData("nitrogen", "oc"))
      doc->documentElement().appendChild("oc").setValue(formatString(oc()));
   if (hasData("nitrogen", "ph"))
      doc->documentElement().appendChild("ph").setValue(formatString(ph()));
   doc->documentElement().appendChild("fbiom").setValue(formatString(fbiom()));
   doc->documentElement().appendChild("finert").setValue(formatString(finert()));
   doc->documentElement().appendChild("no3ppm").setValue(formatString(no3()));
   doc->documentElement().appendChild("nh4ppm").setValue(formatString(nh4()));

   // add crop nodes.
   vector<string> cropNames = crops();
   for (unsigned c = 0; c != cropNames.size(); c++)
      {
      XMLNode cropNode = doc->documentElement().appendChild("Crop", true);
      cropNode.setAttribute("name", cropNames[c]);
      cropNode.appendChild("ll").setValue(formatString(ll(cropNames[c])));
      cropNode.appendChild("kl").setValue(formatString(kl(cropNames[c], modifyKlXf)));
      cropNode.appendChild("xf").setValue(formatString(xf(cropNames[c], modifyKlXf)));
      }

   Macro macro;
   macro.go(doc->documentElement(), macroTemplate, out);

   eraseNodes(doc->documentElement(), "thickness");
   eraseNodes(doc->documentElement(), "airdry");
   eraseNodes(doc->documentElement(), "ll15");
   eraseNodes(doc->documentElement(), "dul");
   eraseNodes(doc->documentElement(), "sat");
   eraseNodes(doc->documentElement(), "swcon");
   eraseNodes(doc->documentElement(), "bd");
   eraseNodes(doc->documentElement(), "oc");
   eraseNodes(doc->documentElement(), "ph");
   eraseNodes(doc->documentElement(), "fbiom");
   eraseNodes(doc->documentElement(), "finert");
   eraseNodes(doc->documentElement(), "no3ppm");
   eraseNodes(doc->documentElement(), "nh4ppm");
   eraseNodes(doc->documentElement(), "ureappm");
   eraseNodes(doc->documentElement(), "SoilCrop");
   }
// ------------------------------------------------------------------
// Export the soil as a file using the specified template.
// Modify the kl and xf values if possible if modifyKlXf = true
// ------------------------------------------------------------------
void Soil::writeApsimPar(ostream& out, bool modifyKlXf)
   {
   static const char* MACRO_TEMPLATE =
      "[soil.soilwat2.parameters]\n"
      "   #for_each water\n"
      "   diffus_const = water.diffusconst   ! coeffs for unsaturated water flow\n"
      "   diffus_slope = water.diffusslope\n"
      "   cn2_bare     = water.cn2bare    ! bare soil runoff curve number\n"
      "   cn_red       = water.cnred    ! potetial reduction in curve number due to residue\n"
      "   cn_cov       = water.cncov   ! cover for maximum reduction in curve number\n"
      "   salb         = water.salb  ! bare soil albedo\n"
      "   cona         = water.cona     ! stage 2 evap coef.\n"
      "   u            = water.u     ! stage 1 soil evaporation coefficient (mm)\n"
      "   #endfor\n"
      "\n"
      "   dlayer  = .thickness   ! layer thickness mm soil\n"
      "   air_dry = .airdry   ! air dry mm water/ mm soil\n"
      "   ll15    = .ll15   ! lower limit mm water/mm soil\n"
      "   dul     = .dul   ! drained upper limit mm water/mm soil\n"
      "   sat     = .sat   ! saturation mm water/mm soil\n"
      "   sw      = .sw   ! starting soil water mm water/mm soil\n"
      "   swcon   = .swcon   ! drainage coefficient\n"
      "   bd      = .bd   ! bulk density gm dry soil/cc moist soil\n"
      "\n"
      "[soil.soiln2.parameters]\n"
      "   #for_each nitrogen\n"
      "   root_cn      = nitrogen.rootcn     ! C:N ratio of initial root residues\n"
      "   root_wt      = nitrogen.rootwt   ! root residues as biomass (kg/ha)\n"
      "   soil_cn      = nitrogen.soilcn   ! C:N ratio of soil\n"
      "   enr_a_coeff  = nitrogen.enracoeff\n"
      "   enr_b_coeff  = nitrogen.enrbcoeff\n"
      "   profile_reduction =  off\n"
      "   #endfor\n"
      "\n"
      "   oc      = .oc   ! Soil Organic Carbon\n"
      "   ph      = .ph   ! pH of soil\n"
      "   fbiom   = .fbiom   ! Organic C Biomass Fraction\n"
      "   finert  = .finert   ! Inert Organic C Fraction\n"
      "   no3ppm  = .no3ppm   ! Nitrate Concentration\n"
      "   nh4ppm  = .nh4ppm   ! Ammonium Concentration\n"
      "\n"
      "#for_each crop\n"
      "[soil.crop.name.parameters]\n"
      "   ll      = crop.ll\n"
      "   kl      = crop.kl\n"
      "   xf      = crop.xf\n"
      "#endfor";

   exportSoil(out, MACRO_TEMPLATE, modifyKlXf);
   }
// ------------------------------------------------------------------
// Auto correct the soil profile properties if necessary.
// Return true if the sw values were bounded.
// ------------------------------------------------------------------
bool Soil::autoCorrect(void)
   {
   bool neededCorrecting = false;
   vector<double> sw = this->sw();
   vector<double> airdry = this->airdry();
   vector<double> ll15 = this->airdry();
   vector<double> dul = this->dul();
   vector<double> sat = this->sat();

   for (unsigned layer = 0; layer != sw.size(); layer++)
      {
      if (sw[layer] < ll15[layer])
         {
         neededCorrecting = true;
         sw[layer] = ll15[layer];
         }
      else if (sw[layer] > dul[layer])
         {
         neededCorrecting = true;
         sw[layer] = dul[layer];
         }
      }
   if (neededCorrecting)
      setSw(sw);
   return neededCorrecting;
   }
// ------------------------------------------------------------------
// Return name of soil
// ------------------------------------------------------------------
std::string Soil::name(void)
   {
   return doc->documentElement().getAttribute("name");
   }
// ------------------------------------------------------------------
// Return name of soil
// ------------------------------------------------------------------
void Soil::setName(const std::string& newName)
   {
   doc->documentElement().setAttribute("name", newName);
   }
// ------------------------------------------------------------------
// Return a specific property of the soil
// ------------------------------------------------------------------
std::string Soil::getInfo(const char* typeName)
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
// Set a specific property of the soil.
// ------------------------------------------------------------------
void Soil::setInfo(const char* infoType, const std::string& value)
   {
   if (value != "")
      {
      XMLNode childNode = doc->documentElement().appendChild(infoType);
      childNode.setValue(value);
      }
   }
// ------------------------------------------------------------------
// Return a data property of the soil
// ------------------------------------------------------------------
double Soil::getData(const char* property,const char* dataType)
   {
   XMLNode propertyNode = findNode(doc->documentElement(),property);
   XMLNode::iterator i = find_if(propertyNode.begin(), propertyNode.end(),
                                 EqualToName<XMLNode>(dataType));
   if (i == propertyNode.end())
      return missingValue<double>();
   else
      return lexical_cast<double> (i->getValue());
   }

// ------------------------------------------------------------------
// Set a data property of the soil
// ------------------------------------------------------------------
void Soil::setData(const char* property,const char* dataType, double value)
   {
   XMLNode propertyNode = appendChildIfNotExist(doc->documentElement(),property,"");
   if (!isMissingValue(value))
      {
        propertyNode.appendChild(dataType).setValue(lexical_cast<string>(value));
      }
   }
// ------------------------------------------------------------------
// Return depth
// ------------------------------------------------------------------
vector<unsigned> Soil::depth()
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
// Set depth
// ------------------------------------------------------------------
void Soil::setDepth(const vector<unsigned>& depth)
   {
   vector<unsigned> thickness;
   if (depth.size() > 0)
      thickness.push_back(depth[0]);
   for (unsigned l = 1; l != depth.size(); l++)
      thickness.push_back(depth[l] - depth[l-1]);

   setThickness(thickness);
   }
// ------------------------------------------------------------------
// Return sw as a percent.
// ------------------------------------------------------------------
vector<double> Soil::swPercent()
   {
   return multiply_value(getLayered("", "sw"), 100.0);
   }
// ------------------------------------------------------------------
// Set sw as a percent.
// ------------------------------------------------------------------
void Soil::setSwPercent(const std::vector<double>& newValues)
   {
   vector<double> values = newValues;
   divide_value(values, 100.0);
   setLayered("", "sw", values);
   }
// ------------------------------------------------------------------
// Return ph (caCl)
// From: Aitken and Moody, 1991, Aust. J. Soil Res. 29, 483-491.
// ------------------------------------------------------------------
vector<double> Soil::phCaCl()
   {
   vector<double> phCaCl;
   vector<double> phWater = ph();
   for (unsigned l = 0; l != phWater.size(); l++)
      phCaCl.push_back(0.9054 * phWater[l] + 0.1245);
   return phCaCl;
   }
// ------------------------------------------------------------------
// Set ph as caCl.
// ------------------------------------------------------------------
void Soil::setPhCaCl(const std::vector<double>& newValues)
   {
   vector<double> phCaCl = newValues;
   vector<double> phWater;
   for (unsigned l = 0; l != phWater.size(); l++)
      phWater.push_back((phWater[l] - 0.1245) / 0.9054);
   setPh(phWater);
   }
// ------------------------------------------------------------------
// Return a list of crops
// ------------------------------------------------------------------
std::vector<std::string> Soil::crops(void)
   {
   vector<string> crops;
   for_each_if(doc->documentElement().begin(), doc->documentElement().end(),
               GetNameAttributeFunction<XMLNode>(crops),
               EqualToName<XMLNode>("SoilCrop"));

   return crops;
   }
// ------------------------------------------------------------------
// Add a crop. Does nothing if crop already exists.
// ------------------------------------------------------------------
void Soil::addCrop(const std::string& cropName)
   {
   XMLNode cropNode = appendChildIfNotExist(getSoilNode(),"SoilCrop", cropName);
   }
// ------------------------------------------------------------------
// Delete the specified crop's data
// ------------------------------------------------------------------
void Soil::deleteCrop(const std::string& cropName)
   {
   XMLNode node = getSoilNode();
   XMLNode::iterator childI = std::find_if(node.begin(),
                                           node.end(),
                                           NodeEquals<XMLNode>("SoilCrop", cropName));
   if (childI != node.end())
      node.erase(childI);
   else
      throw runtime_error("Cannot delete crop " + cropName + ". Crop doesn't exist.");
   }
// ------------------------------------------------------------------
// Add a Sample. Does nothing if Sample already exists.
// ------------------------------------------------------------------
void Soil::addSample(const std::string& sampleName)
   {
   XMLNode sampleNode = appendChildIfNotExist(getSoilNode(),"Sample", sampleName);
   }

XMLNode Soil::getSample(const std::string& sampleName)
   {
   XMLNode node = getSoilNode();
   XMLNode::iterator childI = std::find_if(node.begin(),
                                           node.end(),
                                           NodeEquals<XMLNode>("Sample", sampleName));
   return childI.operator *();
   //findNode(doc->documentElement(),sampleName);
   }

std::vector<std::string> Soil::samples(void)
   {
   vector<string> samples;
   for_each_if(doc->documentElement().begin(), doc->documentElement().end(),
               GetNameAttributeFunction<XMLNode>(samples),
               EqualToName<XMLNode>("Sample"));

   return samples;
   }

// ------------------------------------------------------------------
// Delete the specified Sample Data.
// ------------------------------------------------------------------
void Soil::deleteSample(const std::string& sampleName)
   {
   XMLNode node = getSoilNode();
   XMLNode::iterator childI = std::find_if(node.begin(),
                                           node.end(),
                                           NodeEquals<XMLNode>("Sample", sampleName));
   if (childI != node.end())
      node.erase(childI);
   else
      throw runtime_error("Cannot delete sample " + sampleName + ". Sample doesn't exist.");
   }
// ------------------------------------------------------------------
// Get a crop layer property from a specified layer
// ------------------------------------------------------------------
template<class T>
struct GetCropLayerValue
   {
   string name;
   string cropName;
   vector<T>& values;
   GetCropLayerValue(const string& cropname, const string& n, vector<T>& v)
      : cropName(cropname), name(n), values(v)
      {  }
   void operator() (XMLNode& node)
      {
      XMLNode::iterator i = find_if(node.begin(),
                                    node.end(),
                                    EqualToName<XMLNode>(name));
      if (i != node.end())
         {
         string value = i->getValue();
         stripLeadingTrailing(value, " ");
         values.push_back(lexical_cast<T>(value));
         }
      else
         {
         string xml = node.write();
         throw runtime_error("Cannot find " + string(name) + " in crop layer:\n" + xml);
         }
      }
   };

// ------------------------------------------------------------------
// Return crop data for the specified crop on the specified soil.
// Throws if crop doesn't exist.
// ------------------------------------------------------------------
vector<double> Soil::getCropLayered(const std::string& cropName, const char* cropInfo)
   {
   vector<double> data;
   GetCropLayerValue<double> getCropValue(cropName, cropInfo, data);
   XMLNode cropNode = findNodeWithName(doc->documentElement(), cropName);
   if(cropNode.isValid())
      {
      for_each_if(cropNode.begin(),
                  cropNode.end(),
                  getCropValue,
                  EqualToName<XMLNode>("layer"));
      return data;
      }
   else
      {
         throw runtime_error("Cannot find a crop: " + string(cropName));
      }
   }
// ------------------------------------------------------------------
// Set crop data for the specified crop on the specified soil.
// Throws if crop doesn't exist. Throws if the number of values
// doesn't equal the number of depths.
// ------------------------------------------------------------------
void Soil::setCropLayered(const std::string& cropName, const char* cropInfo,
                          const std::vector<double>& values)
   {
   if (values.size() > 0)
      {
      if (thickness().size() == values.size())
         {

         XMLNode cropNode = appendChildIfNotExist(doc->documentElement(), "SoilCrop", cropName);
         for (unsigned l = 0; l != values.size(); l++)
            {
            if (!isMissingValue(values[l]))
               {
                 XMLNode layerNode = appendChildIfNotExist(cropNode,"layer", lexical_cast<string>(l+1));
                 XMLNode childNode = layerNode.appendChild(cropInfo);
                 childNode.setValue(lexical_cast<string>(values[l]));
               }
            }
         }

      else
         {
         string msg = "Invalid number of values for variable: ";
         msg += cropInfo;
         msg += ". The number of values does not equal the number of layers.";
         throw runtime_error(msg);
         }
      }
   }
// ------------------------------------------------------------------
// Return kl to caller.
// Modify the kl values if possible if modifyKl = true
// ------------------------------------------------------------------
vector<double> Soil::kl(const string &cropName, bool modifyKl)
   {
   return getCropLayered(cropName, "kl");
/*   vector<double> kl = getCropLayered(cropName, "kl");
   if (modifyKl)
      {
      try
         {
         // if user has ec and esp then derive kl from esp and kl0
         // if user has just ec and no esp then kl = kl0
         vector<double> kl0 = getCropLayered(cropName, "kl0");
         vector<double> ec = getLayered("ec");
         kl = kl0;
         vector<double> esp = getLayered("esp");
         for (unsigned l = 0; l != ec.size(); l++)
            {
            double klFactor;
            if (ec[l] <= 0.68)
               klFactor = 1.0;
            else
               klFactor = 2.06 / (1 + pow(2.0, ec[l])) - 0.351;
            klFactor = max(klFactor, 0.0);

            kl[l] = kl0[l] * klFactor;
            }
         }
      catch (const exception& err)
         { }
      }
   return kl;
*/   }
// ------------------------------------------------------------------
// Return xf to caller.
// Modify the xf values if possible if modifyXf = true
// ------------------------------------------------------------------
vector<double> Soil::xf(const string &cropName, bool modifyXf)
   {
   vector<double> xf = getCropLayered(cropName, "xf");
   if (modifyXf)
      {
      try
         {
         vector<double> ec = getLayered("", "ec");
         for (unsigned l = 0; l != ec.size(); l++)
            {
            if (ec[l] <= 0.75)
               xf[l] = 1.0;
            else
               xf[l] = max(-1.325 * ec[l] + 2.0, 0.0);
            }
         }
      catch (const exception& err)
         { }

      }
   return xf;
   }
// ------------------------------------------------------------------
// Return a complete list of crop names where we can predict
// lower limits for the specified soil. Will throw if refno is
// invalid.
// ------------------------------------------------------------------
void Soil::predictedCrops(std::vector<std::string>& names)
   {


   }
// ------------------------------------------------------------------
// Get predicted crop data. Will throw if no crop data exists for
// the specified soil and crop.
// ------------------------------------------------------------------
void Soil::calcPredictedCropData(const std::string& cropName,
                                 std::vector<double>& ll,
                                 std::vector<double>& kl,
                                 std::vector<double>& xf)
   {

   }

// ------------------------------------------------------------------
// return plant available water content for profile
// ------------------------------------------------------------------
double _export calcPAWC(const vector<unsigned>& depths,
                        const vector<double>& ll,
                        const vector<double>& dul)
   {
   vector<double> pawc;
   calcPAWC(depths, ll, dul, pawc);
   return accumulate(pawc.begin(), pawc.end(), 0.0);
   }
// ------------------------------------------------------------------
// return plant available water content by layer (mm) given
// depth, lower limit and dul all in (mm).
// ------------------------------------------------------------------
void _export calcPAWC(const vector<unsigned>& depths,
                      const vector<double>& ll,
                      const vector<double>& dul,
                      vector<double>& pawc)
   {
   pawc.erase(pawc.begin(), pawc.end());

   // calculate depth increments.
   if (depths.size() > 0)
      {
      // calculate layer thicknesses
      vector<unsigned> thickness;
      calcThickness(depths, thickness);

      for (unsigned layer = 0; layer != ll.size(); layer++)
         pawc.push_back( (dul[layer]-ll[layer])*thickness[layer]);
      }
   }

