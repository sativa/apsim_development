//---------------------------------------------------------------------------
#ifndef SoilSampleH
#define SoilSampleH
#include <vector>
#include <iostream>
#include <Soil\SoilBase.h>
class Soil;
//---------------------------------------------------------------------------
// Class for encapsulating all access to the soil data in a database.
//---------------------------------------------------------------------------
class __declspec(dllexport) SoilSample : public SoilBase
   {
   public:
      SoilSample(void);
      SoilSample(const std::string& xml);
      ~SoilSample(void);

     SoilSample(const XMLNode& xml);  //added

      //ADDED
      std::string name(void);
      std::string getInfo(const char* typeName);
      std::string date(void)                   {return getInfo("Date");};

      std::vector<unsigned> depth(void);                         // in mm
      std::vector<double> wet(void)            {return getLayered("Water", "wet");}
      std::vector<double> dry(void)            {return getLayered("Water", "dry");}
      void setName(const std::string& newName);
      void setInfo(const char* typeName, const std::string& newValue);
      void setDepth(const std::vector<unsigned>& depth);
      void setWet(const std::vector<double>& newValues) {setLayered("Water", "wet", newValues);}
      void setDry(const std::vector<double>& newValues) {setLayered("Water", "dry", newValues);}
      void setDate(const std::string& newValue)            {setInfo("Date", newValue);}
      XMLNode getSampleNode() { return doc->documentElement();};
      // ------------------------------------------------------------------
      // Write the soil sample contents to the specified stream.
      // ------------------------------------------------------------------
      void write(std::ostream& out);

      // ------------------------------------------------------------------
      // Thickness methods.
      // ------------------------------------------------------------------
      std::vector<unsigned> thickness(void) {return dlayer();}  // in mm
      void setThickness(const std::vector<unsigned>& thickness) {setDlayer(thickness);}

      // ------------------------------------------------------------------
      // soil water methods.
      // ------------------------------------------------------------------
      std::vector<double> sw()   {return getLayered("water", "sw");}    // in prop.
      std::vector<double> swMm();
      std::vector<double> swPercent();
      void setSw(const std::vector<double>& sw) {setLayered("water", "sw", sw);}
      void setSwMm(const std::vector<double>& swMm);
      void setSwPercent(const std::vector<double>& swPercent);

      // ------------------------------------------------------------------
      // soil no3 methods.
      // ------------------------------------------------------------------
      std::vector<double> no3()  {return getLayered("nitrogen", "no3");}   // as ppm
      std::vector<double> no3KgHa(const std::vector<double>& bd);
      void setNo3(const std::vector<double>& no3ppm) {setLayered("nitrogen", "no3", no3ppm);}
      void setNo3KgHa(const std::vector<double>& no3KgHa, const std::vector<double>& bd);

      // ------------------------------------------------------------------
      // soil nh4 methods.
      // ------------------------------------------------------------------
      std::vector<double> nh4()  {return getLayered("nitrogen", "nh4");}   // as ppm
      std::vector<double> nh4KgHa(const std::vector<double>& bd);
      void setNh4(const std::vector<double>& nh4ppm) {setLayered("nitrogen", "nh4", nh4ppm);}
      void setNh4KgHa(const std::vector<double>& nh4KgHa, const std::vector<double>& bd);

      // ------------------------------------------------------------------
      // soil oc methods.
      // ------------------------------------------------------------------
      std::vector<double> oc()  {return getLayered("Nitrogen", "oc");}   // as %
      std::vector<double> ocPpm();
      void setOc(const std::vector<double>& oc) {setLayered("Nitrogen", "oc", oc);}
      void setOcPpm(const std::vector<double>& ocPpm);

      // ------------------------------------------------------------------
      // soil ec methods.
      // ------------------------------------------------------------------
      std::vector<double> ec()  {return getLayered("other", "ec");}   // as dS/m
      void setEc(const std::vector<double>& ec) {setLayered("other", "ec", ec);}

      // ------------------------------------------------------------------
      // soil ph methods.
      // ------------------------------------------------------------------
      std::vector<double> ph()  {return getLayered("Nitrogen", "ph");}   // as Ca/Cl2
      void setPh(const std::vector<double>& ph) {setLayered("Nitrogen", "ph", ph);}

      // ------------------------------------------------------------------
      // soil esp methods.
      // ------------------------------------------------------------------
      std::vector<double> esp()  {return getLayered("other", "esp");}   // as %
      void setEsp(const std::vector<double>& esp) {setLayered("other", "esp", esp);}

      // ------------------------------------------------------------------
      // Map the sample to the specified soil. Takes care of mapping soil
      // sample layer structure to soil layer structure.
      // ------------------------------------------------------------------
      void mapSampleToSoil(Soil& soil);

   private:

      // ------------------------------------------------------------------
      // Spatial mass redistribution algorithm.
      // ------------------------------------------------------------------
      std::vector<double> spatialRedistribute(const std::vector<double>& fromMass,
                                         const std::vector<unsigned>& fromThickness,
                                         const std::vector<unsigned>& toThickness);

      // ------------------------------------------------------------------
      // Uses bulk density to do a spatial mass redistribution algorithm.
      // ------------------------------------------------------------------
      std::vector<double> massRedistribute(const std::vector<double>& values,
                                      const std::vector<unsigned>& fromThickness,
                                      const std::vector<unsigned>& toThickness,
                                      const std::vector<double>& toBd);

      // ------------------------------------------------------------------
      // Create a vector of values ready for interpolation. Values come from
      // this sample as well as the specified soil. A dummy layer will optionally
      // be inserted so that layer thicknesses line up with the soil.
      // ------------------------------------------------------------------
      void createVariableForMapping(const std::string& propertyType,
                                    const std::string& dataType,
                                    Soil& soil,
                                    std::vector<unsigned>& sampleThickness,
                                    std::vector<double>& sampleValues);

      // ------------------------------------------------------------------
      // template for copying values into an array below a specified depth.
      // ------------------------------------------------------------------
      template <class T>
      void copyValuesBelowDepth(const std::vector<unsigned>& fromThickness,
                                const std::vector<T>& fromValues,
                                std::vector<T>& toValues,
                                unsigned depthBelowWhichToCopyFrom)
         {
         unsigned cumDepthSoFar = 0;
         for (unsigned l = 0; l != fromThickness.size(); l++)
            {
            cumDepthSoFar += fromThickness[l];
            if (cumDepthSoFar > depthBelowWhichToCopyFrom)
               toValues.push_back(fromValues[l]);
            }
         }

   };

// ------------------------------------------------------------------
// return sw values given
// wet, dry, and bd variables.
// ------------------------------------------------------------------
void _export calcSW(const vector<double>& wet,
                    const vector<double>& dry,
                     const vector<double>& bd,
                      vector<double>& sw);

// ------------------------------------------------------------------
// return true if depths are the same
// ------------------------------------------------------------------
bool _export depthEqual(const vector<unsigned>& depth1,
                    const vector<unsigned>& depth2);


#endif
