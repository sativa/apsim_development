//---------------------------------------------------------------------------
#ifndef SoilBaseH
#define SoilBaseH
#include <general\xml.h>
class XMLDocument;
//---------------------------------------------------------------------------
// Class for encapsulating all access to the soil data in a database.
//---------------------------------------------------------------------------
class __declspec(dllexport) SoilBase
   {
   public:
      // ------------------------------------------------------------------
      // Return true if sample as the specified data.
      // ------------------------------------------------------------------
      bool hasData(const std::string& propertyType, const std::string& dataType);
      XMLNode getSoilNode(){ return doc->documentElement();}

      // ------------------------------------------------------------------
      // Getter and Setter for throwexception variable
      // ------------------------------------------------------------------
      void dothrowExceptions(){throwexceptions = true;}
      void doNotThrowExceptions(){throwexceptions = false;}
   protected:
      XMLDocument* doc;

      // ------------------------------------------------------------------
      // When true will throw an exception when getters are called and
      // data does not exist
      // ------------------------------------------------------------------
      bool throwexceptions;

      SoilBase(void) {throwexceptions = true; };

      // ------------------------------------------------------------------
      // Return a specific layered data
      // ------------------------------------------------------------------
      std::vector<double> getLayered(const char* propertyType, const char* layeredType);

      // ------------------------------------------------------------------
      // Set a layered data property
      // ------------------------------------------------------------------
      void setLayered(const char* propertyType, const char* layeredType, const std::vector<double>& values);


      // ------------------------------------------------------------------
      // Return the soil profile depths.
      // ------------------------------------------------------------------
      std::vector<unsigned> dlayer(void);

      // ------------------------------------------------------------------
      // Set the soil profile depths.
      // ------------------------------------------------------------------
      void setDlayer(const std::vector<unsigned>& dlayer);

      // ------------------------------------------------------------------
      // Convert the values from ppm to kg/ha
      // ------------------------------------------------------------------
      std::vector<double> ppmToKgHa(const std::vector<double>& ppm, const std::vector<double>& bd);

      // ------------------------------------------------------------------
      // Convert kg/ha to ppm
      // ------------------------------------------------------------------
      std::vector<double> kgHaToPpm(const std::vector<double>& kgHa, const std::vector<double>& bd);

   };

// ------------------------------------------------------------------
// convert a vector of soil thicknesses(mm) to a vector of layer strings(cm)
// e.g. "0-10", "10-30" ...
// ------------------------------------------------------------------
std::vector<std::string> thicknessToLayerStrings(const std::vector<unsigned>& thickness);

// ------------------------------------------------------------------
// convert a vector of soil layer strings e.g. "0-10", "10-30" ...
// to a vector of layer strings(cm) e.g. 10, 20 ...
// ------------------------------------------------------------------
std::vector<unsigned> layerStringsToThickness(const std::vector<std::string>& layerStrings);

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
void calcDepthCenter(const std::vector<unsigned>& depths,
                     std::vector<double>& depthCenter);
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
void calcThickness (const std::vector<unsigned>& depths,
                    std::vector<unsigned>& thickness);


#endif
