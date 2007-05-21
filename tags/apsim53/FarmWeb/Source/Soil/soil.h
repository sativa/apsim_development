//---------------------------------------------------------------------------
#ifndef SoilH
#define SoilH

#include <vector>
#include <iostream>
#include <soil\SoilBase.h>
class SoilSample;
//---------------------------------------------------------------------------
// Class for encapsulating all access to the soil data in a database.
//---------------------------------------------------------------------------
class __declspec(dllexport) Soil : public SoilBase
   {
   public:
      Soil(void);
      Soil(const std::string& xml);
      Soil(const char* filename);
      ~Soil(void);


      // ------------------------------------------------------------------
      // Write the soil contents to the specified stream.
      // ------------------------------------------------------------------
      void write(std::ostream& out);

      // ------------------------------------------------------------------
      // Export the soil as an APSIM parameter file.
      // Modify the kl and xf values if possible if modifyKlXf = true
      // ------------------------------------------------------------------
      void writeApsimPar(std::ostream& out, bool modifyKlXf = false);

      // ------------------------------------------------------------------
      // Export the soil using the specified template.
      // ------------------------------------------------------------------
      void exportSoil(std::ostream& out, const std::string& macroTemplate, bool modifyKlXf);

      // ------------------------------------------------------------------
      // Auto correct the soil profile properties if necessary.
      // Return true if the sw values were bounded.
      // ------------------------------------------------------------------
      bool autoCorrect(void);

      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      std::string name(void);
      std::string region(void)            {return getInfo("Region");}
      std::string site(void)              {return getInfo("Site");}
      std::string localName(void)         {return getInfo("LocalName");}
      std::string soilType(void)          {return getInfo("SoilType");}
      std::string nearestTown(void)       {return getInfo("NearestTown");}
      std::string comment(void)           {return getInfo("Comment");}
      std::string gps(void)               {return getInfo("Gps");}
      std::string gpsType(void)           {return getInfo("GpsType");}
      std::string mapId(void)             {return getInfo("MapId");}
      std::string naturalVegetation(void) {return getInfo("NaturalVegetation");}

      void setName(const std::string& newValue);
      void setRegion(const std::string& newValue)            {setInfo("Region", newValue);}
      void setSite(const std::string& newValue)              {setInfo("Site", newValue);}
      void setLocalName(const std::string& newValue)         {setInfo("LocalName", newValue);}
      void setSoilType(const std::string& newValue)          {setInfo("SoilType", newValue);}
      void setNearestTown(const std::string& newValue)       {setInfo("NearestTown", newValue);}
      void setComment(const std::string& newValue)           {setInfo("Comment", newValue);}
      void setGps(const std::string& newValue)               {setInfo("Gps", newValue);}
      void setGpsType(const std::string& newValue)           {setInfo("GpsType", newValue);}
      void setMapId(const std::string& newValue)             {setInfo("MapId", newValue);}
      void setNaturalVegetation(const std::string& newValue) {setInfo("NaturalVegetation", newValue);}


      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      double cona(void)         {return getData("Water", "Cona");}
      double diffusConst(void)  {return getData("Water", "DiffusConst");}
      double diffusSlope(void)  {return getData("Water", "DiffusSlope");}
      double u(void)            {return getData("Water", "U");}
      double salb(void)         {return getData("Water", "Salb");}
      double cn2Bare(void)      {return getData("Water", "Cn2Bare");}
      double cnRed(void)        {return getData("Water", "CnRed");}
      double cnCov(void)        {return getData("Water", "CnCov");}
      double cnCanopyFact(void) {return getData("Nitrogen", "CnCanopyFact");}
      double rootCN(void)       {return getData("Nitrogen", "RootCn");}
      double rootWt(void)       {return getData("Nitrogen", "RootWt");}
      double soilCN(void)       {return getData("Nitrogen", "SoilCn");}
      double enrAcoeff(void)    {return getData("Nitrogen", "EnrACoeff");}
      double enrBcoeff(void)    {return getData("Nitrogen", "EnrBCoeff");}

      void setCona(double newValue)         {setData("Water", "Cona", newValue);}
      void setDiffusConst(double newValue)  {setData("Water", "DiffusConst", newValue);}
      void setDiffusSlope(double newValue)  {setData("Water", "DiffusSlope", newValue);}
      void setU(double newValue)            {setData("Water", "U", newValue);}
      void setSalb(double newValue)         {setData("Water", "Salb", newValue);}
      void setCn2Bare(double newValue)      {setData("Water", "Cn2Bare", newValue);}
      void setCnRed(double newValue)        {setData("Water", "CnRed", newValue);}
      void setCnCov(double newValue)        {setData("Water", "CnCov", newValue);}
      void setCnCanopyFact(double newValue) {setData("Nitrogen", "CnCanopyFact", newValue);}
      void setRootCN(double newValue)       {setData("Nitrogen", "RootCn", newValue);}
      void setRootWt(double newValue)       {setData("Nitrogen", "RootWt", newValue);}
      void setSoilCN(double newValue)       {setData("Nitrogen", "SoilCn", newValue);}
      void setEnrAcoeff(double newValue)    {setData("Nitrogen", "EnrACoeff", newValue);}
      void setEnrBcoeff(double newValue)    {setData("Nitrogen", "EnrBCoeff", newValue);}


      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // functions that call getLayered are they Water or Nitrogen
      std::vector<unsigned> thickness(void) {return dlayer();}   // in mm
      std::vector<unsigned> depth(void);                         // in mm
      std::vector<double> bd(void)                {return getLayered("Water", "bd");}
      std::vector<double> ll15(void)              {return getLayered("Water", "ll15");}
      std::vector<double> airdry(void)            {return getLayered("Water", "airdry");}
      std::vector<double> dul(void)               {return getLayered("Water", "dul");}
      std::vector<double> sat(void)               {return getLayered("Water", "sat");}
      std::vector<double> sw(void)                {return getLayered("Water", "sw");}
      std::vector<double> swPercent(void);
      std::vector<double> oc(void)                {return getLayered("Nitrogen", "oc");}
      std::vector<double> ec(void)                {return getLayered("other", "ec");}
      std::vector<double> esp(void)               {return getLayered("other", "esp");}
      std::vector<double> ph(void)                {return getLayered("Nitrogen", "ph");}
      std::vector<double> phCaCl(void);
      std::vector<double> cl(void)                {return getLayered("other", "cl");}
      std::vector<double> cec(void)               {return getLayered("other", "cec");}
      std::vector<double> ca(void)                {return getLayered("other", "ca");}
      std::vector<double> mg(void)                {return getLayered("other", "mg");}
      std::vector<double> na(void)                {return getLayered("other", "na");}
      std::vector<double> k(void)                 {return getLayered("other", "k");}
      std::vector<double> exchangableSodium(void) {return getLayered("other", "exchangableSodium");}
      std::vector<double> particleSizeSand(void)  {return getLayered("other", "particleSizeSand");}
      std::vector<double> particleSizeSilt(void)  {return getLayered("other", "particleSizeSilt");}
      std::vector<double> particleSizeClay(void)  {return getLayered("other", "particleSizeClay");}
      std::vector<double> swcon(void)             {return getLayered("Water", "swcon");}
      std::vector<double> fbiom(void)             {return getLayered("Nitrogen", "fbiom");}
      std::vector<double> finert(void)            {return getLayered("Nitrogen", "finert");}
      std::vector<double> no3(void)               {return getLayered("Nitrogen", "no3");}
      std::vector<double> nh4(void)               {return getLayered("Nitrogen", "nh4");}

      void setThickness(const std::vector<unsigned>& thickness) {setDlayer(thickness);}
      void setDepth(const std::vector<unsigned>& depth);
      void setBd(const std::vector<double>& newValues)                {setLayered("Water", "bd", newValues);}
      void setLl15(const std::vector<double>& newValues)              {setLayered("Water", "ll15", newValues);}
      void setAirdry(const std::vector<double>& newValues)            {setLayered("Water", "airdry", newValues);}
      void setDul(const std::vector<double>& newValues)               {setLayered("Water", "dul", newValues);}
      void setSw(const std::vector<double>& newValues)                {setLayered("Water", "sw", newValues);}
      void setSwPercent(const std::vector<double>& newValues);
      void setSat(const std::vector<double>& newValues)               {setLayered("Water", "sat", newValues);}
      void setOc(const std::vector<double>& newValues)                {setLayered("Nitrogen", "oc", newValues);}
      void setEc(const std::vector<double>& newValues)                {setLayered("other", "ec", newValues);}
      void setEsp(const std::vector<double>& newValues)               {setLayered("other", "esp", newValues);}
      void setPh(const std::vector<double>& newValues)                {setLayered("Nitrogen", "ph", newValues);}
      void setPhCaCl(const std::vector<double>& newValues);
      void setCl(const std::vector<double>& newValues)                {setLayered("other", "cl", newValues);}
      void setCec(const std::vector<double>& newValues)               {setLayered("other", "cec", newValues);}
      void setCa(const std::vector<double>& newValues)                {setLayered("other", "ca", newValues);}
      void setMg(const std::vector<double>& newValues)                {setLayered("other", "mg", newValues);}
      void setNa(const std::vector<double>& newValues)                {setLayered("other", "na", newValues);}
      void setK(const std::vector<double>& newValues)                 {setLayered("other", "k", newValues);}
      void setExchangableSodium(const std::vector<double>& newValues) {setLayered("other", "exchangableSodium", newValues);}
      void setParticleSizeSand(const std::vector<double>& newValues)  {setLayered("other", "particleSizeSand", newValues);}
      void setParticleSizeSilt(const std::vector<double>& newValues)  {setLayered("other", "particleSizeSilt", newValues);}
      void setParticleSizeClay(const std::vector<double>& newValues)  {setLayered("other", "particleSizeClay", newValues);}
      void setSwcon(const std::vector<double>& newValues)             {setLayered("Water", "swcon", newValues);}
      void setFbiom(const std::vector<double>& newValues)             {setLayered("Nitrogen", "fbiom", newValues);}
      void setFinert(const std::vector<double>& newValues)            {setLayered("Nitrogen", "finert", newValues);}
      void setNo3(const std::vector<double>& newValues)               {setLayered("Nitrogen", "no3", newValues);}
      void setNh4(const std::vector<double>& newValues)               {setLayered("Nitrogen", "nh4", newValues);}

      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // Return a list of crops
      // ------------------------------------------------------------------
      std::vector<std::string> crops();
      std::vector<string> samples();

      // ------------------------------------------------------------------
      // Add a crop. Does nothing if crop already exists.
      // ------------------------------------------------------------------
      void addCrop(const std::string& cropName);

      // ------------------------------------------------------------------
      // Delete the specified crop's data
      // ------------------------------------------------------------------
      void deleteCrop(const std::string& cropName);

      // ------------------------------------------------------------------
      // Add a crop. Does nothing if crop already exists.
      // ------------------------------------------------------------------
      void addSample(const std::string& sampleName);
      XMLNode getSample(const std::string& sampleName);
      // ------------------------------------------------------------------
      // Delete the specified sample data
      // ------------------------------------------------------------------
      void deleteSample(const std::string& sampleName);

      // ------------------------------------------------------------------
      // Return crop data.
      // Modify the kl and xf values if possible if modifyKl = true
      // ------------------------------------------------------------------
      std::vector<double> ll(const std::string &cropName) {return getCropLayered(cropName, "ll");}
      std::vector<double> kl(const std::string &cropName, bool modifyKl = false);
      std::vector<double> kl0(const std::string &cropName) {return getCropLayered(cropName, "kl0");}
      std::vector<double> xf(const std::string &cropName, bool modifyXf = false);

      void setll(const std::string &cropName, const std::vector<double>& newValues)
         {setCropLayered(cropName, "ll", newValues);}
      void setkl(const std::string &cropName, const std::vector<double>& newValues)
         {setCropLayered(cropName, "kl", newValues);}
      void setkl0(const std::string &cropName, const std::vector<double>& newValues)
         {setCropLayered(cropName, "kl0", newValues);}
      void setxf(const std::string &cropName, const std::vector<double>& newValues)
         {setCropLayered(cropName, "xf", newValues);}


      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // Return a complete list of crop names where we can predict
      // lower limits for the specified soil. Will throw if refno is
      // invalid.
      // ------------------------------------------------------------------
      void predictedCrops(std::vector<std::string>& names);

      // ------------------------------------------------------------------
      // Get predicted crop data. Will throw if no crop data exists for
      // the specified soil and crop.
      // ------------------------------------------------------------------
      void calcPredictedCropData(const std::string& cropName,
                                 std::vector<double>& ll,
                                 std::vector<double>& kl,
                                 std::vector<double>& xf);

   private:
      Soil(const Soil& rhs) { };
      // ------------------------------------------------------------------
      // Return a specific property of the soil
      // ------------------------------------------------------------------
      std::string getInfo(const char* typeName);
      double getData(const char* propertyType, const char* typeName);
      std::vector<double> getCropLayered(const std::string& cropName, const char* typeName);

      // ------------------------------------------------------------------
      // set a specific property of the soil
      // ------------------------------------------------------------------
      void setInfo(const char* typeName, const std::string& newValue);
      void setData(const char* propertyType, const char* dataType, double value);
      void setCropLayered(const std::string& cropName, const char* typeName, const std::vector<double>& newValue);

      friend SoilSample;
   };

// ------------------------------------------------------------------
// return plant available water content for profile
// ------------------------------------------------------------------
double _export calcPAWC(const std::vector<unsigned>& depths,
                        const std::vector<double>& ll,
                        const std::vector<double>& dul);

// ------------------------------------------------------------------
// return plant available water content by layer (mm) given
// depth, lower limit and dul all in (mm).
// ------------------------------------------------------------------
void _export calcPAWC(const std::vector<unsigned>& depths,
                      const std::vector<double>& ll,
                      const std::vector<double>& dul,
                      std::vector<double>& pawc);

#endif
