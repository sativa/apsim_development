//---------------------------------------------------------------------------
#ifndef SoilsH
#define SoilsH
#include <vector>
#include <string>
#include "Soil.h"
#include "SoilSample.h"

class XMLDocument;
class XMLNode;
//---------------------------------------------------------------------------
// Class for encapsulating all access to the soil data in a database.
//---------------------------------------------------------------------------
class __declspec(dllexport) Soils
   {
   public:
      Soils(void);
      ~Soils(void);

      std::string getFileName(void) {return fileName;}

      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------
      // Create a new soils file but don't open it.
      //---------------------------------------------------------------------------
      static void newSoilsFile(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Open the specified soils file. Will throw if filename doesn't exist.
      //---------------------------------------------------------------------------
      void open(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Close the specified soils file. Will not keep any unsaved changes.
      // Make sure you do a save before a close.
      //---------------------------------------------------------------------------
      void close(void);

      //---------------------------------------------------------------------------
      // Save the specified database under the specified filename. If the
      // file aready exists, it will be overwritten.
      //---------------------------------------------------------------------------
      void save(const std::string& fileName);
      void write(std::ostream& out);

      // ------------------------------------------------------------------
      // Return a list of any property of the soil to caller
      // ------------------------------------------------------------------
      void getAllProperties(const std::string property, std::vector<std::string>& properties) const;

      // ------------------------------------------------------------------
      // Return a list of all possible properties of soil to caller.
      // ------------------------------------------------------------------
      void allRegions(std::vector<std::string>& regions) const{getAllProperties("Region", regions);};
      void allSites(std::vector<std::string>& sites) const{getAllProperties("Site", sites);};
      void allSoilTypes(std::vector<std::string>& soilTypes) const{getAllProperties("SoilType", soilTypes);};
      void allSamples(std::vector<std::string>& soilTypes
                    , std::vector<std::string>& relatesoils
                    , std::vector<std::string>& dates);
      void allSamplesGivenSoil(const std::string& Soilname
                                   , std::vector<std::string>& thesamples);
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // Return a sorted list of all possible regions to caller.
      // ------------------------------------------------------------------
      //void allRegions(std::vector<std::string>& regions) const;

      // ------------------------------------------------------------------
      // Return a sorted list of all possible sites to caller.
      // ------------------------------------------------------------------
      //void allSites(std::vector<std::string>& sites) const;

      // ------------------------------------------------------------------
      // Return a sorted list of all possible soil types to caller.
      // ------------------------------------------------------------------
      //void allSoilTypes(std::vector<std::string>& soilTypes) const;



      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      // Add a soil to the database and return its name.
      // ------------------------------------------------------------------
      std::string add(Soil& soil);
      std::string replace(const std::string& name, Soil& soil);
      std::string add(const std::string& soilname, SoilSample& sample);
      std::string replace(const std::string& Soilname,
                          const std::string& Samplename,
                          SoilSample& sample);
      std::string replace(const std::string& OldSoilname,
                          const std::string& NewSoilname,
                          const std::string& Samplename,
                          SoilSample& sample);

      // ------------------------------------------------------------------
      // Add a new empty soil to the database. A unique name is given to
      // the soil. This name is returned.
      // ------------------------------------------------------------------
      std::string create(void);

      // ------------------------------------------------------------------
      // Delete a soil from the database. Will throw if name doesn't exist.
      // ------------------------------------------------------------------
      void erase(const std::string& name);
      void eraseSample(const std::string& Soilname,const std::string& Samplename);

      bool zeroSamples();
      // ------------------------------------------------------------------
      // return a complete list of soil names
      // ------------------------------------------------------------------
      std::vector<std::string> names() const;

      // ------------------------------------------------------------------
      // return a soil
      // ------------------------------------------------------------------
      Soil* get(const std::string& name);
      SoilSample* get(const std::string& soilname, const std::string& samplename);

      string calcUniqueSampleNameGivenSoil(const std::string& soilName, const std::string& baseName) const;

   private:
      std::string fileName;
      XMLDocument* doc;

      // ------------------------------------------------------------------
      // Calculate a new unique soil name. Return name to caller.
      // ------------------------------------------------------------------
      std::string calcUniqueSoilName(const std::string& baseName) const;

      // ------------------------------------------------------------------
      // return true if a soil exists in database
      // ------------------------------------------------------------------
      bool existsSoil(const std::string& name) const;
      bool existsSample(const std::string& Soilname, const std::string& Samplename) const;      

   };

#endif
