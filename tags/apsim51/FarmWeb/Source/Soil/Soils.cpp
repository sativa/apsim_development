//---------------------------------------------------------------------------
#include <general\pch.h>
#pragma hdrstop

#include "Soils.h"
#include "SoilSample.h"
#include "Soil.h"

#include <boost\bind.hpp>
#include <boost\bind.hpp>
#include <boost\filesystem\operations.hpp>
#include <boost\lexical_cast.hpp>
#include <general\xml.h>
#include <general\stl_functions.h>

using namespace std;
using namespace boost;
using namespace boost::filesystem;
#pragma package(smart_init)

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
Soils::Soils()
   {
   doc = NULL;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
Soils::~Soils(void)
   {
   close();
   }
//---------------------------------------------------------------------------
// Create a new soils file but don't open it.
//---------------------------------------------------------------------------
void Soils::newSoilsFile(const std::string& fileName)
   {
   ofstream out(fileName.c_str());
   out << "<soils>" << endl;
   out << "</soils>" << endl;
   }
//---------------------------------------------------------------------------
// Open the specified database file. Will throw if filename doesn't exist.
//---------------------------------------------------------------------------
void Soils::open(const string& filename)
   {
   fileName = filename;
   if (exists(path(fileName, native)))
      {
      close();
      XMLDocument* newDoc = new XMLDocument(fileName);
      doc = newDoc;
      }
   else
      throw runtime_error("Cannot find file: " + fileName);
   }
//---------------------------------------------------------------------------
// Close the specified database file.
//---------------------------------------------------------------------------
void Soils::close()
   {
   delete doc;
   doc = NULL;
   }
//---------------------------------------------------------------------------
// Save the specified database under the specified filename.
//---------------------------------------------------------------------------
void Soils::save(const std::string& fileName)
   {
   doc->write(fileName);
   }
// ------------------------------------------------------------------
// Write the soil contents to the specified stream.
// ------------------------------------------------------------------
void Soils::write(std::ostream& out)
   {
   out << doc->documentElement().write();
   }
// ------------------------------------------------------------------
// retrieve the value of a child node
// ------------------------------------------------------------------
void retrieveChildValuesMatching(const std::string& childNodeType,
                                 vector<string>& names,
                                 XMLNode& parent)
   {
   // we need to make sure that we always add something.
   int c = names.size();
   GetValueFunction<vector<string>, XMLNode> getValue(names);
   for_each_if(parent.begin(), parent.end(), getValue,
               EqualToName<XMLNode>(childNodeType));
   if (c == names.size())
      names.push_back("");
   }
// ------------------------------------------------------------------
// Return a list of any property of the soil to caller
// ------------------------------------------------------------------
void Soils::getAllProperties(const std::string property, std::vector<std::string>& properties)const
   {
      XMLNode allSoils = doc->documentElement();

      for(XMLNode::iterator node = allSoils.begin(); node !=  allSoils.end(); node++){
         retrieveChildValuesMatching(property, properties, *node);
      }
   }
/*void Soils::allRegions(std::vector<std::string>& regions) const
   {
   XMLNode allSoils = doc->documentElement();
   for_each(allSoils.begin(), allSoils.end(),
            bind(&retrieveChildValuesMatching, "region", regions, _1));
   sort(regions.begin(), regions.end());
   }
// ------------------------------------------------------------------
// Return a sorted list of all possible soil types to caller.
// ------------------------------------------------------------------
void Soils::allSites(std::vector<std::string>& sites) const
   {
   XMLNode allSoils = doc->documentElement();
   for_each(allSoils.begin(), allSoils.end(),
            bind(&retrieveChildValuesMatching, "site", sites, _1));
   sort(sites.begin(), sites.end());
   }
// ------------------------------------------------------------------
// Return a sorted list of all possible soil types to caller.
// ------------------------------------------------------------------
void Soils::allSoilTypes(std::vector<std::string>& soilTypes) const
   {
   XMLNode allSoils = doc->documentElement();
   for_each(allSoils.begin(), allSoils.end(),
            bind(&retrieveChildValuesMatching, "soiltype", soilTypes, _1));
   sort(soilTypes.begin(), soilTypes.end());
   }
*/
// ------------------------------------------------------------------
// Calculate a new unique soil name. Return name to caller.
// ------------------------------------------------------------------
string Soils::calcUniqueSoilName(const std::string& baseName) const
   {
   string name = baseName;
   if(baseName.length()==0){
      name = "unnamed";
   }
   if (existsSoil(name))
      {
      vector<string> soilNames = names();
      unsigned soilNumber=0;
      string uniqueSoilName;
      do
         {
         soilNumber++;
         uniqueSoilName = name + lexical_cast<string>(soilNumber);
         }
      while (existsSoil(uniqueSoilName));
      return uniqueSoilName;
      }
   else
      return name;
   }
// ------------------------------------------------------------------
// Calculate a new unique soil name. Return name to caller.
// ------------------------------------------------------------------
string Soils::calcUniqueSampleNameGivenSoil(const std::string& soilName, const std::string& baseName) const
   {
   string name = baseName;
   if(baseName.length()==0){
      name = "unnamed";
   }
   if (existsSample(soilName, name))
      {
      vector<string> sampleNames;
      allSamplesGivenSoil(soilName, sampleNames);
      unsigned sampleNumber=0;
      string uniqueSampleName;
      do
         {
         sampleNumber++;
         uniqueSampleName = name + lexical_cast<string>(sampleNumber);
         }
      while (existsSample(soilName, uniqueSampleName));
      return uniqueSampleName;
      }
   else
      return name;
   }
// ------------------------------------------------------------------
// Add a new empty soil to the database. Return its name.
// ------------------------------------------------------------------
string Soils::create(void)
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode childNode = allSoils.appendChild("soil", true);
   string newName = calcUniqueSoilName("soil");
   childNode.setAttribute("name", newName);
   return newName;
   }
// ------------------------------------------------------------------
// Add a soil to the database and return its name.
// ------------------------------------------------------------------
std::string Soils::add(Soil& soil)
   {
   string newName = calcUniqueSoilName(soil.name());
   soil.setName(newName);

   XMLNode allSoils = doc->documentElement();
   XMLNode childNode = allSoils.appendChild(soil.getSoilNode(), true);
   return newName;
   }
// ------------------------------------------------------------------
// Delete a soil from the database. Will throw if name doesn't exist.
// ------------------------------------------------------------------
void Soils::erase(const std::string& name)
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", name));  //EqualToName<XMLNode>(name)
   if (i == allSoils.end())
      throw runtime_error("Cannot delete soil " + name + ". Soil doesn't exist.");
   allSoils.erase(i);
   }
// ------------------------------------------------------------------
// Delete a Sample soil from the database. Will throw if sample name doesn't exist.
// ------------------------------------------------------------------
void Soils::eraseSample(const std::string& Soilname,const std::string& Samplename)
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", Soilname));  //EqualToName<XMLNode>(name)
   if (i == allSoils.end())
      throw runtime_error("Cannot delete sample " + Samplename + ". When the "+ Soilname +" soil doesn't exist.");
   else
     {
     XMLNode::iterator i2 = find_if(i->begin(), i->end(),
                             AttributeEquals<XMLNode>("name", Samplename));
     if (i2 == i->end())
        throw runtime_error("Cannot delete sample " + Samplename + ". Sample doesn't exist.");
     i->erase(i2);
     }

   }

// ------------------------------------------------------------------
// return a complete list of soil numbers
// ------------------------------------------------------------------
vector<string> Soils::names() const
   {
   vector<string> soilNames;
   XMLNode allSoils = doc->documentElement();
   for_each(allSoils.begin(), allSoils.end(),
            GetNameAttributeFunction<XMLNode>(soilNames));
//   sort(soilNames.begin(), soilNames.end());
   return soilNames;
   }
// ------------------------------------------------------------------
// return a soil
// ------------------------------------------------------------------
Soil* Soils::get(const string& name)
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", name));
                                 //
   if (i == allSoils.end())
      throw runtime_error("Cannot find soil " + name + ". Soil doesn't exist.");
   return new Soil(i->write());;
   }

// ------------------------------------------------------------------
// return a sample
// ------------------------------------------------------------------
SoilSample* Soils::get(const std::string& soilname, const std::string& samplename)
   {
   Soil* thesoil = get(soilname);
   return new SoilSample(thesoil->getSample(samplename));
   }

// ------------------------------------------------------------------
// adds a sample to the soil and returns the samples name
// ------------------------------------------------------------------
std::string Soils::add(const std::string& soilname, SoilSample& sample)
   {
   string newName = calcUniqueSampleNameGivenSoil(soilname, sample.name());
   sample.setName(newName);

   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", soilname));
   XMLNode childNode = i->appendChild(sample.getSampleNode(), true);
//   XMLNode childNode = allSoils.appendChild(soil.getSoilNode(), true);

   return newName;
   }

// ------------------------------------------------------------------
// replace a sample that belongs to soil and returns the samples name
// ------------------------------------------------------------------
std::string Soils::replace(const std::string& Soilname,
                           const std::string& Samplename, SoilSample& sample)
   {
       if(existsSample(Soilname, Samplename)){
           eraseSample(Soilname, Samplename);
       }
       return add(Soilname, sample);
   }
// ------------------------------------------------------------------
// change the soil a sample belongs to and returns the samples name
// ------------------------------------------------------------------
std::string Soils::replace(const std::string& OldSoilname, const std::string& NewSoilname,
                           const std::string& Samplename, SoilSample& sample)
   {
       if(existsSample(OldSoilname, Samplename)){
           eraseSample(OldSoilname, Samplename);
       }
       return add(NewSoilname, sample);
   }

// ------------------------------------------------------------------
// return true if a soil exists in database
// ------------------------------------------------------------------
bool Soils::existsSoil(const std::string& name) const
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", name));
   return i != allSoils.end();
   }
// ------------------------------------------------------------------
// return true if a sample exists in database
// ------------------------------------------------------------------
bool Soils::existsSample(const std::string& SoilName, const std::string& SampleName) const
   {
   XMLNode allSoils = doc->documentElement();
   XMLNode::iterator i = find_if(allSoils.begin(), allSoils.end(),
                                 AttributeEquals<XMLNode>("name", SoilName));
   if(i != allSoils.end()){
       XMLNode::iterator i2 = find_if(i->begin(), i->end(),
                                 AttributeEquals<XMLNode>("name", SampleName));

      return i2 != i->end();
   } else {
      return false;
   }

   }

// ------------------------------------------------------------------
// return true there are no samples in this soils database
// ------------------------------------------------------------------
bool Soils::zeroSamples()
   {
   vector<string> thenames;
   thenames = names();
   for(int i = 0; i < thenames.size(); i++)
      {
         vector<string> thesamples;
         Soil *processSoil;
         processSoil = get(thenames[i]);
         thesamples = processSoil->samples();
         if(thesamples.size() > 0){
            return false;
         }
      }
   return true;
   }

// ------------------------------------------------------------------
// return list of all the samples belonging to a particular soil
// ------------------------------------------------------------------
void Soils::allSamplesGivenSoil(const std::string& Soilname
                     , std::vector<std::string>& thesamples)
   {
      if(existsSoil(Soilname)){
         Soil *processSoil;
         processSoil = get(Soilname);
         thesamples = processSoil->samples();
      }
   }

// ------------------------------------------------------------------
// return all the samples in this soil database
// ------------------------------------------------------------------
void Soils::allSamples(std::vector<std::string>& samples
                     , std::vector<std::string>& relatesoils
                     , std::vector<std::string>& dates)
   {
   vector<string> thenames;
   thenames = names();
   for(int i = 0; i < thenames.size(); i++)
      {
         vector<string> thesamples;
         Soil *processSoil;
         processSoil = get(thenames[i]);
         thesamples = processSoil->samples();
         for(int j = 0; j < thesamples.size(); j++)
           {
           SoilSample *currentsample;
           currentsample = new SoilSample(processSoil->getSample(thesamples[j]));
           samples.push_back(thesamples[j]);
           relatesoils.push_back(thenames[i]);
           dates.push_back(currentsample->date());
           }
//       soilTypes.push_back()
      }
   }

// ------------------------------------------------------------------
// Make a function that replaces a soil node already in the soils file
// ------------------------------------------------------------------
std::string Soils::replace(const std::string& name, Soil& soil)
   {
       if(existsSoil(name)){
           erase(name);
       }
       return add(soil);
   }
