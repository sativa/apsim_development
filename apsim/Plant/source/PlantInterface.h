//---------------------------------------------------------------------------
#ifndef PlantIfaceH
#define PlantIfaceH
#include <boost/function.hpp>
#include <boost/bind.hpp>

// Maximum size_of of tables
#define max_table 30

////////////////////////
// array size settings
// Maximum number of layers in soil
#define max_layer 100

// Forward definitions..
namespace protocol {
  class Component;
  class QuerySetValueData;
  class ApsimGetQueryData;
};
class environment_t;
class CompositePart;

// An abstract plant interface, as seen from protocol::Component (outside)
class IPlant {
 public:
   virtual ~IPlant() {};
   virtual void onInit1() = 0;
   virtual void onInit2() = 0;
   virtual bool respondToSet(unsigned int& /*fromID*/, protocol::QuerySetValueData& /*setValueData*/) = 0;
};

//      crop status names
typedef enum {out, dead, alive} status_t;

// Abstact plant interface, as seen from plant things (inside)
class plantInterface {
   public:
      virtual ~plantInterface() {};

      virtual void writeString (const char *line) = 0;
      virtual void warningError (const char *msg) = 0;

      virtual float getLeafNo (void) = 0;           // Leaf number (leaves/m^2)
      virtual float getLAI(void) = 0;
      virtual std::string Name() = 0;
      virtual float getPlants (void) = 0;           // Planting density (plants/m^2)
      virtual float getCo2 (void) = 0;              // CO2 level (ppm)
      virtual float getStageCode (void) = 0;        // Phenological stage code AAACK DIE YOU BASTARD
      virtual float getStageNumber (void) = 0;        // Phenological stage code AAACK DIE YOU BASTARD
      virtual float getDltDMPotRueVeg(void) = 0;
      virtual float getDmTops(void) = 0;
      virtual float getDltDmGreen(void)  = 0;
      virtual float getDltDm(void) = 0;
      virtual float getDmVeg(void) = 0;
      virtual float getDmGreenStem(void) = 0;
      virtual float getDmGreenTot(void) = 0;
        // FIXME - remove next line after P demand corrections activated
      virtual float getRelativeGrowthRate(void) = 0;
      virtual float getTotalPotentialGrowthRate(void) = 0;
      // temporary
      virtual float getDyingFractionPlants(void) = 0;
      virtual float getCo2ModifierRue(void) = 0;
      virtual float getCo2ModifierTe(void) = 0;
      virtual float getCo2ModifierNConc(void) = 0;
      virtual float getVpd(void) = 0;

      virtual float getTempStressPhoto(void) = 0;
      virtual float getNfactPhoto(void) = 0;
      virtual float getNfactGrainConc(void) = 0;
      virtual float getOxdefPhoto(void) = 0;
      virtual float getPfactPhoto(void) = 0;
      virtual float getSwdefPhoto(void) = 0;
      virtual float getCumSwdefPheno(void) = 0;
      virtual float getCumSwdefPhoto(void) = 0;
      virtual bool phosphorusAware(void) = 0;       // Whether P is present in system
      virtual bool removeBiomassReport(void) = 0;
      virtual void doPlantEvent(const string &) = 0;      // Something is asking the plant to do something
      virtual bool on_day_of(const string &) = 0;
      virtual bool inPhase(const string &) = 0;
      virtual int daysInCurrentPhase() = 0;
      virtual float ttInCurrentPhase() = 0;
      virtual status_t Status() = 0;
      virtual void SetStatus(status_t NewStatus) = 0;
      virtual CompositePart& Tops() = 0;

      virtual const environment_t *getEnvironment(void) = 0;
      virtual const string & getCropType(void) = 0;
      virtual protocol::Component *getComponent(void) = 0;
};

// Something that plugs into a plant
class plantThing {
   protected:
      ScienceAPI& scienceAPI;
   public:
     plantThing(ScienceAPI& api) : scienceAPI(api) {};
     virtual ~plantThing() {};
     virtual void onInit1(protocol::Component *) {};
     virtual void readConstants (protocol::Component *, const string &) {};
     virtual void readSpeciesParameters (protocol::Component *, vector<string> &) {};
     virtual void readCultivarParameters (protocol::Component *, const string &) {};
     virtual void read() { }
     virtual void onPlantEvent(const string &) = 0;
     virtual void update(void) = 0;

     virtual void zeroAllGlobals(void) = 0;
     virtual void zeroDeltas(void) = 0;
};


#define setupEvent(s,name,type,address, DDML) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   s->addEvent(name, type, fn, DDML);\
   }

#define setupGetFunction(s,name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }

#endif
