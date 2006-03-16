//---------------------------------------------------------------------------
#ifndef PlantIfaceH
#define PlantIfaceH
#include <boost/function.hpp>
#include <boost/bind.hpp>

typedef enum {pw_C3, pw_C4, pw_UNDEF} photosynthetic_pathway_t;

// Class for communications - a sink for messages.
class commsInterface
  {
  public:
      virtual void writeString (const char *line) = 0;
      virtual void warningError (const char *msg) = 0;
  };


namespace protocol {
  class Component;
};

// Abstact plant interface
class plantInterface {
   public:
      virtual void doInit1(protocol::Component *) = 0;
      virtual void doRegistrations(protocol::Component *) = 0;
      virtual void initialise(void) = 0;

      virtual float getLeafNo (void) const = 0;           // Leaf number (leaves/m^2)
      virtual float getPlants (void) const = 0;           // Planting density (plants/m^2)
      virtual float getCo2 (void) const = 0;              // CO2 level (ppm)
      virtual photosynthetic_pathway_t getPhotosynthetic_pathway(void) const = 0;              // CO2 level (ppm)
      virtual float getStageCode (void) const = 0;        // Phenological stage code AAACK DIE YOU BASTARD
      virtual float getStageNumber (void) const = 0;        // Phenological stage code AAACK DIE YOU BASTARD
//      virtual float getRadnInterceptedPod(void) const = 0;
      virtual float getDltDMPotRueVeg(void) const = 0;
      virtual float getDmGreenVeg(void) const = 0;
//      virtual float getDltDmVeg(void) const = 0;
      virtual float getWaterSupplyPod(void) const = 0;
      virtual float getDmTops(void) const = 0;
      virtual float getDltDm(void) const = 0;
      virtual float getDmVeg(void) const = 0;
      virtual float getDmGreenStem(void) const = 0;
      virtual float getDmGreenTot(void) const = 0;

      virtual float getRelativeGrowthRate(void) = 0;
      // temporary
      virtual float getDyingFractionPlants(void) = 0;
      virtual float getCo2ModifierRue(void) const = 0;
      virtual float getCo2ModifierTe(void) const = 0;
      virtual float getCo2ModifierNConc(void) const = 0;
      virtual float getVpd(void) const = 0;

      virtual float getTempStressPhoto(void) const = 0;
      virtual float getNfactPhoto(void) const = 0;
      virtual float getOxdefPhoto(void) const = 0;
      virtual float getPfactPhoto(void) const = 0;
      virtual float getSwdefPhoto(void) const = 0;
      virtual bool phosphorusAware(void) const = 0;       // Whether P is present in system
      virtual bool removeBiomassReport(void) const = 0;
      virtual void doPlantEvent(const string &) = 0;      // Something is asking the plant to do something
      virtual bool on_day_of(const string &) = 0;
      virtual bool inPhase(const string &) = 0;
};

// Something that plugs into a plant
class plantThing {
   public:
     virtual void doRegistrations(protocol::Component *) = 0;
     virtual void readConstants (protocol::Component *, const string &) = 0;
     virtual void readSpeciesParameters (protocol::Component *, vector<string> &) = 0;
     virtual void readCultivarParameters (protocol::Component *, const string &) = 0;
     virtual void onPlantEvent(const string &) = 0;
     virtual void update(void) = 0;

     virtual void zeroAllGlobals(void) = 0;
     virtual void zeroDeltas(void) = 0;
};


#define setupEvent(s,name,type,address) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   s->addEvent(name, type, fn);\
   }

#define setupGetFunction(s,name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }

#endif