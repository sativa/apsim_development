//---------------------------------------------------------------------------
#ifndef PlantIfaceH
#define PlantIfaceH

// Abstract (Interface) class for communications
class commsInterface {
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
      virtual float getStageCode (void) const = 0;        // Phenological stage code AAACK DIE YOU BASTARD
      virtual bool phosphorusAware(void) const = 0;       // Whether P is present in system
      virtual void doPlantEvent(const string &) = 0;      // Something is asking the plant to do something
};

// Something that plugs into a plant
class plantThing {
   public:
     virtual void doRegistrations(protocol::Component *) = 0;
     virtual void readSpeciesParameters (protocol::Component *, vector<string> &) = 0;
     virtual void readCultivarParameters (protocol::Component *, const string &) = 0;
     virtual void onPlantEvent(const string &) = 0;
     virtual void update(void) = 0;

     virtual void zeroAllGlobals(void) = 0;
     virtual void zeroDeltas(void) = 0;
};

#endif