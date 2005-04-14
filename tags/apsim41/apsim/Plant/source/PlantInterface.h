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

class plantInterface {
	public:
      virtual void doInit1(protocol::Component *) = 0;
      virtual void doRegistrations(protocol::Component *) = 0;
      virtual void initialise(void) = 0;

      virtual float getLeafNo (void) const = 0;           // Leaf number (leaves/m^2)
};

#endif