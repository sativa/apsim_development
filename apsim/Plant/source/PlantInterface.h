//---------------------------------------------------------------------------
#ifndef PlantIfaceH
#define PlantIfaceH

// Abstract (Interface) class for communications
class commsInterface {
	public:
      virtual void writeString (const char *line) = 0;
      virtual void warningError (const char *msg) = 0;
};

#endif