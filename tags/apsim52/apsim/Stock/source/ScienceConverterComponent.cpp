#include "ScienceConverterComponent.h"
#include "Conversion.h"

#pragma package(smart_init)
using namespace std;

const float kg2g = 1000.0;
const float ha2sm = 10000.0;
const float g2kg = (1.0/kg2g);
const float sm2ha = (1.0/ha2sm);
const float cmol2mol = (1.0/100.0);
const float mm2m = (1.0/1000.0);



// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new ScienceConverterComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScienceConverterComponent::ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ScienceConverterComponent::~ScienceConverterComponent(void)
   {
    if (conversion) delete conversion;
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);
   endRunID = addRegistration(RegistrationType::respondToEvent, "end_run", protocol::DDML("").c_str());

    conversion_model = readParameter ("constants", "conversion_model");

    if (conversion_model == "")
       throw std::invalid_argument("The parameter 'conversion_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (conversion_model == "herbage")
       conversion = new HerbageConverter(this);
//    else if (scratch == "surfaceom")
//       conversion = new ResidueHerbage();
    else if (conversion_model == "nonherbage")
       conversion = new NonHerbageConverter(this);
    else
       throw std::invalid_argument("Unknown conversion model '" + conversion_model + "'");

    conversion->doInit1(sdml);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit2(void)
   {
      ostringstream msg;
      msg << "Conversion model:- " << conversion_model << endl << ends;
      writeString (msg.str().c_str());

    conversion->doInit2();
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID == endRunID)  //FIXME!! Coding to avoid stack being wiped out.
   {
            ostringstream msg;
            msg << "Stock Science Converter Component Exiting" << endl << ends;
            cerr << msg;
            exit(1);
   }
    conversion->respondToEvent(fromID, eventID, variant);

}

void ScienceConverterComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
{
    conversion->respondToGet(fromID, queryData);
}

