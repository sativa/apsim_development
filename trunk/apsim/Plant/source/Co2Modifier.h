#ifndef Co2ModifierH
#define Co2ModifierH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>


#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "Environment.h"

typedef enum {photosynthetic_pathway_UNDEF, photosynthetic_pathway_C3, photosynthetic_pathway_C4} photosynthetic_pathway_t;

class Co2Modifier {
public:
   Co2Modifier(ScienceAPI& scienceAPI, protocol::Component *p);
   ~Co2Modifier(void);
   void init(void);
   void zero_co2_variables (void);
   void doPlant_Co2Modifier (const Environment& Environment);
   void read_co2_constants (void);
   float rue (void) const;
   float te (void) const;
   float n_conc (void);

   StressDeficit tFact;

private:
   ScienceAPI& scienceAPI;

   protocol::Component *parent;                 // The plant we are attached to
   interpolationFunction cTE;
   interpolationFunction cNConc;


      float     co2_modifier_rue;
      float     co2_modifier_te;
      float     co2_modifier_n_conc;

   struct {
      photosynthetic_pathway_t photosynthetic_pathway;
   }  c;   // Constants

   float plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                               float meanT);          //!modifier (-)
}; //Co2Modifier


#endif //Co2Modifier_H_

