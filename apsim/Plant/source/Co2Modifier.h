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
   Co2Modifier(ScienceAPI& scienceAPI,PlantComponent *p);
   ~Co2Modifier(void);
   void init(void);
   void zero_co2_variables (void);
   void doPlant_Co2Modifier (environment_t& Environment);
   void read_co2_constants (void);
   float rue (void) {return co2_modifier_rue;};
   float te (void) {return co2_modifier_te;};
   float n_conc (void) {return co2_modifier_n_conc;};

   StressDeficit tFact;

private:
   ScienceAPI& scienceAPI;

   PlantComponent *parent;                 // The plant we are attached to

      float     co2_modifier_rue;
      float     co2_modifier_te;
      float     co2_modifier_n_conc;

   struct {
      float      x_co2_te_modifier[max_table], y_co2_te_modifier[max_table];
      int        num_co2_te_modifier;
      float      x_co2_nconc_modifier[max_table], y_co2_nconc_modifier[max_table];
      int        num_co2_nconc_modifier;
      photosynthetic_pathway_t photosynthetic_pathway;
   }  c;   // Constants

   float plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                               float avet);          //!modifier (-)
}; //Co2Modifier


#endif //Co2Modifier_H_

