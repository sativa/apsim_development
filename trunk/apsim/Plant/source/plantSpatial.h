#ifndef PlantSpatialH
#define PlantSpatialH

#include <ComponentInterface/Component.h>
#include <general/string_functions.h>

#include "PlantLibrary.h"


class PlantSpatial {
  public:
  PlantSpatial(void);
  ~PlantSpatial(void);

   PlantSpatial(const PlantSpatial &PlantSpatial); 			// copy constructor
   const PlantSpatial &operator=(const PlantSpatial &other);		// Assigment operator

   void init(plantInterface *p);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void startCrop (protocol::Component *system, protocol::Variant &v);
   void setPlants(float plants);
   void setCanopyWidth(float canopy_width);
   float canopyFac(void);
   float rowSpacing(void);

//  private:
      float dlt_plants;
      float row_spacing_default;
      float skip_row_default;                           //Default skip row ()
      float skip_plant_default;                         //Default skip plant ()

      float sowing_depth;
      float row_spacing;                                // row spacing (m) [optional]
      float skip_row;                                   // skip row (0, 1, 2)
      float skip_plant;                                 // skip plant (0, 1, 2)
      float skip_row_fac;                               // skip row factor
      float skip_plant_fac;                             // skip plant factor

  private:
      float plants;
      float canopy_width;

      plantInterface *plant;                 // The plant we are attached to
};

#endif /* PlantSpatialH */

