
#include "PlantSpatial.h"
////using namespace std;

PlantSpatial::PlantSpatial(void)
   {
   }

PlantSpatial::~PlantSpatial(void)
   {
   }

// copy constructor
//	copy data members of object
PlantSpatial::PlantSpatial(const PlantSpatial &/* PlantSpatial*/)
//===========================================================================
{
	throw std::invalid_argument("Copy constructor NI for PlantSpatial");
}


// Assigment operator
//	assign data members of object
const PlantSpatial &PlantSpatial::operator=(const PlantSpatial &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for PlantSpatial");
}

void PlantSpatial::init(plantInterface *p)
   {
     zeroAllGlobals();
     plant = p;

   }

void PlantSpatial::zeroAllGlobals(void)
{
      row_spacing_default = 0.0;
      skip_row_default = 0.0;
      skip_plant_default = 0.0;
      canopy_width = 0.0;

      plants         = 0.0;
      sowing_depth   = 0.0;
      row_spacing    = 0.0;
      skip_row       = 0.0;
      skip_plant     = 0.0;
      skip_row_fac   = 0.0;
      skip_plant_fac = 0.0;

      zeroDeltas();
}

void PlantSpatial::zeroDeltas(void)
{
      dlt_plants     = 0.0;
}


void PlantSpatial::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
    system->readParameter (sections
                   ,"row_spacing_default"//, "(mm)"
                   , row_spacing_default
                   , 0.0, 2000.);

    system->readParameter (sections
                   ,"skiprow_default"//, "()"
                   , skip_row_default
                   , 0.0, 2.0);
}

void PlantSpatial::startCrop (protocol::Component *system, protocol::Variant &v/*(INPUT) message arguments*/)
   //===========================================================================
    {
           protocol::ApsimVariant incomingApsimVariant(system);
           incomingApsimVariant.aliasTo(v.getMessageData());

           // get other sowing criteria
           if (incomingApsimVariant.get("plants", protocol::DTsingle, false, plants) == false)
               {
               throw std::invalid_argument("plant density ('plants') not specified");
               }
           bound_check_real_var(plant, plants, 0.0, 1000.0, "plants");

           if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
               {
               throw std::invalid_argument("sowing_depth not specified");
               }
           bound_check_real_var(plant, sowing_depth, 10.0, 200.0, "sowing_depth");

           if (incomingApsimVariant.get("row_spacing", protocol::DTsingle, false, row_spacing) == false)
               {
               row_spacing = row_spacing_default;
               }
           bound_check_real_var(plant, row_spacing, 0.0, 2000.0, "row_spacing");


           if (incomingApsimVariant.get("skipplant", protocol::DTsingle, false, skip_plant) == false)
               {
               skip_plant = skip_plant_default;
               }
           bound_check_real_var(plant, skip_plant, 0.0, 2.0, "skipplant");
           skip_plant_fac = (2.0 + skip_plant)/2.0;

           if (incomingApsimVariant.get("skiprow", protocol::DTsingle, false, skip_row) == false)
               {
               skip_row = skip_row_default;
               }
           bound_check_real_var(plant, skip_row, 0.0, 2.0, "skiprow");
           skip_row_fac = (2.0 + skip_row)/2.0;
    }


float PlantSpatial::canopyFac (void)
   //===========================================================================
{
    float canopy_fac = 0.0;
    if (canopy_width > 0.0)
        {
        legnew_canopy_fac (row_spacing
                           , plants
                           , skip_row_fac
                           , skip_plant_fac
                           , canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = skip_row_fac;
        }
    return canopy_fac;
}

float PlantSpatial::rowSpacing (void)
   //===========================================================================
{
    return row_spacing;
}

void PlantSpatial::setPlants (float plant_density)
   //===========================================================================
{
    plants = plant_density;
}

void PlantSpatial::setCanopyWidth (float width)
   //===========================================================================
{
    canopy_width = width;
}

