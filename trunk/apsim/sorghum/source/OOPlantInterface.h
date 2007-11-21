//---------------------------------------------------------------------------
#ifndef OOPlantInterfaceH
#define OOPlantInterfaceH

class ScienceAPI;

#include "OOPlant.h"

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class PlantInterface 
   {
   public:
   PlantInterface(ScienceAPI & api) : scienceAPI(api)
      {
      scienceAPI.subscribe("init1", nullFunction(&PlantInterface::onInit1));
      scienceAPI.subscribe("init2", nullFunction(&PlantInterface::onInit2));
      plant = NULL;
      };

   ~PlantInterface(void)
      {
      if (plant) delete plant;
      };

   private:
      OOPlant     *plant;    // The plant module
      ScienceAPI  &scienceAPI;

   void onInit1(void)
      {
      plant = new OOPlant(scienceAPI);
      plant->plantInit1();
      }
   void onInit2(void)
      {
      plant->plantInit2();
      }
   };
#endif
