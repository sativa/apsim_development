//---------------------------------------------------------------------------

#ifndef OORachisH
#define OORachisH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Rachis : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   // nitrogen
   float initialNConc;
   float targetNConc;
   float structRachisNConc;

//  Variables  -----------------------------------------------------


// Private Methods -------------------------------------------------------
   void doRegistrations(void);
   void initialize(void);

// public Methods -------------------------------------------------------
   public:
   Rachis(OOPlant *p);
   ~Rachis();

   void   readParams (string cultivar);                    // plant
   void   updateVars(void);                     // plant

   float partitionDM(float dltDM);                        // biomass

   float calcNDemand(void);                         // nitrogen
   float calcStructNDemand(void);                   // nitrogen
   float provideN(float requiredN);

   // phosphorus
   float calcPDemand(void);

   // phenology
   void phenologyEvent(int);
  };


//---------------------------------------------------------------------------
#endif
