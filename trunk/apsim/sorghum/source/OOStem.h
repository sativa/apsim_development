//---------------------------------------------------------------------------

#ifndef OOStemH
#define OOStemH

#include "OOPlantComponents.h"
#include "utilities.h"
//------------------------------------------------------------------------------------------------

class Stem : public PlantPart
   {

// Parameters ----------------------------------------------------------

   float initialDM;
   TableFn heightFn;
   float translocFrac;
   // nitrogen
   float initialNConc;
//   float targetNConc;
   TableFn targetNFn;
   TableFn structNFn;
   float dltNConc;

   float density;

//  Variables  -----------------------------------------------------

   float canopyHeight;
   float dltCanopyHeight;

   // biomass
   float dmGreenStem;           // stem dry weight / plant



// Private Methods -------------------------------------------------------
   void doRegistrations(void);
   void initialize(void);


// public Methods -------------------------------------------------------
   public:
   // plant
   void calcCanopyHeight(void);
   Stem(OOPlant *p);
   void   readParams (string cultivar);
   void   updateVars(void);
   void   process(void);

   // nitrogen
   float calcNDemand(void);
   float calcStructNDemand(void);
   float provideN(float requiredN);

   // phosphorus
   float calcPDemand(void);


   // biomass
   void   partitionDM(float dltDM){dltDmGreen = dltDM;}
   float  dmRetransAvailable(void);
   void   dmRetrans(float dltDm){dmRetranslocate = dltDm;}

   // phenology
   void phenologyEvent(int);
   };


//---------------------------------------------------------------------------
#endif
