#ifndef CropPhenologyH
#define CropPhenologyH

#include "PlantPhenology.h"

class CropPhenology : public PlantPhenology
   {
   protected:
       CropPhenology(plantInterface *p) : PlantPhenology(p) {};
       virtual void zeroDeltas(void);
       virtual void zeroAllGlobals(void);
       virtual void readConstants (protocol::Component *s, const string &section);
       virtual void doRegistrations (protocol::Component *s);
       virtual void onSow(unsigned &, unsigned &, protocol::Variant &v);
       virtual void onEndCrop(unsigned &, unsigned &, protocol::Variant &v);
       virtual void setupTTTargets()=0;
       virtual void onKillStem(unsigned &, unsigned &, protocol::Variant &);
       virtual void onHarvest(unsigned &, unsigned &, protocol::Variant &);
       virtual void readSpeciesParameters(protocol::Component *s, vector<string> &sections);
       virtual void update(void);
       virtual bool plant_germination(float pesw_germ, float sowing_depth, float pesw_seed);
       int   das;

       // Rates
       float dlt_tt;
       float dlt_tt_phenol;

       // Parameters
       float shoot_lag;                                  // minimum growing degree days for
                                                         // germination (deg days)
       float shoot_rate;                                 // growing deg day increase with depth
                                                         // for germination (deg day/mm depth)
       float sowing_depth;
       float pesw_germ;                                  // plant extractable soil water in
                                                         // seedling layer inadequate for
                                                         // germination (mm/mm)
       interpolationFunction y_tt, rel_emerg_rate;
       interpolationFunction y_removeFractPheno;
       lookupFunction stage_reduction_harvest;
       lookupFunction stage_reduction_kill_stem;

    private:
    public:
    };
#endif

