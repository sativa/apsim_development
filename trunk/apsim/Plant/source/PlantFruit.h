
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

      //*****        FIXME when this becomes proper class
      //      indices of plant part names
//      const int  root = 0 ;
//      const int  leaf = 1 ;
//      const int  stem = 2 ;
//      const int  pod  = 3 ;
//      const int  meal = 4 ; // excludes oil component
//      const int  oil  = 5 ; // seed oil
//
//      // number of plant parts
//      const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

class PlantFruit
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
	public:												// member functions
		PlantFruit(Plant *P);			// default constructor
//            PlantFruit(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
		const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

            void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);

//            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
            float coverTotal() const;
            float coverGreen() const;
            float coverSen() const;
            float coverDead() const;
            float interceptRadiation(float radiation);
//		float total() const;  	// Query

		virtual void display(ostream &os = cout) const;	// display function
            float calcCover (float cExtinctCoef, float pai);  // calc pod cover

            float divide (float dividend, float divisor, float default_value) const;  // Command

            void legnew_bio_grain_oil (
                                        float  c_grain_oil_conc
                                       ,float  c_carbo_oil_conv_ratio
                                       ,float  *grain_energy
                                      );

            void legnew_bio_yieldpart_demand1 (float c_twilight
                                              ,int   g_day_of_year
                                              ,float g_latitude
                                              ,int  *yield_parts
                                              ,int   num_yield_parts
                                              ,int   root_part
                                              ,int   max_part
                                              ,float g_dlt_dm
                                              ,float *g_dm_green
                                              ,float *g_dm_senesced
                                              ,float g_dm_stress_average
                                              ,float *p_x_pp_hi_incr
                                              ,float *p_y_hi_incr
                                              ,int   p_num_pp_hi_incr
                                              ,float *p_x_hi_max_pot_stress
                                              ,float *p_y_hi_max_pot
                                              ,int   p_num_hi_max_pot
                                              ,float g_grain_energy
                                              ,float *dlt_dm_yieldpart_demand
                                              ) ;

            void plant_grain_n_demand1(float C_sfac_slope
                                       , float C_sw_fac_max
                                       , float C_temp_fac_min
                                       , float C_tfac_slope
                                       , float G_maxt
                                       , float G_mint
                                       , float G_nfact_grain_conc
                                       , float *G_n_conc_crit
                                       , float G_swdef_expansion
                                       , float *G_n_conc_min
                                       , float *G_dlt_dm_green
                                       , float *G_dlt_dm_green_retrans
                                       , float *G_dm_green
                                       , float *G_n_conc_max
                                       , float *G_n_green
                                       , float *grain_n_demand
                                       );

            float dm_yield_demand ( float  c_frac_pod
                                  , float  g_grain_energy
                                  , float  g_dlt_dm_veg
                                  , double g_dlt_dm
                                  , float  g_dlt_dm_grain_demand
                                  );

            void yieldpart_demand_stress1(float nutrientFactPhoto
                                          , float G_swdef_photo
                                          , float G_temp_stress_photo
                                          , float *Dlt_dm_stress_max
                                          );

            void dm_partition1 (float  c_frac_pod
                               ,float  g_grain_energy
                               ,float  c_grain_oil_conc
                               ,double g_dlt_dm
                               ,float  g_dlt_dm_grain_demand
                               ,float  *dlt_dm_oil_conv
                               ,float  *dlt_dm_green
                               );

  void legnew_dm_retranslocate1
    (
     float  c_frac_pod
    ,float  g_grain_energy
    ,float  c_grain_oil_conc
    ,int    pod
    ,int    meal
    ,int    oil
    ,int    max_part
    ,int    *supply_pools
    ,int    num_supply_pools
    ,float  g_dlt_dm_grain_demand
    ,float  g_dlt_dm_oil_conv
    ,float  *g_dlt_dm_green
    ,float  *g_dm_green
    ,float  *g_dm_plant_min
    ,float  g_plants
    ,float  *dm_oil_conv_retranslocate
    ,float  *dm_retranslocate
    ) ;


#if TEST_PlantFruit
		virtual ~PlantFruit();							// destructor
#else
		virtual ~PlantFruit();
#endif

//      float cover_green
//      float cover_sen
//      float cover_dead

      struct Cover
      {
         float green;
         float sen;
         float dead;
      };

      Cover cover;

//      struct PlantPartType
//         {
//         float leaf;
//         float stem;
//         };
//
//         PlantPartType green;
//         PlantPartType senesced;
//         PlantPartType dead;
	private:
      void read_constants (Plant *systemInterface);
      void zeroVariables();

      struct Constants
      {
         float extinctionCoeff;
      };
      Constants c;

      struct Globals
      {
         float pai;
      };
      Globals g;

      // The plant we hook into
      Plant *plant;
      PlantComponent *parentPlant;
      PlantPhenology *phenology;

};

#endif
