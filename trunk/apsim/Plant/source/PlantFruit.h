
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
   typedef enum {pw_C3, pw_C4, pw_UNDEF} photosynthetic_pathway_t;    //  FIXME temporary until proper fruit class
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

            void pod_area (int option /* (INPUT) option number*/);

            void dm_pot_rue (float  rue_pod
                           , double  radn_int_pod
                           , double  stress_factor
                           , float g_co2
                           , float meanT
                           , photosynthetic_pathway_t c_photosynthetic_pathway
                           , float  *dlt_dm_pot_pod);                         // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)

            void rue_co2_modifier(photosynthetic_pathway_t photosyntheticType //!please use 'C3' or 'C4' for photosyntheticType
                                , float co2                                   // CO2 level (ppm)
                                , float meanT                                 // daily mean temp (oC)
                                , float *modifier);                           // modifier (-)

            void transp_eff_co2(float svp_fract          // (INPUT)  fraction of distance between svp at mi
                              , float transp_eff_cf      // (INPUT)  transpiration efficiency coefficien
                              , float maxt               // (INPUT)  maximum air temperature (oC)
                              , float mint               // (INPUT)  minimum air temperature (oC)
                              , float co2level           // (INPUT)  current co2 level (ppm)
                              , float *co2_level_te      // (INPUT)  co2 levels (ppm)
                              , float *te_co2_modifier   // (INPUT)  te modifiers of co2 levels (0-1)
                              , int   num_co2_level_te   // (INPUT)  number of table elements in co2-te modifier table
                              , float *transp_eff);      // (OUTPUT) transpiration coefficient

            void sw_demand1(float dlt_dm_pot_rue      //(INPUT)  potential dry matter production with opt
                          , float transp_eff          //(INPUT)  transpiration efficiency (g dm/m^2/mm wa
                          , float *sw_demand);        //(OUTPUT) crop water demand (mm)

            void bio_water1(float sw_supply        //(INPUT)  potential water to take up (supply)
                          , float transp_eff       //(INPUT)  transpiration efficiency (g dm/m^2/m
                          , float *dlt_dm_pot_te); //(OUTPUT) potential dry matter production
                                                   //         by transpiration (g/m^2)
            void bio_grain_oil (
                                float  c_grain_oil_conc
                               , float  c_carbo_oil_conv_ratio
                               , float  *grain_energy
                               );

            void bio_yieldpart_demand1 (float c_twilight
                                      , int   g_day_of_year
                                      , float g_latitude
                                      , float g_dlt_dm                            // (INPUT)  the daily biomass production (
                                      , float dm_tops                             // (INPUT)  green dry weight of tops (g/m^2)
                                      , float dm_green_yield_parts                // (INPUT)  dry matter of yield parts (g/m^2)
                                      , float g_dm_stress_average
                                      , float *p_x_pp_hi_incr
                                      , float *p_y_hi_incr
                                      , int   p_num_pp_hi_incr
                                      , float *p_x_hi_max_pot_stress
                                      , float *p_y_hi_max_pot
                                      , int   p_num_hi_max_pot
                                      , float g_grain_energy
                                      , float g_mint
                                      , float p_minTempGrnFill
                                      , int   p_daysDelayGrnFill
                                      , float *dlt_dm_yieldpart_demand
                                      ) ;

            void grain_n_demand1(float C_sfac_slope
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

           void dm_retranslocate1(float  c_frac_pod
                                , float  g_grain_energy
                                , float  c_grain_oil_conc
                                , int    pod
                                , int    meal
                                , int    oil
                                , int    max_part
                                , float  g_dlt_dm_retrans_to_fruit
                                , int    *supply_pools
                                , int    num_supply_pools
                                , float  g_dlt_dm_grain_demand
                                , float  g_dlt_dm_oil_conv
                                , float  *g_dlt_dm_green
                                , float  *g_dm_green
                                , float  *g_dm_plant_min
                                , float  g_plants
                                , float  *dm_oil_conv_retranslocate
                                , float  *dm_retranslocate
                                ) ;

            void bio_actual (int option /* (INPUT) option number*/);

            void dm_senescence1 (const int num_part
                               , float independant_variable
                               , float **c_x_dm_sen_frac
                               , float **c_y_dm_sen_frac
                               , int   *c_num_dm_sen_frac
                               , float *g_dm_green
                               , float *g_dlt_dm_green
                               , float *g_dlt_dm_green_retrans
                               , float *dlt_dm_senesced);       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)

               void retrans_init(float  c_pod_trans_frac
                               , float  g_plants
                               , float *dm_green
                               , float *dm_plant_min
                               );

               void n_senescence1(int num_part
                                , float  *c_n_sen_conc
                                , float  *g_dlt_dm_senesced
                                , float  *g_n_green
                                , float  *g_dm_green
                                , float  *dlt_n_senesced_trans
                                , float  *dlt_n_senesced
                                ) ;

               void n_conc_grain_limits(float  c_n_conc_crit_grain
            				  , float  c_n_conc_max_grain
            				  , float  c_n_conc_min_grain
            				  , float  *g_dlt_dm_green_retrans
            				  , float  *g_dlt_dm_green
            				  , float  *g_dm_green
            				  , float  *n_conc_crit
            				  , float  *n_conc_max
            				  , float  *n_conc_min) ;

               void nit_init (void);

               void n_retranslocate( float  *g_n_conc_min               // (INPUT)  minimum N concentration (g N/g
                                   , float  *g_dm_green                 // (INPUT)  live plant dry weight (biomass
                                   , float  *g_n_green                  // (INPUT)  plant nitrogen content (g N/m^
                                   , float  g_grain_n_demand            //  INPUT
                                   , float  *dlt_n_retrans              // (OUTPUT) plant N taken out from plant parts (g N/m^2)
                                   );

               void N_retrans_avail(const int meal
                                  , float *g_N_conc_min
                                  , float *g_dm_green
                                  , float *g_N_green
                                  , float *N_avail);

                void n_demand(const int max_part,           // (INPUT)
                              int   *demand_parts,          // (INPUT)
                              const int num_demand_parts,   // (INPUT)
                              float G_dlt_dm,           // (INPUT)  the daily biomass production (g/m^2)
                              float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                              float G_dlt_dm_pot_rue,       // (INPUT)  potential dry matter production from pods (g/m^2)
                              float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out from plant parts (g/m^2)
                              float *G_dm_green,            // (INPUT)  live plant dry weight (biomass g/m^2)
                              float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/g dm)
                              float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g dm)
                              float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^2)
                              float *N_demand,              // (OUTPUT) critical plant nitrogen demand (g/m^2)
                              float *N_max);                 // (OUTPUT) max plant nitrogen demand (g/m^2)

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
         bool delayGrnFill;
         int daysDelayedGrnFill;
      };
      Globals g;

      // The plant we hook into
      Plant *plant;
      PlantComponent *parentPlant;
      PlantPhenology *phenology;

};

#endif
