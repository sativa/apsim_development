#ifndef SoilH
#define SoilH

class Soil
   {
   public:
      ~Soil(){};
      Soil(ScienceAPI& scienceAPI);
      void onInit1(protocol::Component *system);
      void onNewProfile(protocol::NewProfileType &v);
      void zero(void);
      void ZeroDeltas(void);
      void Read(void);
      float pesw(int depth);
      float fasw(int depth);
      float layer_fasw(int layerNo);
      void write();
      void doSWAvailable(float root_depth);
      void doSWSupply(float root_depth);
      void doPotentialExtractableSW(float root_depth);
      void doWaterSupply (float root_depth);
      void doWaterUptakeInternal (float sw_demand, float root_depth);
      float peswTotal(float root_depth);
      float swSupply(float root_depth);
      float root_proportion (int layer, float root_depth);
      void plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array);              //(OUTPUT) crop uptake array
      void getOtherVariables();
      void doWaterUptakeExternal (string uptake_source, string crop_type);
      void UpdateOtherVariables(string uptake_source);
      void crop_check_sw(float minsw, float *dlayer, float *dul_dep, float *sw_dep, float *ll_dep);

      float swAvailable(float root_depth);
      float sw_lb;                                      // lower limit of soilwater  (mm/mm)
      float sw_ub;                                      // upper limit of soilwater  (mm/mm)
      float sw_dep_ub;                                  // upper limit of soilwater depth (mm)
      float sw_dep_lb;                                  // lower limit of soilwater depth (mm)
      float kl_ub;                                      // upper limit of water uptake factor
      float no3_ub;                                     // upper limit of soil NO3 (kg/ha)
      float no3_lb;                                     // lower limit of soil NO3 (kg/ha)
      float nh4_ub;                                     // upper limit of soil NH4 (kg/ha)
      float nh4_lb;                                     // lower limit of soil NH4 (kg/ha)


      float dlayer[max_layer];                         // thickness of soil layer I (mm)
      float ll_dep[max_layer];                          // lower limit of plant-extractable
      vector<float> kl;                                // root length density factor for water
      vector<float> xf;                                 // root exploration factor (0-1)

      float ll15_dep[max_layer];
      float dul_dep[max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      int num_layers;

      float sw_avail_pot[max_layer];                    // potential extractable soil water (mm)
      float sw_avail[max_layer];                        // actual extractable soil water (mm)
      float sw_supply [max_layer];                      // potential water to take up (supply)
                                                        // from current soil water (mm)
      float dlt_nh4gsm[max_layer];                      // actual NH4 uptake from soil (g/m^2)
      float dlt_no3gsm[max_layer];                      // actual NO3 uptake from soil (g/m^2)


      int find_layer_no(float);
      int find_layer_no(float,const vector<float>&);
      int find_layer_no(float, float *, int);

      float dlt_sw_dep[max_layer];                      // water uptake in each layer (mm water)
      float sw_dep[max_layer];                         // soil water content of layer L (mm)
      float no3gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float nh4gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
   private:



   protected:
      ScienceAPI& scienceAPI;



   private:


   };


#endif /* SoilH */
