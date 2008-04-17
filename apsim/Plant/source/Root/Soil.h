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

      float dlayer[max_layer];                         // thickness of soil layer I (mm)
      float ll15_dep[max_layer];
      float dul_dep[max_layer];                        // drained upper limit soil water content for soil layer L (mm water)
      float sat_dep[max_layer];
      float bd[max_layer];
      int num_layers;

      int find_layer_no(float);
      int find_layer_no(float,const vector<float>&);
      int find_layer_no(float, float *, int);

      float sw_dep[max_layer];                         // soil water content of layer L (mm)
      float no3gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)
      float nh4gsm [max_layer];                         // nitrate nitrogen in layer L (g N/m^2)

   protected:
      ScienceAPI& scienceAPI;



   private:


   };


#endif /* SoilH */
