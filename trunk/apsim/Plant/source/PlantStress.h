#ifndef PlantStressH
#define PlantStressH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>


#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "Environment.h"

class RootBase;
class plantPart;

class SWStress {
public:
   SWStress(ScienceAPI& scienceAPI,PlantComponent *p);
   ~SWStress(void);
   void init(RootBase *root);
   void get_swdef_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swdef_fixation(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_swstress_fixation(protocol::Component *, protocol::QueryValueData &);

      void zeroAllGlobals(void);
      void doPlantWaterStress (float sw_demand);
      float SWDefExpansion(float sw_demand);
      float SWDefPhoto(float sw_demand);
      float SWDefPheno(int num_sw_avail_ratio, float x_sw_avail_ratio[], float y_swdef_pheno[]);
      float SWDefFixation(void);
      float SWDefOxygen (float wet_root_fr);

   StressDeficit swDef;

private:
   ScienceAPI& scienceAPI;

   PlantComponent *parent;                 // The plant we are attached to
   RootBase *rootPart;

      int   num_sw_avail_ratio;
      float x_sw_avail_ratio [max_table];
      float y_swdef_pheno [max_table];

      int        num_sw_avail_ratio_flower;
      float      x_sw_avail_ratio_flower[max_table];
      float      y_swdef_pheno_flower [max_table];

      int        num_sw_avail_ratio_grainfill;
      float      x_sw_avail_ratio_grainfill [max_table];
      float      y_swdef_pheno_grainfill [max_table];

      int   num_sw_demand_ratio;
      float x_sw_demand_ratio [max_table];
      float y_swdef_leaf [max_table];

      int   num_sw_avail_fix;
      float x_sw_avail_fix [max_table];
      float y_swdef_fix [max_table];

      float oxdef_photo [max_table];
      float oxdef_photo_rtfr[max_table];
      int   num_oxdef_photo;

}; //SWStress

class TempStress {
public:
   TempStress(ScienceAPI& scienceAPI,PlantComponent *p);
   ~TempStress(void);
   void init(void);
   void doPlant_temp_stress (environment_t& Environment);
   void read_t_constants (void);
   void get_tstress_photo(protocol::Component *, protocol::QueryValueData &);

   StressDeficit tFact;

private:
   ScienceAPI& scienceAPI;

   PlantComponent *parent;                 // The plant we are attached to

   struct {
      float x_ave_temp[max_table];                      // critical temperatures for
                                                        // photosynthesis (oC)
      float y_stress_photo[max_table];                  // Factors for critical temperatures
                                                        // (0-1)
      int   num_ave_temp;                               // size_of critical temperature table
      int   num_factors;                                // size_of table
   }  c;   // Constants

}; //TempStress

class NStress {
public:
   NStress(ScienceAPI& scienceAPI,PlantComponent *p);
   ~NStress(void);
   void init(void);
   void plant_nit_stress (plantPart* leafPart, plantPart* stemPart);
   float critNFactor(vector< plantPart *> &, float );
   void read_n_constants (void);
   void get_nfact_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed_pot(protocol::Component *, protocol::QueryValueData &);
   void get_dlt_n_fixed(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nfact_grain(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_photo(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_pheno(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_expan(protocol::Component *, protocol::QueryValueData &);
   void get_nstress_grain(protocol::Component *, protocol::QueryValueData &);

   StressDeficit nFact;

private:
   ScienceAPI& scienceAPI;

   PlantComponent *parent;                 // The plant we are attached to
   struct {
      StressDeficit nFact;
      int   n_stress_option;
   }  c;   // Constants

}; //NStress

class PStress {
public:
   PStress(ScienceAPI& scienceAPI,PlantComponent *p);
   ~PStress(void);
   void init(void);
   bool isPhosphorusAware(void);
   void zero_p_variables (void);
   void  PlantP_Stress (vector<plantPart *>&);
   void read_p_constants (void);
   void get_pfact_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_expan(protocol::Component *, protocol::QueryValueData &qd);
   void get_pfact_grain(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_photo(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_pheno(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_expansion(protocol::Component *, protocol::QueryValueData &qd);
   void get_pstress_grain(protocol::Component *, protocol::QueryValueData &qd);

   StressDeficit pFact;

private:
   ScienceAPI& scienceAPI;
   void PlantP_set_phosphorus_aware (void);
   float PlantP_Pfact (vector<plantPart *>&);

   PlantComponent *parent;                 // The plant we are attached to
   bool  phosphorus_aware;
   struct {
      StressDeficit pFactSlope;
   }  c;   // Constants

}; //PStress

#endif //PlantStress_H_

