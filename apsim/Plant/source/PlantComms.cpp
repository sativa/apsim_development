//---------------------------------------------------------------------------
#include <string>
#include <math.h>
#include "Plantlibrary.h"


//============================================================================
extern "C" void _stdcall _export crop_root_incorp (float *dlt_dm_root,
      float *dlt_N_root, float *g_dlayer, float *g_root_length, float *g_root_depth,
      const char *c_crop_type, const int *max_layer)
//============================================================================

/*  Purpose
*       Calculate and provide root matter incorporation information
*       to the APSIM messaging system.
*
*  Mission Statement
*   Pass root material to the soil modules (based on root length distribution)
*
*  Changes
*     <insert here>
*     280800 jngh changed literal incorp_fom to ACTION_incorp_fom
*     011100 dph  added event_interface as a parameter.
*
*  Constant Values
*     character  my_name*(*)           ! name of procedure
*     parameter (my_name  = 'crop_root_incorp')
*
*  Sub-Program Arguments
*      float *dlt_dm_root              ! (INPUT) new root residue dm (g/m^2)
*      float *dlt_N_root               ! (INPUT) new root residue N (g/m^2)
*      float *g_dlayer(*)              ! (INPUT) layer thicknesses (mm)
*      float *g_root_length(*)         ! (INPUT) layered root length (mm)
*      float *g_root_depth             ! (INPUT) root depth (mm)
*      char *c_crop_type*(*)           ! (INPUT) crop type
*      int *max_layer                  ! (INPUT) maximum no of soil layers
*
*/
   {   
   fatal_error (&err_internal, "Crop_root_incorp NYI");
#if 0
   //  Local Variables
   int deepest_layer;         // deepest layer in which the roots are
                             // growing
   float *dlt_dm_incorp = new float[crop_max_layer]; // root residue (kg/ha)
   float *dlt_N_incorp = new float[crop_max_layer];  // root residue N (kg/ha)
   float first;         //Temp vars to store composite argument for functions
   float second;        //that are expecting pointers
   // Implementation Section ----------------------------------
   if (*max_layer > crop_max_layer)
      {
      fatal_error (&err_internal, "Too many layers for crop routines");
      }
   else
      {
      if (*dlt_dm_root > 0.0)
         {
         // send out root residue

         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_dm_incorp,
                  dlt_dm_root * gm2kg /sm2ha, max_layer);

         first = *dlt_dm_root * gm2kg / sm2ha;
         bound_check_real_array(dlt_dm_incorp, &zero, &first,
               "dlt_dm_incorp", max_layer);

         first = *dlt_N_root * gm2kg /sm2ha;
         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_N_incorp,
               &first, max_layer);

         first = dlt_n_root * gm2kg / sm2ha;
         bound_check_real_array(dlt_n_incorp, &zero, &first, "dlt_n_incorp",
               max_layer);

         deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer);

         new_postbox ();

         post_char_var("dlt_fom_type=","()",c_crop_type);

         post_real_array ("dlt_fom_wt", "(kg/ha)", dlt_dm_incorp, deepest_layer);

         post_real_array ("dlt_fom_n", "(kg/ha)", dlt_n_incorp, deepest_layer);

         event_send(&ACTION_Incorp_Fom);

         delete_postbox ();
         }

      else
         {
            // no roots to incorporate
         }
      }
#endif      
   return;
  }

//==========================================================================
extern "C" void _stdcall _export crop_top_residue (char *c_crop_type,
         float *dlt_residue_weight, float *dlt_residue_n)
//==========================================================================

/*  Purpose
*       Add residue to residue pool
*
*  Mission Statement
*   Pass surface residues to the residue module
*
*  Changes
*     <insert here>
*     280800 jngh changed ok = Loader_SendActionToFirstComp
*                    to call   Loader_SendActionToAllComps (D372)
*     280800 jngh changed literal add_residue to ACTION_add_residue
*     011100 dph  added event_interface as a parameter.
*
*  Constant Values
*      character  my_name*(*)           ! name of procedure
*      parameter (my_name  = 'crop_top_residue')
*
*  Sub-Program Arguments
*      char *c_crop_type*(*)
*      float *dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
*      float *dlt_residue_n         ! (INPUT) new surface residue N (g/m^2)
*
*/
   {  /*
   //  Local Variables
   bool ok;
   float temp;
   // Implementation Section ----------------------------------
   if (*dlt_residue_weight  > 0.0)
      {         // send out surface residue

      new_postbox ();

      post_char_var("dlt_residue_type","()",c_crop_type);

      temp = *dlt_residue_weight * gm2kg / sm2ha;
      post_real_var ("dlt_residue_wt", "(kg/ha)", &temp);

      temp = *dlt_residue_n * gm2kg / sm2ha;
      post_real_var ("dlt_residue_n", "(kg/ha)" , &temp);

      event_send(&ACTION_Add_Residue);

      delete_postbox ();
      }

   else
      {
         // no surface residue
      }
   return;
  */ }

#if 0
//===========================================================================
extern "C" bool _stdcall _export crop_my_type (char *c_crop_type)
//===========================================================================

/*  Purpose
*       Returns true if 'type' is equal to the crop type or is absent.
*
*  Mission Statement
*   the crop type = %1
*
*  Assumptions
*       If type is not specified, it is assumed the message was addressed
*        directly to the module.
*
*  Changes
*     <insert here>
*
*  Constant Values
*      character  my_name*(*)           ! name of procedure
*      parameter (my_name = 'crop_my_type')
*
*  Sub-Program Arguments
*      char *c_crop_type*(*)
*/
   {/*
   //  Local Variables
   char crop_type[50];          // crop type in data string
   int numvals;               // number of values returned

   // Implementation Section ----------------------------------
   collect_char_var_optional ("type", "()", crop_type, &numvals)

   if ((strcmp(crop_type,c_crop_type) == 0) || numvals == 0)
      {
      return true;
      }
   return false;
   */}
#endif

//==========================================================================
void crop_get_ext_uptakes (const char *uptake_source,
                           const char *crop_type, const char *uptake_type, float *unit_conversion_factor,
                           float *uptake_lbound, float *uptake_ubound, float *uptake_array,
                           const int *max_layer)
//==========================================================================

/*  Purpose
*     Ask swim for uptakes of water or solute
*
*  Mission Statement
*   Get the soil uptake for %3 from another module
*
*  Notes
*      Bounds should probably be passed in when crops decide what
*      these should be (ie when ini files have limits for uptake
*      in them)
*
*  Changes
*     08-05-1997 - huth - Programmed and Specified
*     20/5/2003 ad converted to BC++
*  Calls
*
*
*  Constant Values
*      character*(*) myname               ! name of current procedure
*      parameter (myname = 'crop_get_ext_uptakes')
*
*  Sub-Program Arguments
*      char *uptake_source          !(INPUT) uptake flag
*      char *crop_type              !(INPUT) crop type name
*      char *uptake_type            !(INPUT) uptake name
*      float *unit_conversion_factor !(INPUT) unit conversion factor
*      float *uptake_lbound          !(INPUT) uptake lower limit
*      float *uptake_ubound          !(INPUT) uptake upper limit
*      float *uptake_array          !(OUTPUT) crop uptake array
*      int *max_layer                !(INPUT) max layer number
*
*/
   { 
   fatal_error (&err_internal, "crop_get_ext_uptakes NYI");
#if 0
   //  Local Variables
   int layer;                        // layer counter
   int num_uptakes;                  // num uptake vals
   Strin uptake_name;  // Uptake variable name
   String Source(uptake_source);
   String Type(crop_type);
   // Implementation Section ----------------------------------
   if ((Source == "apsim") && (Type != " "))
      {
         // NB - if crop type is blank then swim will know nothing
         // about this crop (eg if not initialised yet)

      uptake_name.sprintf("uptake_%s_%s",uptake_type, crop_type);
      get_real_array (&Unknown_module, uptake_name.c_str(), max_layer, "()",
               uptake_array, num_uptakes, uptake_lbound, uptake_ubound);

      for(layer = 0; layer < *num_uptakes; layer++)
         {
         uptake_array[layer] = uptake_array[layer] * *unit_conversion_factor;
         }
      }
   else
      {
      }
   return;
#endif   
   }

