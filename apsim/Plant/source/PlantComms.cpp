#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "Plantlibrary.h"

//==========================================================================
#if 0
void crop_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag             
                           const char *crop_type,            //(INPUT) crop type name          
                           const char *uptake_type,          //(INPUT) uptake name             
                           float unit_conversion_factor,     //(INPUT) unit conversion factor  
                           float uptake_lbound,              //(INPUT) uptake lower limit      
                           float uptake_ubound,              //(INPUT) uptake upper limit      
                           float *uptake_array,              //(OUTPUT) crop uptake array      
                           int max_layer)                    //(INPUT) max layer number        

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
*/
   {
   char uptake_name[80];             // Uptake variable name
   unsigned  int id;
   protocol::vector<float> values;   // Scratch area

   if (strcmp(uptake_source, "apsim") == 0 && *crop_type != '\0')
      {
      // NB - if crop type is blank then swim will know nothing
      // about this crop (eg if not initialised yet)

      sprintf(uptake_name, "uptake_%s_%s", uptake_type, crop_type);

      id = parent->addRegistration(protocol::getVariableReg,
                                   uptake_name, floatArrayType,
                                   "", "");
                                   
      parent->getVariable(id, values, uptake_lbound, uptake_ubound);

      for (unsigned int i=0; i< values.size(); i++) {
        uptake_array[i] = values[i] * unit_conversion_factor;
      }

   }
#endif