#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "PlantLibrary.h"

//===========================================================================
void crop_temperature_stress_photo (int   num_ave_temp ,     // (INPUT)  size_of critical temperature table                               
                                    float *x_ave_temp,        // (INPUT)  critical temperatures for photosynthesis (oC)                    
                                    float *y_stress_photo,    // (INPUT)  Factors for critical temperatures (0-1)                          
                                    float maxt,              // (INPUT)  maximum air temperature (oC)                                     
                                    float mint,              // (INPUT)  minimum air temperature (oC)                                     
                                    float *temp_stress_photo) // (OUTPUT) photosynthetic reduction factor for  temperature stress (0-1)    
//===========================================================================

/*  Purpose
*     photosynthetic reduction factor for temperature stress (0-1)
*
*   Mission Statement
*     Calculate the temperature factor for photosynthesis
*
*/

   {
   //  Local Variables
   float ave_temp;         //mean temperature for the day (oC)
   // Implementation Section ----------------------------------

   // now get the temperature stress factor that reduces
   // photosynthesis (0-1)
   ave_temp = (maxt + mint) / 2.0;
   *temp_stress_photo = linear_interp_real (ave_temp, x_ave_temp, y_stress_photo,
                                            num_ave_temp);
   *temp_stress_photo = bound (*temp_stress_photo, 0.0, 1.0);
   }
