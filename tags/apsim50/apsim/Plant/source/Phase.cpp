#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "Phase.h"

bool operator == (const pPhase &a, const pPhase &b)
// ===================================================================================
   {
   return (a.name() == b.name());
   };

void pPhase::add(float dlt_days, float dlt_tt, float *balance_days, float *balance_tt)
// ===================================================================================
// Add tt and days to the accumulator. Return a balance if too much.
// A target of 0.0 or less indicates it's a stage that doesn't use
// thermal time accumulation. So avoid taking any tt if you can avoid it..
   {
   if (target > 0.0)
      {
      if ((tt + dlt_tt) > target)
         {
         // only take a enough to fill, and don't take any out..
         float tt_in_old = max(0.0, target - tt);
         float days_in_old = dlt_days * divide(tt_in_old, dlt_tt, 0.0);

         tt += tt_in_old;
         days += days_in_old;
         *balance_tt = dlt_tt - tt_in_old;
         *balance_days = dlt_days - days_in_old;
         }
      else
         {
         // take it all
         tt += dlt_tt;
         days += dlt_days;
         *balance_tt = 0.0; *balance_days = 0.0;
         }
      }
   else
      {
      // no target - don't take any.
      *balance_tt = dlt_tt;
      *balance_days = dlt_days;
      }
   }
