using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Model.Howwet
    {
    public class NitrogenRequirement
        {
        public NitrogenRequirement()
            {
            }

        public double calcNitrogenGap()
            {
            nitrogenRequirementGap = 0;
            nitrogenRequirementGap = this.nitrogenRequirementDemand - this.nitrateEnd;
            return nitrogenRequirementGap;
            }

        public double calcNitrogenDemand()
            {
            nitrogenRequirementDemand = 0;
            double grainProtein = 11.5;
            double efficiencyOfNUptake = 1.7;
            double fractionOfNinProtein = (10 / 5.7);
            nitrogenRequirementDemand = Math.Round((nitrogenRequirementYield * grainProtein * fractionOfNinProtein) * efficiencyOfNUptake);
            return nitrogenRequirementDemand;
            }

        public double calcYield(double inCropRainfall, double thresholdWater, double wue)
            {
            nitrogenRequirementYield = 0;
            double totalWater = (nitrogenRequirementPAW + inCropRainfall) - thresholdWater;
            nitrogenRequirementYield = (totalWater * wue) / 1000;
            return nitrogenRequirementYield;
            }

        public double calcPAWEnd(double[] cLL)
            {
            nitrogenRequirementPAW = 0;
            for (int layer = soilWaterEndByLayer.Length - 1; layer > 0; layer--)
                {
                double diffference = (Convert.ToDouble(soilWaterEndByLayer[layer]) - cLL[layer]);
                if (diffference >= 0)
                    {
                    nitrogenRequirementPAW = nitrogenRequirementPAW + (diffference * thickness[layer]);
                    }
                }
            return Math.Round(nitrogenRequirementPAW);
            }
        }
    }
