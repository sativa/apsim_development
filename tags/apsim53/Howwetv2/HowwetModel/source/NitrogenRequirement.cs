using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Data;
using CSGeneral;

namespace APSRU.Model.Howwet
    {
    public class NitrogenRequirement
        {
        private double nitrogenRequirementPAW, nitrogenRequirementYield, nitrogenRequirementDemand, nitrogenRequirementGap;
        private double nitrateEnd;
        private double[] soilWaterEndByLayer;
        private double[] thickness;
        private CropNDemand crop;

        public NitrogenRequirement(SimulationIn dataIn, SimulationOut dataOut, CropNDemand crop)
            {
            this.crop = crop;
            DataRow lastRow = (DataRow)dataOut.Data.Rows[dataOut.Data.Rows.Count - 1];
            nitrateEnd = Convert.ToDouble(lastRow["NO3Total"]);
            soilWaterEndByLayer = (double[])lastRow["SoilWaterLayers"];
            thickness = dataIn.Soil.Thickness;
            }

        public double calcNitrogenGap()
            {
            nitrogenRequirementGap = 0;
            nitrogenRequirementGap = nitrogenRequirementDemand - nitrateEnd;
            return nitrogenRequirementGap;
            }

        public double calcNitrogenDemand()
            {
            nitrogenRequirementDemand = 0;
            double grainProtein = crop.Protein_target;
            double efficiencyOfNUptake = crop.N_uptake_efficiency;
            double fractionOfNinProtein = (10 / crop.Fraction_of_n_in_protein);
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
