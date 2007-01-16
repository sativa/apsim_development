using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Data;
using CSGeneral;


namespace APSRU.Model.Howwet
    {

    //This class encapsluates the data manipulation activities required 
    //for the UI to display summary results and build charts given the simulation output object  
    public class Results
        {
        public String simulationFileName, soilName, metFileName;
        public String startDate, endDate;
        public double oc;
        public double soilWaterStart, soilWaterEnd;
        public String slope, slopeLength, erodibility;
        public double rainfall, evaporation, runoff, soilLoss, drain;
        public double startCover, endCover;
        public String coverType;
        public double fallowWaterGain, fallowWaterEfficiency;
        public double nitrateStart, nitrateEnd, nitrateGain;
        public double[] soilWaterEndByLayer;
        public double[] thickness;
        public String nitrogenRequirementCropType;
        public double nitrogenRequirementPAW, nitrogenRequirementYield, nitrogenRequirementDemand, nitrogenRequirementGap;
        public String softwareVersion, reportDate;

        public void loadResults(SimulationIn dataIn,SimulationOut dataOut)
            {
            String errString = "";
            //const String FUNCTION_NAME = "SimulationOut";
            //Simulation Name
            simulationFileName = dataIn.FileName;
            //Soil Name
            soilName = dataIn.Soil.Name;
            //Met File
            metFileName = dataIn.MetFileName;
            //Start Date
            startDate = dataIn.StartDate;
            //End Date
            endDate = dataIn.EndDate;
            //Oc
            oc = dataIn.Soil.OC[0];
            //Erosion
            slope = dataIn.ErosionSlope;
            slopeLength = dataIn.ErosionSlopeLength;
            erodibility = dataIn.ErosionErodibilty;

            reportDate=DateTime.Today.Date + "/" + DateTime.Today.Month + "/" + DateTime.Today.Year;

            thickness = dataIn.Soil.Thickness;
            errString = "summing the fields";
            foreach (DataRow row in dataOut.Data.Rows)
                {
                rainfall = rainfall + Convert.ToDouble(row["Rainfall"]);
                evaporation = evaporation + Convert.ToDouble(row["Evapoation"]);
                runoff = runoff + Convert.ToDouble(row["Runoff"]);
                soilLoss = soilLoss + Convert.ToDouble(row["SoilLoss"]);
                drain = drain + Convert.ToDouble(row["Drain"]);
                }
            rainfall =Math.Round(rainfall);
            evaporation = Math.Round(evaporation);
            runoff = Math.Round(runoff);
            soilLoss = Math.Round(soilLoss);
            drain = Math.Round(drain);

            //nitrate start
            nitrateStart = MathUtility.Sum(dataIn.Soil.InitialNitrogen.NO3KgHa);
            //Nitrate End
            DataRow lastRow = (DataRow)dataOut.Data.Rows[dataOut.Data.Rows.Count - 1];
            nitrateEnd = Convert.ToDouble(lastRow["NO3Total"]);
            nitrateGain = nitrateEnd - nitrateStart;

            nitrateStart = Math.Round(nitrateStart);
            nitrateEnd = Math.Round(nitrateEnd);
            nitrateGain = Math.Round(nitrateGain);
            
            //SoilWater Start
            for (int layer = 0; layer < dataIn.Soil.InitialWater.SW.Length - 1; layer++)
                {
                soilWaterStart = soilWaterStart + (Math.Max((dataIn.Soil.InitialWater.SW[layer] - dataIn.Soil.LL(dataIn.GetCrop)[layer]),0.0) * dataIn.Soil.Thickness[layer]);
                }
            //SoilWater End
            soilWaterEndByLayer = (double[])lastRow["SoilWaterLayers"];
            for (int layer = 0; layer < soilWaterEndByLayer.Length; layer++)
                {
                soilWaterEnd = soilWaterEnd + (Math.Max((Convert.ToDouble(soilWaterEndByLayer[layer]) - dataIn.Soil.LL(dataIn.GetCrop)[layer]),0.0) * dataIn.Soil.Thickness[layer]);
                }
            //fallow water gain
            fallowWaterGain = soilWaterEnd - soilWaterStart;
            fallowWaterEfficiency = Math.Round((fallowWaterGain / rainfall) * 100);
            soilWaterStart = Math.Round(soilWaterStart);
            soilWaterEnd = Math.Round(soilWaterEnd);
            fallowWaterGain = Math.Round(fallowWaterGain);
            //Cover
            startCover = Math.Round(Convert.ToDouble(dataIn.SOMMass));
            endCover = Math.Round(Convert.ToDouble(lastRow["SurfaceOrganicMatter"]));
            coverType = dataIn.SOMType;
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

        public double calcYield(double inCropRainfall,double thresholdWater,double wue)
            {
            nitrogenRequirementYield = 0;
            double totalWater = (nitrogenRequirementPAW + inCropRainfall) - thresholdWater;
            nitrogenRequirementYield = (totalWater * wue) / 1000;
            return nitrogenRequirementYield;
            }

        public double calcPAWEnd(double[] cLL)
            {
            nitrogenRequirementPAW = 0;
            for (int layer = soilWaterEndByLayer.Length-1; layer >0; layer--)
                {
                double diffference=(Convert.ToDouble(soilWaterEndByLayer[layer]) - cLL[layer]);
                if(diffference>=0)
                    {
                    nitrogenRequirementPAW = nitrogenRequirementPAW + (diffference* thickness[layer]);
                    }
                }
            return Math.Round(nitrogenRequirementPAW);
            }

        public String SoftwareVersion
            {
            set{softwareVersion=value;}
            }
        }
    }
