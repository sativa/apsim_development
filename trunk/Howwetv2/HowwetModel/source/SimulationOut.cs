using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Data;
using VBGeneral;
using CSGeneral;
using APSRU.Error;

namespace APSRU.Model.Howwet
    {
    public class SimulationOut
        {
        private DataTable output;
        private double rainfall, evaporation, runoff, soilLoss,drain, nitrateEnd;
        private ArrayList soilWaterEndByLayer = new ArrayList();
        private ArrayList soilWaterStartByLayer = new ArrayList();

        public SimulationOut(String fileName)
            {
            String errString = "";
            const String FUNCTION_NAME = "SimulationOut";
            try
                {
                APSIMInputFile outputData = new APSIMInputFile();
                errString = "reading file";
                outputData.ReadFromFile(fileName);
                output = outputData.Data;
                //add new column to datatable
                output.Columns.Add("SoilWaterLayers" ,typeof(ArrayList)); 
                
                //how many soilwater layers are there
                int numLayers=0;
                foreach(DataColumn col in output.Columns)
                    {
                    int index = col.ColumnName.IndexOf("SoilWater(");
                    if (index == 0)
                        {
                        numLayers++;
                        }
                    }
                //re arrange soilwater columns in to a single column of arraylist                
                foreach (DataRow row in output.Rows)
                    {
                    ArrayList soilWaterLayersTmp =new ArrayList();
                    for (int layer = 1; layer <= numLayers; layer++)
                        {
                        soilWaterLayersTmp.Add(Convert.ToDouble(row["SoilWater(" + layer + ")"]));
                        }
                    row["SoilWaterLayers"] = soilWaterLayersTmp;
                    }
                
                //add two new columns to datatable for 
                output.Columns.Add("SoilLossCum", typeof(double));
                output.Columns.Add("RunoffCum", typeof(double));
                double runoffCum = 0;
                double soilLossCum = 0;

                foreach (DataRow row in output.Rows)
                    {
                    runoffCum = runoffCum + Convert.ToDouble(row["Runoff"]);
                    row["RunoffCum"] = runoffCum;
                    soilLossCum = soilLossCum + Convert.ToDouble(row["SoilLoss"]);
                    row["SoilLossCum"] = soilLossCum;
                    }

                //convert soilwater on the top layer to mm of water


                errString = "summing the fields";
                foreach (DataRow row in output.Rows)
                    {
                    rainfall = rainfall + Convert.ToDouble(row["Rainfall"]);
                    evaporation = evaporation + Convert.ToDouble(row["Evapoation"]);
                    runoff = runoff + Convert.ToDouble(row["Runoff"]);
                    soilLoss = soilLoss + Convert.ToDouble(row["SoilLoss"]);
                    drain = drain + Convert.ToDouble(row["Drain"]);
                    }
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Problem reading Howwet output file", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }
        public DataTable Data
            {
            get { return output; }
            }
        public double RainfallTotal
            {
            get {return rainfall;}
            }
        public double EvaporationTotal
            {
            get { return evaporation; }
            }
        public double RunoffTotal
            {
            get { return runoff; }
            }
        public double SoilLossTotal
            {
            get { return soilLoss; }
            }
        public double DrainTotal
            {
            get { return drain; }
            }
        public double NitrateEnd
            {
            get 
                {
                DataRow lastRow = (DataRow)output.Rows[output.Rows.Count - 1];
                nitrateEnd = Convert.ToDouble(lastRow["NO3Total"]); 
                return nitrateEnd;
                }
            }
        public ArrayList SoilWaterEndByLayer
            {
            get
                {
                DataRow lastRow = (DataRow)output.Rows[output.Rows.Count - 1];
                ArrayList soilWaterEndByLayer = (ArrayList)lastRow["SoilWaterLayers"];
                return soilWaterEndByLayer;
                }
            }
        public double SoilWaterEnd(Soil soil)
            {
            DataRow lastRow = (DataRow)output.Rows[output.Rows.Count - 1];
            ArrayList soilWaterEndByLayer = (ArrayList)lastRow["SoilWaterLayers"];
            double soilWaterEnd = 0;
            for (int layer = 0; layer < soilWaterEndByLayer.Count; layer++)
                {
                soilWaterEnd = soilWaterEnd + (Math.Abs(Convert.ToDouble(soilWaterEndByLayer[layer]) - soil.LL15[layer]) * soil.Thickness[layer]); 
                }
            return soilWaterEnd;
            }

        public double SoilWaterStart(Soil soil)
            {
            DataRow firstRow = (DataRow)output.Rows[0];
            ArrayList soilWaterStartByLayer = (ArrayList)firstRow["SoilWaterLayers"];
            double soilWaterStart = 0;
            for (int layer = 0; layer < soilWaterStartByLayer.Count; layer++)
                {
                soilWaterStart = soilWaterStart + (Math.Abs(Convert.ToDouble(soilWaterStartByLayer[layer]) - soil.LL15[layer]) * soil.Thickness[layer]); 
                }
            return soilWaterStart;
            }
        }
    }
