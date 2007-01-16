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
    // This class reads in the output file and builds a datatable in the format required
    public class SimulationOut
        {
        private DataTable output;
        public SimulationOut(SimulationIn simulationIn)
            {
            String errString = "";
            const String FUNCTION_NAME = "SimulationOut";
            try
                {
                errString = "reading file";
                APSIMInputFile outputData = new APSIMInputFile();
                outputData.ReadFromFile(simulationIn.OutputFileName);
                output = outputData.Data;
                //add new column to datatable
                output.Columns.Add("SoilWaterLayers" ,typeof(double[])); 
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
                    Double[] soilWaterLayersTmp=new double[numLayers];
                    for (int layer = 1; layer <= numLayers; layer++)
                        {
                        soilWaterLayersTmp[layer-1]=(Convert.ToDouble(row["SoilWater(" + layer + ")"]));
                        }
                    row["SoilWaterLayers"] = soilWaterLayersTmp;
                    }
                //add three new columns to datatable for 
                output.Columns.Add("SoilLossCum", typeof(double));
                output.Columns.Add("RunoffCum", typeof(double));
                output.Columns.Add("SoilWater", typeof(double));
                double runoffCum = 0;
                double soilLossCum = 0;

                foreach (DataRow row in output.Rows)
                    {
                    runoffCum = runoffCum + Convert.ToDouble(row["Runoff"]);
                    row["RunoffCum"] = runoffCum;
                    soilLossCum = soilLossCum + Convert.ToDouble(row["SoilLoss"]);
                    row["SoilLossCum"] = soilLossCum;
                    double[] tmpSoilWaterLayers = (double[])row["SoilWaterLayers"];
                    double sw = 0;
                    for (int layer = 0; layer < tmpSoilWaterLayers.Length; layer++)
                        {
                        sw = sw + (simulationIn.Soil.Thickness[layer] * (Math.Max((tmpSoilWaterLayers[layer] - simulationIn.Soil.LL(simulationIn.GetCrop)[layer]),0.0)));
                    //    Console.WriteLine("Layer=" + layer + " SoilWater=" + tmpSoilWaterLayers[layer] + " " + simulationIn.GetCrop + " ll=" + simulationIn.Soil.LL(simulationIn.GetCrop)[layer]+" PAW="+(simulationIn.Soil.Thickness[layer] * (Math.Abs(tmpSoilWaterLayers[layer] - simulationIn.Soil.LL(simulationIn.GetCrop)[layer]))));
                        }
                    row["SoilWater"] = sw;
                   // Console.WriteLine("Total PAW="+sw);
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
        }
    }
