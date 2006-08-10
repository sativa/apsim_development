using System;
using System.Collections.Generic;
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
        private int rainfall,evaporation,runoff,soilLoss,nitrate,soilWater;
        

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
                DataRow lastRow = (DataRow)output.Rows[output.Rows.Count - 1];
                //get the last 
                nitrate = nitrate + Convert.ToInt16(lastRow["NO3Total"]);
                soilWater = soilWater + Convert.ToInt16(lastRow["ExtractableSoilWater"]);
               
                errString = "summing the fields";
                foreach (DataRow row in output.Rows)
                    {
                    rainfall = rainfall + Convert.ToInt16(row["Rainfall"]);
                    evaporation = evaporation + Convert.ToInt16(row["Evapoation"]);
                    runoff = runoff + Convert.ToInt16(row["Runoff"]);
                    soilLoss = soilLoss + Convert.ToInt16(row["SoilLoss"]);
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
        public int Rainfall
            {
            get {return rainfall;}
            }
        public int Evaporation
            {
            get { return evaporation; }
            }
        public int Runoff
            {
            get { return runoff; }
            }
        public int SoilLoss
            {
            get { return soilLoss; }
            }
        public int Nitrate
            {
            get { return nitrate; }
            }
        public int SoilWater
            {
            get { return soilWater; }
            }
        }
    }
