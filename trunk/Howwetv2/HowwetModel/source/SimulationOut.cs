using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using VBGeneral;
using CSGeneral;

namespace APSRU.Model.Howwet
    {
    public class SimulationOut
        {
        private DataTable output;
        private int rainfall,evaporation,runoff,soilLoss,nitrate,soilWater;

        public SimulationOut(String fileName)
            {
            APSIMInputFile outputData = new APSIMInputFile();
            outputData.ReadFromFile(fileName);
            output = outputData.Data;
            DataRow lastRow = (DataRow)output.Rows[output.Rows.Count - 1];
            //get the last 
            nitrate = nitrate + Convert.ToInt16(lastRow["NO3Total"]);
            soilWater = soilWater + Convert.ToInt16(lastRow["ExtractableSoilWater"]);
            //sum the required fields
            foreach (DataRow row in output.Rows)
                {
                rainfall =rainfall+Convert.ToInt16(row["Rainfall"]);
                evaporation = evaporation + Convert.ToInt16(row["Evapoation"]);
                runoff = runoff + Convert.ToInt16(row["Runoff"]);
                soilLoss = soilLoss + Convert.ToInt16(row["SoilLoss"]);
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
