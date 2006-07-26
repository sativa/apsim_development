using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using VBGeneral;
using CSGeneral;


namespace APSRU.Model.Howwet
    {
    public class SimulationIn
        {
        private APSIMData myData;
        private String fileName;
        private double soilWaterCapacity=0;
        private Soil newSoil;
      
        public SimulationIn(APSIMData data)
            {
            myData = new APSIMData();
            myData = data;
            }
        public APSIMData Data
            {
            get { return myData; }
            }
        public void AddSoil(APSIMData selectedSoil)
            {
            try
                {
                APSIMData paddockNode = myData.FindChild("HowWet|paddock", '|');
                StringCollection soils = paddockNode.ChildList("Soil");

                if (!(soils.Count == 0))
                    {
                    String soil = soils[0];
                    APSIMData soilNode = paddockNode.Child(soil);
                    paddockNode.Delete(soilNode.Name);
                    paddockNode.Add(selectedSoil);
                    }
                else
                    {
                    paddockNode.Add(selectedSoil);
                    }
                newSoil = new Soil(selectedSoil);
                //sum water layers
                foreach (double layer in newSoil.PAWC())
                    {
                    soilWaterCapacity = soilWaterCapacity + layer;
                    }
                }
            catch (Exception e)
                {
                }
            }

        public Soil Soil
            {
            get { return newSoil; }
            }

        public String FileName
            {
            set{fileName = value;}
            get{return fileName;}
            }

        public double SoilWaterCapacity
            {
            get { return soilWaterCapacity; }
            }

        public String StartDate
            {
            set
                {
                APSIMData clockNode = myData.FindChild("HowWet|clock", '|');
                clockNode.set_ChildValue("start_date", value);
                }
            get
                {
                APSIMData clockNode = myData.FindChild("HowWet|clock", '|');
                return clockNode.get_ChildValue("start_date"); 
                }
            }

        public String EndDate
            {
            set
                {
                APSIMData clockNode = myData.FindChild("HowWet|clock", '|');
                clockNode.set_ChildValue("end_date", value);
                }
            get
                {
                APSIMData clockNode = myData.FindChild("HowWet|clock", '|');
                return clockNode.get_ChildValue("end_date"); 
                }
            }

        public String ErosionSlope
            {
            set
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                erosionNode.set_ChildValue("slope", value);
                }
            get {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                return erosionNode.get_ChildValue("slope");
                }
            }

        public String ErosionSlopeLength
            {
            set
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                erosionNode.set_ChildValue("slope_length", value);
                }
            get
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                return erosionNode.get_ChildValue("slope_length");
                }
               
            }

        public String ErosionErodibilty
            {
            set
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                erosionNode.set_ChildValue("k_factor", value);
                }
            get {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                return erosionNode.get_ChildValue("k_factor");
                }
            }

        public String ErosionBedDepth
            {
            set
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                erosionNode.set_ChildValue("bed_depth", value);
                }
            get
                {
                APSIMData erosionNode = myData.FindChild("HowWet|paddock|erosion", '|');
                return erosionNode.get_ChildValue("bed_depth");
                }
            }

        public String SOMType
            {
            set
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                somNode.set_ChildValue("type", value);
                }
            get
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                return somNode.get_ChildValue("type");
                }
            }

        public String SOMMass
            {
            set
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                somNode.set_ChildValue("mass", value);
                }
            get
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                return somNode.get_ChildValue("mass");
                }
            }

        public String SOMCNRatio
            {
            set
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                somNode.set_ChildValue("cnr", value);
                }
            get
                {
                APSIMData somNode = myData.FindChild("HowWet|paddock|surface organic matter", '|');
                return somNode.get_ChildValue("cnr");
                }
            }

        public String SummaryFileName
            {
            set
                {
                APSIMData summaryFileNode = myData.FindChild("HowWet|summaryfile", '|');
                summaryFileNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData summaryFileNode = myData.FindChild("HowWet|summaryfile", '|');
                return summaryFileNode.get_ChildValue("filename");
                }
            }

        public String OutputFileName
            {
            set
                {
                APSIMData outputFileNode = myData.FindChild("HowWet|paddock|outputfile", '|');
                outputFileNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData outputFileNode = myData.FindChild("HowWet|paddock|outputfile", '|');
                return outputFileNode.get_ChildValue("filename");
                }
            }

        public String MetFileName
            {
            set
                {
                APSIMData metNode = myData.FindChild("HowWet|met", '|');
                metNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData metNode = myData.FindChild("HowWet|met", '|');
                return metNode.get_ChildValue("filename");
                }
            }
        
        }
    }
