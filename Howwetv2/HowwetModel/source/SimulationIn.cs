using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Xml;
using VBGeneral;
using CSGeneral;
using APSRU.Error;


namespace APSRU.Model.Howwet
    {
    public class SimulationIn
        {
        private APSIMData myData;
        private String fileName;
        private String oldCrop="";
        
      
        public SimulationIn(APSIMData data)
            {
            myData = new APSIMData();
            myData = data;
            }
        public APSIMData Data
            {
            get { return myData; }
            //set { myData = value; }
            }
        public void AddSoil(APSIMData selectedSoil)
            {
            String errString = "";
            const String FUNCTION_NAME = "AddSoil";
            try
                {
                APSIMData paddockNode = myData.FindChildByType("simulation|area", '|');
                StringCollection soils = paddockNode.ChildList("Soil");

                if (!(soils.Count == 0))
                    {
                    errString = "removing old soil";
                    String soil = soils[0];
                    APSIMData soilNode = paddockNode.Child(soil);
                    paddockNode.Delete(soilNode.Name);
                    }
                errString = "adding new soil";
                paddockNode.Add(selectedSoil);
          //      newSoil = new Soil(selectedSoil);
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Problem adding soil", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }

        public void AddCrop(String newCrop)
            {
            String errString = "";
            const String FUNCTION_NAME = "AddCrop";
            try
                {
                APSIMData soilCropNode = myData.FindChildByType("simulation|area", '|');
                if(!(oldCrop==""))
                    {
                    errString = "removing old crop";
                    soilCropNode.Delete(oldCrop);
                    }
                errString = "adding new crop";
                APSIMData newNode=new APSIMData(newCrop,"");
                soilCropNode.Add(newNode);
                oldCrop = newCrop;
                
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Problem adding Corp", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }

        public Soil Soil
            {
            get
                {
                APSIMData paddockNode = myData.FindChildByType("simulation|area", '|');
                StringCollection soils = paddockNode.ChildList("Soil");
                Soil soil =null;
                if (!(soils.Count == 0))
                    {
                    APSIMData soilNode = paddockNode.Child(soils[0]);//first soil
                    soil = new Soil(soilNode);
                    }
                return soil;
                }
            }

        public String FileName
            {
            set{fileName = value;}
            get{return fileName;}
            }

        public String SimulationName
            {
            set
                {
                APSIMData simulationNode = myData.FindChildByType("simulation", '|');
                simulationNode.Name = value;
                }
            get
                {
                APSIMData simulationNode = myData.FindChildByType("simulation", '|');
                return simulationNode.Name;
                }
            }

        public String StartDate
            {
            set
                {
                APSIMData clockNode = myData.FindChildByType("simulation|clock", '|');
                clockNode.set_ChildValue("start_date", value);
                }
            get
                {
                APSIMData clockNode = myData.FindChildByType("simulation|clock", '|');
                return clockNode.get_ChildValue("start_date"); 
                }
            }
       
        public String EndDate
            {
            set
                {
                APSIMData clockNode = myData.FindChildByType("simulation|clock", '|');
                clockNode.set_ChildValue("end_date", value);
                }
            get
                {
                APSIMData clockNode = myData.FindChildByType("simulation|clock", '|');
                return clockNode.get_ChildValue("end_date"); 
                }
            }

        

        public String ErosionSlope
            {
            set
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                erosionNode.set_ChildValue("slope", value);
                }
            get {
            APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                return erosionNode.get_ChildValue("slope");
                }
            }

        public String ErosionSlopeLength
            {
            set
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                erosionNode.set_ChildValue("slope_length", value);
                }
            get
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                return erosionNode.get_ChildValue("slope_length");
                }
               
            }

        public String ErosionErodibilty
            {
            set
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                erosionNode.set_ChildValue("k_factor", value);
                }
            get {
            APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                return erosionNode.get_ChildValue("k_factor");
                }
            }

        public String ErosionBedDepth
            {
            set
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                erosionNode.set_ChildValue("bed_depth", value);
                }
            get
                {
                APSIMData erosionNode = myData.FindChildByType("simulation|area|erosion", '|');
                return erosionNode.get_ChildValue("bed_depth");
                }
            }

        public String SOMType
            {
            set
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                somNode.set_ChildValue("type", value);
                }
            get
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                return somNode.get_ChildValue("type");
                }
            }

        public String SOMMass
            {
            set
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                somNode.set_ChildValue("mass", value);
                }
            get
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                return somNode.get_ChildValue("mass");
                }
            }

        public String SOMCNRatio
            {
            set
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                somNode.set_ChildValue("cnr", value);
                }
            get
                {
                APSIMData somNode = myData.FindChildByType("simulation|area|surfaceom", '|');
                return somNode.get_ChildValue("cnr");
                }
            }

        public String SummaryFileName
            {
            set
                {
                APSIMData summaryFileNode = myData.FindChildByType("simulation|summaryfile", '|');
                summaryFileNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData summaryFileNode = myData.FindChildByType("simulation|summaryfile", '|');
                return summaryFileNode.get_ChildValue("filename");
                }
            }

        public String OutputFileName
            {
            set
                {
                APSIMData outputFileNode = myData.FindChildByType("simulation|area|outputfile", '|');
                outputFileNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData outputFileNode = myData.FindChildByType("simulation|area|outputfile", '|');
                return outputFileNode.get_ChildValue("filename");
                }
            }

        public String MetFileName
            {
            set
                {
                APSIMData metNode = myData.FindChildByType("simulation|metfile", '|');
                metNode.set_ChildValue("filename", value);
                }
            get
                {
                APSIMData metNode = myData.FindChildByType("simulation|metfile", '|');
                return metNode.get_ChildValue("filename");
                }
            }
        
        }
    }
