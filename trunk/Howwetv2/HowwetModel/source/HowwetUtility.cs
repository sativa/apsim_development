using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Data;
using VBGeneral;
using CSGeneral;
using System.Xml;
using APSRU.Error;

namespace APSRU.Model.Howwet
    {
    public class CoverCrop
        {
        private String name;
        private double specificArea;
        private int cnr;
        
        public String Name
            {
            set { name = value; }
            get{return name;}
            }
        public double SpecificArea
            {
            set { specificArea = value; }
            get{return specificArea;}
            }
        public int Cnr
            {
            set { cnr = value; }
            get{return cnr;}
            }
    }

    public class Region
        {
        private String name;
        private String description;
        private String[] averageMonthlyRain;
        private String[] averageMonthlyRadiation;
        private String[] averageMonthlyMaxT;
        private String[] averageMonthlyMinT;

        public String Name
            {
            set { name = value; }
            get { return name; }
            }
        public String Description
            {
            set { description = value; }
            get { return description; }
            }
        public String[] AverageMonthlyRain
            {
            set { averageMonthlyRain = value; }
            get { return averageMonthlyRain; }
            }
        public String[] AverageMonthlyRadiation
            {
            set { averageMonthlyRadiation = value; }
            get { return averageMonthlyRadiation; }
            }
        public String[] AverageMonthlyMaxT
            {
            set { averageMonthlyMaxT = value; }
            get { return averageMonthlyMaxT; }
            }
        public String[] AverageMonthlyMinT
            {
            set { averageMonthlyMinT = value; }
            get { return averageMonthlyMinT; }
            }
        }

    public class HowwetConfiguration
        {
        String version;
        bool debug;
        bool trainingMode;
        String templateFileName;
        String defaultSoilFileName;
        String defaultSoilName;
        String defaultMetfile;
        String defaultRegionName;
        ArrayList cropList;
        ArrayList regionList;
        XmlDocument setupDoc;
        XmlDocument regionsDoc;
        String setupFile;
        String regionsFile;
        

        public HowwetConfiguration(String setupFileName,String regionsFileName)
            {
            ReadSetupFile(setupFileName);
            ReadRegionsFile(regionsFileName);
            }

        public void ReadRegionsFile(String fileName)
            {
            String errString = "";
            try
                {
                regionsFile = fileName;
                errString = "reading " + fileName;
                regionsDoc = new XmlDocument();
                regionsDoc.Load(fileName);
                XmlElement element = regionsDoc.DocumentElement;
                                        
                //Region
                XmlNodeList regionNodeList = element.SelectNodes("region");
                IEnumerator regionNodeEnum = regionNodeList.GetEnumerator();
                ArrayList list = new ArrayList();
                while (regionNodeEnum.MoveNext())
                    {
                    Region region = new Region();
                    XmlNode regionNode = (XmlNode)regionNodeEnum.Current;
                    region.Name = regionNode.SelectSingleNode("name").InnerText;
                    region.Description = regionNode.SelectSingleNode("description").InnerText;
                    region.AverageMonthlyRain = regionNode.SelectSingleNode("average_monthly_rain").InnerText.Split(new Char[] { ',' });
                    region.AverageMonthlyRadiation = regionNode.SelectSingleNode("average_monthly_radiation").InnerText.Split(new Char[] { ',' });
                    region.AverageMonthlyMaxT = regionNode.SelectSingleNode("average_monthly_max_temperture").InnerText.Split(new Char[] { ',' });
                    region.AverageMonthlyMinT = regionNode.SelectSingleNode("average_monthly_min_temperture").InnerText.Split(new Char[] { ',' });
                    list.Add(region);
                    }
                this.regionList = list;
               
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Problem reading Howwet region file", errString + "\n Exception:" + e.ToString(), this.GetType().Name, this.GetType().FullName, true));
                }
            }

        public void ReadSetupFile(String fileName)
            {
            String errString = "";
            try
                {
                setupFile = fileName;
                errString = "reading " + fileName;
                setupDoc = new XmlDocument();
                setupDoc.Load(fileName);
                XmlElement element = setupDoc.DocumentElement;
                //version
                this.Version = element.SelectSingleNode("/howwet/version").InnerText;
                //debug
                this.debug = false;
                if (element.SelectSingleNode("/howwet/debug").InnerText == "true") this.debug = true;
                //training Mode
                this.trainingMode = false;
                if (element.SelectSingleNode("/howwet/training_mode").InnerText == "true") this.trainingMode = true;
                //Template file
                this.TemplateFileName = element.SelectSingleNode("/howwet/template_name").InnerText;
                //Default soil file
                this.DefaultSoilFileName = element.SelectSingleNode("/howwet/default_soil_file_name").InnerText;
                //Default soil 
                this.DefaultSoilName = element.SelectSingleNode("/howwet/default_soil").InnerText;
                //Default Metfile 
                this.DefaultMetfile = element.SelectSingleNode("/howwet/default_metfile").InnerText;
                //Default Region 
                this.DefaultRegionName = element.SelectSingleNode("/howwet/default_region").InnerText;
                //SurfaceOM
                XmlNodeList surfaceOMNodeList = element.SelectNodes("/howwet/surfaceOM/crop");
                IEnumerator surfaceOMNodeEnum = surfaceOMNodeList.GetEnumerator();
                ArrayList list = new ArrayList();
                while (surfaceOMNodeEnum.MoveNext())
                    {
                    CoverCrop newCrop = new CoverCrop();
                    XmlNode cropNode = (XmlNode)surfaceOMNodeEnum.Current;
                    newCrop.Name = cropNode.SelectSingleNode("name").InnerText;
                    newCrop.SpecificArea = Convert.ToDouble(cropNode.SelectSingleNode("specific_area").InnerText);
                    newCrop.Cnr = Convert.ToInt16(cropNode.SelectSingleNode("cnr").InnerText);
                    list.Add(newCrop);
                    }
                this.CropList = list;
                //RecentFiles
                //Other defaults
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Problem reading Howwet setup file", errString + "\n Exception:" + e.ToString(), this.GetType().Name, this.GetType().FullName, true));
                }
}

        public void SaveDefaults()
            {
            XmlElement element = setupDoc.DocumentElement;
            element.SelectSingleNode("/howwet/default_soil_file_name").InnerText = this.DefaultSoilFileName;
            element.SelectSingleNode("/howwet/default_soil").InnerText = this.DefaultSoilName;
            element.SelectSingleNode("/howwet/default_metfile").InnerText = this.DefaultMetfile;
            element.SelectSingleNode("/howwet/default_region").InnerText = this.DefaultRegionName;
            setupDoc.Save(setupFile);
            }

        public String Version
            {
            set { version = value; }
            get { return version; }
            }
        public bool Debug
            {
            set { debug = value; }
            get { return debug; }
            }
        public bool TrainingMode
            {
            set { trainingMode = value; }
            get { return trainingMode; }
            }
        public String TemplateFileName
            {
            set { templateFileName = value; }
            get { return templateFileName; }
            }
        public String DefaultSoilFileName
            {
            set { defaultSoilFileName = value; }
            get { return defaultSoilFileName; }
            }
        public String DefaultSoilName
            {
            set { defaultSoilName = value; }
            get { return defaultSoilName; }
            }
        public String DefaultMetfile
            {
            set { defaultMetfile = value; }
            get { return defaultMetfile; }
            }
        public String DefaultRegionName
            {
            set { defaultRegionName = value; }
            get { return defaultRegionName; }
            }
        public ArrayList CropList
            {
            set {cropList=value;}
            get {return cropList;}
            }
        public ArrayList RegionList
            {
            set { regionList = value; }
            get { return regionList; }
            }
        }

    public class HowwetUtility
        {
        private APSIMData apsimData=new APSIMData();
        private String applicationDirectory = "";

        public HowwetUtility(String appPath,String howwetSetupFileName)
            {
            String exeFolder = Path.GetDirectoryName(appPath);
            if (File.Exists(exeFolder + howwetSetupFileName))
                {
                applicationDirectory= exeFolder;
                }
            else
                {
                String newPath = Directory.GetParent(exeFolder).ToString();
                if (File.Exists(newPath + howwetSetupFileName))
                    {
                    applicationDirectory = newPath;
                    }
                }
            }

        public String ApplicationDirectory
            {
            get
                {
                return applicationDirectory;
                }
            }

        public CoverCrop GetCrop(ArrayList cropList,String crop)
            {
            CoverCrop cropOut=new CoverCrop();
            for (int i = 0; i < cropList.Count; i++)
                {
                CoverCrop cropTemp = (CoverCrop)cropList[i];
                if (cropTemp.Name == crop)
                    {
                    cropOut=cropTemp;
                    break;
                    }
                }
            return cropOut;
            }

        public decimal ConvertCoverKgToPercent(double mass,double specific_area)
            {
            decimal percent=Convert.ToDecimal((1-Math.Exp(-specific_area*mass))*100);
            return percent;
            }

        public double ConvertCoverPercentToKg(decimal percent,double specific_area)
            {
            double mass = -((Math.Log(1 - (Convert.ToDouble(percent)), 2.718281828459)) / specific_area);
            return mass;
            }

        public APSIMData GetSoil(string soilName)
            {
            APSIMData soil = new APSIMData();
            return apsimData.Child(soilName);
            }

        public APSIMData ReadTemplateFile(String fileLocation)
            {
            APSIMData template = new APSIMData();
            template.LoadFromFile(fileLocation);
            return template;
            }

        public APSIMData SoilData
            {
            get { return apsimData; }
            }
        }
    }
