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

namespace APSRU.Translator.Howwet
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
    public class HowwetConfiguration
        {
        String version;
        bool debug;
        bool trainingMode;
        String templateFileName;
        String defaultSoilFileName;
        ArrayList cropList;
        

        public HowwetConfiguration(String fileName)
            {
            String errString = "";
            try
                {
                errString = "reading " + fileName;
                XmlDocument doc = new XmlDocument();
                doc.Load(fileName);
                XmlElement element = doc.DocumentElement;
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
        public void Save()
            {
            //TODO
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
        public ArrayList CropList
            {
            set {cropList=value;}
            get {return cropList;}
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
