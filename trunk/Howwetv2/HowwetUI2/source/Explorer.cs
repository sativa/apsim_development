using System;
using System.IO;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using CSGeneral;
using APSRU.Model.Howwet;
using System.Diagnostics;
using System.Xml;

namespace APSRU.Howwet
    {
    public partial class Explorer : System.Windows.Forms.Form,IHowwetModel
        {

        private delegate void howwetModel_eventHandler(IHowwetModel publisher);
        private event howwetModel_eventHandler howwetModel_event;

        private TypeInstanceCache pageTypeInstanceCache;
        private String soilFileName;
        private String metFileName;
        private String soilName;
        private String soilFileFullName;
        private String soilRegion;
        private String ocDepthFirstLayer;
        private double organicCarbonContent;
        private double soilDepth;
        private double pAWC;
        private double initialNitrogen;
        private double initialWater;
        private int initialWaterPercent;
        private String cropToGrow;
        private decimal coverStart=30;
        private decimal coverEnd=10;
        private DateTime fallowDateStart=DateTime.Now;
        private DateTime fallowDateEnd=DateTime.Now;
        private ArrayList coverTypeCropList;
        private ArrayList regionList;
        private String region;
        private String[] cropToGrowList;
        private double startPAW; 
        private double fallowRainfall; 
        private double fallowEvaporation; 
        private double fallowRunoff; 
        private double fallowDrainage; 
        private double endPAW; 
        private double gainPAW; 
        private double fallowEfficiency;
        private double startSoilNitrate;
        private double endSoilNitrate;
        private double gainNitrate;
        private DataTable chartData;
        
        public Boolean inUpdate = false;

        public String soilsFileFullName = "";
        private String selectedFileName = "";

        private SimulationOut outputObject;
        private APSRU.Model.Howwet.Results result;
        public SimulationIn simulationObject;
        private HowwetUtility util;
        private HowwetConfiguration config;
        public MetData metObject;
        public String defaultErosionSlope;
        public String defaultErosionSlopeLength;
        public String defaultErosionErodibilty;
        private String apsimPath = "\\bin\\apsim.exe";
        private String apsimToSimPath = "\\bin\\apsimtosim.exe";
        private String howwetReportFileName = "\\howwetv2\\HowwetReport.xml";
        private String howwetSetupFileName = "\\howwetv2\\HowwetSetup.xml";
        private String howwetRegionSetupFileName = "\\howwetv2\\HowwetRegions.xml";
         
        public Explorer()
            {
            InitializeComponent();
            pageTypeInstanceCache = new TypeInstanceCache();
            }

        public void Startup()
            {
            util = new HowwetUtility(Application.ExecutablePath, howwetSetupFileName);
            config = new HowwetConfiguration(util.ApplicationDirectory + howwetSetupFileName, util.ApplicationDirectory + howwetRegionSetupFileName);
            //load drop down lists
            CoverTypeCropList = config.CropList;
            RegionList = config.RegionList;
            //load template or existing simulation
            if (this.selectedFileName == "")
                {
                simulationObject = new SimulationIn(util.ReadTemplateFile(util.ApplicationDirectory + "\\howwetv2\\" + config.TemplateFileName));
                simulationObject.FileName = "untitled";
                if (!(config.DefaultSoilFileName == ""))
                    {
                    if (LoadSoilFile(config.DefaultSoilFileName))
                        {
                        if (!(config.DefaultSoilName == ""))
                            {
                            APSIMData soils = new APSIMData();
                            soils.LoadFromFile(config.DefaultSoilFileName);
                            APSIMData soil = soils.GetNode("name",config.DefaultSoilName);
                            LoadSoil(soil);
                            }
                        }
                    }
                if (!(config.DefaultMetfile == ""))
                    {
                    LoadMetFile(config.DefaultMetfile);
                    }
                if (!(config.DefaultRegionName == ""))
                    {
                    }
                }
            else
                {
                APSIMData apsimDataObject = new APSIMData();
                apsimDataObject.LoadFromFile(this.selectedFileName);
                simulationObject = new SimulationIn(apsimDataObject);
                }
          
            inUpdate = true;
            this.NotifyListeners();
            inUpdate = false;
            }

      //  private void loadDefaultSoilFile()
        //    {
        //    FileInfo fileInfo = new FileInfo(config.DefaultSoilFileName);
        //    if (fileInfo.Exists)
        //        {
        //        SoilFileName = fileInfo.Name;
        //        SoilFileFullName = fileInfo.FullName;
        //        }
        //    else
        //        {
        //        SoilFileName = "<Select a Soil file>";
        //        }
       //     }

        private void AddPageToExplorer(Page page)
            {
            pageSite.Controls.Add(page);
            page.Location = new System.Drawing.Point(0, 0);
            page.BringToFront();
            page.Explorer = this;
            }

        public void Go(Page page)
            {
            if (page != null)
                {
                AddPageToExplorer(page);
                RegisterListener(page);
                }
            }

        public void Go(Type pageType)
            {
            Page page = (Page)pageTypeInstanceCache.GetInstanceOfType(pageType);
            Go(page);
            }

        private void backButton_Click(object sender, EventArgs e)
            {

            }

        private void forwardButton_Click(object sender, EventArgs e)
            {

            }

        private void homeButton_Click(object sender, EventArgs e)
            {

            }

        public void LoadMetFile(String metFileName)
            {
            metObject = new MetData(metFileName);
            MetFileName = metFileName;

            simulationObject.MetFileName = metObject.FileName;
            //set datetime picker
            if (!(metObject.EndDate().Subtract(new TimeSpan(400, 0, 0, 0, 0)) < metObject.StartDate()))
                {
                FallowDateStart= metObject.EndDate().Subtract(new TimeSpan(400, 0, 0, 0, 0));
                }
            else
                {
                FallowDateStart = metObject.StartDate();
                }
            FallowDateEnd = metObject.EndDate();
            config.DefaultMetfile = MetFileName;
            this.NotifyListeners();
            }

        public Boolean LoadSoilFile(String soilFileName)
            {
            Boolean isLoaded = false;
            FileInfo fileInfo = new FileInfo(soilFileName);
            if (fileInfo.Exists)
                {
                SoilFileName = fileInfo.Name;
                SoilFileFullName = fileInfo.FullName;
                config.DefaultSoilFileName = SoilFileFullName;
                isLoaded = true;
                }
            else
                {
                SoilFileName = "<Select a Soil file>";
                }
            this.NotifyListeners();
            return isLoaded;
            }

        public void LoadSoil(APSIMData soil)
            {
            //add soil to simulation object
            simulationObject.AddSoil(soil);
            SoilName = simulationObject.Soil.Name;
            SoilRegion = simulationObject.Soil.Region;
            String[] layers = simulationObject.Soil.DepthStrings;
            OcDepthFirstLayer = layers[0];//top layer string
            OrganicCarbonContent = Convert.ToDouble(simulationObject.Soil.OC.GetValue(0));
            SoilDepth = MathUtility.Sum(simulationObject.Soil.Thickness);
            PAWC=MathUtility.Sum(simulationObject.Soil.PAWC());
            InitialNitrogen= MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa);
            int percentage = 20;
            simulationObject.Soil.InitialWater.SetUsingPercent(percentage, true);
            double proportion = percentage / 100.0;
            InitialWater = MathUtility.Sum(simulationObject.Soil.PAWC()) * proportion;
            InitialWaterPercent = simulationObject.Soil.InitialWater.Percent;
            CropToGrowList = simulationObject.Soil.Crops;
            config.DefaultSoilName=SoilName;
            inUpdate = true;
            this.NotifyListeners();
            inUpdate = false;
            }

        public void LoadRegion(String region)
            {
            //build temp met file
            //if selected met file does not have raidation and max/min temp then build a metobject that



            config.DefaultRegionName = Region;

            this.NotifyListeners();
            }

        public void UpdateInitialWater(int percentage)
            {
            simulationObject.Soil.InitialWater.SetUsingPercent(percentage, true);
            double proportion = percentage / 100.0;
            InitialWater = MathUtility.Sum(simulationObject.Soil.PAWC()) * proportion;
            InitialWaterPercent = simulationObject.Soil.InitialWater.Percent;
            this.NotifyListeners();
            }

        public void UpdateInitialWater(double watermm)
            {
            double TotalPAWC = MathUtility.Sum(simulationObject.Soil.PAWC());
            int percentage = 0;
            percentage = Convert.ToInt32(Convert.ToDouble(watermm) / TotalPAWC * 100);
            percentage = Math.Min(percentage, 100);
            percentage = Math.Max(percentage, 0);
            simulationObject.Soil.InitialWater.SetUsingPercent(percentage, true);
            InitialWaterPercent = simulationObject.Soil.InitialWater.Percent;
            InitialWater = watermm;
            this.NotifyListeners();
            }

        public void UpdateCoverType(String selectedCrop)
            {
            CoverCrop crop = util.GetCrop(CoverTypeCropList, selectedCrop);
            simulationObject.SOMType = crop.Name;
            simulationObject.SOMCNRatio = Convert.ToString(crop.Cnr);
            UpdateCover();
            }

        public void UpdateCropToGrow(String selectedCrop)
            {
            simulationObject.AddCrop(selectedCrop);
            }

        public bool ExecuteAPSIM()
            {
            bool success = false;
           
            String ApsimToSimPath = util.ApplicationDirectory +  apsimToSimPath;
            String Apsim = util.ApplicationDirectory+ apsimPath;
            String simulationFileName = simulationObject.FileName;
            String simulationParentPath = Directory.GetParent(simulationFileName).ToString();
            //remove old .sum, .out, and .sim files if they exist
            if (File.Exists(simulationObject.OutputFileName)) File.Delete(simulationObject.OutputFileName);
            if (File.Exists(simulationObject.SummaryFileName)) File.Delete(simulationObject.SummaryFileName);
            if (File.Exists(simulationObject.SimulationName + ".sim")) File.Delete(simulationObject.SimulationName + ".sim");

            if (File.Exists(ApsimToSimPath))
                {

                Process p = Process.Start("\"" + ApsimToSimPath + "\"", "\"" + simulationFileName + "\"");
                p.WaitForExit();
                this.Refresh();

                String simFileName = simulationParentPath + "\\" + simulationObject.SimulationName + ".sim";

                if (File.Exists(simFileName))
                    {
                    if (File.Exists(Apsim))
                        {

                        p = Process.Start("\"" + Apsim + "\"", "\"" + simFileName + "\"");
                        p.WaitForExit();
                        this.Refresh();

                        String outFileName = simulationObject.SimulationName + ".out";

                        if (File.Exists(outFileName))
                            {
                            success = true;
                            }
                        }
                    }
                }
            return success;
        }

        public void GraphResults()
            {
            }

        public void BuildResultObject()
            {
            outputObject = new SimulationOut(simulationObject);
            ChartData = outputObject.Data;
            result = new APSRU.Model.Howwet.Results();
            result.loadResults(simulationObject, outputObject);
            result.softwareVersion = config.Version;

            StartPAW= result.soilWaterStart;
            FallowRainfall = result.rainfall;
            FallowEvaporation = result.evaporation;
            FallowRunoff = result.runoff;
            FallowDrainage = result.drain;
            EndPAW = result.soilWaterEnd;
            GainPAW = result.fallowWaterGain;
            FallowEfficiency = result.fallowWaterEfficiency;

            //Nitrogen
            StartSoilNitrate = result.nitrateStart;
            EndSoilNitrate = result.nitrateEnd;
            GainNitrate = result.nitrateGain;

            this.NotifyListeners();
            }

        public void SaveAllData()
            {
            //write setup XML file
            config.SaveDefaults();
            //write form values
            if (simulationObject.FileName == "untitled")
                {
                SaveFileDialog saveDialog = new SaveFileDialog();
                saveDialog.Filter = "APSIM files (*.apsim)|*.apsim";
                if (saveDialog.ShowDialog() == DialogResult.OK)
                    {
                    simulationObject.FileName = saveDialog.FileName;
                    }
                }
            FileInfo fileInfo = new FileInfo(simulationObject.FileName);
            simulationObject.SimulationName = fileInfo.Name.Substring(0, fileInfo.Name.IndexOf(".apsim"));
            simulationObject.OutputFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".out";
            simulationObject.SummaryFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".sum";

            APSIMData apsimObject = new APSIMData();
            apsimObject = simulationObject.Data;

            apsimObject.SaveToFile(simulationObject.FileName);
            this.Text = simulationObject.FileName;
            }

        public void UpdateStartCover(decimal coverAmount)
            {
            CoverStart=coverAmount;
            UpdateCover();
            }

        public void UpdateEndCover(decimal coverAmount)
            {
            CoverEnd = coverAmount;
            UpdateCover();
            }

        private void UpdateCover()
            {
            CoverCrop crop = util.GetCrop(coverTypeCropList, simulationObject.SOMType);
            double startCoverKg = util.ConvertCoverPercentToKg(CoverStart/100, crop.SpecificArea);
            double endCoverKg = util.ConvertCoverPercentToKg(CoverEnd/100, crop.SpecificArea);
            double diffCover = startCoverKg - endCoverKg;
            TimeSpan diffTime = FallowDateEnd.Subtract(FallowDateStart);
            simulationObject.SOMMass = startCoverKg.ToString();
            simulationObject.AddLogic(diffCover, diffTime.TotalDays);
            }


        public void UpdateFallowDates()
            {
            simulationObject.StartDate=FallowDateStart.ToShortDateString();
            simulationObject.EndDate = FallowDateEnd.ToShortDateString();
            UpdateCover();
            }

        public void UpdateStartFallowDate(DateTime date)
            {
            FallowDateStart = date;
            UpdateFallowDates();
            }

        public void UpdateEndFallowDate(DateTime date)
            {
            FallowDateEnd = date;
            UpdateFallowDates();
            }

        public void UpdateErosion(string slope, string slopeLength, string erodibilty)
            {
            simulationObject.ErosionSlope = slope;
            simulationObject.ErosionSlopeLength = slopeLength;
            simulationObject.ErosionErodibilty = erodibilty;
            }

        public void DisplayReport()
            {
            String fileName = util.ApplicationDirectory + howwetReportFileName;
            StreamWriter stream = new StreamWriter(fileName);
            stream.WriteLine("<?xml-stylesheet type=\"text/xsl\" href=\"HowwetReport.xsl\"?>");
            System.Xml.Serialization.XmlSerializer xmlStream = new System.Xml.Serialization.XmlSerializer(result.GetType());
            xmlStream.Serialize(stream, result);
         //   RainfallSWChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\RainfallSWChart.gif");
         //   SoilNitrogenChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\SoilNitorgenChart.gif");
         //   ErosionChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\ErosionChart.gif");
         //   LTRainfallChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\LTRainfallChart.gif");
         //   ProfileChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\ProfileChart.gif");
            System.Diagnostics.Process.Start("IExplore.exe", util.ApplicationDirectory + howwetReportFileName);
            }

        public string SoilFileFullName
            {
            get { return this.soilFileFullName; }
            set { this.soilFileFullName = value; }
            }

        public string SoilFileName
            {
            get {return this.soilFileName;}
            set {this.soilFileName = value;}
            }

        public string SoilName
            {
            get { return this.soilName; }
            set { this.soilName = value; }
            }

        public string MetFileName
            {
            get { return this.metFileName; }
            set { this.metFileName = value; }
            }

        public string SoilRegion
            {
            get{return this.soilRegion;}
            set { this.soilRegion = value; }
            }

        public String OcDepthFirstLayer
            {
            get { return this.ocDepthFirstLayer; }
            set { this.ocDepthFirstLayer = value; }
            }

        public double OrganicCarbonContent
            {
            get { return this.organicCarbonContent; }
            set { this.organicCarbonContent = value; }
            }

        public double SoilDepth 
            {
            get { return this.soilDepth; }
            set { this.soilDepth = value; }
            }

        public double PAWC 
            {
            get { return this.pAWC; }
            set { this.pAWC = value; }
            }

        public double InitialNitrogen
            {
            get { return this.initialNitrogen; }
            set { this.initialNitrogen = value; }
            }

        public double InitialWater
            {
            get { return this.initialWater; }
            set { this.initialWater = value; }
            }

        public int InitialWaterPercent
            {
            get { return this.initialWaterPercent; }
            set { this.initialWaterPercent = value; }
            }

        public string CropToGrow 
            { 
            get{return this.cropToGrow;}
            set{this.cropToGrow=value;}
            }

        public ArrayList CoverTypeCropList
            {
            get { return this.coverTypeCropList; }
            set { this.coverTypeCropList = value; }
            }

        public string Region
            {
            get { return this.region; }
            set { this.region = value; }
            }

        public ArrayList RegionList
            {
            get { return this.regionList; }
            set { this.regionList = value; }
            }

        public String[] CropToGrowList
            {
            get { return this.cropToGrowList; }
            set { this.cropToGrowList = value; }
            }

        public decimal CoverStart
            {
            get { return this.coverStart; }
            set {this.coverStart=value;}
            }

        public decimal CoverEnd
            {
            get { return this.coverEnd; }
            set { this.coverEnd = value; }
            }

        public DateTime FallowDateStart
           {
            get { return this.fallowDateStart; }
            set { this.fallowDateStart = value; }
            }

        public DateTime FallowDateEnd
            {
            get { return this.fallowDateEnd; }
            set { this.fallowDateEnd = value; }
            }

        public double StartPAW
            {
            get { return this.startPAW; }
            set { this.startPAW = value; }
            }

        public double FallowRainfall
            {
            get { return this.fallowRainfall; }
            set { this.fallowRainfall = value; }
            }

        public double FallowEvaporation
            {
            get { return this.fallowEvaporation; }
            set { this.fallowEvaporation = value; }
            }

        public double FallowRunoff
            {
            get { return this.fallowRunoff; }
            set { this.fallowRunoff = value; }
            }

        public double FallowDrainage
            {
            get { return this.fallowDrainage; }
            set { this.fallowDrainage = value; }
            }

        public double EndPAW
            {
            get { return this.endPAW; }
            set { this.endPAW = value; }
            }

        public double GainPAW
            {
            get { return this.gainPAW; }
            set { this.gainPAW = value; }
            }

        public double FallowEfficiency
            {
            get { return this.fallowEfficiency; }
            set { this.fallowEfficiency = value; }
            }

        public double StartSoilNitrate
            {
            get { return this.startSoilNitrate; }
            set { this.startSoilNitrate = value; }
            }

        public double EndSoilNitrate
            {
            get { return this.endSoilNitrate; }
            set { this.endSoilNitrate = value; }
            }

        public double GainNitrate
            {
            get { return this.gainNitrate; }
            set { this.gainNitrate = value; }
            }

        public DataTable ChartData
            {
            get { return this.chartData; }
            set { this.chartData = value; }
            }

        #region IHowwetModel Members

        public void RegisterListener(IEventListener listener)
            {
            howwetModel_event += new howwetModel_eventHandler(listener.OnNotification);
            }

        public void UnregisterListener(IEventListener listener)
            {
            howwetModel_event -= new howwetModel_eventHandler(listener.OnNotification);
            }

        public void NotifyListeners()
            {
            if (null != howwetModel_event)
                howwetModel_event(this);
            }
        #endregion
        }
    }
