using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using CSGeneral;
using VBGeneral;
using ApsimFile;
using Soils;
using System.Xml;
using System.Globalization;

namespace ProcessYPDirectory
    {
    class Program
        {
        static void Main(string[] args)
            {
            try
                {
                string TemplateFileName;
                if (args.Length == 1)
                    TemplateFileName = "C:\\hol353\\ApsimWork\\YieldProphet\\Template.apsim";
                else if (args.Length == 2)
                    TemplateFileName = args[1];
                else
                    throw new Exception("Usage: ProcessYPDirectory DirName ApsimTemplateFileName");

                ProcessJobDirectory(args[0], TemplateFileName);
                }
            catch (Exception err)
                {
                Console.WriteLine(err.Message);
                Console.ReadLine();
                }
            }

        private static void ProcessJobDirectory(string JobFolder, string ApsimTemplateFileName)
            {
            ApsimFile.Configuration Config = new Configuration("ApsimUI");


            // load the paddock file: YieldProphet.xml
            string PaddockFileName = JobFolder + "\\YieldProphet.xml";
            XmlDocument Doc = new XmlDocument();
            Doc.Load(PaddockFileName);
            XmlNode YPXml = Doc.DocumentElement;

            // Load the template file.
            ApsimFile.ApsimFile YPApsim = new ApsimFile.ApsimFile(Config);
            YPApsim.OpenFile(ApsimTemplateFileName);

            int PaddockNumber = 1;
            foreach (XmlNode PaddockXml in XmlHelper.ChildNodes(YPXml, "paddock"))
                {
                Component PaddockSimulation = YPApsim.RootComponent.ChildNodes[0];

                // If this is a validation run then remove unwanted outputfiles from the simulation.
                if (PaddockNumber == 1 && XmlHelper.Value(YPXml, "ReportType") == "")
                    {
                    Component Daily = PaddockSimulation.Find("Paddock/Daily");
                    Daily.Parent.Delete(Daily);
                    Component Decile = PaddockSimulation.Find("Paddock/Decile");
                    Decile.Parent.Delete(Decile);
                    Component FutureRain = PaddockSimulation.Find("Paddock/FutureRain");
                    FutureRain.Parent.Delete(FutureRain);
                    }

                if (PaddockNumber > 1)
                    {
                    if (XmlHelper.Value(YPXml, "ReportType") == "")
                        {
                        // validation run - don't use shortcuts.
                        PaddockSimulation = YPApsim.RootComponent.Duplicate(PaddockSimulation);
                        }
                    else
                        {
                        // A normal YP report - use shortcuts.
                        PaddockSimulation = YPApsim.RootComponent.AddShortCut(PaddockSimulation);
                        }
                    }

                PaddockSimulation.Name = "Paddock" + PaddockNumber.ToString();

                ConvertPaddockToApsim(PaddockXml, PaddockSimulation, JobFolder);

                PaddockNumber++;
                }

            // Go do report type alterations to the final .apsim file.
            DoReportTypes(YPApsim, YPXml);

            YPApsim.SaveAs(JobFolder + "\\YieldProphet.apsim");
            }

        private static void ConvertPaddockToApsim(XmlNode YPXml, Component Simulation, string JobFolder)
            {
            // --------------------------------------------------------------------
            // Go through specific nodes under the specified YP XML and pass their
            // values to the right component in the specified Simulation.
            // --------------------------------------------------------------------
            SetValue(Simulation, "Paddock/Yearly/Variables", "Constants/Title", XmlHelper.Attribute(YPXml, "name"));
            SetValue(Simulation, "Paddock/Yearly/Variables", "Constants/Crop", XmlHelper.Value(YPXml, "Crop"));
            SetValue(Simulation, "Clock", "start_date", XmlHelper.Value(YPXml, "StartDate"));
            SetValue(Simulation, "Clock", "end_date", XmlHelper.Value(YPXml, "EndDate"));

            string MetFileName = XmlHelper.Value(YPXml, "MetFileName");
            if (MetFileName == "")
                SetValue(Simulation, "Met", "station_number", XmlHelper.Value(YPXml, "StationNumber"));
            else
                {
                Simulation.Delete(Simulation.Find("met"));
                Simulation.Add("<metfile name=\"met\"><filename>" + MetFileName + "</filename></metfile>");
                }
            string RainfallFileName = XmlHelper.Value(YPXml, "RainfallFilename");
            SetValue(Simulation, "PatchInput", "FileName", RainfallFileName);
            
            // Make sure the rainfall file name exists. For the validation runs, sometimes they don't
            if (!File.Exists(JobFolder + "\\" + RainfallFileName))
               {
               StreamWriter Rain = new StreamWriter(JobFolder + "\\" + RainfallFileName);
               Rain.WriteLine("[grower.Rainfall.data]");
               Rain.WriteLine("allow_sparse_data = true ()");
               Rain.WriteLine("patch_all_years = true ()");
               Rain.WriteLine("patch_variables_long_term = maxt mint radn ()");
               Rain.WriteLine("           date     patch_rain");
               Rain.WriteLine("             ()             ()");
               Rain.Close();
               }

            SetValue(Simulation, "Paddock/Management/Reset", "ui/ResetDate", XmlHelper.Value(YPXml, "ResetDate"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/Crop", XmlHelper.Value(YPXml, "Crop"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/SowDate", XmlHelper.Value(YPXml, "SowDate"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/SowingDensity", XmlHelper.Value(YPXml, "SowingDensity"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/Cultivar", XmlHelper.Value(YPXml, "Cultivar"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/RowSpacing", XmlHelper.Value(YPXml, "RowSpacing"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/SkipRow", XmlHelper.Value(YPXml, "SkipRow"));
            SetValue(Simulation, "Paddock/Management/Sow", "ui/FTN", XmlHelper.Value(YPXml, "FTN"));
            SetValue(Simulation, "Paddock/Management/Harvest", "ui/Crop", XmlHelper.Value(YPXml, "Crop"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/ProteinContent", XmlHelper.Value(YPXml, "ProteinContent"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/ProteinIncrement", XmlHelper.Value(YPXml, "ProteinIncrement"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/Price", XmlHelper.Value(YPXml, "Price"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/FertiliserCost", XmlHelper.Value(YPXml, "FertiliserCost"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/FertiliserApplicationCost", XmlHelper.Value(YPXml, "FertiliserApplicationCost"));
            SetValue(Simulation, "Paddock/Management/GrossMargin", "ui/WaterCost", XmlHelper.Value(YPXml, "WaterCost"));

            SetValue(Simulation, "Paddock/Erosion", "Slope", XmlHelper.Value(YPXml, "Slope"));
            SetValue(Simulation, "Paddock/Erosion", "Slope_length", XmlHelper.Value(YPXml, "SlopeLength"));

            // stubble stuff.
            string StubbleType = XmlHelper.Value(YPXml, "StubbleType");
            if (StubbleType.ToLower() == "none")
                {
                SetValue(Simulation, "Paddock/SurfaceOM", "Type", "wheat");
                SetValue(Simulation, "Paddock/SurfaceOM", "Mass", "0");
                }
            else
                {
                SetValue(Simulation, "Paddock/SurfaceOM", "Type", StubbleType);
                SetValue(Simulation, "Paddock/SurfaceOM", "Mass", XmlHelper.Value(YPXml, "StubbleMass"));
                }

            DoApplications(Simulation, YPXml, "Fertilise");
            DoApplications(Simulation, YPXml, "Irrigate");
            DoApplications(Simulation, YPXml, "Tillage");
            DoApplications(Simulation, YPXml, "StubbleRemoved");

            EnableCrop(Simulation, XmlHelper.Value(YPXml, "Crop"));
            if (Simulation.ShortCutTo == null)
                DoSoil(Simulation, YPXml);

            // Write constants
            string Constants = "\n";
            string TodayDate = XmlHelper.Value(YPXml.ParentNode, "TodayDate");
            if (TodayDate.ToLower() == "29-feb")
                TodayDate = "28-feb";
            Constants += "TodayDate=date('" + TodayDate + "')\n";
            Constants += "TodayDateFull=date('" + XmlHelper.Value(YPXml.ParentNode, "TodayDateFull") + "')\n";
            Constants += "StartSeasonDate=date('" + XmlHelper.Value(YPXml, "StartSeasonDate") + "')\n";
            Constants += "StartSeasonDateFull=date('" + XmlHelper.Value(YPXml, "StartSeasonDateFull") + "')\n";
            Constants += "ResetDate=date('" + XmlHelper.Value(YPXml, "ResetDate") + "')\n";
            Constants += "ResetDateFull=date('" + XmlHelper.Value(YPXml, "ResetDateFull") + "')\n";
            Constants += "SowDateFull=date('" + XmlHelper.Value(YPXml, "SowDateFull") + "')\n";
            Constants += "SowDate=date('" + XmlHelper.Value(YPXml, "SowDate") + "')\n";
            SetValue(Simulation, "Paddock/Management/Constants", "start_of_day/text", Constants);
            }
        private static void DoSoil(Component Simulation, XmlNode YPXml)
            {
            // --------------------------------------------------------
            // Do soil stuff.
            // --------------------------------------------------------
            XmlNode SoilNode = XmlHelper.FindByType(YPXml, "Soil");
            if (SoilNode == null)
                throw new Exception("Cannot find soil node in paddock: " + XmlHelper.Name(YPXml));
            Soil YPSoil = new Soil(SoilNode);
            if (XmlHelper.Value(SoilNode, "MaxRootDepth") != "")
                YPSoil.MaxRootDepth = Convert.ToInt32(XmlHelper.Value(SoilNode, "MaxRootDepth"));

            // convert Wakely Black OC values to Total OC ready for APSIM.
            double[] OC = YPSoil.OC;
            if (OC.Length > 0)
                {
                for (int i = 0; i != OC.Length; i++)
                    OC[i] = OC[i] * 1.3;
                YPSoil.OC = OC;
                }
            
            // get rid of <soiltype> from the soil
            // this is necessary because NPD uses this field and puts in really long
            // descriptive classifications. Soiln2 bombs with an FString internal error.
            YPSoil.Classification = "";

            // Get the crop name
            string CropName = XmlHelper.Value(YPXml, "Crop");

            // check to make sure we have barley LL values. In 2005 there weren't any barley LL values.
            if (YPSoil.LL(CropName).Length == 0 || YPSoil.LL(CropName)[0] == MathUtility.MissingValue)
                {
                double[] kl = YPSoil.KL("wheat");
                double[] ll = YPSoil.LL("wheat");
                double[] xf = YPSoil.XF("wheat");
                YPSoil.SetCrop(CropName, ll, kl, xf);
                }

            // Set the soil name to 'soil'
            YPSoil.Name = "Soil";

            // If an InitTotalWater and InitTotalNitrogen was specified then get rid of the 
            // soil samples and replace with <InitWater> and <InitNitrogen> elements.
            string InitWater = XmlHelper.Value(YPXml, "InitTotalWater");
            string InitNitrogen = XmlHelper.Value(YPXml, "InitTotalNitrogen");
            if (InitWater != "" && InitNitrogen != "")
                {
                // remove existing soil sample nodes from soil
                XmlNode Sample = XmlHelper.FindByType(YPSoil.Data, "SoilSample");
                while (Sample != null)
                    {
                    YPSoil.Data.RemoveChild(Sample);
                    Sample = XmlHelper.FindByType(YPSoil.Data, "SoilSample");
                    }

                // Add in the InitWater node to soil
                XmlNode InitWaterXml = XmlHelper.FindByType(YPSoil.Data, "InitWater");
                if (InitWaterXml == null)
                    {
                    InitWaterXml = YPSoil.Data.AppendChild(YPSoil.Data.OwnerDocument.CreateElement("InitWater"));
                    InitWaterXml.InnerXml = "<percentmethod><percent>1</percent><distributed>filled from top</distributed></percentmethod><RelativeTo>Wheat</RelativeTo>";
                    }
                Soils.InitWater InitWaterNode = new InitWater(InitWaterXml, YPSoil);
                
                int Percent = (int) MathUtility.Round(Convert.ToDouble(InitWater) / MathUtility.Sum(YPSoil.PAWC("Wheat")) * 100, 0);
                InitWaterNode.SetUsingPercent(Percent, true);

                // Add in the InitNitrogen node to soil.
                XmlNode InitNitrogenXml = YPSoil.Data.AppendChild(YPSoil.Data.OwnerDocument.CreateElement("InitNitrogen"));
                InitNitrogenXml.InnerXml = "<profile><layer><no3>6</no3><nh4>0.5</nh4><thickness>150</thickness></layer><layer><no3>2.1</no3><nh4>0.1</nh4><thickness>150</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer><layer><no3>0.1</no3><nh4>0.1</nh4><thickness>300</thickness></layer></profile>";
                Soils.InitNitrogen InitNitrogenNode = new InitNitrogen(InitNitrogenXml, YPSoil);
                InitNitrogenNode.TotalNO3KgHa = Convert.ToDouble(InitNitrogen);
                }

            // Replace the soil in the simulation with this one.
            Component PaddockSoil = Simulation.Find("Paddock/Soil");
            if (PaddockSoil == null)
                throw new Exception("Cannot find a soil in the template simulation");
            PaddockSoil.Replace(YPSoil.Data.OuterXml);
            }
        private static void EnableCrop(Component Simulation, string CropNameToEnable)
            {
            // --------------------------------------------------------------------
            // Enable the specified crop and disable all others.
            // --------------------------------------------------------------------
            string[] CropNames = {"Wheat", "Barley", "Canola", "Oats", "Sorghum"};

            foreach (string CropName in CropNames)
                {
                Component Crop = Simulation.Find("Paddock/" + CropName);
                if (Crop == null)
                    throw new Exception("Cannot find crop " + CropName);
                Crop.Enabled = (CropName.ToLower() == CropNameToEnable.ToLower());
                }
            }
        private static void DoApplications(Component Simulation, XmlNode YPXml, string ApplicationType)
            {
            // --------------------------------------------------------------------
            // Copy application nodes (e.g. <fertilise>) from the YP XML passed in
            // into the appropriate place in the specified simulation. There may
            // be multiple nodes to copy so mutiple components in the simulation 
            // may need to be created (duplicated).
            // --------------------------------------------------------------------
            bool IsFirstApplication = true;

            string NodePath = "Paddock/Management/" + ApplicationType;
            Component FirstApplication = Simulation.Find(NodePath);
            if (FirstApplication == null)
                throw new Exception("Cannot find node: " + NodePath);

            foreach (XmlNode ApplicationNode in XmlHelper.ChildNodes(YPXml, ApplicationType))
                {
                // try and find an application that exactly matches the current one.

                Component MatchingApplication = FindApplication(Simulation, ApplicationNode);
                if (MatchingApplication == null)
                    {
                    if (IsFirstApplication)
                        MatchingApplication = FirstApplication;
                    else
                        MatchingApplication = FirstApplication.Parent.Duplicate(FirstApplication);

                    MatchingApplication.MakeConcrete();
                    XmlDocument ContentsDoc = new XmlDocument();
                    ContentsDoc.LoadXml(MatchingApplication.Contents);
                    foreach (XmlNode Node in XmlHelper.ChildNodes(ApplicationNode, ""))
                        XmlHelper.SetValue(ContentsDoc.DocumentElement, "ui/" + Node.Name, Node.InnerText);
                    MatchingApplication.Contents = ContentsDoc.DocumentElement.OuterXml;
                    }
                IsFirstApplication = false;
                }
            }

        private static Component FindApplication(Component Simulation, XmlNode ApplicationNode)
            {
            Component Management = Simulation.Find("Paddock/Management");
            if (Management == null)
                throw new System.Exception("Cannot find a management node in simulation");

            foreach (Component Manager in Management.ChildNodes)
                {
                XmlDocument ContentsDoc = new XmlDocument();
                ContentsDoc.LoadXml(Manager.Contents);

                bool IsAMatch = true;
                foreach (XmlNode Node in XmlHelper.ChildNodes(ApplicationNode, ""))
                    {
                    string CurrentValue = XmlHelper.Value(ContentsDoc.DocumentElement, "ui/" + Node.Name);
                    IsAMatch = IsAMatch && (CurrentValue == Node.InnerText);
                    }
                if (IsAMatch)
                    return Manager;
                }
            return null;
            }
        private static void SetValue(Component Simulation, string ComponentPath, string ParameterPath, string Value)
            {
            // --------------------------------------------------------------------
            // Set the value of a paremeter in a specified component.
            // --------------------------------------------------------------------
            if (Value != "")
                {
                Component SimulationComponent = Simulation.Find(ComponentPath);
                if (SimulationComponent == null)
                    throw new Exception("Cannot find simulation node: " + ComponentPath);

                XmlDocument ContentsDoc = new XmlDocument();
                ContentsDoc.LoadXml(SimulationComponent.Contents);

                if (XmlHelper.Value(ContentsDoc.DocumentElement, ParameterPath) != Value)
                    {
                    SimulationComponent.MakeConcrete();

                    XmlHelper.SetValue(ContentsDoc.DocumentElement, ParameterPath, Value);
                    SimulationComponent.Contents = ContentsDoc.DocumentElement.OuterXml;
                    }
                }
//            else
//                Console.WriteLine("Cannot find value for " + ComponentPath + " " + ParameterPath);
            }
        private static void DoReportTypes(ApsimFile.ApsimFile YPApsim, XmlNode YPXml)
            {
            string ReportType = XmlHelper.Value(YPXml, "ReportType");

            if (ReportType.ToLower() != "crop report")
                {
                // Get rid of NUnlimited and Next10Days.
                YPApsim.RootComponent.Delete(YPApsim.RootComponent.Find("NUnlimited"));
                YPApsim.RootComponent.Delete(YPApsim.RootComponent.Find("Next10Days"));
                if (ReportType.ToLower() == "fallow monitoring report")
                    {
                    // Disable the reset rule and set the simulation start date to the reset date.
                    XmlNode ResetNode = XmlHelper.FindByType(YPXml, "paddock/ResetDateFull");
                    if (ResetNode == null)
                        throw new Exception("Cannot find reset date in yieldprophet.xml");
                    string ResetDate = ResetNode.InnerText;

                    Component ResetRule = YPApsim.RootComponent.Find("Paddock1/Paddock/Management/Reset");
                    ResetRule.Enabled = false;

                    Component Clock = YPApsim.RootComponent.Find("Paddock1/Clock");
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml(Clock.Contents);
                    XmlHelper.SetValue(Doc.DocumentElement, "start_date", ResetDate);
                    Clock.Contents = Doc.DocumentElement.OuterXml;
                    }
                }
             if (ReportType.ToLower() == "stubble management report")
                {
                // Remove all fertiliser, tillage and irrigation applications before the reset date. The 
                // reset date is usually harvest, so we're not interested in anything before then. In 
                // particular we want to disable anything that happened in the previous fallow.
                // Disable the reset rule and set the simulation start date to the reset date.
                XmlNode ResetNode = XmlHelper.FindByType(YPXml, "paddock/ResetDateFull");
                if (ResetNode == null)
                   throw new Exception("Cannot find reset date in yieldprophet.xml");
                DateTime ResetDate = DateTime.Parse(ResetNode.InnerText);

                Component Management = YPApsim.RootComponent.Find("Paddock1/Paddock/Management");
                if (Management == null)
                   throw new System.Exception("Cannot find a management node in simulation");

                foreach (Component ManagerNode in Management.ChildNodes)
                   {
                   XmlDocument ManagerDoc = new XmlDocument();
                   ManagerDoc.LoadXml(ManagerNode.Contents);

                   string ApplicationDateString = XmlHelper.Value(ManagerDoc.DocumentElement, "ui/FertDateFull");
                   if (ApplicationDateString == "")
                      ApplicationDateString = XmlHelper.Value(ManagerDoc.DocumentElement, "ui/IrrigateDateFull");
                   if (ApplicationDateString == "")
                      ApplicationDateString = XmlHelper.Value(ManagerDoc.DocumentElement, "ui/TillageDateFull");
                   if (ApplicationDateString == "")
                      ApplicationDateString = XmlHelper.Value(ManagerDoc.DocumentElement, "ui/StubbleRemovedDateFull");
                   if (ApplicationDateString != "")
                      {
                      DateTime ApplicationDate = DateTime.Parse(ApplicationDateString);
                      if (ApplicationDate < ResetDate)
                         ManagerNode.Enabled = false;
                      }
                   }

                }
            }


        }
    }
