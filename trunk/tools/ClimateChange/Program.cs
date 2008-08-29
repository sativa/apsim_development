using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using CSGeneral;
using System.Collections.Specialized;
using ApsimFile;
using System.Diagnostics;

namespace Tools
   {
   public class ClimateChange
      {
      static int Main(string[] args)
         {
         try
            {
            bool AutoRun = true;
            string ApsimFileName;
            if (args.Length == 2 && args[1] == "/dontrun")
               {
               AutoRun = false;
               ApsimFileName = args[0];
               }
            else if (args.Length != 1)
               throw new Exception("Usage: ClimateChange ApsimFileName");
            else
               ApsimFileName = args[0];
            
            ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
            Apsim.OpenFile(ApsimFileName);
            Go(ref Apsim, Path.GetDirectoryName(ApsimFileName), AutoRun);
            Apsim.SaveAs(ApsimFileName);
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }


      public static void Go(ref ApsimFile.ApsimFile Apsim, string JobFolder, bool AutoRun)
         {
         // Loop through all simulations.
         ApsimFile.ApsimFile NewApsim = new ApsimFile.ApsimFile();
         NewApsim.New();
         foreach (Component Simulation in Apsim.RootComponent.ChildNodes)
            {
            Component Met = Simulation.Find("met");
            if (Met == null)
               throw new Exception("Cannot find met component in simulation: " + Simulation.Name);
            string MetFileName = XmlHelper.Value(Met.ContentsAsXML, "FileName");
            if (MetFileName == "")
               throw new Exception("No met file specified in simulation: " + Simulation.Name);
            MetFileName = JobFolder + "\\" + MetFileName;

            // Extract the latitude and longitude from the .met file.
            StreamReader MetFile = new StreamReader(MetFileName);
            StringCollection Constants = new StringCollection();
            StringCollection Headings = new StringCollection();
            APSIMInputFile.ReadApsimHeaderLines(MetFile, ref Constants, ref Headings);
            MetFile.Close();
            double Latitude = Convert.ToDouble(GetConstant(Constants, "Latitude"));
            double Longitude = Convert.ToDouble(GetConstant(Constants, "Longitude"));

            // We need to create a climate change .xml file with all details so that
            // the reporting can display the info.
            XmlDocument ClimateDoc = new XmlDocument();
            XmlNode ClimateXml = ClimateDoc.AppendChild(ClimateDoc.CreateElement("ClimateChange"));

            ApsimFile.Configuration Config = new Configuration("ClimateChange");
            string OzClimFolder = Config.Setting("OzClimFolder");
            NewApsim.RootComponent.Add(Simulation.FullXML());
            foreach (string OzClimScenarioFolder in Directory.GetDirectories(OzClimFolder))
               {
               string ScenarioName = Path.GetFileName(OzClimScenarioFolder);
               if (ScenarioName != "BaseStations")
                  {
                  Component Scenario;
                  string NewMetFileName = OzClimToMQuantile(OzClimScenarioFolder, MetFileName, Latitude, Longitude, Config, ClimateXml, AutoRun);
                  Scenario = NewApsim.RootComponent.AddShortCut(NewApsim.RootComponent.ChildNodes[0]);

                  Scenario.Name = Simulation.Name + "-" + ScenarioName;
                  Component ScenarioMet = Scenario.Find("met");
                  ScenarioMet.MakeConcrete();
                  ScenarioMet.Contents = "<met><filename>" + Path.GetFileName(NewMetFileName) + "</filename></met>";
                  }
               }
            ClimateDoc.Save(JobFolder + "\\ClimateChangeDetails.xml");

            }
         Apsim = NewApsim;
         }

      private static string GetConstant(StringCollection Constants, string ConstantName)
         {
         foreach (string Line in Constants)
            {
            string[] LineParts = Line.Split(" =".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (LineParts.Length >= 2 && LineParts[0].ToLower() == ConstantName.ToLower())
               return LineParts[1];
            }
         throw new Exception("Cannot find constant: " + ConstantName + " in met file");
         }

      private static string OzClimToMQuantile(string OzClimScenarioFolder, string MetFileName, double Latitude, double Longitude, Configuration Config, XmlNode ClimateXml, bool AutoRun)
         {
         string ScenarioName = Path.GetFileName(OzClimScenarioFolder);
         string JobFolder = Path.GetDirectoryName(MetFileName);
         string NewMetFileName = JobFolder + "\\" + Path.GetFileNameWithoutExtension(MetFileName) + ScenarioName + ".met";
         if (!File.Exists(NewMetFileName))
            {
            string[] Months = { "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };

            string ChangeFileName = JobFolder + "\\" + ScenarioName + "Change.csv";
            StreamWriter Out = new StreamWriter(ChangeFileName);
            Out.WriteLine("month,rain,tmean");
            for (int Month = 1; Month <= 12; Month++)
               {
               string RainValue = GeoLocate(Latitude, Longitude, OzClimScenarioFolder + "\\" + Months[Month - 1] + " rain.csv");
               string TempValue = GeoLocate(Latitude, Longitude, OzClimScenarioFolder + "\\" + Months[Month - 1] + " temp.csv");
               Out.WriteLine(Month.ToString() + "," + RainValue + "," + TempValue);
               XmlHelper.SetValue(ClimateXml, ScenarioName + "/" + Months[Month - 1] + "/Rain", RainValue);
               XmlHelper.SetValue(ClimateXml, ScenarioName + "/" + Months[Month - 1] + "/Temp", TempValue);
               }
            Out.Close();

            // get the co2 level for this scenario.
            StreamReader CO2 = new StreamReader(OzClimScenarioFolder + "\\co2.txt");
            CO2.ReadLine();
            string CO2Value = CO2.ReadLine();
            XmlHelper.SetValue(ClimateXml, ScenarioName + "/C02", CO2Value);

            // write the R script to create the projections met file.
            string QuantileTrendFileName = FindClosestTo(Latitude, Longitude, Config);
            string BaseStationName = Path.GetFileNameWithoutExtension(QuantileTrendFileName);
            BaseStationName = BaseStationName.Remove(BaseStationName.IndexOf(".quantile"));
            XmlHelper.SetValue(ClimateXml, "BaseStation", BaseStationName);
            string BaseLineFileName = MetFileName + ".baseline";
            StreamWriter RInput = new StreamWriter(JobFolder + "\\" + ScenarioName + ".R");
            RInput.WriteLine("nYears<-2030 - 1990");
            RInput.WriteLine("co2 <- " + CO2Value);
            RInput.WriteLine("quantileChanges <- \"" + Path.GetFileName(QuantileTrendFileName).Replace('\\', '/') + "\"");
            RInput.WriteLine("meanChanges <- \"" + Path.GetFileName(ChangeFileName).Replace('\\', '/') + "\"");
            RInput.WriteLine("inMetFile <- \"" + Path.GetFileName(MetFileName).Replace('\\', '/') + "\"");
            RInput.WriteLine("outBaselineFile <- \"" + Path.GetFileName(BaseLineFileName).Replace('\\', '/') + "\"");
            RInput.WriteLine("outScenarioFile <- \"" + Path.GetFileName(NewMetFileName).Replace('\\', '/') + "\"");
            RInput.WriteLine("source(\"mkProjection.R\")");
            RInput.Close();

            // Copy the files that we will need to run this.
            File.Copy(QuantileTrendFileName, JobFolder + "\\" + Path.GetFileName(QuantileTrendFileName), true);
            File.Copy(Config.Setting("OzClimFolder") + "\\mkProjection.R", JobFolder + "\\mkProjection.R", true);
            File.Copy(Config.Setting("OzClimFolder") + "\\read.SILO.R", JobFolder + "\\read.SILO.R", true);

            if (AutoRun)
               {
               Process R = new Process();
               R.StartInfo.FileName = Config.Setting("RPath");
               R.StartInfo.Arguments = "--slave --quiet";
               R.StartInfo.UseShellExecute = false;
               R.StartInfo.WorkingDirectory = JobFolder;
               R.StartInfo.RedirectStandardOutput = true;
               R.StartInfo.RedirectStandardError = true;
               R.StartInfo.RedirectStandardInput = true;
               R.Start();
               StreamReader In = new StreamReader(JobFolder + "\\" + ScenarioName + ".R");
               R.StandardInput.Write(In.ReadToEnd());
               In.Close();
               R.StandardInput.Close();
               CSUtility.CheckProcessExitedProperly(R);
               
               string msg = R.StandardOutput.ReadToEnd();
               if (msg == "")
                  msg = R.StandardError.ReadToEnd();
               if (msg != "")
                  throw new Exception("From R: " + msg);
               R.Close();

               // remove temp files.
               File.Delete(BaseLineFileName);
               File.Delete(JobFolder + "\\" + Path.GetFileName(QuantileTrendFileName));
               File.Delete(JobFolder + "\\mkProjection.R");
               File.Delete(JobFolder + "\\read.SILO.R");
               File.Delete(JobFolder + "\\" + ScenarioName + ".R");
               }
            }
         return NewMetFileName;
         }

      private static string GeoLocate(double Latitude, double Longitude, string FileName)
         {
         if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

         char[] Delimiter = { ',' };
         StreamReader In = new StreamReader(FileName);
         In.ReadLine();
         In.ReadLine();
         In.ReadLine();
         In.ReadLine();
         In.ReadLine();

         bool Found = false;
         bool FoundLatitude = false;
         while (!Found && !In.EndOfStream)
            {
            string[] LineBits = In.ReadLine().Split(Delimiter, StringSplitOptions.RemoveEmptyEntries);
            if (LineBits.Length == 3)
               {
               if (!FoundLatitude)
                  {
                  if (Convert.ToDouble(LineBits[0]) - Latitude < 0.25)
                     FoundLatitude = true;
                  }
               else
                  {
                  if (Longitude - Convert.ToDouble(LineBits[1]) < 0.25)
                     {
                     In.Close();
                     if (LineBits[2] == "-9999.0")
                        throw new Exception("No OzClim data for latitude: " + Latitude.ToString() + " and longitude: " + Longitude.ToString());
                     return LineBits[2];
                     }
                  }
               }
            }
         throw new Exception("Cannot find cell in: " + FileName);
         }

      private static string FindClosestTo(double LatitudeToFind, double LongitudeToFind, Configuration Config)
         {
         double ClosestDistance = 100000;
         string ClosestFileName = "";
         foreach (string MetFileName in Directory.GetFiles(Config.Setting("OzClimFolder") + "\\BaseStations", "*.met"))
            {
            // Extract the latitude and longitude from the .met file.
            StreamReader MetFile = new StreamReader(MetFileName);
            StringCollection Constants = new StringCollection();
            StringCollection Headings = new StringCollection();
            APSIMInputFile.ReadApsimHeaderLines(MetFile, ref Constants, ref Headings);
            MetFile.Close();
            double Latitude = Convert.ToDouble(GetConstant(Constants, "Latitude"));
            double Longitude = Convert.ToDouble(GetConstant(Constants, "Longitude"));

            double Distance = Math.Abs(Latitude - LatitudeToFind) + Math.Abs(Longitude - LongitudeToFind);
            if (Distance < ClosestDistance)
               {
               ClosestDistance = Distance;
               ClosestFileName = MetFileName;
               }
            }
         if (ClosestFileName == "")
            throw new Exception("Cannot find a base station for latitude: " + LatitudeToFind.ToString() + " and longitude: " + LongitudeToFind.ToString());

         return ClosestFileName.Replace(".met", ".quantileTrendChange.csv");
         }

      }
   }
