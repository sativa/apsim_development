using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;
using System.IO;
using System.Net;
using ApsimFile;

namespace Tools
   {
   public class ConvertSimsFromSILOToMetFile
      {
      static int Main(string[] args)
         {
         try
            {
            if (args.Length == 1)
               Go(args[0], -1);
            else if (args.Length == 2)
               Go(args[0], Convert.ToInt32(args[1]));
            else if (args.Length != 2)
               throw new Exception("Usage: ConvertSimsFromSILOToMetFile ApsimFile Year");
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      public static void Go(string ApsimFile, int Year)
         {
         ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
         Apsim.OpenFile(ApsimFile);

         Go(Apsim, Year);
         Apsim.Save();
         }
      public static void Go(ApsimFile.ApsimFile ApsimFile, int Year)
         {
         foreach (Component Simulation in ApsimFile.RootComponent.ChildNodes)
            {
            Component SILO = Simulation.Find("met");
            if (SILO == null)
               Console.WriteLine("No SILO met file found in simulation: " + Simulation.Name);
            else
               {
               string StationNumber = XmlHelper.Value(SILO.ContentsAsXML, "station_number");

               // Extract the met data out of SILO
               DateTime Start;
               DateTime End;
               if (Year != -1)
                  {
                  Start = new DateTime(Year, 1, 1);
                  End = new DateTime(Year + 1, 3, 1);
                  }
               else
                  {
                  Start = new DateTime(1890, 1, 1);
                  End = DateTime.Today;
                  }
               string MetFileName = ExtractMetFromSILO(StationNumber, Start, End, Path.GetDirectoryName(ApsimFile.FileName));

               SILO.Type = "metfile";
               SILO.Name = "met";
               SILO.Contents = "<met><filename>" + Path.GetFileName(MetFileName) + "</filename></met>";
               }
            }
         }
      private static string ExtractMetFromSILO(string StationNumber, DateTime Start, DateTime End, string ResultsDirectory)
         {
         string MetFileName = ResultsDirectory + "\\" + StationNumber + ".met";
         StreamWriter Out = new StreamWriter(MetFileName);

         string RequestString = "http://192.168.0.60/cgi-bin/getData.tcl?format=apsim&station=" +
                                StationNumber.ToString() +
                                "&ddStart=" + Start.Day.ToString() +
                                "&mmStart=" + Start.Month.ToString() +
                                "&yyyyStart=" + Start.Year.ToString() +
                                "&ddFinish=" + End.Day.ToString() +
                                "&mmFinish=" + End.Month.ToString() +
                                "&yyyyFinish=" + End.Year.ToString();

         HttpWebRequest SILO = (HttpWebRequest)WebRequest.Create(RequestString);
         HttpWebResponse SILOResponse = (HttpWebResponse)SILO.GetResponse();
         Stream StreamResponse = SILOResponse.GetResponseStream();
         StreamReader In = new StreamReader(StreamResponse);

         // Reads 256 characters at a time.    
         Char[] read = new Char[256];
         int count = In.Read(read, 0, 256);
         while (count > 0)
            {
            // Dumps the 256 characters on a string and displays the string to the console.
            Out.Write(new String(read, 0, count));
            count = In.Read(read, 0, 256);
            }

         // Releases the resources of the response.
         SILOResponse.Close();

         // Releases the resources of the Streams.
         In.Close();
         Out.Close();

         return MetFileName;
         }
      }
   }
