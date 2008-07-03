using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;
using System.IO;
using System.Net;

namespace ConvertSimsFromSILOToMetFile
   {
   class RemoveSILO
      {
      static void Main(string[] args)
         {
         try
            {
            if (args.Length != 2)
               throw new Exception("Usage: ConvertSimsFromSILOToMetFile ApsimFile Year");
            Go(args[0], Convert.ToInt32(args[1]));
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            Console.ReadLine();
            }
         }

      private static void Go(string ApsimFile, int Year)
         {
         XmlDocument Doc = new XmlDocument();
         Doc.Load(ApsimFile);
         foreach (XmlNode Simulation in Doc.DocumentElement)
            {
            XmlNode SILO = XmlHelper.FindByType(Simulation, "siloinput");
            if (SILO == null)
               Console.WriteLine("No SILO met file found in simulation: " + XmlHelper.Name(Simulation));
            else
               {
               string StationNumber = XmlHelper.Value(SILO, "station_number");
               string DestDirectory = Path.GetDirectoryName(ApsimFile);
               if (DestDirectory == "")
                  DestDirectory = Directory.GetCurrentDirectory();

               XmlNode Met = Simulation.InsertBefore(Doc.CreateElement("metfile"), SILO);
               XmlHelper.SetName(Met, "met");

               Simulation.RemoveChild(SILO);

               // Extract the met data out of SILO
               DateTime Start = new DateTime(Year, 1, 1);
               DateTime End = new DateTime(Year + 1, 3, 1);
               string MetFileName = ExtractMetFromSILO(StationNumber, Start, End, DestDirectory);

               XmlHelper.SetValue(Met, "filename", Path.GetFileName(MetFileName));
               }
            }
         Doc.Save(ApsimFile);
         
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
