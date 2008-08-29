using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml;
using CSGeneral;

namespace Tools
   {
   class ExtractXMLElements
      {
      static int Main(string[] args)
         {
         try
            {
            if (args.Length != 3)
               throw new Exception("Usage: ExtractXMLElements XmlFile ElementNamesFile DestinationFile");
            Go(args[0], args[1], args[2]);
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      private static void Go(string XmlFileName, string ElementNamesFile, string DestinationFile)
         {
         XmlDocument InDoc = new XmlDocument();
         InDoc.Load(XmlFileName);
         
         XmlDocument OutDoc = new XmlDocument();
         OutDoc.AppendChild(OutDoc.CreateElement(InDoc.DocumentElement.Name));
         if (XmlHelper.Attribute(InDoc.DocumentElement, "version") != "")
            XmlHelper.SetAttribute(OutDoc.DocumentElement, "version", XmlHelper.Attribute(InDoc.DocumentElement, "version"));

         StreamReader Names = new StreamReader(ElementNamesFile);

         string Name = Names.ReadLine();
         while (Name != null && Name != "")
            {
            XmlNode Element = XmlHelper.Find(InDoc.DocumentElement, Name);
            if (Element == null)
               Console.WriteLine("Cannot find element: " + Name);
            else
               OutDoc.DocumentElement.AppendChild(OutDoc.ImportNode(Element, true));
            Name = Names.ReadLine();
            }


         OutDoc.Save(DestinationFile);
         }
      }
   }
