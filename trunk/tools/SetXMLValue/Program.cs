using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.Xml;

namespace SetXMLValue
   {
   class Program
      {
      static void Main(string[] args)
         {
         try
            {
            if (args.Length != 3)
               Console.WriteLine("SetXMLValue FileName XMLPath NewValue");
            else
               {
               XmlDocument Doc = new XmlDocument();
               Doc.Load(args[0]);
               XmlHelper.SetValue(Doc.DocumentElement, args[1], args[2]);
               Doc.Save(args[0]);
               }
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            }
         }
      }
   }
