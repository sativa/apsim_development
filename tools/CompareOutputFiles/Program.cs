using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using CSGeneral;
using System.Diagnostics;
using System.Xml;
using System.IO;

namespace CompareOutputFiles
   {
   class Program
      {
      static int RequiredSigFigs = 5;

      static int Main(string[] args)
         {
         try
            {
            StreamReader File2;
            if (args.Length == 1)
               File2 = GetTipVersionFromSVN(args[0]);
            else if (args.Length == 2)
               File2 = new StreamReader(args[1]);
            else
               throw new Exception("Usage: CompareOutputFiles File1 File2");

            StreamReader File1 = new StreamReader(args[0]);
            Compare(File1, File2);
            return 0;
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            return 1;
            }

         }

      private static StreamReader GetTipVersionFromSVN(string FileName)
         {
         Process SVN = Process.Start("svn cat " + FileName);
         SVN.WaitForExit();
         return SVN.StandardOutput;
         }

      private static void Compare(StreamReader FileStream1, StreamReader FileStream2)
         {
         APSIMInputFile File1 = new APSIMInputFile();
         File1.ReadFromFile(FileName1);

         APSIMInputFile File2 = new APSIMInputFile();
         File2.ReadFromFile(FileName2);

         CheckConstants(File1, File2);
         for (int r = 0; r != File1.Data.Rows.Count; r++)
            {
            for (int c = 0; c != File1.Data.Columns.Count; c++)
               CompareValue(File1.Data.Rows[r][c].ToString(), File2.Data.Rows[r][c].ToString(),
                            File1.Data.Columns[c].ColumnName);
            }
         }

      private static void CheckConstants(APSIMInputFile File1, APSIMInputFile File2)
         {
         // Check that the constants are the same.
         if (File1.Constants.Count != File2.Constants.Count)
            throw new Exception("The number of constants are different");
         for (int i = 0; i != File1.Constants.Count; i++)
            {
            APSIMConstant Constant1 = (APSIMConstant)File1.Constants[i];
            APSIMConstant Constant2 = (APSIMConstant)File2.Constants[i];
            if (Constant1.Name != Constant2.Name)
               throw new Exception("Constant names don't match");
            CompareValue(Constant1.Value, Constant2.Value, Constant1.Name);
            if (Constant1.Units != Constant2.Units)
               throw new Exception("Constant units don't match");
            }

         }

      private static void CompareValue(string Value1, string Value2, string ValueName)
         {
         double DoubleValue1;
         bool Convert1 = Double.TryParse(Value1, out DoubleValue1);
         if (Convert1)
            {
            // Number was numerical.
            double DoubleValue2;
            if (!Double.TryParse(Value2, out DoubleValue2))
               throw new Exception("Value " + ValueName + " is different");
            if (DoubleValue1 >= 0)
               {
               int SigFigs1 = (int) Math.Log10(DoubleValue1) + 1;
               int SigFigs2 = (int) Math.Log10(DoubleValue2) + 1;
               if (SigFigs1 != SigFigs2)
                  throw new Exception("Different number of significant figures in " + ValueName);
               int SigFigsToAdd = RequiredSigFigs - SigFigs1;

               int IntValue1;
               int IntValue2;
               IntValue1 = (int) (DoubleValue1 * Math.Pow(10, SigFigsToAdd));
               IntValue2 = (int) (DoubleValue2 * Math.Pow(10, SigFigsToAdd));
               if (IntValue1 != IntValue2)
                  throw new Exception("Numerically, " + ValueName + " is significantly different. Value1 = " + Value1 + ". Value2 = " + Value2);
               }
            }
         else if (Value1 != Value2)
            throw new Exception(ValueName + " is different. Value1 = " + Value1 + ". Value2 = " + Value2);

         }

      

      
      }

   }
