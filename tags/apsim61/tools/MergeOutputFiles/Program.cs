using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.IO;
using CSGeneral;

namespace MergeOutputFiles
   {
   class MergeOutputs
      {
      static void Main(string[] args)
         {
         try
            {
            if (args.Length != 1)
               throw new Exception("Usage: MergeOutputFiles Directory");
            Go(args[0], "*.out", args[0] + "\\All.out");

            // Remove all outputs.
            foreach (string FileName in Directory.GetFiles(args[0], "*.out"))
               {
               if (Path.GetFileName(FileName) != "All.out")
                  File.Delete(FileName);
               }
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            Console.ReadLine();
            }

         }

      private static void Go(string Dir, string FileSpec, string DestinationFile)
         {
         StreamWriter Out = new StreamWriter(DestinationFile);
         bool First = true;
         foreach (string FileName in Directory.GetFiles(Dir, FileSpec))
            {
            if (Path.GetFileName(FileName) != Path.GetFileName(DestinationFile))
               {
               StreamReader In = new StreamReader(FileName);
               StringCollection ConstantLines = new StringCollection();
               StringCollection HeadingLines = new StringCollection();
               APSIMInputFile.ReadApsimHeaderLines(In, ref ConstantLines, ref HeadingLines);

               const int TitleFieldSize = 60;
               string Title = "";
               foreach (string ConstantLine in ConstantLines)
                  {
                  if (ConstantLine.IndexOf("Title = ") == -1)
                     {
                     if (First)
                        Out.WriteLine(ConstantLine);
                     }
                  else
                     Title = ConstantLine.Substring(8);
                  if (First)
                     {
                     Out.WriteLine(new string(' ', TitleFieldSize-8) + "FileName" + HeadingLines[0]);
                     Out.WriteLine(new string(' ', TitleFieldSize-2) + "()" + HeadingLines[1]);
                     First = false;
                     }
                  }
               if (Title.Length >= TitleFieldSize)
                  Title = Title.Substring(0, TitleFieldSize-1);
               Title = "\"" + Title + "\"";
               Title = new string(' ', TitleFieldSize - Title.Length) + Title;

               string Line = In.ReadLine();
               while (Line != null && Line != "")
                  {
                  Out.WriteLine(Title + Line);
                  Line = In.ReadLine();
                  }

               In.Close();
               }
            }

         Out.Close();
         }
      }
   }
