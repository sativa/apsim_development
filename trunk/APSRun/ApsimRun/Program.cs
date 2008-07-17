using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ApsimRun;
using CSGeneral;
using System.IO;
using ApsimFile;
using CSUserInterface;

namespace ApsimRun
   {
   static class Program
      {
      /// <summary>
      /// The main entry point for the application.
      /// </summary>
      [STAThread]
      static void Main(string[] Args)
         {
         Application.EnableVisualStyles();
         Application.SetCompatibleTextRenderingDefault(false);

         if (SingleApplicationInstance.NoPreviousInstance("apsimrun", Args))
            {
            SimulationRunnerForm MainForm = new SimulationRunnerForm(Args);
            Application.Run(MainForm);
            }
         }


      }
   }