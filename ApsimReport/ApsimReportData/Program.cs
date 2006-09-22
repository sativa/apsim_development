using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace ApsimReportData
    {
    static class Program
        {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string[] args)
            {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            MainForm Main = new MainForm();
            if (args.Length == 1)
                Main.LoadFile(args[0]);
            Application.Run(Main);
            }
        }
    }