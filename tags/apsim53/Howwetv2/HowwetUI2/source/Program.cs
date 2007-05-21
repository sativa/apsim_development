using System;
using System.Collections.Generic;
using System.Windows.Forms;
using Microsoft.Samples.Windows.Forms.Navigation;

namespace APSRU.Howwet
    {
    static class Program
        {
            /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        public static void Main()
            {
         //   Application.EnableVisualStyles();
         //   Application.SetCompatibleTextRenderingDefault(false);
            Explorer e = new Explorer();
            Page homePage = new Home();
            e.Go(typeof(Home));
            e.Go(typeof(FallowSettings));
            e.Go(typeof(Results));
            e.Go(typeof(Graphs));
            e.Go(typeof(NRequirement));
            e.Go(typeof(Home));
            e.Startup();
            Application.Run(e);
            }
        
        }
    }