using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using VBUserInterface;

namespace Graph
    {
    public partial class ApsimReportDataForm : Form
        {
        private DataProcessor Processor;
        public ApsimReportDataForm()
            {
            InitializeComponent();
            }

        public void Go(string CommandLine)
            {
            if (CommandLine == "")
                MessageBox.Show("Invalid command line to ApsimReportData: " + CommandLine);
            else
                {
                Application.EnableVisualStyles();
                Application.DoEvents();
                Application.DoEvents();

                Processor = new DataProcessor(Convert.ToUInt32(CommandLine));
                ChartPage.Processor = Processor;

                APSIMData Data = new APSIMData(Processor.XML());
                BaseController Controller = new BaseController(null, "ApsimUI");
                Controller.LoadFromXML(Data.XML);
                ChartPage.OnLoad(Controller, Data.FullPath);
                ChartPage.OnRefresh();
                this.ShowDialog();
                }
            }

        private void OnFormClosing(object sender, FormClosingEventArgs e)
            {
            ChartPage.OnSave();
            }

        }
    }