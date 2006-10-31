using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using Graph;
using VBGeneral;

namespace ApsimReportData
    {
    public partial class MainForm : Form
       {
        private GraphController Graph;
        private ExplorerUI DataExplorer;
        private GraphController Toolbox;
        private ExplorerUI ToolboxExplorer;

        public MainForm()
            {
            InitializeComponent();
            }

        public void Go(string CommandLine)
            {
            int PosComma = CommandLine.IndexOf(',');
            if (PosComma == -1)
                MessageBox.Show("Invalid command line to ApsimReportData: " + CommandLine);
            else
                {
                UInt32 DataContainer = Convert.ToUInt32(CommandLine.Substring(0, PosComma));
                string xml = CommandLine.Substring(PosComma + 1);
                Graph = new GraphController(SmallImages, DataContainer);
                Graph.AllData = new APSIMData(xml);
                this.Show();
                Application.Run(this);
                }
            }

        private void MainForm_Load(object sender, System.EventArgs e)
            {
            // Show the Simulation Explorer.
            DataExplorer = new ExplorerUI(null);
            DataExplorer.Parent = MainPanel;
            DataExplorer.Dock = DockStyle.Fill;
            DataExplorer.Visible = true;

            DataExplorer.RefreshView(Graph);

            // Setup but don't show the Toolbox Explorer.
            Toolbox = new GraphController(SmallImages, 0);
            ToolboxExplorer = new ExplorerUI(null);
            ToolboxExplorer.Parent = ToolboxPanel;
            ToolboxExplorer.Dock = DockStyle.Fill;
            ToolboxExplorer.Visible = true;
            Toolbox.FileOpen(APSIMSettings.ApsimDirectory() + "\\apsimui\\graph.xml");
            ToolboxExplorer.RefreshView(Toolbox);
            }

        private void ShowHideToolboxButton_Click(object sender, EventArgs e)
            {
            Splitter.Visible = ShowHideToolboxButton.Checked;
            ToolboxLabel.Visible = ShowHideToolboxButton.Checked;
            BottomPanel.Visible = ShowHideToolboxButton.Checked;
            }

        private void CloseButtonClick(object sender, EventArgs e)
            {
            Close();
            }

        }
    }