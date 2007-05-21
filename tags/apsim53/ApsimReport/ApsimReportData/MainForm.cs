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
using VBUserInterface;

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
            if (CommandLine == "")
                MessageBox.Show("Invalid command line to ApsimReportData: " + CommandLine);
            else
                {
                UInt32 DataContainer = Convert.ToUInt32(CommandLine);
                Graph = new GraphController(SmallImages, DataContainer);
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
            ToolboxExplorer = new ExplorerUI(null);
            ToolboxExplorer.Parent = ToolboxPanel;
            ToolboxExplorer.Dock = DockStyle.Fill;
            ToolboxExplorer.Visible = true;
            APSIMData ToolBoxData = new APSIMData();
            ToolBoxData.LoadFromFile(APSIMSettings.ApsimDirectory() + "\\apsimui\\graph.xml");
            Toolbox = new GraphController(SmallImages, ToolBoxData);
            Toolbox.FileNew(ToolBoxData);
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

        private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
            {
            e.Cancel = false;
            DataExplorer.SaveCurrentView();
            ToolboxExplorer.SaveCurrentView();
            Graph.Save();
            }

        }
    }