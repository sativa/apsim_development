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
        private string FileName;
        private GraphController Graph;
        private ExplorerUI DataExplorer;
        private GraphController Toolbox;
        private ExplorerUI ToolboxExplorer;

        public MainForm()
            {
            InitializeComponent();
            }

        public void LoadFile(string CommandFileName)
            {
            // Load the specified file. Called when a command line arg is used.
            FileName = CommandFileName.Replace("\"", "");
            }

        private void MainForm_Load(object sender, System.EventArgs e)
            {
            // Show the Simulation Explorer.
            DataExplorer = new ExplorerUI(null);
            DataExplorer.Parent = MainPanel;
            DataExplorer.Dock = DockStyle.Fill;
            DataExplorer.Visible = true;

            // Form has been loaded - set everything up
            Graph = new GraphController(SmallImages);
            
            // Load up the file from the command line if necessary.
            if (FileName != "")
                {
                Graph.FileOpen(FileName);
                DataExplorer.RefreshView(Graph);
                }

            // Setup but don't show the Toolbox Explorer.
            Toolbox = new GraphController(SmallImages);
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

        private void OkButton_Click(object sender, EventArgs e)
            {
            // write back to the file
            Graph.FileSave();
            Close();
            }

        private void CancelBut_Click(object sender, EventArgs e)
            {
            File.Delete(FileName);
            Close();
            }

        }
    }