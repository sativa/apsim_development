using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using CSUserInterface;
using VBGeneral;
using System.IO;

namespace Graph
    {
    public partial class TabbedPageUI : BaseView
        {
        private DataProcessor DataProcessor;
        private APSIMData Data;

        public TabbedPageUI()
            {
            InitializeComponent();
            }

        public DataProcessor Processor
            {
            get { return DataProcessor; }
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            DataProcessor = new DataProcessor();

            TabControl.TabPages.Clear();
            Data = Controller.ApsimData.Find(NodePath);
            foreach (APSIMData Page in Data.get_Children("page"))
                {
                TabPage NewTabPage = new TabPage(Page.Name);
                ChartPageUI NewCanvas = new ChartPageUI();
                NewCanvas.Parent = NewTabPage;
                NewCanvas.Dock = DockStyle.Fill;
                TabControl.TabPages.Add(NewTabPage);
                NewCanvas.Processor = DataProcessor;
                NewCanvas.OnLoad(Controller, Page.FullPath);
                }
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();
            foreach (TabPage Page in TabControl.TabPages)
                {
                foreach (ChartPageUI ChartPage in Page.Controls)
                    ChartPage.OnRefresh();
                }
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
            foreach (TabPage Page in TabControl.TabPages)
                {
                ChartPageUI Canvas = (ChartPageUI)Page.Controls[0];
                Canvas.OnSave();
                }
            if (DataProcessor != null)
                {
                DataProcessor.Shutdown();
                DataProcessor = null;
                }
            }

        }
    }

