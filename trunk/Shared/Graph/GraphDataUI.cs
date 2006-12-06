using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;


namespace Graph
    {
    public partial class GraphDataUI : VBGeneral.BaseView
        {
        private string DataPath;
        private UInt32 DataWindow = 0;
        private GraphController GraphController;

        public GraphDataUI()
            {
            InitializeComponent();
            }
        protected override void Dispose(bool disposing)
            {
            // Clean up any resources being used.
            if (disposing && (components != null))
                {
                components.Dispose();
                }
            base.Dispose(disposing);
            }

        public override void RefreshView(BaseController Controller)
            {
            // ----------------------------------------------------------
            // Refresh this graphdata.
            // ----------------------------------------------------------
            base.RefreshView(Controller);

            // Capture event that occurs when user changes properties.
            GenericUI.PropertiesChangedEvent -= new GenericUI.NotifyEventHandler(OnPropertiesChanged);
            GenericUI.PropertiesChangedEvent += new GenericUI.NotifyEventHandler(OnPropertiesChanged);

            // Need to go and find the root node for all graph data.
            APSIMData GraphData = Controller.Data.Parent;
            while (GraphData.Type != "Data" && GraphData.Parent != null)
                GraphData = GraphData.Parent;

            // Give all graph data to a newly created graphcontroller.
            this.GraphController = new GraphController(Controller, GraphData.FullPath);

            // Work out a data path relative to the root node data path.
            DataPath = Controller.Data.FullPath.Replace(GraphData.FullPath + "\\", "");

            // Create a data window if necessary.
            if (DataWindow == 0)
                DataWindow = GraphController.CreateDataWindow(DataPanel.Handle);

            PopulateView();
            }

        private void PopulateView()
            {
            // ----------------------------------------------------------
            // Populate this control.
            // ----------------------------------------------------------

            // refresh our genericUI
            GenericUI.RefreshView(GraphController);

            // refresh our data window
            GraphController.RefreshDataWindow(DataWindow, DataPath);

            // refresh our help text.
            HelpText = GraphController.GetErrorMessage(DataPath);

            DataPanel_Resize(null, null);
            }

        public void OnPropertiesChanged()
            {
            // ----------------------------------------------------------
            // User has changed our properties - refresh everything.
            // ----------------------------------------------------------

            // give new properties to our graph controller.
            GraphController.SetProperties(DataPath, Controller.Data.XML);

            PopulateView();
            }

        public override void Save()
            {
            // ----------------------------------------------------------
            // We're about to close so remove our interest in the 
            // property changed event.
            // ----------------------------------------------------------
            base.Save();
            GenericUI.PropertiesChangedEvent -= new GenericUI.NotifyEventHandler(OnPropertiesChanged);
            if (DataWindow != 0)
                {
                GraphController.DeleteDataWindow(DataWindow);
                DataWindow = 0;
                }
            }

        private void DataPanel_Resize(object sender, EventArgs e)
            {
            // ----------------------------------------------------------
            // User has resized window - resize our data window
            // ----------------------------------------------------------
            if (DataWindow != 0)
                GraphController.SizeDataWindow(DataWindow, DataPanel.Width, DataPanel.Height);
            }

        }
    }

