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
        private string ParentDataPath;
        private string DataPath;
        private UInt32 DataWindow = 0;

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
            GraphController Graph = (GraphController)Controller;
            Graph.DeleteDataWindow(DataWindow);
            }

        public override void RefreshView(BaseController Controller)
            {
            base.RefreshView(Controller);

            GenericUI.PropertiesChangedEvent -= new GenericUI.NotifyEventHandler(OnPropertiesChanged);
            GenericUI.PropertiesChangedEvent += new GenericUI.NotifyEventHandler(OnPropertiesChanged);

            GraphController Graph = (GraphController)Controller;
            if (Controller.AllowDataChanges)
                Graph.SetProperties(Controller.AllData.FullPath, Controller.AllData.XML);

            PopulateView();
            }

        private void PopulateView()
            {
            GraphController Graph = (GraphController)Controller;
            ParentDataPath = Controller.Data.Parent.FullPath + "\\";
            DataPath = Controller.Data.FullPath;

            GenericUI.RefreshView(Controller);

            if (DataWindow == 0)
                DataWindow = Graph.CreateDataWindow(DataPanel.Handle);

            Graph.RefreshDataWindow(DataWindow, DataPath);
            HelpText = Graph.GetErrorMessage(DataPath);

            DataPanel_Resize(null, null);
            }

        public void OnPropertiesChanged()
            {
            GraphController Graph = (GraphController)Controller;
            Graph.SetProperties(Controller.Data.FullPath, Controller.Data.XML);
            PopulateView();
            }

        public override void Save()
            {
            base.Save();
            GenericUI.PropertiesChangedEvent -= new GenericUI.NotifyEventHandler(OnPropertiesChanged);
            }

        private void DataPanel_Resize(object sender, EventArgs e)
            {
            if (DataWindow != 0)
                {
                GraphController Graph = (GraphController)Controller;
                Graph.SizeDataWindow(DataWindow, DataPanel.Width, DataPanel.Height);
                }
            }

        }
    }

