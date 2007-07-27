using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using VBUserInterface;
using System.Collections.Specialized;


namespace Graph
    {
    public partial class GraphDataUI : BaseView
        {
        private UInt32 DataWindow = 0;
        private GraphController GraphController;
        private string NodePath;

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

        public override void OnLoad(BaseController Controller)
            {
            if (Controller is GraphController)
                this.GraphController = (GraphController)Controller;
            else
                {
                // Need to go and find the root node for all graph data.
                string ThisPath = "";
                APSIMData GraphData = Controller.Data;
                while (GraphData.Type != "Data" && GraphData.Parent != null)
                    {
                    if (ThisPath != "")
                        ThisPath = GraphData.Name + "\\" + ThisPath;
                    else
                        ThisPath = GraphData.Name;
                    GraphData = GraphData.Parent;
                    }
                ThisPath = "Data\\" + ThisPath;

                // Give all graph data to a newly created graphcontroller.
                if (Controller is GraphController)
                    this.GraphController = (GraphController)Controller;
                else
                    {
                    this.GraphController = new GraphController(Controller.IconImageList("SmallIcon"), GraphData);
                    StringCollection Selections = new StringCollection();
                    Selections.Add(ThisPath);
                    GraphController.SelectedPaths = Selections;
                    }
                }
            GenericUI.OnLoad(Controller);
            }
        public override void OnRefresh(string NodePath)
            {
            // ----------------------------------------------------------
            // Refresh this graphdata.
            // ----------------------------------------------------------
            this.NodePath = NodePath;

            // Capture event that occurs when user changes properties.
            GenericUI.PropertiesChangedEvent -= new GenericUI.NotifyEventHandler(OnPropertiesChanged);
            GenericUI.PropertiesChangedEvent += new GenericUI.NotifyEventHandler(OnPropertiesChanged);

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
            GenericUI.OnRefresh(NodePath);

            // refresh our data window
            if (GraphController.Data != null)
                {
                GraphController.RefreshDataWindow(DataWindow, GraphController.Data.FullPath);

                // refresh our help text.
                HelpText = GraphController.GetErrorMessage(GraphController.Data.FullPath);
                }

            DataPanel_Resize(null, null);
            }

        public void OnPropertiesChanged()
            {
            // ----------------------------------------------------------
            // User has changed our properties - refresh everything.
            // ----------------------------------------------------------

            // give new properties to our graph controller.
            GraphController.Save();

            PopulateView();
            }

        public override void OnSave()
            {
            // ----------------------------------------------------------
            // We're about to close so remove our interest in the 
            // property changed event.
            // ----------------------------------------------------------
            OnPropertiesChanged();
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

