using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;

namespace Graph
    {
    public partial class SplitScreenPageUI : BaseView
        {
        private DataProcessor DataProcessor;
        private APSIMData Data;

        public SplitScreenPageUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            DataProcessor = new DataProcessor();
            Data = Controller.ApsimData.Find(NodePath);
            if (Data.get_Children("page").Length >= 1)
                {
                Page1.Processor = DataProcessor;
                Page1.OnLoad(Controller, Data.get_Children("page")[0].FullPath);
                }
            if (Data.get_Children("page").Length == 2)
                {
                Page2.Processor = DataProcessor;
                Page2.OnLoad(Controller, Data.get_Children("page")[1].FullPath);
                }
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();

            if (Data.get_Children("page").Length >= 1)
                Page1.OnRefresh();
            if (Data.get_Children("page").Length == 2)
                Page2.OnRefresh();
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
            Page1.OnSave();
            Page2.OnSave();
            if (DataProcessor != null)
                {
                DataProcessor.Shutdown();
                DataProcessor = null;
                }
            }        
        
        
        }
    }

