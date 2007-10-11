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
    public partial class FilterUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public FilterUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();
            GroupBox.Text = Name;
            Data = Controller.ApsimData.Find(NodePath);
            FilterBox.Text = Data.get_ChildValue("FilterString");
            }

        private void OnFilterChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("FilterString", FilterBox.Text);
            ParentUI.DoRefresh(Data);
            }


        }
    }

