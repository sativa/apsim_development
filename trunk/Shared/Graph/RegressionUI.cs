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
    public partial class RegressionUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public RegressionUI()
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
            
            XDropDown.TextChanged -= OnXChanged;
            YDropDown.TextChanged -= OnYChanged;

            Data = Controller.ApsimData.Find(NodePath);
            XDropDown.Items.Clear();
            XDropDown.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(Data.get_ChildValue("source")));
            YDropDown.Items.Clear();
            YDropDown.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(Data.get_ChildValue("source")));
            XDropDown.Text = Data.get_ChildValue("XFieldName");
            YDropDown.Text = Data.get_ChildValue("YFieldName");

            XDropDown.TextChanged += OnXChanged;
            YDropDown.TextChanged += OnYChanged;
            }

        private void OnXChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("XFieldName", XDropDown.Text);
            ParentUI.DoRefresh(Data);
            }

        private void OnYChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("YFieldName", YDropDown.Text); 
            ParentUI.DoRefresh(Data);
            }


        }
    }

