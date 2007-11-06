using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using VBUserInterface;
using CSGeneral;

namespace Graph
    {
    public partial class FilterUI : VBUserInterface.BaseView
        {
        private ChartPageUI ParentUI;

        public FilterUI()
            {
            InitializeComponent();
            }

        protected override void OnLoad()
            {
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();
            GroupBox.Text = Name;
            FilterBox.Text = XmlHelper.Value(Data, "FilterString");
            }

        private void OnFilterChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "FilterString", FilterBox.Text);
            PublishViewChanged(Data);
            }


        }
    }

