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
    public partial class RecordFilterUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public RecordFilterUI()
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

            FirstRecordCheck.CheckedChanged -= OnFirstRecordChanged;
            LastRecordCheck.CheckedChanged -= OnLastRecordChanged;
            RecordNumberEdit.TextChanged -= OnRecordNumberChanged;

            FirstRecordCheck.Checked = (Data.get_ChildValue("FirstRecord") == "yes");
            LastRecordCheck.Checked = (Data.get_ChildValue("LastRecord") == "yes");
            RecordNumberEdit.Text = Data.get_ChildValue("RecordNumber");

            FirstRecordCheck.CheckedChanged += OnFirstRecordChanged;
            LastRecordCheck.CheckedChanged += OnLastRecordChanged;
            RecordNumberEdit.TextChanged += OnRecordNumberChanged;
            }


        private void OnFirstRecordChanged(object sender, EventArgs e)
            {
            string YesNo;
            if (FirstRecordCheck.Checked)
                YesNo = "Yes";
            else
                YesNo = "No";
            Data.set_ChildValue("FirstRecord", YesNo);
            ParentUI.DoRefresh(Data);
            }
        private void OnLastRecordChanged(object sender, EventArgs e)
            {
            string YesNo;
            if (LastRecordCheck.Checked)
                YesNo = "Yes";
            else
                YesNo = "No";
            Data.set_ChildValue("LastRecord", YesNo);
            ParentUI.DoRefresh(Data);
            }
        private void OnRecordNumberChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("RecordNumber", RecordNumberEdit.Text);
            ParentUI.DoRefresh(Data);
            }



        }
    }

