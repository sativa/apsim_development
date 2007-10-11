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
    public partial class REMSUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public REMSUI()
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

            FileNameEdit.TextChanged -= OnFileNameChanged;
            ExperimentDropDown.TextChanged -= OnExperimentChanged;
            TreatmentDropDown.TextChanged -= OnTreatmentChanged;
            DataSourceDropDown.TextChanged -= OnDataSourceChanged;

            Data = Controller.ApsimData.Find(NodePath);

            FileNameEdit.Text = Data.get_ChildValue("FileName");
            ExperimentDropDown.Items.Clear();
            ExperimentDropDown.Items.AddRange(ParentUI.Processor.GetExperimentNames(FileNameEdit.Text));
            ExperimentDropDown.Text = Data.get_ChildValue("Experiment");

            TreatmentDropDown.Items.Clear();
            TreatmentDropDown.Items.AddRange(ParentUI.Processor.GetTreatmentNames(FileNameEdit.Text, ExperimentDropDown.Text));
            TreatmentDropDown.Text = Data.get_ChildValue("Treatment");

            DataSourceDropDown.Text = Data.get_ChildValue("DataSource");

            FileNameEdit.TextChanged += OnFileNameChanged;
            ExperimentDropDown.TextChanged += OnExperimentChanged;
            TreatmentDropDown.TextChanged += OnTreatmentChanged;
            DataSourceDropDown.TextChanged += OnDataSourceChanged;
            }

        private void OnFileNameChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("FileName", FileNameEdit.Text);
            ParentUI.DoRefresh(Data);
            }
        private void OnExperimentChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("Experiment", ExperimentDropDown.Text);
            ParentUI.DoRefresh(Data);
            }
        private void OnTreatmentChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("Treatment", TreatmentDropDown.Text);
            ParentUI.DoRefresh(Data);
            }
        private void OnDataSourceChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("DataSource", DataSourceDropDown.Text);
            ParentUI.DoRefresh(Data);
            }

        }
    }

