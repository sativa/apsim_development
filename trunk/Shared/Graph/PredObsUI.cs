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
    public partial class PredObsUI : BaseView
        {
        private APSIMData Data;
        private bool InRefresh = false;
        private ChartPageUI ParentUI;

        public PredObsUI()
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

            InRefresh = true;
            GroupBox.Text = Name;
            Data = Controller.ApsimData.Find(NodePath);
            string DataSource = Data.get_ChildValue("Source");
            string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);

            // setup the Field list.
            FieldList.Items.Clear();
            FieldList.Items.AddRange(FieldNames);
            foreach (string FieldName in Data.get_Values("FieldName"))
                {
                int PosItem = FieldList.Items.IndexOf(FieldName);
                if (PosItem != -1)
                    FieldList.SetItemChecked(PosItem, true);
                else
                    Data.DeleteNode(Data.ChildByTypeAndValue("FieldName", FieldName));
                }
            InRefresh = false; 
            }

        private void OnFieldListChanged(object sender, ItemCheckEventArgs e)
            {
            if (!InRefresh)
                {
                string FieldName = FieldList.Items[e.Index].ToString();
                if (e.NewValue == CheckState.Checked)
                    {
                    APSIMData NewField = new APSIMData("FieldName", "");
                    NewField.Value = FieldName;
                    Data.Add(NewField);
                    }
                else
                    Data.DeleteNode(Data.ChildByTypeAndValue("FieldName", FieldName));
                ParentUI.DoRefresh(Data);
                }
            }

        }
    }

