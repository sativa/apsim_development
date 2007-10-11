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
    public partial class StatsUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public StatsUI()
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

            FieldList.ItemCheck -= OnItemCheck;
            StatsList.ItemCheck -= OnStatItemCheck;

            Data = Controller.ApsimData.Find(NodePath);
            FieldList.Items.Clear();
            FieldList.Items.AddRange(ParentUI.Processor.GetFieldNamesForDataSet(Data.get_ChildValue("source")));
            foreach (string FieldName in Data.get_Values("FieldName"))
                {
                int FieldIndex = FieldList.Items.IndexOf(FieldName);
                if (FieldIndex == -1)
                    Data.DeleteNode(Data.ChildByTypeAndValue("FieldName", FieldName));
                else
                    FieldList.SetItemChecked(FieldIndex, true);
                }
            foreach (string Stat in Data.get_Values("Stat"))
                {
                int StatIndex = StatsList.Items.IndexOf(Stat);
                if (StatIndex == -1)
                    Data.DeleteNode(Data.ChildByTypeAndValue("Stat", Stat));
                else
                    StatsList.SetItemChecked(StatIndex, true);
                }

            FieldList.ItemCheck += OnItemCheck;
            StatsList.ItemCheck += OnStatItemCheck;
            }


        private void OnItemCheck(object sender, ItemCheckEventArgs e)
            {
            if (e.NewValue == CheckState.Checked)
                {
                APSIMData NewField = new APSIMData("FieldName", "");
                NewField.Value = FieldList.Items[e.Index].ToString();
                Data.Add(NewField);
                }
            else
                Data.DeleteNode(Data.ChildByTypeAndValue("FieldName", FieldList.Items[e.Index].ToString()));
            ParentUI.DoRefresh(Data);
            }

        private void OnStatItemCheck(object sender, ItemCheckEventArgs e)
            {
            if (e.NewValue == CheckState.Checked)
                {
                APSIMData NewStat = new APSIMData("Stat", "");
                NewStat.Value = StatsList.Items[e.Index].ToString();
                Data.Add(NewStat);
                }
            else
                Data.DeleteNode(Data.ChildByTypeAndValue("Stat", StatsList.Items[e.Index].ToString()));
            ParentUI.DoRefresh(Data);
            }

        }
    }

