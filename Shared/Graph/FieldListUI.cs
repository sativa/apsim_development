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
    public partial class FieldListUI : BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public FieldListUI()
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

            FieldList.ItemCheck += OnItemCheck;
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
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

        }
    }

