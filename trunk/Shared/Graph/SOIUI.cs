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
    public partial class SOIUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public SOIUI()
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
            MonthDropDown.TextChanged -= OnMonthChanged;
            PhaseList.ItemCheck -= OnPhaseItemCheck;

            Data = Controller.ApsimData.Find(NodePath);
            FileNameEdit.Text = Data.get_ChildValue("FileName");
            MonthDropDown.Text = Data.get_ChildValue("Month");

            foreach (string PhaseName in Data.get_Values("Phase"))
                {
                int Index = PhaseList.Items.IndexOf(PhaseName);
                if (Index == -1)
                    Data.DeleteNode(Data.ChildByTypeAndValue("Phase", PhaseName));
                else
                    PhaseList.SetItemChecked(Index, true);
                }


            FileNameEdit.TextChanged += OnFileNameChanged;
            MonthDropDown.TextChanged += OnMonthChanged;
            PhaseList.ItemCheck += OnPhaseItemCheck;
            }

        private void OnFileNameChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("FileName", FileNameEdit.Text);
            ParentUI.DoRefresh(Data);
            }

        private void OnPhaseItemCheck(object sender, ItemCheckEventArgs e)
            {
            if (e.NewValue == CheckState.Checked)
                {
                APSIMData NewPhase = new APSIMData("Phase", "");
                NewPhase.Value = PhaseList.Items[e.Index].ToString();
                Data.Add(NewPhase);
                }
            else
                Data.DeleteNode(Data.ChildByTypeAndValue("Phase", PhaseList.Items[e.Index].ToString()));
            ParentUI.DoRefresh(Data);
            }

        private void OnMonthChanged(object sender, EventArgs e)
            {
            Data.set_ChildValue("Month", MonthDropDown.Text);
            ParentUI.DoRefresh(Data);
            }        
        }
    }

