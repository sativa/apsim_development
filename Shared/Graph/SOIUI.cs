using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;
using CSGeneral;
using System.Xml;

namespace Graph
    {
    public partial class SOIUI : VBUserInterface.BaseView
        {
        public SOIUI()
            {
            InitializeComponent();
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
            PhaseList.CellChanged -= OnPhaseChanged;

            FileNameEdit.Text = XmlHelper.Value(Data, "FileName");
            MonthDropDown.Text = XmlHelper.Value(Data, "Month");

            int Row = 0;
            foreach (string PhaseName in XmlHelper.Values(Data, "Phase"))
                PhaseList.Cells[Row, 0].Text = PhaseName;

            FileNameEdit.TextChanged += OnFileNameChanged;
            MonthDropDown.TextChanged += OnMonthChanged;
            PhaseList.CellChanged += OnPhaseChanged;
            }

        private void OnFileNameChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "FileName", FileNameEdit.Text);
            PublishViewChanged();
            }

        private void OnPhaseChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            int NumPhases = GridUtils.FindFirstBlankCell(PhaseList, 0);
            List<string> Phases = new List<string>(GridUtils.GetColumnAsStrings(PhaseList, 0, NumPhases));
            XmlHelper.SetValues(Data, "Phases", Phases);
            PublishViewChanged();
            }

        private void OnMonthChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "Month", MonthDropDown.Text);
            PublishViewChanged();
            }
      
        }
    }

