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
    public partial class FrequencyUI : VBUserInterface.BaseView
        {
        private ChartPageUI ParentUI;

        public FrequencyUI()
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
            
            Grid.CellChanged -= OnCellChanged;

            Grid.ClearRange(0, 0, Grid.RowCount, Grid.ColumnCount, true);
            List<string> Labels = XmlHelper.Values(Data, "Label");
            List<string> Filters = XmlHelper.Values(Data, "FilterString");
            for (int Row = 0; Row < Labels.Count && Row < Grid.RowCount; Row++)
                Grid.Cells[Row, 0].Text = Labels[Row];

            for (int Row = 0; Row < Filters.Count && Row < Grid.RowCount; Row++)
                Grid.Cells[Row, 1].Text = Filters[Row];
            Grid.CellChanged += OnCellChanged;
            }

        private void OnCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            int NumValues = GridUtils.FindFirstBlankCell(Grid, 0);
            List<string> Labels = new List<string>();
            Labels.AddRange(GridUtils.GetColumnAsStrings(Grid, 0, NumValues));
            List<string> Filters = new List<string>();
            Filters.AddRange(GridUtils.GetColumnAsStrings(Grid, 1, NumValues));
            XmlHelper.SetValues(Data, "Label", Labels);
            XmlHelper.SetValues(Data, "FilterString", Filters);
            PublishViewChanged(Data);
            }


        }
    }

