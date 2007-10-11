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
    public partial class FrequencyUI : VBUserInterface.BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;

        public FrequencyUI()
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

            Grid.CellChanged -= OnCellChanged;

            Grid.ClearRange(0, 0, Grid.RowCount, Grid.ColumnCount, true);
            string[] Labels = Data.get_Values("Label");
            string[] Filters = Data.get_Values("FilterString");
            for (int Row = 0; Row < Labels.Length && Row < Grid.RowCount; Row++)
                Grid.Cells[Row, 0].Text = Labels[Row];

            for (int Row = 0; Row < Filters.Length && Row < Grid.RowCount; Row++)
                Grid.Cells[Row, 1].Text = Filters[Row];
            Grid.CellChanged += OnCellChanged;
            }

        private void OnCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            int NumValues = GridUtils.FindFirstBlankCell(Grid, 0);
            string[] Labels = GridUtils.GetColumnAsStrings(Grid, 0, NumValues);
            string[] Filters = GridUtils.GetColumnAsStrings(Grid, 1, NumValues);
            Data.set_Values("Label", Labels);
            Data.set_Values("FilterString", Filters);
            ParentUI.DoRefresh(Data);
            }


        }
    }

