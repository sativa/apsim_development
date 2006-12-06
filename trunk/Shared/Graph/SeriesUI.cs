using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using FarPoint.Win.Spread;
using FarPoint.Win.Spread.CellType;
using VBGeneral;

namespace Graph
    {
    public partial class SeriesUI : VBGeneral.BaseView
        {
        private GraphController GraphController;
        private bool Updating = false;

        public SeriesUI()
            {
            InitializeComponent();
            }

        public override void RefreshView(BaseController Controller)
            {
            // -----------------------------------------------------------
            // Refresh the control.
            // -----------------------------------------------------------
            base.RefreshView(Controller);

            APSIMData GraphData = Controller.Data.Parent.Child("Data");
            if (GraphData == null)
                GraphData = Controller.Data.Parent.Parent.Child("Data");
            this.GraphController = new GraphController(Controller, GraphData.FullPath);

            Updating = true;

            // give the dataset names to the combobox in the first column.
            ComboBoxCellType DataSourceCombo = (ComboBoxCellType) Grid.Columns[0].Editor;
            DataSourceCombo.Items = GraphController.GetAllDataSets();

            int Row = 0;
            foreach (APSIMData xy in Controller.Data.get_Children("xy"))
                {
                Grid.Cells[Row, 0].Text = xy.get_ChildValue("DataSource");
                PopulateXYColumns(Row);
                Grid.Cells[Row, 1].Text = xy.get_ChildValue("X");
                Grid.Cells[Row, 2].Text = xy.get_ChildValue("Y");
                Grid.Cells[Row, 3].Text = xy.get_ChildValue("SeriesType");
                Grid.Cells[Row, 4].Text = xy.get_ChildValue("PointType");
                Grid.Cells[Row, 5].Text = xy.get_ChildValue("X2");
                Grid.Cells[Row, 6].Text = xy.get_ChildValue("Y2");
                string ColourString = xy.get_ChildValue("Colour");
                if (ColourString != "")
                    Grid.Cells[Row, 7].BackColor = Color.FromArgb(Convert.ToInt32(ColourString));
                Row++;
                }

            Updating = false;
            }

        private void Grid_CellChanged(object sender, SheetViewEventArgs e)
            {
            // -----------------------------------------------------------
            // The value of a cell has changed.
            // -----------------------------------------------------------
            if (!Updating)
                {
                if (e.Column == 0)
                    PopulateXYColumns(e.Row);
                SaveView();
                }
            }

        private void SaveView()
            {
            int NumSeries = GridUtils.FindFirstBlankCell(Grid, 0);
            Controller.Data.EnsureNumberOfChildren("XY", "", NumSeries);

            int Row = 0;
            foreach (APSIMData xy in Controller.Data.get_Children("XY"))
                {
                xy.set_ChildValue("DataSource", Grid.Cells[Row, 0].Text); 
                xy.set_ChildValue("X", Grid.Cells[Row, 1].Text);
                xy.set_ChildValue("Y", Grid.Cells[Row, 2].Text);
                xy.set_ChildValue("SeriesType", Grid.Cells[Row, 3].Text);
                xy.set_ChildValue("PointType", Grid.Cells[Row, 4].Text);
                xy.set_ChildValue("X2", Grid.Cells[Row, 5].Text);
                xy.set_ChildValue("Y2", Grid.Cells[Row, 6].Text);
                xy.set_ChildValue("Colour", Grid.Cells[Row, 7].BackColor.ToArgb().ToString());
                Row++;
                }
            }

        private void PopulateXYColumns(int Row)
            {
            string DataSourceName = Grid.Cells[Row, 0].Text;
            string[] FieldNames = GraphController.GetFieldNamesForDataSet(DataSourceName);
            ComboBoxCellType XCombo = (ComboBoxCellType)Grid.Columns[1].Editor;
            ComboBoxCellType YCombo = (ComboBoxCellType)Grid.Columns[2].Editor;
            XCombo.Items = FieldNames;
            YCombo.Items = FieldNames;
            }

        private void Spreadsheet_CellClick(object sender, CellClickEventArgs e)
            {
            // -----------------------------------------------------------------
            // User has clicked on a cell. See if it is the colour column.
            // -----------------------------------------------------------------
            if (e.Column == 7 && ColorDialog.ShowDialog() == DialogResult.OK)
                {
                Grid.Cells[e.Row, e.Column].BackColor = ColorDialog.Color;
                SaveView();
                }
            }

        private void DeleteSeriesMenu_Click(object sender, EventArgs e)
            {
            Updating = true;
            int Row = Grid.GetSelection(0).Row;
            for (int Col = 0; Col <= 2; Col++)
                Grid.Cells[Row, Col].Text = "";
            SaveView();
            Updating = false;
            }
        
        
        
        }
    }

//Sheet.Cells[Row, 5].BackColor = Color.FromName(SeriesData.get_ChildValue("Colour"));