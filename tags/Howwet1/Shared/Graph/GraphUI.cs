using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections.Specialized;
using VBGeneral;
using System.Collections;
using FarPoint.Win.Spread;
using Xceed.Chart.GraphicsCore;
using Xceed.Chart.Core;
using Xceed.Chart;

namespace Graph
    {
    public partial class GraphUI : VBGeneral.BaseView
        {
        private ArrayList Tables = new ArrayList();
        private ArrayList Charts = new ArrayList(8);
        private FarPoint.Win.Spread.CellType.ComboBoxCellType XAxisCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        private FarPoint.Win.Spread.CellType.ComboBoxCellType YAxisCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        private FarPoint.Win.Spread.CellType.ComboBoxCellType LineCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        private FarPoint.Win.Spread.CellType.ComboBoxCellType MarkerCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        private FarPoint.Win.Spread.CellType.ComboBoxCellType DataSourceCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        private StringCollection DataSetPaths = new StringCollection();
        private bool Updating = false;

        public GraphUI()
            {
            InitializeComponent();
            }

        public override void RefreshView(BaseController Controller)
            {
            base.RefreshView(Controller);

            GraphController Graph = (GraphController)Controller;

            // go find the graphdata node
            APSIMData[] ChildNodes = Controller.Data.get_Children("graphdata");
            if (ChildNodes.Length != 1)
                return;
            APSIMData GraphData = ChildNodes[0];

            // grab hold of all data tables.
            string GraphDataPath = GraphData.FullPath;
            Graph.GetAllDataSets(DataSetPaths);
            string[] DataSets = new string[DataSetPaths.Count];
            for (int i = 0; i != DataSetPaths.Count; i++)
                {
                DataSets[i] = DataSetPaths[i];
                //DataTable Data = Graph.FindData(DataSetPaths[i]);
                //Tables.Add(Data);
                }

            // Create some combos that we can use in the sheet.
            DataSourceCombo.Items = DataSets;
            XAxisCombo.Items = new string[] { "Bottom", "Top" };
            YAxisCombo.Items = new string[] { "Left", "Right" };
            LineCombo.Items = new string[] { "Line", "No line", "Dash", "Dot", "Bar"};
            MarkerCombo.Items = new string[] { "None", "Square", "Up triangle", "Down triangle", "Plus", "Circle", "Cross", 
                                               "Star"};

            // make sure we have the right number of sheets and graphs.
            PopulateForm();
            }

        private void PopulateForm()
            // -------------------------------------------------------------
            // Populate the entire form based on what is in our "Data"
            // -------------------------------------------------------------
            {
            Updating = true;
            APSIMData[] ChartDatas = Controller.Data.get_Children("chart");
            if (ChartDatas.Length > 0)
                NumChartsCombo.Text = (string)NumChartsCombo.Items[ChartDatas.Length - 1];

            for (int ChartIndex = 0; ChartIndex != ChartDatas.Length; ChartIndex++)
                {
                APSIMData ChartData = (APSIMData)ChartDatas[ChartIndex];
                SheetView Sheet = Spread.Sheets[ChartIndex];
                APSIMData[] SeriesDatas = ChartData.get_Children("series");
                for (int Row = 0; Row != SeriesDatas.Length; Row++)
                    {
                    APSIMData SeriesData = (APSIMData)SeriesDatas[Row];
                    Sheet.Cells[Row, 0].Text = SeriesData.get_ChildValue("DataSource");
                    Sheet.Cells[Row, 1].Text = SeriesData.get_ChildValue("X");
                    Sheet.Cells[Row, 2].Text = SeriesData.get_ChildValue("Y");
                    Sheet.Cells[Row, 3].Text = SeriesData.get_ChildValue("Line");
                    Sheet.Cells[Row, 4].Text = SeriesData.get_ChildValue("Marker");
                    Sheet.Cells[Row, 5].BackColor = Color.FromName(SeriesData.get_ChildValue("Colour"));
                    PopulateGraph( (ChartControl)Charts[ChartIndex], Sheet);
                    }
                }
            Updating = false;

            }

        private void PopulateGraph(ChartControl Graph, SheetView Sheet)
            // ------------------------------------------------------------------
            // Populate the graph based on what is in the sheet
            // ------------------------------------------------------------------
            {
            //bool RefreshNecessary = false;
            //ChartHelper ChartHelper = new ChartHelper();
            //ChartHelper.Chart = Graph;

            //// loop through all rows in sheet and draw a chart series for each.
            //for (int Row = 0; Row != Sheet.RowCount; Row++)
            //    {
            //    string TableName = Sheet.Cells[Row, 0].Text;
            //    string XFieldName = Sheet.Cells[Row, 1].Text;
            //    string YFieldName = Sheet.Cells[Row, 2].Text;
            //    Color SeriesColour = Sheet.Cells[Row, 5].BackColor;
            //    string LineTypeString = Sheet.Cells[Row, 6].Text;
            //    string MarkerTypeString = Sheet.Cells[Row, 7].Text;

            //    if (TableName != "" && XFieldName != "" && YFieldName != "")
            //        {
            //        int TableNumber = DataSetPaths.IndexOf(TableName);
            //        DataTable Data = (DataTable)Tables[TableNumber];
            //        if (Data != null)
            //            {
            //            ChartHelper.DataTable = Data;

            //            ChartHelper.CreateChartSeriesFromDataTable(YFieldName, XFieldName, YFieldName, SeriesColour,
            //                                                       MarkerTypeString, LineTypeString, 1,
            //                                                       Xceed.Chart.Core.StandardAxis.PrimaryX, Xceed.Chart.Core.StandardAxis.PrimaryY);
            //            RefreshNecessary = true;

            //            }
            //        }
            //    }
            //if (RefreshNecessary)
            //    ChartHelper.Chart.Refresh();
            }


        private void CreateDefaultSheetsAndCharts()
            // -----------------------------------------------------------
            // Setup the right number of sheets and charts using defaults
            // -----------------------------------------------------------
            {
            int NumChartsRequired = NumChartsCombo.Items.IndexOf(NumChartsCombo.Text) + 1;

            // get rid of unwanted charts if necessary
            while (Spread.Sheets.Count > NumChartsRequired)
                {
                Spread.Sheets.RemoveAt(Spread.Sheets.Count - 1);
                ChartControl ChartToRemove = (ChartControl)Charts[Charts.Count - 1];
                ChartPanel.Controls.Remove(ChartToRemove);
                Charts.RemoveAt(Charts.Count-1);
                }

            // add extra charts if necessary
            while (Spread.Sheets.Count < NumChartsRequired)
                {
                int GraphNumber = Spread.Sheets.Count + 1;

                SheetView NewSheet = new SheetView();
                NewSheet.ColumnCount = 8;
                NewSheet.RowCount = 20;
                NewSheet.RowHeader.ColumnCount = 0;
                NewSheet.ColumnHeader.Cells.Get(0, 0).Value = "File";
                NewSheet.ColumnHeader.Cells.Get(0, 1).Value = "X";
                NewSheet.ColumnHeader.Cells.Get(0, 2).Value = "Y";
                NewSheet.ColumnHeader.Cells.Get(0, 3).Value = "X axis";
                NewSheet.ColumnHeader.Cells.Get(0, 4).Value = "Y axis";
                NewSheet.ColumnHeader.Cells.Get(0, 5).Value = "Colour";
                NewSheet.ColumnHeader.Cells.Get(0, 6).Value = "Line";
                NewSheet.ColumnHeader.Cells.Get(0, 7).Value = "Symbol";

                // setup DataSource column
                NewSheet.Columns.Get(0).CellType = DataSourceCombo;
                NewSheet.Columns.Get(0).Label = "Data";
                NewSheet.Columns.Get(0).Width = 152F;

                // setup X column
                NewSheet.Columns.Get(1).Label = "X";
                NewSheet.Columns.Get(1).Width = 152F;

                // setup Y column
                NewSheet.Columns.Get(2).Label = "Y";
                NewSheet.Columns.Get(2).Width = 152F;

                // setup X axis column
                NewSheet.Columns.Get(3).CellType = XAxisCombo;
                NewSheet.Columns.Get(3).Label = "X axis";
                NewSheet.Columns.Get(3).Width = 68F;

                // setup Y axis column
                NewSheet.Columns.Get(4).CellType = YAxisCombo;
                NewSheet.Columns.Get(4).Label = "Y axis";
                NewSheet.Columns.Get(4).Width = 68F;

                // setup line type column
                NewSheet.Columns.Get(6).CellType = LineCombo;
                NewSheet.Columns.Get(6).Label = "Line/Bar";
                NewSheet.Columns.Get(6).Width = 75F;

                // setup marker type column
                NewSheet.Columns.Get(7).CellType = MarkerCombo;
                NewSheet.Columns.Get(7).Label = "Marker";
                NewSheet.Columns.Get(7).Width = 75F;

                // setup some of the cells on each row.
                for (int Row = 0; Row != NewSheet.RowCount; Row++)
                    {
                    FarPoint.Win.Spread.CellType.ComboBoxCellType FieldCombo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
                    FieldCombo.MaxDrop = 20;
                    NewSheet.Cells[Row, 1].CellType = FieldCombo;
                    NewSheet.Cells[Row, 2].CellType = FieldCombo;
                    NewSheet.Cells[Row, 3].Text = "Bottom";
                    NewSheet.Cells[Row, 4].Text = "Left";
                    NewSheet.Cells[Row, 5].BackColor = Color.Blue;
                    NewSheet.Cells[Row, 6].Text = "Line";
                    NewSheet.Cells[Row, 7].Text = "None";
                    }

                NewSheet.RowHeader.Columns.Default.Resizable = false;
                NewSheet.RowHeader.Visible = false;
                NewSheet.SheetName = "Graph" + GraphNumber.ToString();
                NewSheet.CellChanged += new SheetViewEventHandler(CellChanged);
                Spread.Sheets.Add(NewSheet);
                ChartControl NewChart = new ChartControl();
                ChartPanel.Controls.Add(NewChart);
                NewChart.Charts[0].MarginMode = MarginMode.Stretch;
                NewChart.Charts[0].Margins = new Rectangle(10, 10, 80, 80);
                Charts.Add(NewChart);
                }

            // reposition all charts.
            }

        private void SizeGraphs()
            // ----------------------------------------------------------
            // Size the graph so that it fits into the correct area.
            // ----------------------------------------------------------
            {
            for (int GraphIndex = 0; GraphIndex != Charts.Count; GraphIndex++)
                {
                ChartControl Graph = (ChartControl)Charts[GraphIndex];
                int NumRowsOfGraphs = 2;
                if (Charts.Count <= 2)
                    NumRowsOfGraphs = 1;
                int NumColsOfGraphs = Convert.ToInt32(Math.Round(Charts.Count * 1.0 / NumRowsOfGraphs));
                int Width = ChartPanel.Width / NumColsOfGraphs;
                int Height = ChartPanel.Height / NumRowsOfGraphs;
                int Row = GraphIndex / NumColsOfGraphs;
                int Col = GraphIndex - Row * NumColsOfGraphs;
                int Left = Col * Width;
                int Top = Row * Height;
                Graph.Bounds = new Rectangle(Left, Top, Width, Height);
                }
            }

        private void Spread_CellClick(object sender, CellClickEventArgs e)
            {
            if (e.Column == 5)
                {
                if (ColourDialog.ShowDialog() == DialogResult.OK)
                    {
                    Spread.ActiveSheet.Cells[e.Row, e.Column].BackColor = ColourDialog.Color;
                    PopulateGraph((ChartControl) Charts[Spread.ActiveSheetIndex], Spread.ActiveSheet);
                    }
                }
            }

        private void CellChanged(Object Sender, SheetViewEventArgs e)
            {
            if (!Updating)
                {
                SheetView Sheet = (SheetView)Sender;
                if (e.Column == 0)
                    {
                    int TableNumber = DataSetPaths.IndexOf(Sheet.Cells[e.Row, e.Column].Text);
                    DataTable Data = (DataTable)Tables[TableNumber];
                    if (Data != null)
                        {
                        string[] FieldNames = new string[Data.Columns.Count];
                        for (int i = 0; i != Data.Columns.Count; i++)
                            FieldNames[i] = Data.Columns[i].ColumnName;

                        Array.Sort(FieldNames);
                        FarPoint.Win.Spread.CellType.ComboBoxCellType XCombo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)Sheet.Cells[e.Row, 1].CellType;
                        XCombo.Items = FieldNames;
                        // we don't need to set the YCombo because it uses the same combo as x.
                        }
                    }
                PopulateGraph((ChartControl)Charts[Spread.ActiveSheetIndex], Spread.ActiveSheet);
                }
            }

        private void NumChartsCombo_TextChanged(object sender, EventArgs e)
            {
            CreateDefaultSheetsAndCharts();
            }


        private void ChartPanel_Resize(object sender, EventArgs e)
            {
            SizeGraphs();
            }


        public override void Save()
            {
            // make sure we have the right number of chart children
            Controller.Data.EnsureNumberOfChildren("chart", "", Charts.Count);
            APSIMData[] ChartDataChildren = Controller.Data.get_Children("chart");

            // Loop through all sheets and save them.
            for (int s = 0; s != Spread.Sheets.Count; s++)
                {
                APSIMData ChartData = (APSIMData) ChartDataChildren[s];
                SheetView Sheet = Spread.Sheets[s];

                // Work out how many series there are and make sure we have the
                // right number of Series Data childe.
                int NumSeries = 0;
                for (int Row = 0; Row != Sheet.RowCount; Row++)
                    {
                    if (Spread.ActiveSheet.Cells[Row, 0].Text != "" ||
                        Spread.ActiveSheet.Cells[Row, 1].Text != "" ||
                        Spread.ActiveSheet.Cells[Row, 2].Text != "")
                        NumSeries++;
                    }
                ChartData.EnsureNumberOfChildren("series", "", NumSeries);
                APSIMData[] SeriesDataChildren = ChartData.get_Children("series");
                    
                // loop through all rows in sheet and save each row if it is non blank.
                int SeriesIndex = 0;
                for (int Row = 0; Row != Sheet.RowCount; Row++)
                    {
                    string TableName = Spread.ActiveSheet.Cells[Row, 0].Text;
                    string XFieldName = Spread.ActiveSheet.Cells[Row, 1].Text;
                    string YFieldName = Spread.ActiveSheet.Cells[Row, 2].Text;
                    Color SeriesColour = Spread.ActiveSheet.Cells[Row, 5].BackColor;
                    string LineTypeString = Spread.ActiveSheet.Cells[Row, 6].Text;
                    string MarkerTypeString = Spread.ActiveSheet.Cells[Row, 7].Text;

                    if (TableName != "" && XFieldName != "" && YFieldName != "")
                        {
                        APSIMData SeriesData = (APSIMData) SeriesDataChildren[SeriesIndex];
                        SeriesIndex++;
                        SeriesData.set_ChildValue("DataSource", TableName);
                        SeriesData.set_ChildValue("X", XFieldName);
                        SeriesData.set_ChildValue("Y", YFieldName);
                        SeriesData.set_ChildValue("Line", LineTypeString);
                        SeriesData.set_ChildValue("Marker", MarkerTypeString);
                        SeriesData.set_ChildValue("Colour", SeriesColour.Name);
                        }
                    }
                }
            }


        }
    }

