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
using Steema.TeeChart;
using VBUserInterface;


namespace Graph
    {
    public partial class ChartUI : BaseView
        {
        private APSIMData GraphNode;
        private ChartPageUI ParentUI;

        public ChartUI()
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
            base.OnRefresh();

            GraphNode = Controller.ApsimData.Find(NodePath);
            Chart.Series.Clear();
            Chart.Axes.Bottom.Title.Text = "";
            Chart.Axes.Left.Title.Text = "";
            Chart.Axes.Top.Title.Text = "";
            Chart.Axes.Right.Title.Text = "";
            Chart.Axes.Left.Visible = false;
            Chart.Axes.Bottom.Visible = false;
            Chart.Axes.Right.Visible = false;
            Chart.Axes.Bottom.Visible = false;

            // loop through all series in data and draw a chart series for each.
            foreach (string XYSSeriesName in GraphNode.get_Values("source"))
                {
                APSIMData Series = GraphNode.Parent.Parent.Find(GraphNode.Parent.Parent.Name + "\\Data\\" + XYSSeriesName);
                if (Series != null)
                    DrawSeries(Series, Series.get_ChildValue("Source"));
                }

            foreach (APSIMData XY in GraphNode.get_Children("xy"))
                DrawSeries(XY, XY.Parent.get_ChildValue("Source"));

            // setup the legend.
            Chart.Legend.Visible = (Chart.Series.Count > 1);
            if (!Chart.Axes.Left.Visible && !Chart.Axes.Right.Visible)
                {
                Chart.Axes.Left.Visible = true;     // situation where no data has been plotted.
                Chart.Axes.Bottom.Visible = true;
                }

            // setup a possible bottom date axis.
            if (Chart.Axes.Bottom.IsDateTime)
                {
                double Minimum = 0.0, Maximum = 0.0;
                Chart.Axes.Bottom.CalcMinMax(ref Minimum, ref Maximum);
                if (Maximum - Minimum > 365 * 2)
                    Chart.Axes.Bottom.Labels.DateTimeFormat = "yyyy";
                else
                    Chart.Axes.Bottom.Labels.DateTimeFormat = "MMM";
                }
            }

        private void DrawSeries(APSIMData Series, string DataSource)
            {
            if (DataSource != "")
                {
                string ColourString = ""; //Series.get_ChildValue("colour");
                string SeriesType = Series.get_ChildValue("SeriesType").ToLower();
                string PointType = Series.get_ChildValue("PointType").ToLower();

                // Work out which fields we want to use as X        
                bool XTop = false;
                string[] XFieldNames = Series.get_Values("X");
                if (XFieldNames.Length == 0)
                    {
                    XFieldNames = Series.get_Values("XTop");
                    XTop = true;
                    }

                // If x field names is still empty then assume that the user wants
                // to plot all fields as x except the y field.
                if (XFieldNames.Length > 0)
                    {
                    if (XFieldNames.Length == 1 && XFieldNames[0] == "*")
                        {
                        string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);
                        string YFieldName = Series.get_ChildValue("Y");
                        foreach (string FieldName in FieldNames)
                            if (FieldName.ToLower() != YFieldName.ToLower())
                                DrawSeries(DataSource, FieldName, YFieldName, SeriesType, PointType, XTop, false, ColourString,
                                           FieldName);
                        }
                    else
                        {
                        // By now we should have a single X with 0 or more Y values.
                        string[] YFieldNames = Series.get_Values("Y");
                        if (YFieldNames.Length == 1 && YFieldNames[0] == "*")
                            {
                            string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);
                            foreach (string FieldName in FieldNames)
                                if (FieldName.ToLower() != XFieldNames[0].ToLower())
                                    DrawSeries(DataSource, XFieldNames[0], FieldName, SeriesType, PointType, XTop, false, ColourString,
                                               FieldName);
                            }
                        else
                            {
                            // First plot up the normal Y values.
                            foreach (string YFieldName in YFieldNames)
                                DrawSeries(DataSource, XFieldNames[0], YFieldName, SeriesType, PointType, XTop, false, ColourString, YFieldName);

                            // Now plot up the right Y values.
                            foreach (string YFieldName in Series.get_Values("YRight"))
                                DrawSeries(DataSource, XFieldNames[0], YFieldName, SeriesType, PointType, XTop, true, ColourString, YFieldName);
                            }
                        }
                    }
                }
            }
                
        private void RefreshButton_Click(object sender, EventArgs e)
            {
            OnRefresh();
            }

        private void PropertiesButton_Click(object sender, EventArgs e)
            {
            Chart.ShowEditor();
            }

        private void DrawSeries(string DataSource, string XFieldName, string YFieldName,
                                string SeriesType, string PointType, bool X2, bool Y2,
                                string ColourString, string SeriesName)
            {
            Steema.TeeChart.Styles.Series NewSeries = null;
                    
            if (DataSource != "" && XFieldName != "" && YFieldName != "")
                {
                DataTable Data = ParentUI.Processor.GetXYData(DataSource, XFieldName, YFieldName);

                if (Data != null)
                    {
                    if (SeriesType == "bar")
                        {
                        Steema.TeeChart.Styles.Bar Bar = new Steema.TeeChart.Styles.Bar();
                        int NumSeries = GraphNode.get_Children("xy").Length;
                        Bar.MultiBar = Steema.TeeChart.Styles.MultiBars.None;
                        Bar.BarWidthPercent = 45; //50 / NumSeries) - 5;
                        NewSeries = Bar;
                        NewSeries.Marks.Visible = false;
                        }

                    else
                        {
                        Steema.TeeChart.Styles.Line LineSeries = new Steema.TeeChart.Styles.Line();
                        switch (SeriesType)
                            {
                            case "no line": LineSeries.LinePen.Visible = false; break;
                            case "solid line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Solid; break;
                            case "dash line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dash; break;
                            case "dashdot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDot; break;
                            case "dashdotdot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDotDot; break;
                            case "dot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dot; break;
                            };
                        switch (PointType)
                            {
                            case "none": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Nothing; break;
                            case "circle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Circle; break;
                            case "cross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Cross; break;
                            case "diagonal cross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DiagCross; break;
                            case "diamond": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Diamond; break;
                            case "down triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DownTriangle; break;
                            case "left triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.LeftTriangle; break;
                            case "rectangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle; break;
                            case "right triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.RightTriangle; break;
                            case "small dot": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.SmallDot; break;
                            case "triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Triangle; break;
                            };
                        LineSeries.Pointer.Visible = (PointType != "none");
                        if (PointType != "none")
                            {
                            LineSeries.Pointer.HorizSize = 4;
                            LineSeries.Pointer.VertSize = 4;
                            LineSeries.Pointer.Pen.Color = Color.White;
                            }
                        //LineSeries.LinePen.Width = 2;
                        NewSeries = LineSeries;
                        }
                    // Add colour to the series.
                    if (ColourString != "")
                        NewSeries.Color = Color.FromArgb(Convert.ToInt32(ColourString));

                    // Populate the series with data.
                    Chart.Series.Add(NewSeries);
                    NewSeries.DataSource = Data;
                    if (Data.Columns[XFieldName].DataType == typeof(string))
                        NewSeries.LabelMember = XFieldName;
                    else if (Data.Columns[XFieldName].DataType == typeof(DateTime))
                        {
                        NewSeries.XValues.DataMember = XFieldName;
                        NewSeries.XValues.DateTime = true;
                        }
                    else
                        NewSeries.XValues.DataMember = XFieldName;
                    NewSeries.YValues.DataMember = YFieldName;
                    NewSeries.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
                    NewSeries.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
                    NewSeries.YValues.Sort();
                    NewSeries.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
                    // setup the axes.
                    if (X2)
                        {
                        AddFieldNameToAxisTitle(Chart.Axes.Top, XFieldName);
                        NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
                        }
                    else
                        {
                        AddFieldNameToAxisTitle(Chart.Axes.Bottom, XFieldName);
                        NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Bottom;
                        }
                    if (Y2)
                        {
                        AddFieldNameToAxisTitle(Chart.Axes.Right, YFieldName);
                        NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right;
                        }
                    else
                        {
                        AddFieldNameToAxisTitle(Chart.Axes.Left, YFieldName);
                        NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Left;
                        }
                    Chart.Axes.Left.Inverted = (NewSeries.HorizAxis == Steema.TeeChart.Styles.HorizontalAxis.Top);

                    NewSeries.Title = SeriesName;
                    }
                }
            }


        private void AddFieldNameToAxisTitle(Steema.TeeChart.Axis Axis, string FieldName)
            {
            string[] FieldNames = Axis.Title.Text.Split(", ".ToCharArray());
            if (Array.IndexOf(FieldNames, FieldName) == -1)
                {
                Axis.Visible = true;
                if (Axis.Title.Text != "")
                    Axis.Title.Text = Axis.Title.Text + ", ";
                Axis.Title.Text = Axis.Title.Text + FieldName;
                }
            }

        public void ChartEdit()
            {
            Chart.ShowEditor();
            }



        }
    }

