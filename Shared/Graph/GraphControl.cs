using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using VBGeneral;

namespace Graph
    {
    public partial class GraphControl : UserControl
        {
        public GraphControl()
            {
            InitializeComponent();
            }

        public void Refresh(BaseController Controller)
            {
            APSIMData GraphData = Controller.Data.Parent.Child("Data");
            if (GraphData == null)
                GraphData = Controller.Data.Child("Data");
            GraphController GraphController = new GraphController(Controller, GraphData.FullPath);

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
            foreach (APSIMData Series in Controller.Data.Child("series").get_Children("xy"))
                {
                string DataSource = Series.get_ChildValue("DataSource");
                string XFieldName = Series.get_ChildValue("X");
                string YFieldName = Series.get_ChildValue("Y");
                string SeriesType = Series.get_ChildValue("SeriesType").ToLower();
                string PointType = Series.get_ChildValue("PointType").ToLower();
                bool X2 = (Series.get_ChildValue("x2").ToLower() == "true");
                bool Y2 = (Series.get_ChildValue("y2").ToLower() == "true");
                string ColourString = Series.get_ChildValue("colour");
                if (DataSource != "" && XFieldName != "" && YFieldName != "")
                    {
                    DataTable Data = GraphController.GetXYData(DataSource, XFieldName, YFieldName);

                    if (Data != null)
                        {
                        Steema.TeeChart.Styles.Series NewSeries;
                        if (SeriesType == "bar")
                            {
                            Steema.TeeChart.Styles.Bar Bar = new Steema.TeeChart.Styles.Bar();
                            int NumSeries = Controller.Data.Child("series").get_Children("xy").Length;
                            Bar.BarWidthPercent = 100 / NumSeries;
                            NewSeries = Bar;
                            NewSeries.Marks.Visible = false;
                            }

                        else
                            {
                            Steema.TeeChart.Styles.Line LineSeries = new Steema.TeeChart.Styles.Line();
                            switch (SeriesType)
                                {
                                case "none": LineSeries.LinePen.Visible = false; break;
                                case "line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Solid; break;
                                case "dash": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dash; break;
                                case "dashdot": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDot; break;
                                case "dashdotdot": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDotDot; break;
                                case "dot": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dot; break;
                                };
                            switch (PointType)
                                {
                                case "none": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Nothing; break;
                                case "circle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Circle; break;
                                case "cross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Cross; break;
                                case "diagcross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DiagCross; break;
                                case "diamond": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Diamond; break;
                                case "downtriangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DownTriangle; break;
                                case "lefttriangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.LeftTriangle; break;
                                case "rectangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle; break;
                                case "righttriangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.RightTriangle; break;
                                case "smalldot": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.SmallDot; break;
                                case "triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Triangle; break;
                                };
                            LineSeries.Pointer.Visible = (PointType != "none");
                            if (PointType != "none")
                                {
                                LineSeries.Pointer.HorizSize = 7;
                                LineSeries.Pointer.VertSize = 7;
                                LineSeries.Pointer.Pen.Color = Color.White;
                                }
                            LineSeries.LinePen.Width = 2;
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

                        NewSeries.Title = YFieldName;
                        }
                    }
                }
            // setup the legend.
            if (Chart.Series.Count > 1)
                {
                Chart.Legend.Visible = true;
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

        private void Chart_Click(object sender, EventArgs e)
            {
            Chart.ShowEditor();
            }

        }
    }
