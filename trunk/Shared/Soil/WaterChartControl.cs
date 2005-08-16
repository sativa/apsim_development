using System;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using CSGeneral;
using VBGeneral;
using Xceed.Grid;
using Xceed.Chart.Standard;
using Xceed.Chart.Core;

namespace CSGeneral
	{
	public class WaterChartControl : System.Windows.Forms.UserControl
		{
		internal Xceed.Chart.ChartControl WaterChart;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.CheckBox LineGraphCheck;
		private System.Windows.Forms.CheckedListBox CropList;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Splitter splitter;
		private System.ComponentModel.Container components = null;
		private Soil MySoil;
		private bool ShowSW = false;

		public WaterChartControl()
			{
			// This call is required by the Windows.Forms Form Designer.
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if(components != null)
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Component Designer generated code
		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(WaterChartControl));
			this.WaterChart = new Xceed.Chart.ChartControl();
			this.panel1 = new System.Windows.Forms.Panel();
			this.LineGraphCheck = new System.Windows.Forms.CheckBox();
			this.CropList = new System.Windows.Forms.CheckedListBox();
			this.label1 = new System.Windows.Forms.Label();
			this.splitter = new System.Windows.Forms.Splitter();
			this.panel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// WaterChart
			// 
			this.WaterChart.AutoScrollMargin = new System.Drawing.Size(0, 0);
			this.WaterChart.AutoScrollMinSize = new System.Drawing.Size(0, 0);
			this.WaterChart.BackColor = System.Drawing.SystemColors.ActiveBorder;
			this.WaterChart.Background = ((Xceed.Chart.Standard.Background)(resources.GetObject("WaterChart.Background")));
			this.WaterChart.Charts = ((Xceed.Chart.Core.ChartCollection)(resources.GetObject("WaterChart.Charts")));
			this.WaterChart.Dock = System.Windows.Forms.DockStyle.Fill;
			this.WaterChart.InteractivityOperations = ((Xceed.Chart.Standard.InteractivityOperationsCollection)(resources.GetObject("WaterChart.InteractivityOperations")));
			this.WaterChart.Labels = ((Xceed.Chart.Standard.ChartLabelCollection)(resources.GetObject("WaterChart.Labels")));
			this.WaterChart.Legends = ((Xceed.Chart.Core.LegendCollection)(resources.GetObject("WaterChart.Legends")));
			this.WaterChart.Location = new System.Drawing.Point(0, 0);
			this.WaterChart.Name = "WaterChart";
			this.WaterChart.Settings = ((Xceed.Chart.Core.Settings)(resources.GetObject("WaterChart.Settings")));
			this.WaterChart.Size = new System.Drawing.Size(474, 600);
			this.WaterChart.TabIndex = 21;
			this.WaterChart.Watermarks = ((Xceed.Chart.Standard.WatermarkCollection)(resources.GetObject("WaterChart.Watermarks")));
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this.LineGraphCheck);
			this.panel1.Controls.Add(this.CropList);
			this.panel1.Controls.Add(this.label1);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Right;
			this.panel1.Location = new System.Drawing.Point(477, 0);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(171, 600);
			this.panel1.TabIndex = 22;
			// 
			// LineGraphCheck
			// 
			this.LineGraphCheck.Dock = System.Windows.Forms.DockStyle.Bottom;
			this.LineGraphCheck.Location = new System.Drawing.Point(0, 576);
			this.LineGraphCheck.Name = "LineGraphCheck";
			this.LineGraphCheck.Size = new System.Drawing.Size(171, 24);
			this.LineGraphCheck.TabIndex = 14;
			this.LineGraphCheck.Text = "Line graph";
			this.LineGraphCheck.CheckedChanged += new System.EventHandler(this.LineGraphCheck_CheckedChanged);
			// 
			// CropList
			// 
			this.CropList.CheckOnClick = true;
			this.CropList.Dock = System.Windows.Forms.DockStyle.Fill;
			this.CropList.Location = new System.Drawing.Point(0, 24);
			this.CropList.Name = "CropList";
			this.CropList.Size = new System.Drawing.Size(171, 574);
			this.CropList.TabIndex = 12;
			this.CropList.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.CropList_ItemCheck);
			// 
			// label1
			// 
			this.label1.Dock = System.Windows.Forms.DockStyle.Top;
			this.label1.Location = new System.Drawing.Point(0, 0);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(171, 24);
			this.label1.TabIndex = 13;
			this.label1.Text = "Crops to show on graph";
			this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// splitter
			// 
			this.splitter.Dock = System.Windows.Forms.DockStyle.Right;
			this.splitter.Location = new System.Drawing.Point(474, 0);
			this.splitter.Name = "splitter";
			this.splitter.Size = new System.Drawing.Size(3, 600);
			this.splitter.TabIndex = 23;
			this.splitter.TabStop = false;
			// 
			// WaterChartControl
			// 
			this.Controls.Add(this.WaterChart);
			this.Controls.Add(this.splitter);
			this.Controls.Add(this.panel1);
			this.Name = "WaterChartControl";
			this.Size = new System.Drawing.Size(648, 600);
			this.panel1.ResumeLayout(false);
			this.ResumeLayout(false);

		}
		#endregion


		// ---------------------------------------------
		// MySoil property.
		// ---------------------------------------------
		public Soil LinkedSoil 
			{
			get {return MySoil;}
			set {
				MySoil = value;
				Refresh();
				}		
			}

		// ---------------------------------------------
		// ShowSW property.
		// ---------------------------------------------
		public bool ShowSoilWaterLine 
			{
			get {return ShowSW;}
			set {
				ShowSW = value;
				Refresh();
				}		
			}

		// -------------------
		// Refresh            
		// -------------------
		public override void Refresh()
			{
			if (MySoil != null)
				{
				StringCollection MyCrops = new StringCollection();
				MyCrops.AddRange(MySoil.Crops);

				// Make sure all crops are in listbox.
				foreach (string Crop in MyCrops)
					{
					if (CropList.Items.IndexOf(Crop) == -1)
						CropList.Items.Add(Crop);
					}

				// Make sure we don't have unwanted crop in listbox.
				for (int i = 0; i != CropList.Items.Count; i++)
					{
					if (MyCrops.IndexOf(CropList.Items[i].ToString()) == -1)
						{
						CropList.Items.RemoveAt(i);
						i--;
						}
					}
				
				LineGraphCheck.Checked = (APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), 
																"soil", "DepthChartWithLines") == "yes");
				PopulateWaterChart(CropsToChart());
				WaterChart.Legends[0].Data.OutlineHorizontalLinesProps.Width = 0;
				WaterChart.Legends[0].Data.OutlineVerticalLinesProps.Width = 0;
				WaterChart.Legends[0].Data.VerticalLinesProps.Width = 0;
				WaterChart.Legends[0].Data.HorizontalLinesProps.Width = 0;
				}
			}
	

		// ---------------------------
		// populate water chart.
		// ---------------------------
		private void PopulateWaterChart(StringCollection Crops)
			{
            if (LineGraphCheck.Checked)
				PopulateWaterChartLines(WaterChart, MySoil, Crops, ShowSW);
			else
				PopulateWaterChartBlocky(WaterChart, MySoil, Crops, ShowSW);
			}								


		// -------------------------------------------------
		// Populate the specified chart using 'blocky' style
		// -------------------------------------------------
		private static void PopulateWaterChartBlocky(Xceed.Chart.ChartControl Chart, Soil MySoil, 
													StringCollection Crops, bool ShowSW)
			{
			ChartHelper Helper = new ChartHelper();
			Helper.Chart = Chart;
			Chart.Charts[0].Series.Clear();
			if (MySoil.CumThicknessMidPoints.Length > 0)
				{
				ShapeSeries Shape = (ShapeSeries) Chart.Charts[0].Series.Add(SeriesType.Shape);
				Shape.Appearance.FillMode = AppearanceFillMode.DataPoints;
				Shape.Appearance.LineMode = AppearanceLineMode.DataPoints;
				Shape.UseZValues = false;
				Shape.DataLabels.Mode = DataLabelsMode.None;
				Shape.UseXValues = true;
				Shape.Legend.Mode = SeriesLegendMode.None;

				double[] LL15 = MathUtility.Multiply_Value(MySoil.LL15, 100);
				double[] DUL = MathUtility.Multiply_Value(MySoil.DUL, 100);
				for (int Layer = 0; Layer != MySoil.CumThicknessMidPoints.Length; Layer++)
					{
					double X = (DUL[Layer] + LL15[Layer]) / 2;
					double XSize = DUL[Layer] - LL15[Layer];
					double Y = MySoil.CumThicknessMidPoints[Layer] / 10;
					double YSize = MySoil.Thickness[Layer] / 10;
					Shape.AddShape(Y, X, 0, XSize, YSize, 0, "", 
									new FillEffect(Color.CornflowerBlue), new LineProperties(0, Color.Black));
					}


				double[] CumThickness = MathUtility.Divide_Value(SoilBase.CalcYForPlotting(MySoil.Thickness), 10);
				double[] SAT = MathUtility.Multiply_Value(SoilBase.CalcXForPlotting(MySoil.SAT), 100);
				Helper.CreateChartSeriesFromArray("SAT", 
													SAT, CumThickness, 
													false, Color.Blue, 1, LinePattern.Dash,
													StandardAxis.PrimaryX, StandardAxis.PrimaryY);



				double[] AirDry = MathUtility.Multiply_Value(SoilBase.CalcXForPlotting(MySoil.Airdry), 100);
				Helper.CreateChartSeriesFromArray("AirDry", 
													AirDry, CumThickness,
													false, Color.Red, 1, LinePattern.Dash,
													StandardAxis.PrimaryX, StandardAxis.PrimaryY);

				if (ShowSW && MySoil.InitialWater.SW.Length > 0)
					{
					double[] SW = MathUtility.Multiply_Value(SoilBase.CalcXForPlotting(MySoil.InitialWater.SW), 100);
					Helper.CreateChartSeriesFromArray("SW", 
														SW, CumThickness,
														false, Color.Aquamarine, 3, LinePattern.Solid,
														StandardAxis.PrimaryX, StandardAxis.PrimaryY);
					}
				
				Color[] Colours = {Color.Green, Color.GreenYellow, Color.Pink, Color.SaddleBrown, Color.Silver};
				int ColourIndex = 0;
				for (int i = 0; i != Crops.Count; i++)
					{
					string CropName = Crops[i];
					string SeriesName = CropName + " CLL";

					double[] LL = MathUtility.Multiply_Value(SoilBase.CalcXForPlotting(MySoil.LL(CropName)), 100);
					Helper.CreateChartSeriesFromArray(SeriesName, 
														LL, CumThickness,
														false, Colours[ColourIndex], 3, LinePattern.Solid,
														StandardAxis.PrimaryX, StandardAxis.PrimaryY);
					ColourIndex++;
					if (ColourIndex == Colours.Length) ColourIndex = 0;
					}
				}
			Chart.Refresh();
			}


		// --------------------------------
		// Populate the water chart.
		// --------------------------------
		private static void PopulateWaterChartLines(Xceed.Chart.ChartControl Chart, Soil MySoil,
													StringCollection Crops, bool ShowSW)
			{
			ChartHelper Helper = new ChartHelper();
			Helper.Chart = Chart;
			Chart.Charts[0].Series.Clear();

			Helper.CreateChartSeriesFromArray("SAT", 
												MathUtility.Multiply_Value(MySoil.SAT, 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10), 
												false, Color.Blue, 1, LinePattern.Dash,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);

			Helper.CreateChartSeriesFromArray("DUL", 
												MathUtility.Multiply_Value(MySoil.DUL, 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10), 
												false, Color.Blue, 1, LinePattern.Solid,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);

			Helper.CreateChartSeriesFromArray("LL15", 
												MathUtility.Multiply_Value(MySoil.LL15, 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10),
												false, Color.Red, 1, LinePattern.Solid,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);

			Helper.CreateChartSeriesFromArray("AirDry", 
												MathUtility.Multiply_Value(MySoil.Airdry, 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10),
												false, Color.Red, 1, LinePattern.Dash,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);

			if (ShowSW && MySoil.InitialWater.SW.Length > 0)
				Helper.CreateChartSeriesFromArray("SW", 
													MathUtility.Multiply_Value(MySoil.InitialWater.SW, 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10),
													false, Color.Aquamarine, 3, LinePattern.Solid,
													StandardAxis.PrimaryX, StandardAxis.PrimaryY);
			
			Color[] Colours = {Color.Green, Color.GreenYellow, Color.Pink, Color.SaddleBrown, Color.Silver};
			int ColourIndex = 0;
			for (int i = 0; i != Crops.Count; i++)
				{
				string CropName = Crops[i];
				string SeriesName = CropName + " CLL";

				Helper.CreateChartSeriesFromArray(SeriesName, 
													MathUtility.Multiply_Value(MySoil.LL(CropName), 100), MathUtility.Divide_Value(MySoil.CumThicknessMidPoints, 10),
													false, Colours[ColourIndex], 3, LinePattern.Solid,
													StandardAxis.PrimaryX, StandardAxis.PrimaryY);
				ColourIndex++;
				if (ColourIndex == Colours.Length) ColourIndex = 0;
				}

			Chart.Refresh();
			}



		// ---------------------------------------
		// Return a list of crops to put on chart
		// ---------------------------------------
		private StringCollection CropsToChart()
			{
			StringCollection Crops = new StringCollection();
			for (int i = 0; i != CropList.CheckedItems.Count; i++)
				Crops.Add(CropList.CheckedItems[i].ToString());
			return Crops;
			}


		// ------------------------------------
		// User has clicked on chart list.
		// ------------------------------------
		private void CropList_ItemCheck(object sender, System.Windows.Forms.ItemCheckEventArgs e)
			{
			StringCollection Crops = CropsToChart();
			if (e.NewValue == CheckState.Unchecked)
				Crops.Remove(CropList.Items[e.Index].ToString());
			else
				Crops.Add(CropList.Items[e.Index].ToString());
			PopulateWaterChart(Crops);            
			}

		// ---------------------------------------
		// Use has changed the lines checkbox.
		// ---------------------------------------
		private void LineGraphCheck_CheckedChanged(object sender, System.EventArgs e)
			{
			string CheckValue = "no";
			if (LineGraphCheck.Checked)
				CheckValue = "yes";
			APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), 
									"soil", "DepthChartWithLines", CheckValue);
			PopulateWaterChart(CropsToChart());
			}


	
		}
	}
