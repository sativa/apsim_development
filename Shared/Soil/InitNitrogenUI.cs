using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using CSGeneral;
using VBGeneral;
using Xceed.Chart.Standard;
using Xceed.Chart.Core;


namespace CSGeneral
	{
	public class InitNitrogenUI : VBGeneral.BaseView
		{
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.Panel panel1;
		private FarPoint.Win.Spread.FpSpread Grid;
		private FarPoint.Win.Spread.SheetView NitrogenGrid;
		private bool UserChange = true;
		private InitNitrogen InitialNitrogen;
		internal Xceed.Chart.ChartControl NitrogenChart;
		private Soil SoilData;

		#region Constructor / Destructor
		public InitNitrogenUI()
			{
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if (components != null) 
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(InitNitrogenUI));
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType1 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType2 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType3 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType4 = new FarPoint.Win.Spread.CellType.NumberCellType();
			this.NitrogenChart = new Xceed.Chart.ChartControl();
			this.panel1 = new System.Windows.Forms.Panel();
			this.Grid = new FarPoint.Win.Spread.FpSpread();
			this.NitrogenGrid = new FarPoint.Win.Spread.SheetView();
			this.panel1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).BeginInit();
			this.SuspendLayout();
			// 
			// NitrogenChart
			// 
			this.NitrogenChart.AutoScrollMargin = new System.Drawing.Size(0, 0);
			this.NitrogenChart.AutoScrollMinSize = new System.Drawing.Size(0, 0);
			this.NitrogenChart.BackColor = System.Drawing.SystemColors.ActiveBorder;
			this.NitrogenChart.Background = ((Xceed.Chart.Standard.Background)(resources.GetObject("NitrogenChart.Background")));
			this.NitrogenChart.Charts = ((Xceed.Chart.Core.ChartCollection)(resources.GetObject("NitrogenChart.Charts")));
			this.NitrogenChart.Dock = System.Windows.Forms.DockStyle.Fill;
			this.NitrogenChart.InteractivityOperations = ((Xceed.Chart.Standard.InteractivityOperationsCollection)(resources.GetObject("NitrogenChart.InteractivityOperations")));
			this.NitrogenChart.Labels = ((Xceed.Chart.Standard.ChartLabelCollection)(resources.GetObject("NitrogenChart.Labels")));
			this.NitrogenChart.Legends = ((Xceed.Chart.Core.LegendCollection)(resources.GetObject("NitrogenChart.Legends")));
			this.NitrogenChart.Location = new System.Drawing.Point(0, 328);
			this.NitrogenChart.Name = "NitrogenChart";
			this.NitrogenChart.Settings = ((Xceed.Chart.Core.Settings)(resources.GetObject("NitrogenChart.Settings")));
			this.NitrogenChart.Size = new System.Drawing.Size(726, 445);
			this.NitrogenChart.TabIndex = 32;
			this.NitrogenChart.Watermarks = ((Xceed.Chart.Standard.WatermarkCollection)(resources.GetObject("NitrogenChart.Watermarks")));
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this.Grid);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.panel1.Location = new System.Drawing.Point(0, 40);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(726, 288);
			this.panel1.TabIndex = 36;
			// 
			// Grid
			// 
			this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.Grid.EditModeReplace = true;
			this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
			this.Grid.Location = new System.Drawing.Point(0, 0);
			this.Grid.Name = "Grid";
			this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
																			  this.NitrogenGrid});
			this.Grid.Size = new System.Drawing.Size(726, 288);
			this.Grid.TabIndex = 36;
			this.Grid.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
			// 
			// NitrogenGrid
			// 
			this.NitrogenGrid.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.NitrogenGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.NitrogenGrid.ColumnCount = 5;
			this.NitrogenGrid.ColumnHeader.RowCount = 2;
			this.NitrogenGrid.ColumnHeader.Cells.Get(0, 0).Text = "Depth";
			this.NitrogenGrid.ColumnHeader.Cells.Get(0, 1).Text = "NO3";
			this.NitrogenGrid.ColumnHeader.Cells.Get(0, 2).Text = "NH4";
			this.NitrogenGrid.ColumnHeader.Cells.Get(0, 3).Text = "NO3";
			this.NitrogenGrid.ColumnHeader.Cells.Get(0, 4).Text = "NH4";
			this.NitrogenGrid.ColumnHeader.Cells.Get(1, 0).Text = "(cm)";
			this.NitrogenGrid.ColumnHeader.Cells.Get(1, 1).Text = "(kg/ha)";
			this.NitrogenGrid.ColumnHeader.Cells.Get(1, 2).Text = "(kg/ha)";
			this.NitrogenGrid.ColumnHeader.Cells.Get(1, 3).Text = "(ppm)";
			this.NitrogenGrid.ColumnHeader.Cells.Get(1, 4).Text = "(ppm)";
			this.NitrogenGrid.Columns.Get(0).BackColor = System.Drawing.Color.FromArgb(((System.Byte)(224)), ((System.Byte)(224)), ((System.Byte)(224)));
			this.NitrogenGrid.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.NitrogenGrid.Columns.Get(0).Label = "(cm)";
			this.NitrogenGrid.Columns.Get(0).Locked = true;
			numberCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType1.DecimalPlaces = 3;
			numberCellType1.DropDownButton = false;
			this.NitrogenGrid.Columns.Get(1).CellType = numberCellType1;
			this.NitrogenGrid.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.NitrogenGrid.Columns.Get(1).Label = "(kg/ha)";
			numberCellType2.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType2.DecimalPlaces = 3;
			numberCellType2.DropDownButton = false;
			this.NitrogenGrid.Columns.Get(2).CellType = numberCellType2;
			this.NitrogenGrid.Columns.Get(2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.NitrogenGrid.Columns.Get(2).Label = "(kg/ha)";
			this.NitrogenGrid.Columns.Get(3).BackColor = System.Drawing.Color.FromArgb(((System.Byte)(255)), ((System.Byte)(255)), ((System.Byte)(192)));
			numberCellType3.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType3.DecimalPlaces = 3;
			numberCellType3.DropDownButton = false;
			this.NitrogenGrid.Columns.Get(3).CellType = numberCellType3;
			this.NitrogenGrid.Columns.Get(3).Label = "(ppm)";
			this.NitrogenGrid.Columns.Get(4).BackColor = System.Drawing.Color.FromArgb(((System.Byte)(255)), ((System.Byte)(255)), ((System.Byte)(192)));
			numberCellType4.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType4.DecimalPlaces = 3;
			numberCellType4.DropDownButton = false;
			this.NitrogenGrid.Columns.Get(4).CellType = numberCellType4;
			this.NitrogenGrid.Columns.Get(4).Label = "(ppm)";
			this.NitrogenGrid.RowHeader.Columns.Default.Resizable = false;
			this.NitrogenGrid.RowHeader.Visible = false;
			this.NitrogenGrid.SheetName = "Sheet1";
			this.NitrogenGrid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.NitrogenGrid_CellChanged);
			this.NitrogenGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// InitNitrogenUI
			// 
			this.Controls.Add(this.NitrogenChart);
			this.Controls.Add(this.panel1);
			this.Name = "InitNitrogenUI";
			this.Size = new System.Drawing.Size(726, 773);
			this.Controls.SetChildIndex(this.panel1, 0);
			this.Controls.SetChildIndex(this.NitrogenChart, 0);
			this.panel1.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).EndInit();
			this.ResumeLayout(false);

		}
		#endregion
		#endregion

		override public void Refresh()
			{
			base.Refresh();

			HelpText = "There are two ways of specifying initial soil nitrogen. You can either type a number for each layer (kg/ha or ppm) "
					 + " or a total NO3 / NH4 number (kg/ha only) on the last row of the grid.";

			SoilData = new Soil(Controller.Data.Parent);
			InitialNitrogen = SoilData.InitialNitrogen;
			FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused); 
			InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), 
							FarPoint.Win.Spread.SpreadActions.ClipboardCut); 
			InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), 
							FarPoint.Win.Spread.SpreadActions.MoveToNextRow); 

			PopulateGrid();
			UpdateGraph();
			NitrogenChart.Legends[0].Data.OutlineHorizontalLinesProps.Width = 0;
			NitrogenChart.Legends[0].Data.OutlineVerticalLinesProps.Width = 0;
			NitrogenChart.Legends[0].Data.VerticalLinesProps.Width = 0;
			NitrogenChart.Legends[0].Data.HorizontalLinesProps.Width = 0;
			}

		private void PopulateGrid()
			{
			UserChange = false;
			GridUtils.SetColumnAsStrings(NitrogenGrid, 0, SoilData.DepthStrings);
			GridUtils.SetColumnAsDoubles(NitrogenGrid, 1, InitialNitrogen.NO3KgHa);
			GridUtils.SetColumnAsDoubles(NitrogenGrid, 2, InitialNitrogen.NH4KgHa);
			GridUtils.SetColumnAsDoubles(NitrogenGrid, 3, InitialNitrogen.NO3);
			GridUtils.SetColumnAsDoubles(NitrogenGrid, 4, InitialNitrogen.NH4);
			NitrogenGrid.RowCount = SoilData.DepthStrings.Length+1;

			int SummaryRow = NitrogenGrid.RowCount-1;
			NitrogenGrid.Cells[SummaryRow, 0].Value = "Totals:";
			NitrogenGrid.Cells[SummaryRow, 1].Value = InitialNitrogen.TotalNO3KgHa;
			NitrogenGrid.Cells[SummaryRow, 2].Value = InitialNitrogen.TotalNH4KgHa;
			NitrogenGrid.Cells[SummaryRow, 0, SummaryRow, 2].BackColor = Color.Yellow;
			NitrogenGrid.Cells[SummaryRow, 3, SummaryRow, 4].Locked = true;

			UserChange = true;
			}

		private void NitrogenGrid_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				int SummaryRow = NitrogenGrid.RowCount-1;

				if (e.Row == SummaryRow)
					if (e.Column == 1)
						{
						// User changed total no3
						InitialNitrogen.TotalNO3KgHa = Convert.ToDouble(NitrogenGrid.Cells[e.Row, e.Column].Value);
						GridUtils.SetColumnAsDoubles(NitrogenGrid, 1, InitialNitrogen.NO3KgHa);
						GridUtils.SetColumnAsDoubles(NitrogenGrid, 3, InitialNitrogen.NO3);
						}
					else
						{
						// User changed total nh4
						InitialNitrogen.TotalNH4KgHa = Convert.ToDouble(NitrogenGrid.Cells[e.Row, e.Column].Value);
						GridUtils.SetColumnAsDoubles(NitrogenGrid, 2, InitialNitrogen.NH4KgHa);
						GridUtils.SetColumnAsDoubles(NitrogenGrid, 4, InitialNitrogen.NH4);
						}
				else if (e.Column == 1)
					{
					// user changed layered no3 (kg/ha)
					double[] no3 = GridUtils.GetColumnAsDoubles(NitrogenGrid, 1, SoilData.Thickness.Length);
					InitialNitrogen.NO3KgHa = no3;
					NitrogenGrid.Cells[SummaryRow, 1].Value = InitialNitrogen.TotalNO3KgHa;
					GridUtils.SetColumnAsDoubles(NitrogenGrid, 3, InitialNitrogen.NO3);
					}
				else if (e.Column == 2)
					{
					// user changed layered nh4 (kg/ha)
					double[] nh4 = GridUtils.GetColumnAsDoubles(NitrogenGrid, 2, SoilData.Thickness.Length);
					InitialNitrogen.NH4KgHa = nh4;
					NitrogenGrid.Cells[SummaryRow, 2].Value = InitialNitrogen.TotalNH4KgHa;
					GridUtils.SetColumnAsDoubles(NitrogenGrid, 4, InitialNitrogen.NH4);
					}
				else if (e.Column == 3)
					{
					// user changed layered no3 (ppm)
					double[] no3 = GridUtils.GetColumnAsDoubles(NitrogenGrid, 3, SoilData.Thickness.Length);
					InitialNitrogen.NO3 = no3;
					GridUtils.SetColumnAsDoubles(NitrogenGrid, 1, InitialNitrogen.NO3KgHa);
					NitrogenGrid.Cells[SummaryRow, 1].Value = InitialNitrogen.TotalNO3KgHa;
					}
				else if (e.Column == 4)
					{
					// user changed layered nh4 (ppm)
					double[] nh4 = GridUtils.GetColumnAsDoubles(NitrogenGrid, 4, SoilData.Thickness.Length);
					InitialNitrogen.NH4 = nh4;
					GridUtils.SetColumnAsDoubles(NitrogenGrid, 2, InitialNitrogen.NH4KgHa);
					NitrogenGrid.Cells[SummaryRow, 2].Value = InitialNitrogen.TotalNH4KgHa;
					}
				UpdateGraph();
				UserChange = true;
				}
			}
		
		private void UpdateGraph()
			{
			ChartHelper Helper = new ChartHelper();
			Helper.Chart = NitrogenChart;
			NitrogenChart.Charts[0].Series.Clear();

			Helper.CreateChartSeriesFromArray("NO3", 
												InitialNitrogen.NO3KgHa, MathUtility.Divide_Value(SoilData.CumThicknessMidPoints, 10), 
												false, Color.Green, 3, LinePattern.Solid,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);

			Helper.CreateChartSeriesFromArray("NH4", 
												InitialNitrogen.NH4KgHa, MathUtility.Divide_Value(SoilData.CumThicknessMidPoints, 10), 
												false, Color.Brown, 3, LinePattern.Solid,
												StandardAxis.PrimaryX, StandardAxis.PrimaryY);
			NitrogenChart.Refresh();
			}


	}
}

