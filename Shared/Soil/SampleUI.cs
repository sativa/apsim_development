using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;
using VBGeneral;

namespace CSGeneral
	{

	public class SampleUI : VBGeneral.BaseView
		{
		private System.Windows.Forms.Panel UnitsPanel;
		private System.Windows.Forms.Splitter splitter1;
		private CSGeneral.WaterChartControl waterChartControl1;
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.ComboBox WaterUnits;
		private System.Windows.Forms.DateTimePicker SampleDate;
		private FarPoint.Win.Spread.FpSpread FpSpread;
		private FarPoint.Win.Spread.SheetView Grid;
		private CSGeneral.SoilSample MySample;
		private bool UserChange = true;

		public SampleUI()
			{
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
			this.FpSpread = new FarPoint.Win.Spread.FpSpread();
			this.Grid = new FarPoint.Win.Spread.SheetView();
			this.UnitsPanel = new System.Windows.Forms.Panel();
			this.label2 = new System.Windows.Forms.Label();
			this.WaterUnits = new System.Windows.Forms.ComboBox();
			this.SampleDate = new System.Windows.Forms.DateTimePicker();
			this.label1 = new System.Windows.Forms.Label();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.waterChartControl1 = new CSGeneral.WaterChartControl();
			((System.ComponentModel.ISupportInitialize)(this.FpSpread)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
			this.UnitsPanel.SuspendLayout();
			this.SuspendLayout();
			// 
			// FpSpread
			// 
			this.FpSpread.AllowDragDrop = true;
			this.FpSpread.Dock = System.Windows.Forms.DockStyle.Top;
			this.FpSpread.EditModeReplace = true;
			this.FpSpread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
			this.FpSpread.Location = new System.Drawing.Point(0, 72);
			this.FpSpread.Name = "FpSpread";
			this.FpSpread.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
																				  this.Grid});
			this.FpSpread.Size = new System.Drawing.Size(696, 288);
			this.FpSpread.TabIndex = 2;
			this.FpSpread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
			// 
			// Grid
			// 
			this.Grid.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.Grid.ColumnCount = 9;
			this.Grid.ColumnHeader.RowCount = 2;
			this.Grid.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
			this.Grid.ColumnHeader.Cells.Get(0, 0).Text = "Depth";
			this.Grid.ColumnHeader.Cells.Get(0, 1).Text = "Wet";
			this.Grid.ColumnHeader.Cells.Get(0, 2).Text = "Dry";
			this.Grid.ColumnHeader.Cells.Get(0, 3).Text = "NO3";
			this.Grid.ColumnHeader.Cells.Get(0, 4).Text = "NH4";
			this.Grid.ColumnHeader.Cells.Get(0, 5).Text = "OC";
			this.Grid.ColumnHeader.Cells.Get(0, 6).Text = "EC";
			this.Grid.ColumnHeader.Cells.Get(0, 7).Text = "pH";
			this.Grid.ColumnHeader.Cells.Get(0, 8).Text = "ESP";
			this.Grid.ColumnHeader.Cells.Get(1, 0).Text = "(cm)";
			this.Grid.ColumnHeader.Cells.Get(1, 1).Text = "(grav%)";
			this.Grid.ColumnHeader.Cells.Get(1, 2).Text = "(grav%)";
			this.Grid.ColumnHeader.Cells.Get(1, 3).Text = "(ppm)";
			this.Grid.ColumnHeader.Cells.Get(1, 4).Text = "(ppm)";
			this.Grid.ColumnHeader.Cells.Get(1, 5).Text = "(%C)";
			this.Grid.ColumnHeader.Cells.Get(1, 6).Text = "(dS/m)";
			this.Grid.ColumnHeader.Cells.Get(1, 7).Text = "(CaCl2)";
			this.Grid.ColumnHeader.Cells.Get(1, 8).Text = "(%)";
			this.Grid.RowHeader.Columns.Default.Resizable = false;
			this.Grid.RowHeader.Visible = false;
			this.Grid.SheetName = "Water / Nitrogen";
			this.Grid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.Grid_CellChanged);
			this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// UnitsPanel
			// 
			this.UnitsPanel.Controls.Add(this.label2);
			this.UnitsPanel.Controls.Add(this.WaterUnits);
			this.UnitsPanel.Controls.Add(this.SampleDate);
			this.UnitsPanel.Controls.Add(this.label1);
			this.UnitsPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.UnitsPanel.Location = new System.Drawing.Point(0, 40);
			this.UnitsPanel.Name = "UnitsPanel";
			this.UnitsPanel.Size = new System.Drawing.Size(696, 32);
			this.UnitsPanel.TabIndex = 3;
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(313, 8);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(65, 16);
			this.label2.TabIndex = 3;
			this.label2.Text = "Water units:";
			// 
			// WaterUnits
			// 
			this.WaterUnits.Items.AddRange(new object[] {
															"Volumetric %",
															"Gravimetric %",
															"Wet and Dry %"});
			this.WaterUnits.Location = new System.Drawing.Point(392, 5);
			this.WaterUnits.Name = "WaterUnits";
			this.WaterUnits.Size = new System.Drawing.Size(121, 21);
			this.WaterUnits.TabIndex = 2;
			this.WaterUnits.Text = "Volumetric %";
			this.WaterUnits.SelectedIndexChanged += new System.EventHandler(this.WaterUnits_SelectedIndexChanged);
			// 
			// SampleDate
			// 
			this.SampleDate.Location = new System.Drawing.Point(96, 6);
			this.SampleDate.Name = "SampleDate";
			this.SampleDate.Size = new System.Drawing.Size(192, 20);
			this.SampleDate.TabIndex = 1;
			this.SampleDate.ValueChanged += new System.EventHandler(this.SampleDate_ValueChanged);
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(8, 8);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(71, 16);
			this.label1.TabIndex = 0;
			this.label1.Text = "Sample date:";
			// 
			// splitter1
			// 
			this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
			this.splitter1.Location = new System.Drawing.Point(0, 360);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(696, 3);
			this.splitter1.TabIndex = 4;
			this.splitter1.TabStop = false;
			// 
			// waterChartControl1
			// 
			this.waterChartControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.waterChartControl1.LinkedSoil = null;
			this.waterChartControl1.Location = new System.Drawing.Point(0, 363);
			this.waterChartControl1.Name = "waterChartControl1";
			this.waterChartControl1.ShowSoilWaterLine = false;
			this.waterChartControl1.Size = new System.Drawing.Size(696, 414);
			this.waterChartControl1.TabIndex = 5;
			// 
			// SampleUI
			// 
			this.Controls.Add(this.waterChartControl1);
			this.Controls.Add(this.splitter1);
			this.Controls.Add(this.FpSpread);
			this.Controls.Add(this.UnitsPanel);
			this.Name = "SampleUI";
			this.Size = new System.Drawing.Size(696, 777);
			this.Controls.SetChildIndex(this.UnitsPanel, 0);
			this.Controls.SetChildIndex(this.FpSpread, 0);
			this.Controls.SetChildIndex(this.splitter1, 0);
			this.Controls.SetChildIndex(this.waterChartControl1, 0);
			((System.ComponentModel.ISupportInitialize)(this.FpSpread)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
			this.UnitsPanel.ResumeLayout(false);
			this.ResumeLayout(false);

		}
		#endregion

		override public void RefreshView(BaseController Controller)
			{
            base.RefreshView(Controller);
			try
				{
				if (MySample == null)
					{
					FarPoint.Win.Spread.InputMap InputMap = FpSpread.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused); 
					InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), 
								FarPoint.Win.Spread.SpreadActions.ClipboardCut); 
					InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), 
								FarPoint.Win.Spread.SpreadActions.MoveToNextRow); 
					}
				MySample = new CSGeneral.SoilSample(Controller.Data);
				PopulateGrid();
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}

		private void PopulateGrid()
			{
			UserChange = false;
			Grid.RowCount = 1;
			Grid.RowCount = 20;
			GridUtils.SetColumnAsStrings(Grid, 0, MySample.DepthStrings);

			SampleDate.Value = MySample.SampleDate;
			if (MySample.StoredWaterFormat == SoilSample.StoredWaterFormatType.VolumetricPercent)
				{
				WaterUnits.SelectedIndex = 0;
				GridUtils.SetColumnAsDoubles(Grid, 1, MySample.SW);
				Grid.Columns[2].Visible = false;
				Grid.ColumnHeader.Cells[0, 1].Text = "Water";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%vol)";
				}
			else if (MySample.StoredWaterFormat == SoilSample.StoredWaterFormatType.GravimetricPercent)
				{
				WaterUnits.SelectedIndex = 1;
				GridUtils.SetColumnAsDoubles(Grid, 1, MySample.SWGrav);
				Grid.Columns[2].Visible = false;
				Grid.ColumnHeader.Cells[0, 1].Text = "Water";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%vol)";
				}
			else
				{
				WaterUnits.SelectedIndex = 2;
				GridUtils.SetColumnAsDoubles(Grid, 1, MySample.Wet);
				Grid.Columns[2].Visible = true;
				Grid.ColumnHeader.Cells[0, 1].Text = "Wet";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%grav)";
				Grid.ColumnHeader.Cells[0, 2].Text = "Dry";
				Grid.ColumnHeader.Cells[1, 2].Text = "(%grav)";
				}
			GridUtils.SetColumnAsDoubles(Grid, 3, MySample.NO3);
			GridUtils.SetColumnAsDoubles(Grid, 4, MySample.NH4);
			GridUtils.SetColumnAsDoubles(Grid, 5, MySample.OC);
			GridUtils.SetColumnAsDoubles(Grid, 6, MySample.EC);
			GridUtils.SetColumnAsDoubles(Grid, 7, MySample.PH);
			GridUtils.SetColumnAsDoubles(Grid, 8, MySample.ESP);
			UserChange = true;
			}								   

		private void SaveGrid()
			{
			int NumLayers = GridUtils.FindFirstBlankCell(Grid, 0);
			MySample.DepthStrings = GridUtils.GetColumnAsStrings(Grid, 0, NumLayers);
			if (WaterUnits.SelectedIndex == 0)
                MySample.SW = GridUtils.GetColumnAsDoubles(Grid, 1, NumLayers);
			else if (WaterUnits.SelectedIndex == 1)
                MySample.SWGrav = GridUtils.GetColumnAsDoubles(Grid, 1, NumLayers);
			else if (WaterUnits.SelectedIndex == 2)
				{
                MySample.Wet = GridUtils.GetColumnAsDoubles(Grid, 1, NumLayers);
                MySample.Dry = GridUtils.GetColumnAsDoubles(Grid, 2, NumLayers);
				}

            MySample.NO3 = GridUtils.GetColumnAsDoubles(Grid, 3, NumLayers);
            MySample.NH4 = GridUtils.GetColumnAsDoubles(Grid, 4, NumLayers);
            MySample.OC = GridUtils.GetColumnAsDoubles(Grid, 5, NumLayers);
            MySample.EC = GridUtils.GetColumnAsDoubles(Grid, 6, NumLayers);
            MySample.PH = GridUtils.GetColumnAsDoubles(Grid, 7, NumLayers);
            MySample.ESP = GridUtils.GetColumnAsDoubles(Grid, 8, NumLayers);
			}

		private void SampleDate_ValueChanged(object sender, System.EventArgs e)
			{
			MySample.SampleDate = SampleDate.Value;
			}

		private void WaterUnits_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				SaveGrid();
				PopulateGrid();
				}
			}

		private void Grid_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
                SaveGrid();
			}


		}
	}
