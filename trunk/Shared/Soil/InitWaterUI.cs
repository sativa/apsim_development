using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Collections.Specialized;
using CSGeneral;
using VBGeneral;
using Xceed.Chart;
using Xceed.Chart.Core;
using Xceed.Chart.Standard;
									    
namespace CSGeneral
	{
	public class InitWaterUI : VBGeneral.BaseUI
		{
		private System.Windows.Forms.Panel panel1;
		internal System.Windows.Forms.RadioButton LayeredRadio;
		internal System.Windows.Forms.GroupBox GroupBox;
		internal System.Windows.Forms.Panel DepthWetSoilPanel;
		internal System.Windows.Forms.Label Label3;
		internal System.Windows.Forms.TextBox DepthEdit;
		internal System.Windows.Forms.Panel PercentPanel;
		internal System.Windows.Forms.RadioButton EvenlyDistributedRadio;
		internal System.Windows.Forms.RadioButton FilledFromTopRadio;
		internal System.Windows.Forms.Label Label2;
		internal System.Windows.Forms.TextBox PAWCEdit;
		internal System.Windows.Forms.Label Label1;
		internal System.Windows.Forms.NumericUpDown PercentEdit;
		internal System.Windows.Forms.RadioButton DepthWetSoilRadio;
		internal System.Windows.Forms.RadioButton PercentRadio;
		internal Xceed.Grid.Column Column1;
		internal Xceed.Grid.Column Column2;
		internal Xceed.Grid.Column Column3;
		internal Xceed.Grid.Column Column4;
		internal Xceed.Grid.DataRow dataRowTemplate2;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column1;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column2;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column3;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column4;
		internal Xceed.Grid.GroupByRow GroupByRow2;
		internal Xceed.Grid.ColumnManagerRow ColumnManagerRow2;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column1;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column2;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column3;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column4;
		private System.Windows.Forms.Splitter splitter1;
		private System.ComponentModel.IContainer components = null;
		private Soil SoilData;
		internal Xceed.Grid.GridControl WaterGrid;
		private InitWater InitialWater;
		private CSGeneral.WaterChartControl WaterChartControl;
		private bool UserChange = true;
		

		// -------------------
		// constructor.
		// -------------------
		public InitWaterUI()
			{
			// This call is required by the Windows Form Designer.
			InitializeComponent();
			}

		// ------------------------------------
		// Clean up any resources being used.
		// ------------------------------------
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
			this.panel1 = new System.Windows.Forms.Panel();
			this.WaterGrid = new Xceed.Grid.GridControl();
			this.Column1 = new Xceed.Grid.Column();
			this.Column2 = new Xceed.Grid.Column();
			this.Column3 = new Xceed.Grid.Column();
			this.Column4 = new Xceed.Grid.Column();
			this.dataRowTemplate2 = new Xceed.Grid.DataRow();
			this.celldataRowTemplate2Column1 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate2Column2 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate2Column3 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate2Column4 = new Xceed.Grid.DataCell();
			this.GroupByRow2 = new Xceed.Grid.GroupByRow();
			this.ColumnManagerRow2 = new Xceed.Grid.ColumnManagerRow();
			this.cellColumnManagerRow2Column1 = new Xceed.Grid.ColumnManagerCell();
			this.cellColumnManagerRow2Column2 = new Xceed.Grid.ColumnManagerCell();
			this.cellColumnManagerRow2Column3 = new Xceed.Grid.ColumnManagerCell();
			this.cellColumnManagerRow2Column4 = new Xceed.Grid.ColumnManagerCell();
			this.LayeredRadio = new System.Windows.Forms.RadioButton();
			this.GroupBox = new System.Windows.Forms.GroupBox();
			this.DepthWetSoilPanel = new System.Windows.Forms.Panel();
			this.Label3 = new System.Windows.Forms.Label();
			this.DepthEdit = new System.Windows.Forms.TextBox();
			this.PercentPanel = new System.Windows.Forms.Panel();
			this.EvenlyDistributedRadio = new System.Windows.Forms.RadioButton();
			this.FilledFromTopRadio = new System.Windows.Forms.RadioButton();
			this.Label2 = new System.Windows.Forms.Label();
			this.PAWCEdit = new System.Windows.Forms.TextBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.PercentEdit = new System.Windows.Forms.NumericUpDown();
			this.DepthWetSoilRadio = new System.Windows.Forms.RadioButton();
			this.PercentRadio = new System.Windows.Forms.RadioButton();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.WaterChartControl = new CSGeneral.WaterChartControl();
			this.panel1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate2)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.ColumnManagerRow2)).BeginInit();
			this.GroupBox.SuspendLayout();
			this.DepthWetSoilPanel.SuspendLayout();
			this.PercentPanel.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).BeginInit();
			this.SuspendLayout();
			// 
			// panel1
			// 
			this.panel1.Controls.Add(this.WaterGrid);
			this.panel1.Controls.Add(this.LayeredRadio);
			this.panel1.Controls.Add(this.GroupBox);
			this.panel1.Controls.Add(this.DepthWetSoilRadio);
			this.panel1.Controls.Add(this.PercentRadio);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.panel1.Location = new System.Drawing.Point(0, 20);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(312, 753);
			this.panel1.TabIndex = 2;
			// 
			// WaterGrid
			// 
			this.WaterGrid.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.WaterGrid.Columns.Add(this.Column1);
			this.WaterGrid.Columns.Add(this.Column2);
			this.WaterGrid.Columns.Add(this.Column3);
			this.WaterGrid.Columns.Add(this.Column4);
			this.WaterGrid.DataRowTemplate = this.dataRowTemplate2;
			this.WaterGrid.FixedHeaderRows.Add(this.GroupByRow2);
			this.WaterGrid.FixedHeaderRows.Add(this.ColumnManagerRow2);
			this.WaterGrid.Location = new System.Drawing.Point(8, 224);
			this.WaterGrid.Name = "WaterGrid";
			// 
			// WaterGrid.RowSelectorPane
			// 
			this.WaterGrid.RowSelectorPane.Visible = false;
			this.WaterGrid.Size = new System.Drawing.Size(288, 451);
			this.WaterGrid.TabIndex = 29;
			// 
			// Column1
			// 
			this.Column1.CanBeSorted = false;
			this.Column1.ReadOnly = true;
			this.Column1.Title = "Depth(cm)";
			this.Column1.VisibleIndex = 0;
			this.Column1.Width = 65;
			this.Column1.Initialize("Column1", typeof(string));
			// 
			// Column2
			// 
			this.Column2.CanBeSorted = false;
			this.Column2.ReadOnly = true;
			this.Column2.Title = "LL15 (%)";
			this.Column2.VisibleIndex = 1;
			this.Column2.Width = 63;
			this.Column2.Initialize("Column2", typeof(string));
			// 
			// Column3
			// 
			this.Column3.CanBeSorted = false;
			this.Column3.ReadOnly = true;
			this.Column3.Title = "DUL (%)";
			this.Column3.VisibleIndex = 2;
			this.Column3.Width = 64;
			this.Column3.Initialize("Column3", typeof(string));
			// 
			// Column4
			// 
			this.Column4.CanBeSorted = false;
			this.Column4.Title = "SW (%)";
			this.Column4.VisibleIndex = 3;
			this.Column4.Width = 63;
			this.Column4.Initialize("Column4", typeof(string));
			// 
			// dataRowTemplate2
			// 
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column1);
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column2);
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column3);
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column4);
			this.celldataRowTemplate2Column1.Initialize("Column1");
			this.celldataRowTemplate2Column2.Initialize("Column2");
			this.celldataRowTemplate2Column3.Initialize("Column3");
			this.celldataRowTemplate2Column4.Initialize("Column4");
			// 
			// GroupByRow2
			// 
			this.GroupByRow2.Visible = false;
			// 
			// ColumnManagerRow2
			// 
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column1);
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column2);
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column3);
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column4);
			this.cellColumnManagerRow2Column1.Initialize("Column1");
			this.cellColumnManagerRow2Column2.Initialize("Column2");
			this.cellColumnManagerRow2Column3.Initialize("Column3");
			this.cellColumnManagerRow2Column4.Initialize("Column4");
			// 
			// LayeredRadio
			// 
			this.LayeredRadio.Location = new System.Drawing.Point(8, 56);
			this.LayeredRadio.Name = "LayeredRadio";
			this.LayeredRadio.Size = new System.Drawing.Size(293, 21);
			this.LayeredRadio.TabIndex = 28;
			this.LayeredRadio.Text = "Specify water as layered values";
			this.LayeredRadio.CheckedChanged += new System.EventHandler(this.LayeredRadio_CheckedChanged);
			// 
			// GroupBox
			// 
			this.GroupBox.Controls.Add(this.DepthWetSoilPanel);
			this.GroupBox.Controls.Add(this.PercentPanel);
			this.GroupBox.Location = new System.Drawing.Point(24, 96);
			this.GroupBox.Name = "GroupBox";
			this.GroupBox.Size = new System.Drawing.Size(353, 118);
			this.GroupBox.TabIndex = 27;
			this.GroupBox.TabStop = false;
			this.GroupBox.Text = "Properties";
			// 
			// DepthWetSoilPanel
			// 
			this.DepthWetSoilPanel.Controls.Add(this.Label3);
			this.DepthWetSoilPanel.Controls.Add(this.DepthEdit);
			this.DepthWetSoilPanel.Location = new System.Drawing.Point(13, 21);
			this.DepthWetSoilPanel.Name = "DepthWetSoilPanel";
			this.DepthWetSoilPanel.Size = new System.Drawing.Size(334, 34);
			this.DepthWetSoilPanel.TabIndex = 16;
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(67, 7);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(39, 16);
			this.Label3.TabIndex = 14;
			this.Label3.Text = "cm soil";
			// 
			// DepthEdit
			// 
			this.DepthEdit.Location = new System.Drawing.Point(7, 7);
			this.DepthEdit.Name = "DepthEdit";
			this.DepthEdit.Size = new System.Drawing.Size(53, 20);
			this.DepthEdit.TabIndex = 13;
			this.DepthEdit.Text = "";
			this.DepthEdit.TextChanged += new System.EventHandler(this.DepthEdit_TextChanged);
			// 
			// PercentPanel
			// 
			this.PercentPanel.Controls.Add(this.EvenlyDistributedRadio);
			this.PercentPanel.Controls.Add(this.FilledFromTopRadio);
			this.PercentPanel.Controls.Add(this.Label2);
			this.PercentPanel.Controls.Add(this.PAWCEdit);
			this.PercentPanel.Controls.Add(this.Label1);
			this.PercentPanel.Controls.Add(this.PercentEdit);
			this.PercentPanel.Location = new System.Drawing.Point(13, 21);
			this.PercentPanel.Name = "PercentPanel";
			this.PercentPanel.Size = new System.Drawing.Size(334, 90);
			this.PercentPanel.TabIndex = 15;
			// 
			// EvenlyDistributedRadio
			// 
			this.EvenlyDistributedRadio.Location = new System.Drawing.Point(147, 42);
			this.EvenlyDistributedRadio.Name = "EvenlyDistributedRadio";
			this.EvenlyDistributedRadio.Size = new System.Drawing.Size(113, 20);
			this.EvenlyDistributedRadio.TabIndex = 16;
			this.EvenlyDistributedRadio.Text = "Evenly distributed";
			this.EvenlyDistributedRadio.CheckedChanged += new System.EventHandler(this.DistributionRadioChanged);
			// 
			// FilledFromTopRadio
			// 
			this.FilledFromTopRadio.Checked = true;
			this.FilledFromTopRadio.Location = new System.Drawing.Point(13, 42);
			this.FilledFromTopRadio.Name = "FilledFromTopRadio";
			this.FilledFromTopRadio.Size = new System.Drawing.Size(127, 20);
			this.FilledFromTopRadio.TabIndex = 15;
			this.FilledFromTopRadio.TabStop = true;
			this.FilledFromTopRadio.Text = "Filled from top";
			this.FilledFromTopRadio.CheckedChanged += new System.EventHandler(this.DistributionRadioChanged);
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(167, 10);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(54, 16);
			this.Label2.TabIndex = 14;
			this.Label2.Text = "mm water";
			// 
			// PAWCEdit
			// 
			this.PAWCEdit.Location = new System.Drawing.Point(107, 7);
			this.PAWCEdit.Name = "PAWCEdit";
			this.PAWCEdit.Size = new System.Drawing.Size(53, 20);
			this.PAWCEdit.TabIndex = 13;
			this.PAWCEdit.Text = "";
			this.PAWCEdit.TextChanged += new System.EventHandler(this.PAWCEdit_TextChanged);
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(67, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(14, 16);
			this.Label1.TabIndex = 12;
			this.Label1.Text = "%";
			// 
			// PercentEdit
			// 
			this.PercentEdit.Location = new System.Drawing.Point(13, 7);
			this.PercentEdit.Name = "PercentEdit";
			this.PercentEdit.Size = new System.Drawing.Size(47, 20);
			this.PercentEdit.TabIndex = 11;
			this.PercentEdit.Value = new System.Decimal(new int[] {
																	  100,
																	  0,
																	  0,
																	  0});
			this.PercentEdit.KeyUp += new System.Windows.Forms.KeyEventHandler(this.PercentEdit_KeyUp);
			this.PercentEdit.ValueChanged += new System.EventHandler(this.PercentEdit_ValueChanged);
			// 
			// DepthWetSoilRadio
			// 
			this.DepthWetSoilRadio.Location = new System.Drawing.Point(8, 32);
			this.DepthWetSoilRadio.Name = "DepthWetSoilRadio";
			this.DepthWetSoilRadio.Size = new System.Drawing.Size(293, 20);
			this.DepthWetSoilRadio.TabIndex = 26;
			this.DepthWetSoilRadio.Text = "Specify water as a depth of wet soil";
			this.DepthWetSoilRadio.CheckedChanged += new System.EventHandler(this.DepthWetSoilRadio_CheckedChanged);
			// 
			// PercentRadio
			// 
			this.PercentRadio.Location = new System.Drawing.Point(8, 8);
			this.PercentRadio.Name = "PercentRadio";
			this.PercentRadio.Size = new System.Drawing.Size(300, 21);
			this.PercentRadio.TabIndex = 25;
			this.PercentRadio.Text = "Specify a fraction of maximum available water";
			this.PercentRadio.CheckedChanged += new System.EventHandler(this.PercentRadio_CheckedChanged);
			// 
			// splitter1
			// 
			this.splitter1.Location = new System.Drawing.Point(312, 20);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(3, 753);
			this.splitter1.TabIndex = 21;
			this.splitter1.TabStop = false;
			// 
			// WaterChartControl
			// 
			this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.WaterChartControl.LinkedSoil = null;
			this.WaterChartControl.Location = new System.Drawing.Point(315, 20);
			this.WaterChartControl.Name = "WaterChartControl";
			this.WaterChartControl.Size = new System.Drawing.Size(531, 753);
			this.WaterChartControl.TabIndex = 22;
			// 
			// InitWaterUI
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(846, 808);
			this.Controls.Add(this.WaterChartControl);
			this.Controls.Add(this.splitter1);
			this.Controls.Add(this.panel1);
			this.Name = "InitWaterUI";
			this.Controls.SetChildIndex(this.panel1, 0);
			this.Controls.SetChildIndex(this.splitter1, 0);
			this.Controls.SetChildIndex(this.WaterChartControl, 0);
			this.panel1.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate2)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.ColumnManagerRow2)).EndInit();
			this.GroupBox.ResumeLayout(false);
			this.DepthWetSoilPanel.ResumeLayout(false);
			this.PercentPanel.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).EndInit();
			this.ResumeLayout(false);

		}
		#endregion


		// -----------------------
		// Refresh the form
		// -----------------------
		override public void Refresh()
			{
			base.Refresh();

			HelpLabel.Text = "There are multiple ways of initialising soil water. Select a method by clicking one of the options above "
										+ " and then filling in the details.";

			SoilData = new Soil(Data.Parent);
			InitialWater = SoilData.InitialWater;
			WaterChartControl.LinkedSoil = SoilData;

			PopulateControls();
			}


		// -------------------------------------
		// Populate all controls from the soil
		// -------------------------------------
		private void PopulateControls()
			{
			UserChange = false;
			PercentRadio.Checked = (InitialWater.Method == InitWater.MethodType.Percent);
			DepthWetSoilRadio.Checked = (InitialWater.Method == InitWater.MethodType.DepthWetSoil);
			LayeredRadio.Checked = (InitialWater.Method == InitWater.MethodType.Layered);
			PercentPanel.Visible = PercentRadio.Checked;
			DepthWetSoilPanel.Visible = DepthWetSoilRadio.Checked;
			GroupBox.Visible = !LayeredRadio.Checked;

			if (InitialWater.Method == InitWater.MethodType.Percent)
				{
				PercentEdit.Text = InitialWater.Percent.ToString("f0");
				FilledFromTopRadio.Checked = InitialWater.FilledFromTop;
				EvenlyDistributedRadio.Checked = !InitialWater.FilledFromTop;
				UpdatePAWCBox();
				}
			else if (InitialWater.Method == InitWater.MethodType.DepthWetSoil)
				{
				int DepthCM = InitialWater.DepthWetSoil / 10;
				DepthEdit.Text = DepthCM.ToString();
				}
			PopulateGrid();
			UserChange = true;
			}


		// ------------------------------
		// Save the contents of the grid.
		// ------------------------------
		private void SaveControls()
			{
			if (PercentRadio.Checked)
				InitialWater.SetUsingPercent(Convert.ToInt32(PercentEdit.Text), FilledFromTopRadio.Checked);
			else if (DepthWetSoilRadio.Checked)	
				InitialWater.SetUsingDepthWetSoil(Convert.ToInt32(DepthEdit.Text));
			else
				InitialWater.SetUsingLayered(GridUtils.GetColumnAsDoubles(ref WaterGrid, 3, WaterGrid.DataRows.Count));
			}


		// ----------------------------
		// Update the PAWC edit box.
		// ----------------------------
		private void UpdatePAWCBox()
			{
			UserChange = false;
			double Proportion = Convert.ToInt32(PercentEdit.Value) / 100.0;
			double AmountWater = MathUtility.Sum(SoilData.PAWC()) * Proportion;
			PAWCEdit.Text = AmountWater.ToString("f0");    // This will call PopulateGrid
			UserChange = true;
			}

		// -------------------------------------
		// Populate water grid from the data
		// -------------------------------------
		private void PopulateGrid()
			{
			UserChange = false;
			WaterGrid.Columns[3].ReadOnly = !LayeredRadio.Checked;
			if (LayeredRadio.Checked)
				WaterGrid.BackColor = SystemColors.Window;
			else
				WaterGrid.BackColor = SystemColors.Control;
				
			GridUtils.SetColumnAsStrings(ref WaterGrid, 0, SoilData.DepthStrings);
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 1, MathUtility.Multiply_Value(SoilData.LL15, 100), "f2");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 2, MathUtility.Multiply_Value(SoilData.DUL, 100), "f2");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 3, MathUtility.Multiply_Value(InitialWater.SW, 100), "f2");

			foreach (Xceed.Grid.DataRow Row in WaterGrid.DataRows)
				Row.Cells[3].ValueChanged += new System.EventHandler(CellValueChanged);
			
			WaterChartControl.Refresh();
			UserChange = true;
			}	


		// -----------------------------------------------------
		// User has changed the contents of a cell - update row.
		// -----------------------------------------------------
		private void CellValueChanged(Object sender, EventArgs e)
			{
			if (UserChange)
				{
				SaveControls();
				WaterChartControl.Refresh();
				}
			}


		// ----------------------------------------------
		// User has changed the value of the percent radio button
		// ----------------------------------------------
		private void PercentRadio_CheckedChanged(object sender, System.EventArgs e)
			{
			if (PercentRadio.Checked && UserChange)
				{
				if (InitialWater.Method != InitWater.MethodType.Percent)
					InitialWater.SetUsingPercent(100, true);
				PopulateControls();
				}
			}


		// -------------------------------------------------------------
		// User has changed the value of the depth wet soil radio button
		// -------------------------------------------------------------
		private void DepthWetSoilRadio_CheckedChanged(object sender, System.EventArgs e)
			{
			if (DepthWetSoilRadio.Checked && UserChange)
				{
				if (InitialWater.Method != InitWater.MethodType.DepthWetSoil)
					InitialWater.SetUsingDepthWetSoil(100);
				PopulateControls();
				}
			}


		// -------------------------------------------------------------
		// User has changed the value of the layered radio button
		// -------------------------------------------------------------
		private void LayeredRadio_CheckedChanged(object sender, System.EventArgs e)
			{
			if (LayeredRadio.Checked && UserChange)
				{
				if (InitialWater.Method != InitWater.MethodType.Layered)
					InitialWater.SetUsingLayered(SoilData.LL15);
				PopulateControls();
				}
			}


		// -------------------------------------------------
		// User has changed the percent value using Up|Down
		// -------------------------------------------------
		private void PercentEdit_ValueChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				int Percent = Convert.ToInt32(PercentEdit.Value);
				InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
				UpdatePAWCBox();
				PopulateGrid();
				UserChange = true;
				}		
			}


		// --------------------------------------------
		//User has changed the percent value by typing.
		// --------------------------------------------
		private void PercentEdit_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
			{
			PercentEdit_ValueChanged(sender, null);
			}


		// ------------------------------
		// User has typed in a pawc value
		// ------------------------------
		private void PAWCEdit_TextChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				double TotalPAWC = MathUtility.Sum(SoilData.PAWC());
				int Percent = 0;
				if (PAWCEdit.Text != "")
					Percent = Convert.ToInt32(Convert.ToDouble(PAWCEdit.Text) / TotalPAWC * 100);
				Percent = Math.Min(Percent, 100);
				Percent = Math.Max(Percent, 0);
				PercentEdit.Value = Percent;
				InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
				PopulateGrid();
				UserChange = true;
				}
			}


		// -----------------------------------------
		// User has typed in a depth wet soil value
		// -----------------------------------------
		private void DepthEdit_TextChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				int Depth = 0;
				if (DepthEdit.Text != "")
					Depth = Convert.ToInt32(Convert.ToInt32(DepthEdit.Text)) * 10;
				InitialWater.SetUsingDepthWetSoil(Depth);
				PopulateGrid();				
				UserChange = true;
				}
			}

		// -----------------------------------------
		// User has ticked either the fill from top radio
		// or the evenly dist. radio.
		// -----------------------------------------
		private void DistributionRadioChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				int Percent = Convert.ToInt32(PercentEdit.Value);
				InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
				PopulateGrid();
				UserChange = true;
				}
			}



	}
}

