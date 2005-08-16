using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using CSGeneral;
using VBGeneral;

namespace CSGeneral
	{
	public class InitNitrogenUI : VBGeneral.BaseUI
		{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.TextBox NO3Edit;
		private System.Windows.Forms.TextBox NH4Edit;
		internal Xceed.Grid.GridControl NitrogenGrid;
		internal Xceed.Grid.Column Column1;
		internal Xceed.Grid.Column Column2;
		internal Xceed.Grid.Column Column3;
		internal Xceed.Grid.GroupByRow GroupByRow2;
		internal Xceed.Grid.ColumnManagerRow ColumnManagerRow2;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column1;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column2;
		internal Xceed.Grid.ColumnManagerCell cellColumnManagerRow2Column3;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column1;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column2;
		internal Xceed.Grid.DataCell celldataRowTemplate2Column3;
		internal Xceed.Grid.DataRow dataRowTemplate2;
		private System.ComponentModel.IContainer components = null;
		private InitNitrogen InitialNitrogen;
		private Soil SoilData;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle1;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle2;
		private bool UserChange = true;


		// -----------------
		// constructor
		// -----------------
		public InitNitrogenUI()
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
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.NO3Edit = new System.Windows.Forms.TextBox();
			this.NH4Edit = new System.Windows.Forms.TextBox();
			this.NitrogenGrid = new Xceed.Grid.GridControl();
			this.Column1 = new Xceed.Grid.Column();
			this.Column2 = new Xceed.Grid.Column();
			this.Column3 = new Xceed.Grid.Column();
			this.dataRowTemplate2 = new Xceed.Grid.DataRow();
			this.celldataRowTemplate2Column1 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate2Column2 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate2Column3 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle1 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle2 = new Xceed.Grid.VisualGridElementStyle();
			this.GroupByRow2 = new Xceed.Grid.GroupByRow();
			this.ColumnManagerRow2 = new Xceed.Grid.ColumnManagerRow();
			this.cellColumnManagerRow2Column1 = new Xceed.Grid.ColumnManagerCell();
			this.cellColumnManagerRow2Column2 = new Xceed.Grid.ColumnManagerCell();
			this.cellColumnManagerRow2Column3 = new Xceed.Grid.ColumnManagerCell();
			((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate2)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.ColumnManagerRow2)).BeginInit();
			this.SuspendLayout();
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(64, 96);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(152, 23);
			this.label1.TabIndex = 2;
			this.label1.Text = "Enter amount of NO3 (kg/ha):";
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(64, 160);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(152, 23);
			this.label2.TabIndex = 4;
			this.label2.Text = "Enter amount of NH4 (kg/ha):";
			// 
			// NO3Edit
			// 
			this.NO3Edit.Location = new System.Drawing.Point(64, 120);
			this.NO3Edit.Name = "NO3Edit";
			this.NO3Edit.Size = new System.Drawing.Size(96, 20);
			this.NO3Edit.TabIndex = 5;
			this.NO3Edit.Text = "";
			this.NO3Edit.TextChanged += new System.EventHandler(this.NO3Edit_TextChanged);
			// 
			// NH4Edit
			// 
			this.NH4Edit.Location = new System.Drawing.Point(64, 184);
			this.NH4Edit.Name = "NH4Edit";
			this.NH4Edit.Size = new System.Drawing.Size(96, 20);
			this.NH4Edit.TabIndex = 6;
			this.NH4Edit.Text = "";
			this.NH4Edit.TextChanged += new System.EventHandler(this.NH4Edit_TextChanged);
			// 
			// NitrogenGrid
			// 
			this.NitrogenGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.NitrogenGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.NitrogenGrid.Columns.Add(this.Column1);
			this.NitrogenGrid.Columns.Add(this.Column2);
			this.NitrogenGrid.Columns.Add(this.Column3);
			this.NitrogenGrid.DataRowTemplate = this.dataRowTemplate2;
			this.NitrogenGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle1);
			this.NitrogenGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle2);
			this.NitrogenGrid.FixedHeaderRows.Add(this.GroupByRow2);
			this.NitrogenGrid.FixedHeaderRows.Add(this.ColumnManagerRow2);
			this.NitrogenGrid.Font = new System.Drawing.Font("Tahoma", 8.25F);
			this.NitrogenGrid.ForeColor = System.Drawing.Color.Black;
			this.NitrogenGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.NitrogenGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.NitrogenGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.NitrogenGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.NitrogenGrid.Location = new System.Drawing.Point(248, 56);
			this.NitrogenGrid.Name = "NitrogenGrid";
			// 
			// NitrogenGrid.RowSelectorPane
			// 
			this.NitrogenGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.NitrogenGrid.RowSelectorPane.Visible = false;
			this.NitrogenGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.NitrogenGrid.SelectionForeColor = System.Drawing.Color.White;
			this.NitrogenGrid.Size = new System.Drawing.Size(264, 368);
			this.NitrogenGrid.TabIndex = 30;
			// 
			// Column1
			// 
			this.Column1.ReadOnly = true;
			this.Column1.Title = "Depth(cm)";
			this.Column1.VisibleIndex = 0;
			this.Column1.Width = 74;
			this.Column1.Initialize("Column1", typeof(string));
			// 
			// Column2
			// 
			this.Column2.Title = "NO3 (kg/ha)";
			this.Column2.VisibleIndex = 1;
			this.Column2.Width = 83;
			this.Column2.Initialize("Column2", typeof(string));
			// 
			// Column3
			// 
			this.Column3.Title = "NH4 (kg/ha)";
			this.Column3.VisibleIndex = 2;
			this.Column3.Width = 81;
			this.Column3.Initialize("Column3", typeof(string));
			// 
			// dataRowTemplate2
			// 
			this.dataRowTemplate2.AccessibleName = "Data row 1 in data row template";
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column1);
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column2);
			this.dataRowTemplate2.Cells.Add(this.celldataRowTemplate2Column3);
			this.dataRowTemplate2.Height = 20;
			// 
			// celldataRowTemplate2Column1
			// 
			this.celldataRowTemplate2Column1.BackColor = System.Drawing.SystemColors.Window;
			this.celldataRowTemplate2Column1.ForeColor = System.Drawing.SystemColors.WindowText;
			this.celldataRowTemplate2Column1.Initialize("Column1");
			// 
			// celldataRowTemplate2Column2
			// 
			this.celldataRowTemplate2Column2.BackColor = System.Drawing.SystemColors.Window;
			this.celldataRowTemplate2Column2.ForeColor = System.Drawing.SystemColors.WindowText;
			this.celldataRowTemplate2Column2.Initialize("Column2");
			// 
			// celldataRowTemplate2Column3
			// 
			this.celldataRowTemplate2Column3.BackColor = System.Drawing.SystemColors.Window;
			this.celldataRowTemplate2Column3.ForeColor = System.Drawing.SystemColors.WindowText;
			this.celldataRowTemplate2Column3.Initialize("Column3");
			// 
			// visualGridElementStyle1
			// 
			this.visualGridElementStyle1.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle2
			// 
			this.visualGridElementStyle2.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// GroupByRow2
			// 
			this.GroupByRow2.BackColor = System.Drawing.Color.LightSlateGray;
			this.GroupByRow2.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.GroupByRow2.CellFont = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold);
			this.GroupByRow2.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.GroupByRow2.Visible = false;
			// 
			// ColumnManagerRow2
			// 
			this.ColumnManagerRow2.BackColor = System.Drawing.Color.LightSteelBlue;
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column1);
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column2);
			this.ColumnManagerRow2.Cells.Add(this.cellColumnManagerRow2Column3);
			this.ColumnManagerRow2.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold);
			this.ColumnManagerRow2.Height = 21;
			this.cellColumnManagerRow2Column1.Initialize("Column1");
			this.cellColumnManagerRow2Column2.Initialize("Column2");
			this.cellColumnManagerRow2Column3.Initialize("Column3");
			// 
			// InitNitrogenUI
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(762, 808);
			this.Controls.Add(this.NitrogenGrid);
			this.Controls.Add(this.NH4Edit);
			this.Controls.Add(this.NO3Edit);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Name = "InitNitrogenUI";
			this.Controls.SetChildIndex(this.label1, 0);
			this.Controls.SetChildIndex(this.label2, 0);
			this.Controls.SetChildIndex(this.NO3Edit, 0);
			this.Controls.SetChildIndex(this.NH4Edit, 0);
			this.Controls.SetChildIndex(this.NitrogenGrid, 0);
			((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate2)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.ColumnManagerRow2)).EndInit();
			this.ResumeLayout(false);

		}
		#endregion


		// -----------------------
		// Refresh the form
		// -----------------------
		override public void Refresh()
			{
			base.Refresh();

			HelpLabel.Text = "There are multiple ways of initialising soil nitrogen. You can either type a single number for the whole "
							+ " profile or enter values in the grid for each layer.";

			SoilData = new Soil(Data.Parent);
			InitialNitrogen = SoilData.InitialNitrogen;
			NO3Edit.Text = InitialNitrogen.TotalNO3KgHa.ToString("f1");
			NH4Edit.Text = InitialNitrogen.TotalNH4KgHa.ToString("f1");

			PopulateGrid();
			}


		// -------------------------------------
		// Populate grid from the data
		// -------------------------------------
		private void PopulateGrid()
			{
			UserChange = false;
			GridUtils.SetColumnAsStrings(ref NitrogenGrid, 0, SoilData.DepthStrings);
			GridUtils.SetColumnAsDoubles(ref NitrogenGrid, 1, InitialNitrogen.NO3KgHa, "f2");
			GridUtils.SetColumnAsDoubles(ref NitrogenGrid, 2, InitialNitrogen.NH4KgHa, "f2");
			
			foreach (Xceed.Grid.DataRow Row in NitrogenGrid.DataRows)
				{
				Row.Cells[1].ValueChanged += new System.EventHandler(NO3CellValueChanged);
				Row.Cells[2].ValueChanged += new System.EventHandler(NH4CellValueChanged);
				}
		
			UserChange = true;
			}	

		// -----------------------------------------
		// User has changed an NO3 value in the grid
		// -----------------------------------------
		private void NO3CellValueChanged(Object sender, EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				double[] no3 = GridUtils.GetColumnAsDoubles(ref NitrogenGrid, 1, NitrogenGrid.DataRows.Count);
                InitialNitrogen.NO3KgHa = no3;
				NO3Edit.Text = MathUtility.Sum(no3).ToString("f1");
				UserChange = true;
				}
			}

		// -----------------------------------------
		// User has changed an NH4 value in the grid
		// -----------------------------------------
		private void NH4CellValueChanged(Object sender, EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				double[] nh4 = GridUtils.GetColumnAsDoubles(ref NitrogenGrid, 2, NitrogenGrid.DataRows.Count);
                InitialNitrogen.NH4KgHa = nh4;
				NH4Edit.Text = MathUtility.Sum(nh4).ToString("f1");
				UserChange = true;
				}
			}


		// --------------------------------------
		// User has changed the no3 edit box.
		// --------------------------------------
		private void NO3Edit_TextChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
                UserChange = false;
                if (NO3Edit.Text != "")
					{
					try
						{
						InitialNitrogen.TotalNO3KgHa = Convert.ToDouble(NO3Edit.Text);
						GridUtils.SetColumnAsDoubles(ref NitrogenGrid, 1, InitialNitrogen.NO3KgHa, "f2");
						}
					catch (Exception)
						{
						}
					}
				UserChange = true;
				}
			}

		// --------------------------------------
		// User has changed the no3 edit box.
		// --------------------------------------
		private void NH4Edit_TextChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
                UserChange = false;
                if (NH4Edit.Text != "")
					{
					try
						{
						InitialNitrogen.TotalNH4KgHa = Convert.ToDouble(NH4Edit.Text);
						GridUtils.SetColumnAsDoubles(ref NitrogenGrid, 2, InitialNitrogen.NH4KgHa, "f2");
						}
					catch (Exception)
						{
						}
					}
				UserChange = true;
				}
			}
		
            
	}
}

