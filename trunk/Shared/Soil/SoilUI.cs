using System;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Xceed.Grid;
using Xceed.Chart.Standard;
using Xceed.Chart.Core;
using CSGeneral;
using VBGeneral;

namespace CSGeneral
	{

	// -------------------------------------------
	// A user interface for soil stuff.
	// -------------------------------------------
	public class SoilUI : VBGeneral.BaseUI
		{
		private Xceed.SmartUI.Controls.StatusBar.SmartStatusBar smartStatusBar1;
		private Xceed.Grid.GroupByRow groupByRow1;
		private Xceed.Grid.ColumnManagerRow columnManagerRow1;
		private Xceed.Grid.DataRow dataRowTemplate1;
		private Xceed.Grid.Column column1;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow1column1;
		private Xceed.Grid.DataCell celldataRowTemplate1column1;
		private Xceed.Grid.Column column2;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow1column2;
		private Xceed.Grid.DataCell celldataRowTemplate1column2;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle1;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle2;
		private Xceed.Grid.GridControl GeneralGrid;
		private System.ComponentModel.IContainer components = null;
		private Xceed.Grid.GridControl WaterGrid;
		private Xceed.Grid.Column column3;
		private Xceed.Grid.Column column4;
		private Xceed.Grid.DataRow dataRow1;
		private Xceed.Grid.DataCell dataCell1;
		private Xceed.Grid.DataCell dataCell2;
		private Xceed.Grid.GroupByRow groupByRow2;
		private Xceed.Grid.ColumnManagerRow columnManagerRow2;
		private Xceed.Grid.ColumnManagerCell columnManagerCell1;
		private Xceed.Grid.ColumnManagerCell columnManagerCell2;
		private Xceed.Grid.Column column5;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow2column5;
		private Xceed.Grid.DataCell celldataRow1column5;
		private Xceed.Grid.Column column6;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow2column6;
		private Xceed.Grid.DataCell celldataRow1column6;
		private Xceed.Grid.Column column7;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow2column7;
		private Xceed.Grid.DataCell celldataRow1column7;
		private Xceed.Grid.Column column8;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow2column8;
		private Xceed.Grid.DataCell celldataRow1column8;
		private Xceed.Grid.Column column9;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow2column9;
		private Xceed.Grid.DataCell celldataRow1column9;
		private System.Windows.Forms.TabPage GeneralPage;
		private System.Windows.Forms.TabPage WaterPage;
		private System.Windows.Forms.TabPage ProfilePage;
		private System.Windows.Forms.TabPage APSIMPage;
		private System.Windows.Forms.TabControl TabControl;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle3;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle4;
		private CSGeneral.Soil MySoil;
		private ValueRow SummaryRow;
		private bool PopulatingWaterGrid;
		private System.Windows.Forms.ImageList ButtonImageList;
		private System.Windows.Forms.ToolBar WaterToolBar;
		private static int NUMBER_OF_STATIC_COLS = 7;
		private System.Windows.Forms.ToolBarButton AddCropButton;
		private System.Windows.Forms.Splitter splitter1;
		private Xceed.Grid.Column column10;
		private Xceed.Grid.Column column11;
		private Xceed.Grid.Column column12;
		private Xceed.Grid.Column column13;
		private Xceed.Grid.Column column14;
		private Xceed.Grid.Column column15;
		private Xceed.Grid.Column column16;
		private Xceed.Grid.DataRow dataRow2;
		private Xceed.Grid.DataCell dataCell3;
		private Xceed.Grid.DataCell dataCell4;
		private Xceed.Grid.DataCell dataCell5;
		private Xceed.Grid.DataCell dataCell6;
		private Xceed.Grid.DataCell dataCell7;
		private Xceed.Grid.DataCell dataCell8;
		private Xceed.Grid.DataCell dataCell9;
		private Xceed.Grid.GroupByRow groupByRow3;
		private Xceed.Grid.ColumnManagerRow columnManagerRow3;
		private Xceed.Grid.ColumnManagerCell columnManagerCell3;
		private Xceed.Grid.ColumnManagerCell columnManagerCell4;
		private Xceed.Grid.ColumnManagerCell columnManagerCell5;
		private Xceed.Grid.ColumnManagerCell columnManagerCell6;
		private Xceed.Grid.ColumnManagerCell columnManagerCell7;
		private Xceed.Grid.ColumnManagerCell columnManagerCell8;
		private Xceed.Grid.ColumnManagerCell columnManagerCell9;
		private Xceed.Grid.Column column17;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column17;
		private Xceed.Grid.DataCell celldataRow2column17;
		private Xceed.Grid.Column column18;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column18;
		private Xceed.Grid.DataCell celldataRow2column18;
		private Xceed.Grid.Column column19;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column19;
		private Xceed.Grid.DataCell celldataRow2column19;
		private Xceed.Grid.Column column20;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column20;
		private Xceed.Grid.DataCell celldataRow2column20;
		private Xceed.Grid.Column column21;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column21;
		private Xceed.Grid.DataCell celldataRow2column21;
		private Xceed.Grid.Column column22;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column22;
		private Xceed.Grid.DataCell celldataRow2column22;
		private Xceed.Grid.Column column23;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column23;
		private Xceed.Grid.DataCell celldataRow2column23;
		private Xceed.Grid.Column column24;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column24;
		private Xceed.Grid.DataCell celldataRow2column24;
		private Xceed.Grid.Column column25;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column25;
		private Xceed.Grid.DataCell celldataRow2column25;
		private Xceed.Grid.GridControl ProfileGrid;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle5;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle6;
		private Xceed.Grid.Column column26;
		private Xceed.Grid.DataRow dataRow3;
		private Xceed.Grid.DataCell dataCell10;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle7;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle8;
		private Xceed.Grid.GroupByRow groupByRow4;
		private Xceed.Grid.ColumnManagerRow columnManagerRow4;
		private Xceed.Grid.ColumnManagerCell columnManagerCell10;
		private Xceed.Grid.GridControl APSIMGrid;
		private Xceed.Grid.Column column27;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow4column27;
		private Xceed.Grid.DataCell celldataRow3column27;
		private Xceed.Grid.Column column28;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow4column28;
		private Xceed.Grid.DataCell celldataRow3column28;
		private Xceed.Grid.Group group1;
		private Xceed.Grid.GroupManagerRow groupManagerRow1;
		private Xceed.Grid.Column column29;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column29;
		private Xceed.Grid.DataCell celldataRow2column29;
		private System.Windows.Forms.TabPage PhosphorusPage;
		private Xceed.Grid.GridControl PhosphorusGrid;
		private Xceed.Grid.Column column30;
		private Xceed.Grid.Column column31;
		private Xceed.Grid.Column column32;
		private Xceed.Grid.Column column33;
		private Xceed.Grid.Column column34;
		private Xceed.Grid.DataRow dataRow4;
		private Xceed.Grid.DataCell dataCell11;
		private Xceed.Grid.DataCell dataCell12;
		private Xceed.Grid.DataCell dataCell13;
		private Xceed.Grid.DataCell dataCell14;
		private Xceed.Grid.DataCell dataCell15;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle10;
		private Xceed.Grid.ColumnManagerRow columnManagerRow5;
		private Xceed.Grid.ColumnManagerCell columnManagerCell11;
		private Xceed.Grid.ColumnManagerCell columnManagerCell12;
		private Xceed.Grid.ColumnManagerCell columnManagerCell13;
		private Xceed.Grid.ColumnManagerCell columnManagerCell14;
		private Xceed.Grid.ColumnManagerCell columnManagerCell15;
		private System.Windows.Forms.Splitter splitter3;
		private System.Windows.Forms.Panel panel2;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.TextBox ResideCPEdit;
		private System.Windows.Forms.TextBox RootCPEdit;
		private System.Windows.Forms.TextBox RateDissolRockEdit;
		private Xceed.Grid.GroupByRow groupByRow5;
		private Xceed.Grid.VisualGridElementStyle visualGridElementStyle9;
		private Xceed.Grid.Column column35;
		private Xceed.Grid.ColumnManagerCell cellcolumnManagerRow3column35;
		private Xceed.Grid.DataCell celldataRow2column35;
		private System.Windows.Forms.ToolBarButton DeleteCropButton;
		private CSGeneral.WaterChartControl WaterChartControl;
		private bool InCellLeavingEdit = false;




		// -------------
		// constructor
		// -------------
		public SoilUI()
			{
			// This call is required by the Windows Form Designer.
			InitializeComponent();
            			
			}


		// -----------------------------------
		// Clean up any resources being used.
		// -----------------------------------
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
			this.components = new System.ComponentModel.Container();
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(SoilUI));
			this.smartStatusBar1 = new Xceed.SmartUI.Controls.StatusBar.SmartStatusBar(this.components);
			this.TabControl = new System.Windows.Forms.TabControl();
			this.WaterPage = new System.Windows.Forms.TabPage();
			this.WaterChartControl = new CSGeneral.WaterChartControl();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.WaterGrid = new Xceed.Grid.GridControl();
			this.column3 = new Xceed.Grid.Column();
			this.column4 = new Xceed.Grid.Column();
			this.column5 = new Xceed.Grid.Column();
			this.column6 = new Xceed.Grid.Column();
			this.column7 = new Xceed.Grid.Column();
			this.column8 = new Xceed.Grid.Column();
			this.column9 = new Xceed.Grid.Column();
			this.dataRow1 = new Xceed.Grid.DataRow();
			this.dataCell1 = new Xceed.Grid.DataCell();
			this.dataCell2 = new Xceed.Grid.DataCell();
			this.celldataRow1column5 = new Xceed.Grid.DataCell();
			this.celldataRow1column6 = new Xceed.Grid.DataCell();
			this.celldataRow1column9 = new Xceed.Grid.DataCell();
			this.celldataRow1column7 = new Xceed.Grid.DataCell();
			this.celldataRow1column8 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle3 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle4 = new Xceed.Grid.VisualGridElementStyle();
			this.groupByRow2 = new Xceed.Grid.GroupByRow();
			this.columnManagerRow2 = new Xceed.Grid.ColumnManagerRow();
			this.columnManagerCell1 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell2 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow2column5 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow2column6 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow2column9 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow2column7 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow2column8 = new Xceed.Grid.ColumnManagerCell();
			this.WaterToolBar = new System.Windows.Forms.ToolBar();
			this.AddCropButton = new System.Windows.Forms.ToolBarButton();
			this.DeleteCropButton = new System.Windows.Forms.ToolBarButton();
			this.ButtonImageList = new System.Windows.Forms.ImageList(this.components);
			this.GeneralPage = new System.Windows.Forms.TabPage();
			this.GeneralGrid = new Xceed.Grid.GridControl();
			this.column1 = new Xceed.Grid.Column();
			this.column2 = new Xceed.Grid.Column();
			this.dataRowTemplate1 = new Xceed.Grid.DataRow();
			this.celldataRowTemplate1column1 = new Xceed.Grid.DataCell();
			this.celldataRowTemplate1column2 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle1 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle2 = new Xceed.Grid.VisualGridElementStyle();
			this.groupByRow1 = new Xceed.Grid.GroupByRow();
			this.columnManagerRow1 = new Xceed.Grid.ColumnManagerRow();
			this.cellcolumnManagerRow1column1 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow1column2 = new Xceed.Grid.ColumnManagerCell();
			this.ProfilePage = new System.Windows.Forms.TabPage();
			this.ProfileGrid = new Xceed.Grid.GridControl();
			this.column10 = new Xceed.Grid.Column();
			this.column11 = new Xceed.Grid.Column();
			this.column12 = new Xceed.Grid.Column();
			this.column13 = new Xceed.Grid.Column();
			this.column14 = new Xceed.Grid.Column();
			this.column15 = new Xceed.Grid.Column();
			this.column16 = new Xceed.Grid.Column();
			this.column17 = new Xceed.Grid.Column();
			this.column18 = new Xceed.Grid.Column();
			this.column19 = new Xceed.Grid.Column();
			this.column20 = new Xceed.Grid.Column();
			this.column21 = new Xceed.Grid.Column();
			this.column22 = new Xceed.Grid.Column();
			this.column23 = new Xceed.Grid.Column();
			this.column24 = new Xceed.Grid.Column();
			this.column25 = new Xceed.Grid.Column();
			this.column29 = new Xceed.Grid.Column();
			this.column35 = new Xceed.Grid.Column();
			this.dataRow2 = new Xceed.Grid.DataRow();
			this.dataCell3 = new Xceed.Grid.DataCell();
			this.dataCell4 = new Xceed.Grid.DataCell();
			this.dataCell5 = new Xceed.Grid.DataCell();
			this.dataCell6 = new Xceed.Grid.DataCell();
			this.dataCell7 = new Xceed.Grid.DataCell();
			this.dataCell8 = new Xceed.Grid.DataCell();
			this.dataCell9 = new Xceed.Grid.DataCell();
			this.celldataRow2column17 = new Xceed.Grid.DataCell();
			this.celldataRow2column18 = new Xceed.Grid.DataCell();
			this.celldataRow2column19 = new Xceed.Grid.DataCell();
			this.celldataRow2column20 = new Xceed.Grid.DataCell();
			this.celldataRow2column21 = new Xceed.Grid.DataCell();
			this.celldataRow2column22 = new Xceed.Grid.DataCell();
			this.celldataRow2column23 = new Xceed.Grid.DataCell();
			this.celldataRow2column24 = new Xceed.Grid.DataCell();
			this.celldataRow2column25 = new Xceed.Grid.DataCell();
			this.celldataRow2column29 = new Xceed.Grid.DataCell();
			this.celldataRow2column35 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle5 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle6 = new Xceed.Grid.VisualGridElementStyle();
			this.groupByRow3 = new Xceed.Grid.GroupByRow();
			this.columnManagerRow3 = new Xceed.Grid.ColumnManagerRow();
			this.columnManagerCell3 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell4 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell5 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell6 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell7 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell8 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell9 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column17 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column18 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column19 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column20 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column21 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column22 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column23 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column24 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column25 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column29 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow3column35 = new Xceed.Grid.ColumnManagerCell();
			this.APSIMPage = new System.Windows.Forms.TabPage();
			this.APSIMGrid = new Xceed.Grid.GridControl();
			this.column26 = new Xceed.Grid.Column();
			this.column27 = new Xceed.Grid.Column();
			this.column28 = new Xceed.Grid.Column();
			this.dataRow3 = new Xceed.Grid.DataRow();
			this.dataCell10 = new Xceed.Grid.DataCell();
			this.celldataRow3column27 = new Xceed.Grid.DataCell();
			this.celldataRow3column28 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle7 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle8 = new Xceed.Grid.VisualGridElementStyle();
			this.groupByRow4 = new Xceed.Grid.GroupByRow();
			this.columnManagerRow4 = new Xceed.Grid.ColumnManagerRow();
			this.columnManagerCell10 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow4column27 = new Xceed.Grid.ColumnManagerCell();
			this.cellcolumnManagerRow4column28 = new Xceed.Grid.ColumnManagerCell();
			this.group1 = new Xceed.Grid.Group();
			this.groupManagerRow1 = new Xceed.Grid.GroupManagerRow();
			this.PhosphorusPage = new System.Windows.Forms.TabPage();
			this.panel2 = new System.Windows.Forms.Panel();
			this.RateDissolRockEdit = new System.Windows.Forms.TextBox();
			this.RootCPEdit = new System.Windows.Forms.TextBox();
			this.ResideCPEdit = new System.Windows.Forms.TextBox();
			this.label4 = new System.Windows.Forms.Label();
			this.label3 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.splitter3 = new System.Windows.Forms.Splitter();
			this.PhosphorusGrid = new Xceed.Grid.GridControl();
			this.column30 = new Xceed.Grid.Column();
			this.column31 = new Xceed.Grid.Column();
			this.column32 = new Xceed.Grid.Column();
			this.column33 = new Xceed.Grid.Column();
			this.column34 = new Xceed.Grid.Column();
			this.dataRow4 = new Xceed.Grid.DataRow();
			this.dataCell11 = new Xceed.Grid.DataCell();
			this.dataCell12 = new Xceed.Grid.DataCell();
			this.dataCell13 = new Xceed.Grid.DataCell();
			this.dataCell14 = new Xceed.Grid.DataCell();
			this.dataCell15 = new Xceed.Grid.DataCell();
			this.visualGridElementStyle9 = new Xceed.Grid.VisualGridElementStyle();
			this.visualGridElementStyle10 = new Xceed.Grid.VisualGridElementStyle();
			this.groupByRow5 = new Xceed.Grid.GroupByRow();
			this.columnManagerRow5 = new Xceed.Grid.ColumnManagerRow();
			this.columnManagerCell11 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell12 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell13 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell14 = new Xceed.Grid.ColumnManagerCell();
			this.columnManagerCell15 = new Xceed.Grid.ColumnManagerCell();
			this.TabControl.SuspendLayout();
			this.WaterPage.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow1)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow2)).BeginInit();
			this.GeneralPage.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.GeneralGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate1)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow1)).BeginInit();
			this.ProfilePage.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.ProfileGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow2)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow3)).BeginInit();
			this.APSIMPage.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.APSIMGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow3)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow4)).BeginInit();
			this.PhosphorusPage.SuspendLayout();
			this.panel2.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.PhosphorusGrid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow4)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow5)).BeginInit();
			this.SuspendLayout();
			// 
			// smartStatusBar1
			// 
			this.smartStatusBar1.Location = new System.Drawing.Point(0, 667);
			this.smartStatusBar1.Name = "smartStatusBar1";
			this.smartStatusBar1.Size = new System.Drawing.Size(846, 23);
			this.smartStatusBar1.TabIndex = 3;
			this.smartStatusBar1.Text = "smartStatusBar1";
			// 
			// TabControl
			// 
			this.TabControl.Controls.Add(this.GeneralPage);
			this.TabControl.Controls.Add(this.WaterPage);
			this.TabControl.Controls.Add(this.ProfilePage);
			this.TabControl.Controls.Add(this.APSIMPage);
			this.TabControl.Controls.Add(this.PhosphorusPage);
			this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.TabControl.Location = new System.Drawing.Point(0, 20);
			this.TabControl.Name = "TabControl";
			this.TabControl.SelectedIndex = 0;
			this.TabControl.Size = new System.Drawing.Size(846, 647);
			this.TabControl.TabIndex = 5;
			this.TabControl.SelectedIndexChanged += new System.EventHandler(this.TabControl_SelectedIndexChanged);
			// 
			// WaterPage
			// 
			this.WaterPage.Controls.Add(this.WaterChartControl);
			this.WaterPage.Controls.Add(this.splitter1);
			this.WaterPage.Controls.Add(this.WaterGrid);
			this.WaterPage.Controls.Add(this.WaterToolBar);
			this.WaterPage.Location = new System.Drawing.Point(4, 22);
			this.WaterPage.Name = "WaterPage";
			this.WaterPage.Size = new System.Drawing.Size(838, 621);
			this.WaterPage.TabIndex = 1;
			this.WaterPage.Text = "Water Measured";
			// 
			// WaterChartControl
			// 
			this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.WaterChartControl.LinkedSoil = null;
			this.WaterChartControl.Location = new System.Drawing.Point(0, 315);
			this.WaterChartControl.Name = "WaterChartControl";
			this.WaterChartControl.Size = new System.Drawing.Size(838, 306);
			this.WaterChartControl.TabIndex = 10;
			// 
			// splitter1
			// 
			this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
			this.splitter1.Location = new System.Drawing.Point(0, 312);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(838, 3);
			this.splitter1.TabIndex = 9;
			this.splitter1.TabStop = false;
			// 
			// WaterGrid
			// 
			this.WaterGrid.AllowDrop = true;
			this.WaterGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.WaterGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.WaterGrid.Columns.Add(this.column3);
			this.WaterGrid.Columns.Add(this.column4);
			this.WaterGrid.Columns.Add(this.column5);
			this.WaterGrid.Columns.Add(this.column6);
			this.WaterGrid.Columns.Add(this.column7);
			this.WaterGrid.Columns.Add(this.column8);
			this.WaterGrid.Columns.Add(this.column9);
			this.WaterGrid.DataRowTemplate = this.dataRow1;
			this.WaterGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle3);
			this.WaterGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle4);
			this.WaterGrid.Dock = System.Windows.Forms.DockStyle.Top;
			this.WaterGrid.FixedHeaderRows.Add(this.groupByRow2);
			this.WaterGrid.FixedHeaderRows.Add(this.columnManagerRow2);
			this.WaterGrid.Font = new System.Drawing.Font("Verdana", 9F);
			this.WaterGrid.ForeColor = System.Drawing.Color.Black;
			this.WaterGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.WaterGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.WaterGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.WaterGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.WaterGrid.Location = new System.Drawing.Point(0, 36);
			this.WaterGrid.Name = "WaterGrid";
			// 
			// WaterGrid.RowSelectorPane
			// 
			this.WaterGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.WaterGrid.RowSelectorPane.Visible = false;
			this.WaterGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.WaterGrid.SelectionForeColor = System.Drawing.Color.White;
			this.WaterGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple;
			this.WaterGrid.ShowUnlinkedColumns = true;
			this.WaterGrid.SingleClickEdit = false;
			this.WaterGrid.Size = new System.Drawing.Size(838, 276);
			this.WaterGrid.TabIndex = 1;
			this.WaterGrid.ValueMember = "";
			// 
			// column3
			// 
			this.column3.CanBeSorted = false;
			this.column3.ReadOnly = false;
			this.column3.SortDirection = Xceed.Grid.SortDirection.None;
			this.column3.Title = "Depth (cm)";
			this.column3.VisibleIndex = 0;
			this.column3.Width = 67;
			this.column3.Initialize("column1", typeof(string));
			// 
			// column4
			// 
			this.column4.CanBeSorted = false;
			this.column4.SortDirection = Xceed.Grid.SortDirection.None;
			this.column4.Title = "BD (g/cc)";
			this.column4.VisibleIndex = 1;
			this.column4.Width = 70;
			this.column4.Initialize("column2", typeof(string));
			// 
			// column5
			// 
			this.column5.CanBeSorted = false;
			this.column5.SortDirection = Xceed.Grid.SortDirection.None;
			this.column5.Title = "SAT (mm/mm)";
			this.column5.VisibleIndex = 2;
			this.column5.Width = 90;
			this.column5.Initialize("column5", typeof(string));
			// 
			// column6
			// 
			this.column6.CanBeSorted = false;
			this.column6.SortDirection = Xceed.Grid.SortDirection.None;
			this.column6.Title = "DUL (mm/mm)";
			this.column6.VisibleIndex = 3;
			this.column6.Width = 93;
			this.column6.Initialize("column6", typeof(string));
			// 
			// column7
			// 
			this.column7.CanBeSorted = false;
			this.column7.SortDirection = Xceed.Grid.SortDirection.None;
			this.column7.Title = "AirDry (mm/mm)";
			this.column7.VisibleIndex = 4;
			this.column7.Width = 92;
			this.column7.Initialize("column9", typeof(string));
			// 
			// column8
			// 
			this.column8.CanBeSorted = false;
			this.column8.SortDirection = Xceed.Grid.SortDirection.None;
			this.column8.Title = "LL15 (mm/mm)";
			this.column8.VisibleIndex = 5;
			this.column8.Width = 90;
			this.column8.Initialize("column7", typeof(string));
			// 
			// column9
			// 
			this.column9.BackColor = System.Drawing.Color.Aqua;
			this.column9.CanBeSorted = false;
			this.column9.ReadOnly = true;
			this.column9.SortDirection = Xceed.Grid.SortDirection.None;
			this.column9.Title = "PAWC (mm)";
			this.column9.VisibleIndex = 6;
			this.column9.Width = 84;
			this.column9.Initialize("column8", typeof(string));
			// 
			// dataRow1
			// 
			this.dataRow1.AllowCellNavigation = true;
			this.dataRow1.CanBeCurrent = true;
			this.dataRow1.CanBeSelected = false;
			this.dataRow1.Cells.Add(this.dataCell1);
			this.dataRow1.Cells.Add(this.dataCell2);
			this.dataRow1.Cells.Add(this.celldataRow1column5);
			this.dataRow1.Cells.Add(this.celldataRow1column6);
			this.dataRow1.Cells.Add(this.celldataRow1column9);
			this.dataRow1.Cells.Add(this.celldataRow1column7);
			this.dataRow1.Cells.Add(this.celldataRow1column8);
			// 
			// dataRow1.RowSelector
			// 
			this.dataRow1.RowSelector.Visible = false;
			this.dataRow1.ShowTreeLine = true;
			this.dataCell1.Initialize("column1");
			this.dataCell2.Initialize("column2");
			this.celldataRow1column5.Initialize("column5");
			this.celldataRow1column6.Initialize("column6");
			this.celldataRow1column9.Initialize("column9");
			this.celldataRow1column7.Initialize("column7");
			this.celldataRow1column8.Initialize("column8");
			// 
			// visualGridElementStyle3
			// 
			this.visualGridElementStyle3.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle4
			// 
			this.visualGridElementStyle4.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// groupByRow2
			// 
			this.groupByRow2.BackColor = System.Drawing.Color.LightSlateGray;
			this.groupByRow2.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.groupByRow2.CellFont = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Bold);
			this.groupByRow2.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.groupByRow2.Visible = false;
			// 
			// columnManagerRow2
			// 
			this.columnManagerRow2.BackColor = System.Drawing.Color.LightSteelBlue;
			this.columnManagerRow2.Cells.Add(this.columnManagerCell1);
			this.columnManagerRow2.Cells.Add(this.columnManagerCell2);
			this.columnManagerRow2.Cells.Add(this.cellcolumnManagerRow2column5);
			this.columnManagerRow2.Cells.Add(this.cellcolumnManagerRow2column6);
			this.columnManagerRow2.Cells.Add(this.cellcolumnManagerRow2column9);
			this.columnManagerRow2.Cells.Add(this.cellcolumnManagerRow2column7);
			this.columnManagerRow2.Cells.Add(this.cellcolumnManagerRow2column8);
			this.columnManagerRow2.Font = new System.Drawing.Font("Verdana", 9.75F, System.Drawing.FontStyle.Bold);
			this.columnManagerRow2.Height = 39;
			// 
			// columnManagerRow2.RowSelector
			// 
			this.columnManagerRow2.RowSelector.Visible = false;
			this.columnManagerCell1.Initialize("column1");
			this.columnManagerCell2.Initialize("column2");
			this.cellcolumnManagerRow2column5.Initialize("column5");
			this.cellcolumnManagerRow2column6.Initialize("column6");
			this.cellcolumnManagerRow2column9.Initialize("column9");
			this.cellcolumnManagerRow2column7.Initialize("column7");
			this.cellcolumnManagerRow2column8.Initialize("column8");
			// 
			// WaterToolBar
			// 
			this.WaterToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.WaterToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.AddCropButton,
																							this.DeleteCropButton});
			this.WaterToolBar.DropDownArrows = true;
			this.WaterToolBar.ImageList = this.ButtonImageList;
			this.WaterToolBar.Location = new System.Drawing.Point(0, 0);
			this.WaterToolBar.Name = "WaterToolBar";
			this.WaterToolBar.ShowToolTips = true;
			this.WaterToolBar.Size = new System.Drawing.Size(838, 36);
			this.WaterToolBar.TabIndex = 7;
			this.WaterToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right;
			this.WaterToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.WaterToolBar_ButtonClick);
			// 
			// AddCropButton
			// 
			this.AddCropButton.ImageIndex = 1;
			this.AddCropButton.Text = "Add crop";
			// 
			// DeleteCropButton
			// 
			this.DeleteCropButton.ImageIndex = 2;
			this.DeleteCropButton.Text = "Delete crop";
			// 
			// ButtonImageList
			// 
			this.ButtonImageList.ImageSize = new System.Drawing.Size(24, 24);
			this.ButtonImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ButtonImageList.ImageStream")));
			this.ButtonImageList.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// GeneralPage
			// 
			this.GeneralPage.Controls.Add(this.GeneralGrid);
			this.GeneralPage.Location = new System.Drawing.Point(4, 22);
			this.GeneralPage.Name = "GeneralPage";
			this.GeneralPage.Size = new System.Drawing.Size(838, 621);
			this.GeneralPage.TabIndex = 0;
			this.GeneralPage.Text = "General";
			// 
			// GeneralGrid
			// 
			this.GeneralGrid.AllowDrop = true;
			this.GeneralGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.GeneralGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.GeneralGrid.Columns.Add(this.column1);
			this.GeneralGrid.Columns.Add(this.column2);
			this.GeneralGrid.DataRowTemplate = this.dataRowTemplate1;
			this.GeneralGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle1);
			this.GeneralGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle2);
			this.GeneralGrid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.GeneralGrid.FixedHeaderRows.Add(this.groupByRow1);
			this.GeneralGrid.FixedHeaderRows.Add(this.columnManagerRow1);
			this.GeneralGrid.Font = new System.Drawing.Font("Verdana", 9F);
			this.GeneralGrid.ForeColor = System.Drawing.Color.Black;
			this.GeneralGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.GeneralGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.GeneralGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.GeneralGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.GeneralGrid.Location = new System.Drawing.Point(0, 0);
			this.GeneralGrid.Name = "GeneralGrid";
			// 
			// GeneralGrid.RowSelectorPane
			// 
			this.GeneralGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.GeneralGrid.RowSelectorPane.Visible = false;
			this.GeneralGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.GeneralGrid.SelectionForeColor = System.Drawing.Color.White;
			this.GeneralGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
			this.GeneralGrid.ShowUnlinkedColumns = true;
			this.GeneralGrid.SingleClickEdit = false;
			this.GeneralGrid.Size = new System.Drawing.Size(838, 621);
			this.GeneralGrid.TabIndex = 0;
			this.GeneralGrid.ValueMember = "";
			// 
			// column1
			// 
			this.column1.BackColor = System.Drawing.Color.Aquamarine;
			this.column1.ReadOnly = true;
			this.column1.SortDirection = Xceed.Grid.SortDirection.None;
			this.column1.Title = "";
			this.column1.VisibleIndex = 0;
			this.column1.Width = 132;
			this.column1.Initialize("column1", typeof(string));
			// 
			// column2
			// 
			this.column2.SortDirection = Xceed.Grid.SortDirection.None;
			this.column2.Title = "";
			this.column2.VisibleIndex = 1;
			this.column2.Width = 235;
			this.column2.Initialize("column2", typeof(string));
			// 
			// dataRowTemplate1
			// 
			this.dataRowTemplate1.AllowCellNavigation = true;
			this.dataRowTemplate1.CanBeCurrent = true;
			this.dataRowTemplate1.CanBeSelected = false;
			this.dataRowTemplate1.Cells.Add(this.celldataRowTemplate1column1);
			this.dataRowTemplate1.Cells.Add(this.celldataRowTemplate1column2);
			// 
			// dataRowTemplate1.RowSelector
			// 
			this.dataRowTemplate1.RowSelector.Visible = false;
			this.dataRowTemplate1.ShowTreeLine = true;
			this.celldataRowTemplate1column1.Initialize("column1");
			this.celldataRowTemplate1column2.Initialize("column2");
			// 
			// visualGridElementStyle1
			// 
			this.visualGridElementStyle1.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle2
			// 
			this.visualGridElementStyle2.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// groupByRow1
			// 
			this.groupByRow1.BackColor = System.Drawing.Color.LightSlateGray;
			this.groupByRow1.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.groupByRow1.CellFont = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Bold);
			this.groupByRow1.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.groupByRow1.Visible = false;
			// 
			// columnManagerRow1
			// 
			this.columnManagerRow1.BackColor = System.Drawing.Color.LightSteelBlue;
			this.columnManagerRow1.Cells.Add(this.cellcolumnManagerRow1column1);
			this.columnManagerRow1.Cells.Add(this.cellcolumnManagerRow1column2);
			this.columnManagerRow1.Font = new System.Drawing.Font("Verdana", 9.75F, System.Drawing.FontStyle.Bold);
			// 
			// columnManagerRow1.RowSelector
			// 
			this.columnManagerRow1.RowSelector.Visible = false;
			this.cellcolumnManagerRow1column1.Initialize("column1");
			this.cellcolumnManagerRow1column2.Initialize("column2");
			// 
			// ProfilePage
			// 
			this.ProfilePage.Controls.Add(this.ProfileGrid);
			this.ProfilePage.Location = new System.Drawing.Point(4, 22);
			this.ProfilePage.Name = "ProfilePage";
			this.ProfilePage.Size = new System.Drawing.Size(838, 621);
			this.ProfilePage.TabIndex = 3;
			this.ProfilePage.Text = "Soil profile properties";
			// 
			// ProfileGrid
			// 
			this.ProfileGrid.AllowDrop = true;
			this.ProfileGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.ProfileGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.ProfileGrid.Columns.Add(this.column10);
			this.ProfileGrid.Columns.Add(this.column11);
			this.ProfileGrid.Columns.Add(this.column12);
			this.ProfileGrid.Columns.Add(this.column13);
			this.ProfileGrid.Columns.Add(this.column14);
			this.ProfileGrid.Columns.Add(this.column15);
			this.ProfileGrid.Columns.Add(this.column16);
			this.ProfileGrid.Columns.Add(this.column17);
			this.ProfileGrid.Columns.Add(this.column18);
			this.ProfileGrid.Columns.Add(this.column19);
			this.ProfileGrid.Columns.Add(this.column20);
			this.ProfileGrid.Columns.Add(this.column21);
			this.ProfileGrid.Columns.Add(this.column22);
			this.ProfileGrid.Columns.Add(this.column23);
			this.ProfileGrid.Columns.Add(this.column24);
			this.ProfileGrid.Columns.Add(this.column25);
			this.ProfileGrid.Columns.Add(this.column29);
			this.ProfileGrid.Columns.Add(this.column35);
			this.ProfileGrid.DataRowTemplate = this.dataRow2;
			this.ProfileGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle5);
			this.ProfileGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle6);
			this.ProfileGrid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.ProfileGrid.FixedHeaderRows.Add(this.groupByRow3);
			this.ProfileGrid.FixedHeaderRows.Add(this.columnManagerRow3);
			this.ProfileGrid.Font = new System.Drawing.Font("Verdana", 9F);
			this.ProfileGrid.ForeColor = System.Drawing.Color.Black;
			this.ProfileGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.ProfileGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.ProfileGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.ProfileGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.ProfileGrid.Location = new System.Drawing.Point(0, 0);
			this.ProfileGrid.Name = "ProfileGrid";
			// 
			// ProfileGrid.RowSelectorPane
			// 
			this.ProfileGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.ProfileGrid.RowSelectorPane.Visible = false;
			this.ProfileGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.ProfileGrid.SelectionForeColor = System.Drawing.Color.White;
			this.ProfileGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple;
			this.ProfileGrid.ShowUnlinkedColumns = true;
			this.ProfileGrid.SingleClickEdit = false;
			this.ProfileGrid.Size = new System.Drawing.Size(838, 621);
			this.ProfileGrid.TabIndex = 2;
			this.ProfileGrid.ValueMember = "";
			// 
			// column10
			// 
			this.column10.CanBeSorted = false;
			this.column10.ReadOnly = true;
			this.column10.SortDirection = Xceed.Grid.SortDirection.None;
			this.column10.Title = "Depth (cm)";
			this.column10.VisibleIndex = 0;
			this.column10.Width = 77;
			this.column10.Initialize("column1", typeof(string));
			// 
			// column11
			// 
			this.column11.CanBeSorted = false;
			this.column11.SortDirection = Xceed.Grid.SortDirection.None;
			this.column11.Title = "SWCon";
			this.column11.VisibleIndex = 1;
			this.column11.Width = 60;
			this.column11.Initialize("column2", typeof(string));
			// 
			// column12
			// 
			this.column12.CanBeSorted = false;
			this.column12.SortDirection = Xceed.Grid.SortDirection.None;
			this.column12.Title = "FBiom";
			this.column12.VisibleIndex = 3;
			this.column12.Width = 62;
			this.column12.Initialize("column5", typeof(string));
			// 
			// column13
			// 
			this.column13.CanBeSorted = false;
			this.column13.SortDirection = Xceed.Grid.SortDirection.None;
			this.column13.Title = "FInert";
			this.column13.VisibleIndex = 4;
			this.column13.Width = 64;
			this.column13.Initialize("column6", typeof(string));
			// 
			// column14
			// 
			this.column14.CanBeSorted = false;
			this.column14.SortDirection = Xceed.Grid.SortDirection.None;
			this.column14.Title = "OC";
			this.column14.VisibleIndex = 5;
			this.column14.Width = 64;
			this.column14.Initialize("column9", typeof(string));
			// 
			// column15
			// 
			this.column15.CanBeSorted = false;
			this.column15.SortDirection = Xceed.Grid.SortDirection.None;
			this.column15.Title = "EC";
			this.column15.VisibleIndex = 6;
			this.column15.Width = 65;
			this.column15.Initialize("column7", typeof(string));
			// 
			// column16
			// 
			this.column16.CanBeSorted = false;
			this.column16.ReadOnly = false;
			this.column16.SortDirection = Xceed.Grid.SortDirection.None;
			this.column16.Title = "PH";
			this.column16.VisibleIndex = 7;
			this.column16.Width = 63;
			this.column16.Initialize("column8", typeof(string));
			// 
			// column17
			// 
			this.column17.SortDirection = Xceed.Grid.SortDirection.None;
			this.column17.Title = "CL";
			this.column17.VisibleIndex = 8;
			this.column17.Width = 63;
			this.column17.Initialize("column17", typeof(string));
			// 
			// column18
			// 
			this.column18.SortDirection = Xceed.Grid.SortDirection.None;
			this.column18.Title = "CEC";
			this.column18.VisibleIndex = 9;
			this.column18.Width = 67;
			this.column18.Initialize("column18", typeof(string));
			// 
			// column19
			// 
			this.column19.SortDirection = Xceed.Grid.SortDirection.None;
			this.column19.Title = "Ca";
			this.column19.VisibleIndex = 10;
			this.column19.Width = 62;
			this.column19.Initialize("column19", typeof(string));
			// 
			// column20
			// 
			this.column20.SortDirection = Xceed.Grid.SortDirection.None;
			this.column20.Title = "Mg";
			this.column20.VisibleIndex = 11;
			this.column20.Width = 61;
			this.column20.Initialize("column20", typeof(string));
			// 
			// column21
			// 
			this.column21.SortDirection = Xceed.Grid.SortDirection.None;
			this.column21.Title = "Na";
			this.column21.VisibleIndex = 12;
			this.column21.Width = 58;
			this.column21.Initialize("column21", typeof(string));
			// 
			// column22
			// 
			this.column22.SortDirection = Xceed.Grid.SortDirection.None;
			this.column22.Title = "K";
			this.column22.VisibleIndex = 13;
			this.column22.Width = 57;
			this.column22.Initialize("column22", typeof(string));
			// 
			// column23
			// 
			this.column23.SortDirection = Xceed.Grid.SortDirection.None;
			this.column23.Title = "ESP";
			this.column23.VisibleIndex = 14;
			this.column23.Width = 58;
			this.column23.Initialize("column23", typeof(string));
			// 
			// column24
			// 
			this.column24.SortDirection = Xceed.Grid.SortDirection.None;
			this.column24.Title = "Particle size sand";
			this.column24.VisibleIndex = 15;
			this.column24.Width = 84;
			this.column24.Initialize("column24", typeof(string));
			// 
			// column25
			// 
			this.column25.SortDirection = Xceed.Grid.SortDirection.None;
			this.column25.Title = "Particle size silt";
			this.column25.VisibleIndex = 16;
			this.column25.Width = 78;
			this.column25.Initialize("column25", typeof(string));
			// 
			// column29
			// 
			this.column29.SortDirection = Xceed.Grid.SortDirection.None;
			this.column29.Title = "Particle size clay";
			this.column29.VisibleIndex = 17;
			this.column29.Width = 76;
			this.column29.Initialize("column29", typeof(string));
			// 
			// column35
			// 
			this.column35.Title = "MWCon";
			this.column35.VisibleIndex = 2;
			this.column35.Width = 66;
			this.column35.Initialize("column35", typeof(string));
			// 
			// dataRow2
			// 
			this.dataRow2.AllowCellNavigation = true;
			this.dataRow2.CanBeCurrent = true;
			this.dataRow2.CanBeSelected = false;
			this.dataRow2.Cells.Add(this.dataCell3);
			this.dataRow2.Cells.Add(this.dataCell4);
			this.dataRow2.Cells.Add(this.dataCell5);
			this.dataRow2.Cells.Add(this.dataCell6);
			this.dataRow2.Cells.Add(this.dataCell7);
			this.dataRow2.Cells.Add(this.dataCell8);
			this.dataRow2.Cells.Add(this.dataCell9);
			this.dataRow2.Cells.Add(this.celldataRow2column17);
			this.dataRow2.Cells.Add(this.celldataRow2column18);
			this.dataRow2.Cells.Add(this.celldataRow2column19);
			this.dataRow2.Cells.Add(this.celldataRow2column20);
			this.dataRow2.Cells.Add(this.celldataRow2column21);
			this.dataRow2.Cells.Add(this.celldataRow2column22);
			this.dataRow2.Cells.Add(this.celldataRow2column23);
			this.dataRow2.Cells.Add(this.celldataRow2column24);
			this.dataRow2.Cells.Add(this.celldataRow2column25);
			this.dataRow2.Cells.Add(this.celldataRow2column29);
			this.dataRow2.Cells.Add(this.celldataRow2column35);
			// 
			// dataRow2.RowSelector
			// 
			this.dataRow2.RowSelector.Visible = false;
			this.dataRow2.ShowTreeLine = true;
			this.dataCell3.Initialize("column1");
			this.dataCell4.Initialize("column2");
			this.dataCell5.Initialize("column5");
			this.dataCell6.Initialize("column6");
			this.dataCell7.Initialize("column9");
			this.dataCell8.Initialize("column7");
			this.dataCell9.Initialize("column8");
			this.celldataRow2column17.Initialize("column17");
			this.celldataRow2column18.Initialize("column18");
			this.celldataRow2column19.Initialize("column19");
			this.celldataRow2column20.Initialize("column20");
			this.celldataRow2column21.Initialize("column21");
			this.celldataRow2column22.Initialize("column22");
			this.celldataRow2column23.Initialize("column23");
			this.celldataRow2column24.Initialize("column24");
			this.celldataRow2column25.Initialize("column25");
			this.celldataRow2column29.Initialize("column29");
			this.celldataRow2column35.Initialize("column35");
			// 
			// visualGridElementStyle5
			// 
			this.visualGridElementStyle5.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle6
			// 
			this.visualGridElementStyle6.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// groupByRow3
			// 
			this.groupByRow3.BackColor = System.Drawing.Color.LightSlateGray;
			this.groupByRow3.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.groupByRow3.CellFont = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Bold);
			this.groupByRow3.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.groupByRow3.Visible = false;
			// 
			// columnManagerRow3
			// 
			this.columnManagerRow3.BackColor = System.Drawing.Color.LightSteelBlue;
			this.columnManagerRow3.Cells.Add(this.columnManagerCell3);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell4);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell5);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell6);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell7);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell8);
			this.columnManagerRow3.Cells.Add(this.columnManagerCell9);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column17);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column18);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column19);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column20);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column21);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column22);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column23);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column24);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column25);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column29);
			this.columnManagerRow3.Cells.Add(this.cellcolumnManagerRow3column35);
			this.columnManagerRow3.Font = new System.Drawing.Font("Verdana", 9.75F, System.Drawing.FontStyle.Bold);
			this.columnManagerRow3.Height = 39;
			// 
			// columnManagerRow3.RowSelector
			// 
			this.columnManagerRow3.RowSelector.Visible = false;
			this.columnManagerCell3.Initialize("column1");
			this.columnManagerCell4.Initialize("column2");
			this.columnManagerCell5.Initialize("column5");
			this.columnManagerCell6.Initialize("column6");
			this.columnManagerCell7.Initialize("column9");
			this.columnManagerCell8.Initialize("column7");
			this.columnManagerCell9.Initialize("column8");
			this.cellcolumnManagerRow3column17.Initialize("column17");
			this.cellcolumnManagerRow3column18.Initialize("column18");
			this.cellcolumnManagerRow3column19.Initialize("column19");
			this.cellcolumnManagerRow3column20.Initialize("column20");
			this.cellcolumnManagerRow3column21.Initialize("column21");
			this.cellcolumnManagerRow3column22.Initialize("column22");
			this.cellcolumnManagerRow3column23.Initialize("column23");
			this.cellcolumnManagerRow3column24.Initialize("column24");
			this.cellcolumnManagerRow3column25.Initialize("column25");
			this.cellcolumnManagerRow3column29.Initialize("column29");
			this.cellcolumnManagerRow3column35.Initialize("column35");
			// 
			// APSIMPage
			// 
			this.APSIMPage.Controls.Add(this.APSIMGrid);
			this.APSIMPage.Location = new System.Drawing.Point(4, 22);
			this.APSIMPage.Name = "APSIMPage";
			this.APSIMPage.Size = new System.Drawing.Size(838, 621);
			this.APSIMPage.TabIndex = 4;
			this.APSIMPage.Text = "APSIM";
			// 
			// APSIMGrid
			// 
			this.APSIMGrid.AllowDrop = true;
			this.APSIMGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.APSIMGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.APSIMGrid.Columns.Add(this.column26);
			this.APSIMGrid.Columns.Add(this.column27);
			this.APSIMGrid.Columns.Add(this.column28);
			this.APSIMGrid.DataRowTemplate = this.dataRow3;
			this.APSIMGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle7);
			this.APSIMGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle8);
			this.APSIMGrid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.APSIMGrid.FixedHeaderRows.Add(this.groupByRow4);
			this.APSIMGrid.FixedHeaderRows.Add(this.columnManagerRow4);
			this.APSIMGrid.Font = new System.Drawing.Font("Verdana", 9F);
			this.APSIMGrid.ForeColor = System.Drawing.Color.Black;
			this.APSIMGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.APSIMGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.APSIMGrid.GroupTemplates.Add(this.group1);
			this.APSIMGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.APSIMGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.APSIMGrid.Location = new System.Drawing.Point(0, 0);
			this.APSIMGrid.Name = "APSIMGrid";
			// 
			// APSIMGrid.RowSelectorPane
			// 
			this.APSIMGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.APSIMGrid.RowSelectorPane.Visible = false;
			this.APSIMGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.APSIMGrid.SelectionForeColor = System.Drawing.Color.White;
			this.APSIMGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
			this.APSIMGrid.ShowUnlinkedColumns = true;
			this.APSIMGrid.SingleClickEdit = false;
			this.APSIMGrid.Size = new System.Drawing.Size(838, 621);
			this.APSIMGrid.TabIndex = 1;
			this.APSIMGrid.ValueMember = "";
			// 
			// column26
			// 
			this.column26.BackColor = System.Drawing.Color.Aquamarine;
			this.column26.ReadOnly = true;
			this.column26.SortDirection = Xceed.Grid.SortDirection.None;
			this.column26.Title = "Category";
			this.column26.Visible = false;
			this.column26.VisibleIndex = 0;
			this.column26.Width = 132;
			this.column26.Initialize("column1", typeof(string));
			// 
			// column27
			// 
			this.column27.BackColor = System.Drawing.Color.Gainsboro;
			this.column27.ReadOnly = true;
			this.column27.Title = "Name";
			this.column27.VisibleIndex = 1;
			this.column27.Width = 127;
			this.column27.Initialize("column27", typeof(string));
			// 
			// column28
			// 
			this.column28.Title = "Value";
			this.column28.VisibleIndex = 2;
			this.column28.Width = 193;
			this.column28.Initialize("column28", typeof(string));
			// 
			// dataRow3
			// 
			this.dataRow3.AllowCellNavigation = true;
			this.dataRow3.CanBeCurrent = true;
			this.dataRow3.CanBeSelected = false;
			this.dataRow3.Cells.Add(this.dataCell10);
			this.dataRow3.Cells.Add(this.celldataRow3column27);
			this.dataRow3.Cells.Add(this.celldataRow3column28);
			// 
			// dataRow3.RowSelector
			// 
			this.dataRow3.RowSelector.Visible = false;
			this.dataRow3.ShowTreeLine = true;
			this.dataCell10.Initialize("column1");
			this.celldataRow3column27.Initialize("column27");
			this.celldataRow3column28.Initialize("column28");
			// 
			// visualGridElementStyle7
			// 
			this.visualGridElementStyle7.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle8
			// 
			this.visualGridElementStyle8.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// groupByRow4
			// 
			this.groupByRow4.BackColor = System.Drawing.Color.LightSlateGray;
			this.groupByRow4.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.groupByRow4.CellFont = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Bold);
			this.groupByRow4.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.groupByRow4.Visible = false;
			// 
			// columnManagerRow4
			// 
			this.columnManagerRow4.BackColor = System.Drawing.Color.LightSteelBlue;
			this.columnManagerRow4.Cells.Add(this.columnManagerCell10);
			this.columnManagerRow4.Cells.Add(this.cellcolumnManagerRow4column27);
			this.columnManagerRow4.Cells.Add(this.cellcolumnManagerRow4column28);
			this.columnManagerRow4.Font = new System.Drawing.Font("Verdana", 9.75F, System.Drawing.FontStyle.Bold);
			// 
			// columnManagerRow4.RowSelector
			// 
			this.columnManagerRow4.RowSelector.Visible = false;
			this.columnManagerCell10.Initialize("column1");
			this.cellcolumnManagerRow4column27.Initialize("column27");
			this.cellcolumnManagerRow4column28.Initialize("column28");
			// 
			// group1
			// 
			this.group1.GroupBy = "column1";
			this.group1.HeaderRows.Add(this.groupManagerRow1);
			// 
			// PhosphorusPage
			// 
			this.PhosphorusPage.Controls.Add(this.panel2);
			this.PhosphorusPage.Controls.Add(this.splitter3);
			this.PhosphorusPage.Controls.Add(this.PhosphorusGrid);
			this.PhosphorusPage.Location = new System.Drawing.Point(4, 22);
			this.PhosphorusPage.Name = "PhosphorusPage";
			this.PhosphorusPage.Size = new System.Drawing.Size(838, 621);
			this.PhosphorusPage.TabIndex = 5;
			this.PhosphorusPage.Text = "Phosphorus";
			// 
			// panel2
			// 
			this.panel2.Controls.Add(this.RateDissolRockEdit);
			this.panel2.Controls.Add(this.RootCPEdit);
			this.panel2.Controls.Add(this.ResideCPEdit);
			this.panel2.Controls.Add(this.label4);
			this.panel2.Controls.Add(this.label3);
			this.panel2.Controls.Add(this.label2);
			this.panel2.Dock = System.Windows.Forms.DockStyle.Fill;
			this.panel2.Location = new System.Drawing.Point(0, 283);
			this.panel2.Name = "panel2";
			this.panel2.Size = new System.Drawing.Size(838, 338);
			this.panel2.TabIndex = 5;
			// 
			// RateDissolRockEdit
			// 
			this.RateDissolRockEdit.Location = new System.Drawing.Point(144, 97);
			this.RateDissolRockEdit.Name = "RateDissolRockEdit";
			this.RateDissolRockEdit.TabIndex = 5;
			this.RateDissolRockEdit.Text = "";
			// 
			// RootCPEdit
			// 
			this.RootCPEdit.Location = new System.Drawing.Point(144, 64);
			this.RootCPEdit.Name = "RootCPEdit";
			this.RootCPEdit.TabIndex = 4;
			this.RootCPEdit.Text = "";
			// 
			// ResideCPEdit
			// 
			this.ResideCPEdit.Location = new System.Drawing.Point(144, 32);
			this.ResideCPEdit.Name = "ResideCPEdit";
			this.ResideCPEdit.TabIndex = 3;
			this.ResideCPEdit.Text = "";
			// 
			// label4
			// 
			this.label4.Location = new System.Drawing.Point(40, 96);
			this.label4.Name = "label4";
			this.label4.TabIndex = 2;
			this.label4.Text = "Rate dissol rock:";
			// 
			// label3
			// 
			this.label3.Location = new System.Drawing.Point(40, 64);
			this.label3.Name = "label3";
			this.label3.TabIndex = 1;
			this.label3.Text = "Root CP";
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(40, 31);
			this.label2.Name = "label2";
			this.label2.TabIndex = 0;
			this.label2.Text = "Residue CP:";
			// 
			// splitter3
			// 
			this.splitter3.Dock = System.Windows.Forms.DockStyle.Top;
			this.splitter3.Location = new System.Drawing.Point(0, 280);
			this.splitter3.Name = "splitter3";
			this.splitter3.Size = new System.Drawing.Size(838, 3);
			this.splitter3.TabIndex = 4;
			this.splitter3.TabStop = false;
			// 
			// PhosphorusGrid
			// 
			this.PhosphorusGrid.AllowDrop = true;
			this.PhosphorusGrid.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.PhosphorusGrid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.PhosphorusGrid.Columns.Add(this.column30);
			this.PhosphorusGrid.Columns.Add(this.column31);
			this.PhosphorusGrid.Columns.Add(this.column32);
			this.PhosphorusGrid.Columns.Add(this.column33);
			this.PhosphorusGrid.Columns.Add(this.column34);
			this.PhosphorusGrid.DataRowTemplate = this.dataRow4;
			this.PhosphorusGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle9);
			this.PhosphorusGrid.DataRowTemplateStyles.Add(this.visualGridElementStyle10);
			this.PhosphorusGrid.Dock = System.Windows.Forms.DockStyle.Top;
			this.PhosphorusGrid.FixedHeaderRows.Add(this.groupByRow5);
			this.PhosphorusGrid.FixedHeaderRows.Add(this.columnManagerRow5);
			this.PhosphorusGrid.Font = new System.Drawing.Font("Verdana", 9F);
			this.PhosphorusGrid.ForeColor = System.Drawing.Color.Black;
			this.PhosphorusGrid.GridLineColor = System.Drawing.Color.FromArgb(((System.Byte)(235)), ((System.Byte)(240)), ((System.Byte)(246)));
			this.PhosphorusGrid.GridLineStyle = System.Drawing.Drawing2D.DashStyle.Solid;
			this.PhosphorusGrid.InactiveSelectionBackColor = System.Drawing.Color.DarkSlateBlue;
			this.PhosphorusGrid.InactiveSelectionForeColor = System.Drawing.Color.White;
			this.PhosphorusGrid.Location = new System.Drawing.Point(0, 0);
			this.PhosphorusGrid.Name = "PhosphorusGrid";
			// 
			// PhosphorusGrid.RowSelectorPane
			// 
			this.PhosphorusGrid.RowSelectorPane.BackColor = System.Drawing.Color.LightSteelBlue;
			this.PhosphorusGrid.RowSelectorPane.Visible = false;
			this.PhosphorusGrid.SelectionBackColor = System.Drawing.Color.MediumSlateBlue;
			this.PhosphorusGrid.SelectionForeColor = System.Drawing.Color.White;
			this.PhosphorusGrid.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple;
			this.PhosphorusGrid.ShowUnlinkedColumns = true;
			this.PhosphorusGrid.SingleClickEdit = false;
			this.PhosphorusGrid.Size = new System.Drawing.Size(838, 280);
			this.PhosphorusGrid.TabIndex = 3;
			this.PhosphorusGrid.ValueMember = "";
			// 
			// column30
			// 
			this.column30.CanBeSorted = false;
			this.column30.ReadOnly = true;
			this.column30.SortDirection = Xceed.Grid.SortDirection.None;
			this.column30.Title = "Depth (cm)";
			this.column30.VisibleIndex = 0;
			this.column30.Width = 77;
			this.column30.Initialize("column1", typeof(string));
			// 
			// column31
			// 
			this.column31.CanBeSorted = false;
			this.column31.SortDirection = Xceed.Grid.SortDirection.None;
			this.column31.Title = "Labile P (mg/kg)";
			this.column31.VisibleIndex = 1;
			this.column31.Width = 80;
			this.column31.Initialize("column2", typeof(string));
			// 
			// column32
			// 
			this.column32.CanBeSorted = false;
			this.column32.SortDirection = Xceed.Grid.SortDirection.None;
			this.column32.Title = "Banded P (kg/ha)";
			this.column32.VisibleIndex = 2;
			this.column32.Width = 87;
			this.column32.Initialize("column5", typeof(string));
			// 
			// column33
			// 
			this.column33.CanBeSorted = false;
			this.column33.SortDirection = Xceed.Grid.SortDirection.None;
			this.column33.Title = "Rock P (kg/ha)";
			this.column33.VisibleIndex = 3;
			this.column33.Width = 80;
			this.column33.Initialize("column6", typeof(string));
			// 
			// column34
			// 
			this.column34.CanBeSorted = false;
			this.column34.SortDirection = Xceed.Grid.SortDirection.None;
			this.column34.Title = "Sorption";
			this.column34.VisibleIndex = 4;
			this.column34.Width = 77;
			this.column34.Initialize("column9", typeof(string));
			// 
			// dataRow4
			// 
			this.dataRow4.AllowCellNavigation = true;
			this.dataRow4.CanBeCurrent = true;
			this.dataRow4.CanBeSelected = false;
			this.dataRow4.Cells.Add(this.dataCell11);
			this.dataRow4.Cells.Add(this.dataCell12);
			this.dataRow4.Cells.Add(this.dataCell13);
			this.dataRow4.Cells.Add(this.dataCell14);
			this.dataRow4.Cells.Add(this.dataCell15);
			// 
			// dataRow4.RowSelector
			// 
			this.dataRow4.RowSelector.Visible = false;
			this.dataRow4.ShowTreeLine = true;
			this.dataCell11.Initialize("column1");
			this.dataCell12.Initialize("column2");
			this.dataCell13.Initialize("column5");
			this.dataCell14.Initialize("column6");
			this.dataCell15.Initialize("column9");
			// 
			// visualGridElementStyle9
			// 
			this.visualGridElementStyle9.BackColor = System.Drawing.Color.PowderBlue;
			// 
			// visualGridElementStyle10
			// 
			this.visualGridElementStyle10.BackColor = System.Drawing.Color.FromArgb(((System.Byte)(195)), ((System.Byte)(231)), ((System.Byte)(236)));
			// 
			// groupByRow5
			// 
			this.groupByRow5.AccessibleName = "Group by row 1 in fixed header";
			this.groupByRow5.BackColor = System.Drawing.Color.LightSlateGray;
			this.groupByRow5.CellBackColor = System.Drawing.Color.LightSteelBlue;
			this.groupByRow5.CellFont = new System.Drawing.Font("Verdana", 9F, System.Drawing.FontStyle.Bold);
			this.groupByRow5.CellForeColor = System.Drawing.Color.Black;
			this.groupByRow5.CellLayout = Xceed.Grid.GroupByCellLayout.Hierarchical;
			this.groupByRow5.Font = new System.Drawing.Font("Verdana", 9F);
			this.groupByRow5.ForeColor = System.Drawing.Color.Black;
			this.groupByRow5.Indented = false;
			this.groupByRow5.Visible = false;
			// 
			// columnManagerRow5
			// 
			this.columnManagerRow5.BackColor = System.Drawing.Color.LightSteelBlue;
			this.columnManagerRow5.Cells.Add(this.columnManagerCell11);
			this.columnManagerRow5.Cells.Add(this.columnManagerCell12);
			this.columnManagerRow5.Cells.Add(this.columnManagerCell13);
			this.columnManagerRow5.Cells.Add(this.columnManagerCell14);
			this.columnManagerRow5.Cells.Add(this.columnManagerCell15);
			this.columnManagerRow5.Font = new System.Drawing.Font("Verdana", 9.75F, System.Drawing.FontStyle.Bold);
			this.columnManagerRow5.Height = 39;
			// 
			// columnManagerRow5.RowSelector
			// 
			this.columnManagerRow5.RowSelector.Visible = false;
			this.columnManagerCell11.Initialize("column1");
			this.columnManagerCell12.Initialize("column2");
			this.columnManagerCell13.Initialize("column5");
			this.columnManagerCell14.Initialize("column6");
			this.columnManagerCell15.Initialize("column9");
			// 
			// SoilUI
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(846, 725);
			this.Controls.Add(this.TabControl);
			this.Controls.Add(this.smartStatusBar1);
			this.Name = "SoilUI";
			this.Controls.SetChildIndex(this.smartStatusBar1, 0);
			this.Controls.SetChildIndex(this.TabControl, 0);
			this.TabControl.ResumeLayout(false);
			this.WaterPage.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow1)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow2)).EndInit();
			this.GeneralPage.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.GeneralGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRowTemplate1)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow1)).EndInit();
			this.ProfilePage.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.ProfileGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow2)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow3)).EndInit();
			this.APSIMPage.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.APSIMGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow3)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow4)).EndInit();
			this.PhosphorusPage.ResumeLayout(false);
			this.panel2.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.PhosphorusGrid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dataRow4)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.columnManagerRow5)).EndInit();
			this.ResumeLayout(false);

		}
		#endregion


		// ------------------------------------------
		// User has changed tab's. Refresh ourselves.
		// ------------------------------------------
		private void TabControl_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			Refresh();
			}


		// ------------------------
		// Refresh ourselves
		// ------------------------
		override public void Refresh()
			{
			if (MySoil == null)
				{
				if (SummaryRow == null)
					{
					SummaryRow = new ValueRow();			
					WaterGrid.FixedFooterRows.Add(SummaryRow);
					}
				SummaryRow.Cells[0].Value = "Totals:";

				MySoil = new CSGeneral.Soil(Data);
				WaterChartControl.LinkedSoil = MySoil;

				TabControl.SelectedTab = WaterPage;
				}
			if (TabControl.SelectedTab == GeneralPage)
				PopulateGeneralGrid();
			else if (TabControl.SelectedTab == WaterPage)
				{
				PopulateWaterGrid();
				RefreshPAWCCalculations();
				}
			else if (TabControl.SelectedTab == ProfilePage)
				PopulateProfileGrid();
			else if (TabControl.SelectedTab == APSIMPage)
				PopulateAPSIMGrid();
			else if (TabControl.SelectedTab == PhosphorusPage)
				PopulatePhosphorusPage();
			}


		// --------------
		// Save ourselves
		// --------------
		override public void Save()
			{
			if (TabControl.SelectedTab == GeneralPage)
				SaveGeneralGrid();
			else if (TabControl.SelectedTab == WaterPage)
				SaveWaterGrid();
			else if (TabControl.SelectedTab == ProfilePage)
				SaveProfileGrid();
			else if (TabControl.SelectedTab == APSIMPage)
				SaveAPSIMGrid();
			else if (TabControl.SelectedTab == PhosphorusPage)
				SavePhosphorusPage();
			}


		// ----------------------------
		// Populate the general grid
		// ----------------------------
		private void PopulateGeneralGrid()
			{
			string[] Values = new string[10];
			Values[0] = "Region:";
			Values[1] = "Site:";
			Values[2] = "Name:";
			Values[3] = "Order/SubOrder:";
			Values[4] = "Nearest Town:";
			Values[5] = "Comment:";
			Values[6] = "GPS:";
			Values[7] = "GPS DATUM:";
			Values[8] = "Map ID:";
			Values[9] = "Natural Vegetation:";
			GridUtils.SetColumnAsStrings(ref GeneralGrid, 0, Values);
			
			Values[0] = MySoil.Region;
			Values[1] = MySoil.Site;
			Values[2] = MySoil.Name;
			Values[3] = MySoil.Order;
			Values[4] = MySoil.NearestTown;
			Values[5] = MySoil.Comment;
			Values[6] = MySoil.GPS;
			Values[7] = MySoil.GPSDatum;
			Values[8] = MySoil.MapId;
			Values[9] = MySoil.NaturalVegetation;
			GridUtils.SetColumnAsStrings(ref GeneralGrid, 1, Values);
			GeneralGrid.Columns[1].Width = GeneralGrid.DisplayRectangle.Width - GeneralGrid.Columns[0].Width;
			}


		// ---------------------
		// Save the general grid
		// ---------------------
		private void SaveGeneralGrid()
			{
			string[] Values = GridUtils.GetColumnAsStrings(ref GeneralGrid, 1, 1000);
			MySoil.Region = Values[0];
			MySoil.Site = Values[1];
			MySoil.Name = Values[2];
			MySoil.Order = Values[3];
			MySoil.NearestTown = Values[4];
			MySoil.Comment = Values[5];
			MySoil.GPS = Values[6];
			MySoil.GPSDatum = Values[7];
			MySoil.MapId = Values[8];
			MySoil.NaturalVegetation = Values[9];
			}


		// -----------------------------------------------
		// Populate the water grid from the specified soil
		// -----------------------------------------------
		private void PopulateWaterGrid()
			{
			PopulatingWaterGrid = true;
			WaterGrid.Columns[0].Title = "depth\r\n(cm)";
			WaterGrid.Columns[1].Title = "BD\r\n(g/cc)";
			WaterGrid.Columns[2].Title = "SAT\r\n(mm/mm)";
			WaterGrid.Columns[3].Title = "DUL\r\n(mm/mm)";
			WaterGrid.Columns[4].Title = "AirDry\r\n(mm/mm)";
			WaterGrid.Columns[5].Title = "LL15\r\n(mm/mm)";
			WaterGrid.Columns[6].Title = "PAWC\r\n(mm)";

			GridUtils.SetColumnAsStrings(ref WaterGrid, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 1, MySoil.BD, "f3");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 2, MySoil.SAT, "f3");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 3, MySoil.DUL, "f3");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 4, MySoil.Airdry, "f3");
			GridUtils.SetColumnAsDoubles(ref WaterGrid, 5, MySoil.LL15, "f3");

			// Make sure we have the right number of crop columns.
			string[] CropNames = MySoil.Crops;
			int RequiredNumberOfColumns = CropNames.Length*4 + NUMBER_OF_STATIC_COLS;
			while (WaterGrid.Columns.Count < RequiredNumberOfColumns)
				{
				int ColumnNumber = WaterGrid.Columns.Count+1;
				Column NewColumn = new Xceed.Grid.Column();
				NewColumn.CanBeSorted = false;
				NewColumn.Width = 84;
				NewColumn.Initialize(ColumnNumber.ToString(), typeof(string));
				WaterGrid.Columns.Add(NewColumn);
				}	
			while (WaterGrid.Columns.Count > RequiredNumberOfColumns)
				WaterGrid.Columns.RemoveAt(WaterGrid.Columns.Count-1);

			// Fill all crop columns with numbers.
			for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
				{
				int CropCol = NUMBER_OF_STATIC_COLS + CropNumber*4;
				string CropName = CropNames[CropNumber];
				Color CropColor = Color.GreenYellow;
				if (CropNumber / 2.0 != CropNumber / 2)
					CropColor = Color.PaleGreen;

				// setup the LL column.
				WaterGrid.Columns[CropCol].Title = CropName + " LL\r\n(%vol)";
				GridUtils.SetColumnAsDoubles(ref WaterGrid, CropCol, MySoil.LL(CropName), "f3");
				WaterGrid.Columns[CropCol].BackColor = CropColor;

				// setup the PAWC column.
				WaterGrid.Columns[CropCol+1].Title = CropName + " PAWC\r\n(mm)";
				WaterGrid.Columns[CropCol+1].ReadOnly = true;
				WaterGrid.Columns[CropCol+1].BackColor = CropColor;

				// setup the KL column
				WaterGrid.Columns[CropCol+2].Title = CropName + " KL";
				GridUtils.SetColumnAsDoubles(ref WaterGrid, CropCol+2, MySoil.KL(CropName), "f2");
				WaterGrid.Columns[CropCol+2].BackColor = CropColor;

				// setup the XF column
				WaterGrid.Columns[CropCol+3].Title = CropName + " XF";
				GridUtils.SetColumnAsDoubles(ref WaterGrid, CropCol+3, MySoil.XF(CropName), "f1");
				WaterGrid.Columns[CropCol+3].BackColor = CropColor;
				}

			// make sure we have 12 rows.
			while (WaterGrid.DataRows.Count < 12)
				WaterGrid.DataRows.AddNew().EndEdit();

			// make the width of the column just wide enough.
			foreach( Column column in WaterGrid.Columns )
				column.Width = column.GetFittedWidth();


			// Trap the cellleavingedit event.
			foreach (Xceed.Grid.DataRow row in WaterGrid.DataRows)
				foreach (Xceed.Grid.DataCell cell in row.Cells)
					cell.EditLeft += new EditLeftEventHandler(CellLeavingEdit);
			RefreshPAWCCalculations();

			PopulatingWaterGrid = false;
			}


		// --------------------------
		// Save the water grid
		// --------------------------
		private void SaveWaterGrid()
			{
			int NumLayers = GridUtils.GetNumberOfNonBlankRows(ref WaterGrid, 0);
			MySoil.DepthStrings = GridUtils.GetColumnAsStrings(ref WaterGrid, 0, NumLayers);
			MySoil.BD = GridUtils.GetColumnAsDoubles(ref WaterGrid, 1, NumLayers);
			MySoil.SAT = GridUtils.GetColumnAsDoubles(ref WaterGrid, 2, NumLayers);
			MySoil.DUL = GridUtils.GetColumnAsDoubles(ref WaterGrid, 3, NumLayers);
			MySoil.Airdry = GridUtils.GetColumnAsDoubles(ref WaterGrid, 4, NumLayers);
			MySoil.LL15 = GridUtils.GetColumnAsDoubles(ref WaterGrid, 5, NumLayers);

			string[] CropNames = MySoil.Crops;
			// Save all crop columns
			for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
				{
				int CropCol = NUMBER_OF_STATIC_COLS + CropNumber*4;
				double[] ll = GridUtils.GetColumnAsDoubles(ref WaterGrid, CropCol, NumLayers);
				double[] kl = GridUtils.GetColumnAsDoubles(ref WaterGrid, CropCol+2, NumLayers);
				double[] xf = GridUtils.GetColumnAsDoubles(ref WaterGrid, CropCol+3, NumLayers);
				MySoil.SetCrop(CropNames[CropNumber], ll, kl, xf);
				}
			}


		// --------------------------
		// Populate the profile grid.
		// --------------------------
		private void PopulateProfileGrid()
			{
			ProfileGrid.Columns[0].Title = "Depth\r\n(cm)";
			ProfileGrid.Columns[14].Title = "Particle\r\nsize sand";
			ProfileGrid.Columns[15].Title = "Particle\r\nsize silt";
			ProfileGrid.Columns[16].Title = "Particle\r\nsize clay";
			GridUtils.SetColumnAsStrings(ref ProfileGrid, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 1, MySoil.SWCON, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 2, MySoil.FBIOM, "f2");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 3, MySoil.FINERT, "f2");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 4, MySoil.OC, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 5, MySoil.EC, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 6, MySoil.PH, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 7, MySoil.CL, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 8, MySoil.CEC, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 9, MySoil.Ca, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 10, MySoil.Mg, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 11, MySoil.Na, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 12, MySoil.K, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 13, MySoil.ESP, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 14, MySoil.ParticleSizeSand, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 15, MySoil.ParticleSizeSilt, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 16, MySoil.ParticleSizeClay, "f1");
			GridUtils.SetColumnAsDoubles(ref ProfileGrid, 17, MySoil.MWCON, "f1");

			// make the width of the column just wide enough.
			foreach( Column column in ProfileGrid.Columns )
				column.Width = column.GetFittedWidth();

			}


		// --------------------------
		// Save the profile grid.
		// --------------------------
		private void SaveProfileGrid()
			{
			int NumLayers = GridUtils.GetNumberOfNonBlankRows(ref ProfileGrid, 0);
			MySoil.SWCON  = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 1, NumLayers);
			MySoil.FBIOM  = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 2, NumLayers); 
			MySoil.FINERT = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 3, NumLayers); 
			MySoil.OC     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 4, NumLayers); 
			MySoil.EC     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 5, NumLayers); 
			MySoil.PH     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 6, NumLayers); 
			MySoil.CL     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 7, NumLayers); 
			MySoil.CEC    = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 8, NumLayers); 
			MySoil.Ca     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 9, NumLayers); 
			MySoil.Mg     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 10, NumLayers);
			MySoil.Na     = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 11, NumLayers);
			MySoil.K      = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 12, NumLayers);
			MySoil.ESP    = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 13, NumLayers);
			MySoil.ParticleSizeSand = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 14, NumLayers);
			MySoil.ParticleSizeSilt = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 15, NumLayers);
			MySoil.ParticleSizeClay = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 16, NumLayers);
			MySoil.MWCON  = GridUtils.GetColumnAsDoubles(ref ProfileGrid, 17, NumLayers);
			}


		// ----------------------------
		// Populate the APSIM grid
		// ----------------------------
		private void PopulateAPSIMGrid()
			{
			// setup first 2 columns.
			string[] Col1Values = {"Evaporation", "Evaporation", "Evaporation",
								   "Unsaturated flow", "Unsaturated flow", 
								   "Runoff", "Runoff", "Runoff", 
								   "Organic matter", "Organic matter", "Organic matter", 
								   "Erosion", "Erosion"};
			GridUtils.SetColumnAsStrings(ref APSIMGrid, 0, Col1Values);

			string[] Col2Values = {"u", "Cona", "Salb",
								   "Diffus_const", "Diffus_slope",
								   "CN2Bare", "CNRed", "CNCov",
								   "RootCN", "RootWt", "SoilCN",
								   "EnrACoeff", "EnrBCoeff"};
			GridUtils.SetColumnAsStrings(ref APSIMGrid, 1, Col2Values);

			// setup the values (3rd) column
			string[] Values = {DoubleToString(MySoil.U, "f0"), 
							   DoubleToString(MySoil.Cona, "f1"), 
							   DoubleToString(MySoil.Salb, "f2"),
							   DoubleToString(MySoil.DiffusConst, "f0"),
							   DoubleToString(MySoil.DiffusSlope, "f0"),
                               DoubleToString(MySoil.CN2Bare, "f0"), 
							   DoubleToString(MySoil.CNRed, "f0"), 
							   DoubleToString(MySoil.CNCov, "f1"),
							   DoubleToString(MySoil.RootCN, "f0"), 
							   DoubleToString(MySoil.RootWT, "f0"), 
							   DoubleToString(MySoil.SoilCN, "f1"),
							   DoubleToString(MySoil.EnrACoeff, "f1"), 
							   DoubleToString(MySoil.EnrBCoeff, "f1")};
			GridUtils.SetColumnAsStrings(ref APSIMGrid, 2, Values);
			}


		// ---------------------
		// Save the APSIM grid
		// ---------------------
		private void SaveAPSIMGrid()
			{
			double[] Values = GridUtils.GetColumnAsDoubles(ref APSIMGrid, 2, 13);
			MySoil.U = Values[0];
			MySoil.Cona = Values[1];
			MySoil.Salb = Values[2];
			MySoil.DiffusConst = Values[3];
			MySoil.DiffusSlope = Values[4];
			MySoil.CN2Bare = Values[5];
			MySoil.CNRed = Values[6];
			MySoil.CNCov = Values[7];
			MySoil.RootCN = Values[8];
			MySoil.RootWT = Values[9];
			MySoil.SoilCN = Values[10];
			MySoil.EnrACoeff = Values[11];
			MySoil.EnrBCoeff = Values[12];
			}


		// --------------------------
		// Populate the phosphorus grid.
		// --------------------------
		private void PopulatePhosphorusPage()
			{
			PhosphorusGrid.Columns[0].Title = "Depth\r\n(cm)";
			PhosphorusGrid.Columns[1].Title = "Labile P\r\n(mg/kg)";
			PhosphorusGrid.Columns[2].Title = "Banded P\r\n(kg/ha)";
			PhosphorusGrid.Columns[3].Title = "Rock P\r\n(kg/ha)";

			GridUtils.SetColumnAsStrings(ref PhosphorusGrid, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(ref PhosphorusGrid, 1, MySoil.LabileP, "f1");
			GridUtils.SetColumnAsDoubles(ref PhosphorusGrid, 2, MySoil.BandedP, "f1");
			GridUtils.SetColumnAsDoubles(ref PhosphorusGrid, 3, MySoil.RockP, "f1");
			GridUtils.SetColumnAsDoubles(ref PhosphorusGrid, 4, MySoil.Sorption, "f1");

			ResideCPEdit.Text = DoubleToString(MySoil.ResidueCP, "f0");
			RootCPEdit.Text = DoubleToString(MySoil.RootCP, "f0");
			RateDissolRockEdit.Text = DoubleToString(MySoil.RateDissolRock, "f2");

			// make the width of the column just wide enough.
			foreach( Column column in PhosphorusGrid.Columns )
				column.Width = column.GetFittedWidth();

			}


		// --------------------------
		// Save the phosphorus grid.
		// --------------------------
		private void SavePhosphorusPage()
			{
			int NumLayers = GridUtils.GetNumberOfNonBlankRows(ref PhosphorusGrid, 0);
			MySoil.LabileP  = GridUtils.GetColumnAsDoubles(ref PhosphorusGrid, 1, NumLayers);
			MySoil.BandedP  = GridUtils.GetColumnAsDoubles(ref PhosphorusGrid, 2, NumLayers);
			MySoil.RockP  = GridUtils.GetColumnAsDoubles(ref PhosphorusGrid, 3, NumLayers);
			MySoil.Sorption  = GridUtils.GetColumnAsDoubles(ref PhosphorusGrid, 4, NumLayers);

			if (ResideCPEdit.Text != "")
				MySoil.ResidueCP = Convert.ToDouble(ResideCPEdit.Text);
			if (RootCPEdit.Text != "")
				MySoil.RootCP = Convert.ToDouble(RootCPEdit.Text);
			if (RateDissolRockEdit.Text != "")
				MySoil.RateDissolRock = Convert.ToDouble(RateDissolRockEdit.Text);
			}


		// --------------------------------------
		// Convert a double to a string, handling
		// missing values properly.
		// --------------------------------------
		private string DoubleToString(double Value, string Format)
			{
			if (Value == MathUtility.MissingValue)
				return "";
			else
				return Value.ToString(Format);
			}


		// ------------------------------------------
		// Refresh the summary row on the water grid.
		// ------------------------------------------
		private void RefreshPAWCCalculations()
			{
			if (SummaryRow != null)
				{
				double[] pawc = MySoil.PAWC();
				GridUtils.SetColumnAsDoubles(ref WaterGrid, 6, pawc, "f1");
				SummaryRow.Cells[6].Value = MathUtility.Sum(pawc).ToString("f1");

				// calculate pawc for each crop.
				string[] CropNames = MySoil.Crops;
				for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
					{
					int CropCol = NUMBER_OF_STATIC_COLS + CropNumber*4;
					string CropName = CropNames[CropNumber];
					double[] paw = MySoil.PAWC(CropName);
					GridUtils.SetColumnAsDoubles(ref WaterGrid, CropCol+1, paw, "f1");
					SummaryRow.Cells[CropCol+1].Value = MathUtility.Sum(paw).ToString("f1");
					}
				}
			}


		// ------------------------------------------
		// Grid has changed - update the PAWC columns
		// ------------------------------------------
		private void CellLeavingEdit(object sender, Xceed.Grid.EditLeftEventArgs e)
			{
			if (!InCellLeavingEdit)
				{
				InCellLeavingEdit = true;
				SaveWaterGrid();
				if (WaterGrid.CurrentCell.ParentColumn.Index == 5 ||
					WaterGrid.CurrentCell.ParentColumn.Title.IndexOf("LL\r\n(%vol)") > 0)
					RefreshPAWCCalculations();

				WaterChartControl.Refresh();
				InCellLeavingEdit = false;
				}
			}


		// ----------------------------------------------
		// User has clicked a button - find out which one
		// ----------------------------------------------
		private void WaterToolBar_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
			{
			if (e.Button == AddCropButton)
				{
				string NewCropName = InputDialog.InputBox("Enter the name of the new crop:", "New crop", "");
				if (NewCropName != "")
					{
					MySoil.AddCrop(NewCropName);
					Refresh();
					}
				}
			else if (e.Button == DeleteCropButton)
				{
				string CropNameToDelete = InputDialog.InputBox("Enter the name of the crop to delete:", "New crop", "");
				if (CropNameToDelete != "")
					{
					try
						{
						MySoil.DeleteCrop(CropNameToDelete);
						Refresh();
						}
					catch (Exception)
						{
						MessageBox.Show("Cannot delete crop: " + CropNameToDelete, "Error", 
							            MessageBoxButtons.OK, MessageBoxIcon.Error);
						}
					}
				}
			}


		}
	}

