using System;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using CSGeneral;
using VBGeneral;
using FarPoint.Win.Spread;

namespace CSGeneral
	{

	// -------------------------------------------
	// A user interface for soil stuff.
	// -------------------------------------------
	public class SoilUI : VBGeneral.BaseView
		{
		private System.ComponentModel.IContainer components = null;
		private CSGeneral.Soil MySoil;
		private System.Windows.Forms.ImageList ButtonImageList;
		private static int NUMBER_OF_STATIC_COLS = 6;
		private System.Windows.Forms.FontDialog fontDialog1;
		private FarPoint.Win.Spread.FpSpread Grid;
		private FarPoint.Win.Spread.SheetView General;
		private FarPoint.Win.Spread.SheetView SoilProfile;
		private FarPoint.Win.Spread.SheetView APSIM;
		private FarPoint.Win.Spread.SheetView Phosphorus;
		private FarPoint.Win.Spread.SheetView Water;
		private System.Windows.Forms.ContextMenu WaterMenu;
		private System.Windows.Forms.MenuItem AddCropMenuItem;
		private System.Windows.Forms.MenuItem DeleteCropMenuItem;
		private System.Windows.Forms.Splitter splitter1;
		private CSGeneral.WaterChartControl WaterChartControl;
		private System.Windows.Forms.MenuItem ReorderCropsMenuItem;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem PrintMenuItem;
		private TMGDevelopment.Windows.Forms.PrintForm PrintForm;
		private System.Windows.Forms.PrintPreviewDialog PrintPreviewDialog;
		private System.Windows.Forms.PrintDialog printDialog1;
		private System.Drawing.Printing.PrintDocument printDocument1;
		private System.Windows.Forms.Panel UnitPanel;
		private System.Windows.Forms.RadioButton VolumetricCheck;
		private System.Windows.Forms.RadioButton GravimetricCheck;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.MenuItem CheckSoilMenuItem;
		private System.Windows.Forms.MenuItem menuItem2;
		private bool UserChange = true;
		
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
			FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType1 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
			FarPoint.Win.BevelBorder bevelBorder1 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
			FarPoint.Win.BevelBorder bevelBorder2 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType1 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType2 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType3 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType4 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType5 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType6 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType7 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType8 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType9 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType10 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType11 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType12 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType13 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType14 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType15 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType16 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType17 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType18 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType19 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType20 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType21 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.NumberCellType numberCellType22 = new FarPoint.Win.Spread.CellType.NumberCellType();
			FarPoint.Win.Spread.CellType.TextCellType textCellType1 = new FarPoint.Win.Spread.CellType.TextCellType();
			FarPoint.Win.Spread.CellType.TextCellType textCellType2 = new FarPoint.Win.Spread.CellType.TextCellType();
			FarPoint.Win.Spread.CellType.TextCellType textCellType3 = new FarPoint.Win.Spread.CellType.TextCellType();
			FarPoint.Win.Spread.CellType.TextCellType textCellType4 = new FarPoint.Win.Spread.CellType.TextCellType();
			this.ButtonImageList = new System.Windows.Forms.ImageList(this.components);
			this.fontDialog1 = new System.Windows.Forms.FontDialog();
			this.Grid = new FarPoint.Win.Spread.FpSpread();
			this.WaterMenu = new System.Windows.Forms.ContextMenu();
			this.AddCropMenuItem = new System.Windows.Forms.MenuItem();
			this.DeleteCropMenuItem = new System.Windows.Forms.MenuItem();
			this.ReorderCropsMenuItem = new System.Windows.Forms.MenuItem();
			this.menuItem2 = new System.Windows.Forms.MenuItem();
			this.CheckSoilMenuItem = new System.Windows.Forms.MenuItem();
			this.menuItem1 = new System.Windows.Forms.MenuItem();
			this.PrintMenuItem = new System.Windows.Forms.MenuItem();
			this.General = new FarPoint.Win.Spread.SheetView();
			this.Water = new FarPoint.Win.Spread.SheetView();
			this.SoilProfile = new FarPoint.Win.Spread.SheetView();
			this.APSIM = new FarPoint.Win.Spread.SheetView();
			this.Phosphorus = new FarPoint.Win.Spread.SheetView();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.WaterChartControl = new CSGeneral.WaterChartControl();
			this.PrintForm = new TMGDevelopment.Windows.Forms.PrintForm(this.components);
			this.PrintPreviewDialog = new System.Windows.Forms.PrintPreviewDialog();
			this.printDocument1 = new System.Drawing.Printing.PrintDocument();
			this.printDialog1 = new System.Windows.Forms.PrintDialog();
			this.UnitPanel = new System.Windows.Forms.Panel();
			this.label1 = new System.Windows.Forms.Label();
			this.GravimetricCheck = new System.Windows.Forms.RadioButton();
			this.VolumetricCheck = new System.Windows.Forms.RadioButton();
			((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.General)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.Water)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.APSIM)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.Phosphorus)).BeginInit();
			this.UnitPanel.SuspendLayout();
			this.SuspendLayout();
			// 
			// ButtonImageList
			// 
			this.ButtonImageList.ImageSize = new System.Drawing.Size(24, 24);
			this.ButtonImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ButtonImageList.ImageStream")));
			this.ButtonImageList.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// Grid
			// 
			this.Grid.AllowDragDrop = true;
			this.Grid.ContextMenu = this.WaterMenu;
			this.Grid.Dock = System.Windows.Forms.DockStyle.Top;
			this.Grid.EditModeReplace = true;
			this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
			this.Grid.Location = new System.Drawing.Point(0, 64);
			this.Grid.Name = "Grid";
			this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
																			  this.General,
																			  this.Water,
																			  this.SoilProfile,
																			  this.APSIM,
																			  this.Phosphorus});
			this.Grid.Size = new System.Drawing.Size(1145, 292);
			this.Grid.TabIndex = 12;
			this.Grid.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
			this.Grid.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Always;
			this.Grid.TabStripRatio = 0.417256097560976;
			this.Grid.SetViewportLeftColumn(0, 1);
			this.Grid.SetViewportLeftColumn(1, 0, 6);
			this.Grid.SetActiveViewport(1, 0, -1);
			this.Grid.ActiveSheetIndex = 1;
			// 
			// WaterMenu
			// 
			this.WaterMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.AddCropMenuItem,
																					  this.DeleteCropMenuItem,
																					  this.ReorderCropsMenuItem,
																					  this.menuItem2,
																					  this.CheckSoilMenuItem,
																					  this.menuItem1,
																					  this.PrintMenuItem});
			// 
			// AddCropMenuItem
			// 
			this.AddCropMenuItem.Index = 0;
			this.AddCropMenuItem.Text = "&Add crop";
			this.AddCropMenuItem.Click += new System.EventHandler(this.AddCropMenuItem_Click_1);
			// 
			// DeleteCropMenuItem
			// 
			this.DeleteCropMenuItem.Index = 1;
			this.DeleteCropMenuItem.Text = "&Delete crop";
			this.DeleteCropMenuItem.Click += new System.EventHandler(this.DeleteCropMenuItem_Click_1);
			// 
			// ReorderCropsMenuItem
			// 
			this.ReorderCropsMenuItem.Index = 2;
			this.ReorderCropsMenuItem.Text = "&Reorder crops";
			this.ReorderCropsMenuItem.Click += new System.EventHandler(this.ReorderCropsMenuItem_Click);
			// 
			// menuItem2
			// 
			this.menuItem2.Index = 3;
			this.menuItem2.Text = "-";
			// 
			// CheckSoilMenuItem
			// 
			this.CheckSoilMenuItem.Index = 4;
			this.CheckSoilMenuItem.Text = "&Check soil for errors";
			this.CheckSoilMenuItem.Click += new System.EventHandler(this.CheckSoilMenuItem_Click);
			// 
			// menuItem1
			// 
			this.menuItem1.Index = 5;
			this.menuItem1.Text = "-";
			// 
			// PrintMenuItem
			// 
			this.PrintMenuItem.Index = 6;
			this.PrintMenuItem.Text = "&Print";
			this.PrintMenuItem.Click += new System.EventHandler(this.PrintClick);
			// 
			// General
			// 
			this.General.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.General.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.General.ColumnCount = 2;
			this.General.RowCount = 10;
			this.General.Cells.Get(0, 0).ParseFormatString = "G";
			this.General.Cells.Get(0, 0).Text = "Region: ";
			this.General.Cells.Get(1, 0).ParseFormatString = "G";
			this.General.Cells.Get(1, 0).Text = "Site: ";
			this.General.Cells.Get(2, 0).ParseFormatString = "G";
			this.General.Cells.Get(2, 0).Text = "Name: ";
			this.General.Cells.Get(2, 1).Locked = true;
			this.General.Cells.Get(3, 0).ParseFormatString = "G";
			this.General.Cells.Get(3, 0).Text = "Order / SubOrder: ";
			comboBoxCellType1.Editable = true;
			comboBoxCellType1.Items = new string[] {
													   "Black Vertosol",
													   "Grey Vertosol"};
			this.General.Cells.Get(3, 1).CellType = comboBoxCellType1;
			this.General.Cells.Get(4, 0).ParseFormatString = "G";
			this.General.Cells.Get(4, 0).Text = "Nearest Town: ";
			this.General.Cells.Get(5, 0).ParseFormatString = "G";
			this.General.Cells.Get(5, 0).Text = "GPS: ";
			this.General.Cells.Get(6, 0).ParseFormatString = "G";
			this.General.Cells.Get(6, 0).Text = "GPS DATUM: ";
			this.General.Cells.Get(7, 0).ParseFormatString = "G";
			this.General.Cells.Get(7, 0).Text = "Map ID: ";
			this.General.Cells.Get(8, 0).ParseFormatString = "G";
			this.General.Cells.Get(8, 0).Text = "Natural Vegetation: ";
			this.General.Cells.Get(9, 0).ParseFormatString = "G";
			this.General.Cells.Get(9, 0).Text = "Comments: ";
			this.General.ColumnHeader.Visible = false;
			this.General.Columns.Get(0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.General.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.General.Columns.Get(0).Locked = true;
			this.General.Columns.Get(0).Width = 120F;
			this.General.Columns.Get(1).Width = 546F;
			this.General.FrozenColumnCount = 1;
			this.General.RestrictColumns = true;
			this.General.RowHeader.Columns.Default.Resizable = false;
			this.General.RowHeader.Visible = false;
			this.General.Rows.Get(9).Height = 180F;
			this.General.SheetName = "General";
			this.General.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.General_CellChanged);
			this.General.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// Water
			// 
			this.Water.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.Water.ColumnCount = 6;
			this.Water.ColumnHeader.RowCount = 3;
			this.Water.RowCount = 12;
			this.Water.ColumnHeader.Cells.Get(0, 0).Border = bevelBorder1;
			this.Water.ColumnHeader.Cells.Get(0, 0).ColumnSpan = 6;
			this.Water.ColumnHeader.Cells.Get(0, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
			this.Water.ColumnHeader.Cells.Get(0, 0).Text = "Soil properties";
			this.Water.ColumnHeader.Cells.Get(0, 5).VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.General;
			this.Water.ColumnHeader.Cells.Get(1, 0).Border = bevelBorder2;
			this.Water.ColumnHeader.Cells.Get(1, 0).Text = "Depth";
			this.Water.ColumnHeader.Cells.Get(1, 1).Text = "BD";
			this.Water.ColumnHeader.Cells.Get(1, 2).Text = "SAT";
			this.Water.ColumnHeader.Cells.Get(1, 3).Text = " DUL";
			this.Water.ColumnHeader.Cells.Get(1, 4).Text = "AirDry";
			this.Water.ColumnHeader.Cells.Get(1, 5).Text = "LL15";
			this.Water.ColumnHeader.Cells.Get(2, 0).Text = "(cm)";
			this.Water.ColumnHeader.Cells.Get(2, 1).Text = " (g/cc)";
			this.Water.ColumnHeader.Cells.Get(2, 2).Text = "(%vol)";
			this.Water.ColumnHeader.Cells.Get(2, 3).Text = "(%vol)";
			this.Water.ColumnHeader.Cells.Get(2, 4).Text = "(%vol)";
			this.Water.ColumnHeader.Cells.Get(2, 5).Text = "(%vol)";
			this.Water.Columns.Get(0).Label = "(cm)";
			this.Water.Columns.Get(0).Width = 45F;
			numberCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType1.DecimalPlaces = 3;
			numberCellType1.DropDownButton = false;
			this.Water.Columns.Get(1).CellType = numberCellType1;
			this.Water.Columns.Get(1).Label = " (g/cc)";
			this.Water.Columns.Get(1).Width = 45F;
			numberCellType2.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType2.DecimalPlaces = 2;
			numberCellType2.DropDownButton = false;
			this.Water.Columns.Get(2).CellType = numberCellType2;
			this.Water.Columns.Get(2).Label = "(%vol)";
			this.Water.Columns.Get(2).Width = 48F;
			numberCellType3.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType3.DecimalPlaces = 2;
			numberCellType3.DropDownButton = false;
			this.Water.Columns.Get(3).CellType = numberCellType3;
			this.Water.Columns.Get(3).Label = "(%vol)";
			this.Water.Columns.Get(3).Width = 49F;
			numberCellType4.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType4.DecimalPlaces = 2;
			numberCellType4.DropDownButton = false;
			this.Water.Columns.Get(4).CellType = numberCellType4;
			this.Water.Columns.Get(4).Label = "(%vol)";
			this.Water.Columns.Get(4).Width = 48F;
			numberCellType5.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType5.DecimalPlaces = 2;
			numberCellType5.DropDownButton = false;
			this.Water.Columns.Get(5).CellType = numberCellType5;
			this.Water.Columns.Get(5).Label = "(%vol)";
			this.Water.Columns.Get(5).Width = 49F;
			this.Water.FrozenColumnCount = 6;
			this.Water.RowHeader.Columns.Default.Resizable = false;
			this.Water.RowHeader.Visible = false;
			this.Water.SheetName = "Water";
			this.Water.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.Water_CellChanged);
			this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// SoilProfile
			// 
			this.SoilProfile.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.SoilProfile.ColumnCount = 18;
			this.SoilProfile.ColumnHeader.RowCount = 2;
			this.SoilProfile.RowCount = 15;
			this.SoilProfile.ActiveColumnIndex = 5;
			this.SoilProfile.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
			this.SoilProfile.ColumnHeader.Cells.Get(0, 0).Text = "Depth";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 1).Text = "SWCon";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 2).Text = "MWCon";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 3).Text = "FBiom";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 4).Text = "FInert";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 5).Text = "OC";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 6).Text = "EC";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 7).Text = "PH";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 8).Text = "CL";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 9).Text = "CEC";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 10).Text = "Ca";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 11).Text = "Mg";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 12).Text = "Na";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 13).Text = "K";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 14).Text = "ESP";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 15).Text = "Particle";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 16).Text = "Particle";
			this.SoilProfile.ColumnHeader.Cells.Get(0, 17).Text = "Particle";
			this.SoilProfile.ColumnHeader.Cells.Get(1, 0).Text = "(cm)";
			numberCellType6.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType6.DecimalPlaces = 1;
			numberCellType6.DropDownButton = false;
			this.SoilProfile.Columns.Get(1).CellType = numberCellType6;
			numberCellType7.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType7.DecimalPlaces = 2;
			numberCellType7.DropDownButton = false;
			this.SoilProfile.Columns.Get(2).CellType = numberCellType7;
			numberCellType8.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType8.DecimalPlaces = 2;
			numberCellType8.DropDownButton = false;
			this.SoilProfile.Columns.Get(3).CellType = numberCellType8;
			numberCellType9.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType9.DecimalPlaces = 1;
			numberCellType9.DropDownButton = false;
			this.SoilProfile.Columns.Get(4).CellType = numberCellType9;
			numberCellType10.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType10.DecimalPlaces = 2;
			numberCellType10.DropDownButton = false;
			this.SoilProfile.Columns.Get(5).CellType = numberCellType10;
			numberCellType11.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType11.DecimalPlaces = 1;
			numberCellType11.DropDownButton = false;
			this.SoilProfile.Columns.Get(6).CellType = numberCellType11;
			numberCellType12.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType12.DecimalPlaces = 1;
			numberCellType12.DropDownButton = false;
			this.SoilProfile.Columns.Get(7).CellType = numberCellType12;
			numberCellType13.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType13.DecimalPlaces = 1;
			numberCellType13.DropDownButton = false;
			this.SoilProfile.Columns.Get(8).CellType = numberCellType13;
			numberCellType14.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType14.DecimalPlaces = 1;
			numberCellType14.DropDownButton = false;
			this.SoilProfile.Columns.Get(9).CellType = numberCellType14;
			numberCellType15.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType15.DecimalPlaces = 1;
			numberCellType15.DropDownButton = false;
			this.SoilProfile.Columns.Get(10).CellType = numberCellType15;
			numberCellType16.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType16.DecimalPlaces = 1;
			numberCellType16.DropDownButton = false;
			this.SoilProfile.Columns.Get(11).CellType = numberCellType16;
			numberCellType17.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType17.DecimalPlaces = 1;
			numberCellType17.DropDownButton = false;
			this.SoilProfile.Columns.Get(12).CellType = numberCellType17;
			numberCellType18.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType18.DecimalPlaces = 1;
			numberCellType18.DropDownButton = false;
			this.SoilProfile.Columns.Get(13).CellType = numberCellType18;
			numberCellType19.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType19.DecimalPlaces = 1;
			numberCellType19.DropDownButton = false;
			this.SoilProfile.Columns.Get(14).CellType = numberCellType19;
			numberCellType20.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType20.DecimalPlaces = 1;
			numberCellType20.DropDownButton = false;
			this.SoilProfile.Columns.Get(15).CellType = numberCellType20;
			numberCellType21.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType21.DecimalPlaces = 1;
			numberCellType21.DropDownButton = false;
			this.SoilProfile.Columns.Get(16).CellType = numberCellType21;
			numberCellType22.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			numberCellType22.DecimalPlaces = 1;
			numberCellType22.DropDownButton = false;
			this.SoilProfile.Columns.Get(17).CellType = numberCellType22;
			this.SoilProfile.RowHeader.Columns.Default.Resizable = false;
			this.SoilProfile.SheetName = "Soil profile";
			this.SoilProfile.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.SoilProfile_CellChanged);
			this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// APSIM
			// 
			this.APSIM.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.APSIM.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.APSIM.ColumnCount = 2;
			this.APSIM.RowCount = 18;
			this.APSIM.Cells.Get(0, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.APSIM.Cells.Get(0, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.APSIM.Cells.Get(0, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(0, 0).Text = "Evaporation";
			this.APSIM.Cells.Get(0, 1).Locked = true;
			this.APSIM.Cells.Get(1, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.APSIM.Cells.Get(1, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(1, 0).Text = "U";
			this.APSIM.Cells.Get(2, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.APSIM.Cells.Get(2, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(2, 0).Text = "Cona";
			this.APSIM.Cells.Get(3, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.APSIM.Cells.Get(3, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(3, 0).Text = "Salb";
			this.APSIM.Cells.Get(4, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.APSIM.Cells.Get(4, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(4, 0).Text = "Unsaturated Flow";
			this.APSIM.Cells.Get(4, 1).Locked = true;
			this.APSIM.Cells.Get(5, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(5, 0).Text = "DiffusConst";
			this.APSIM.Cells.Get(6, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(6, 0).Text = "DiffusSlope";
			this.APSIM.Cells.Get(7, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.APSIM.Cells.Get(7, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(7, 0).Text = "Runoff";
			this.APSIM.Cells.Get(7, 1).Locked = true;
			this.APSIM.Cells.Get(8, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(8, 0).Text = "CN2Bare";
			this.APSIM.Cells.Get(9, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(9, 0).Text = "CNRed";
			this.APSIM.Cells.Get(10, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(10, 0).Text = "CNCov";
			this.APSIM.Cells.Get(11, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.APSIM.Cells.Get(11, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(11, 0).Text = "Organic Matter";
			this.APSIM.Cells.Get(11, 1).Locked = true;
			this.APSIM.Cells.Get(12, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(12, 0).Text = "RootCN";
			this.APSIM.Cells.Get(13, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(13, 0).Text = "RootWt";
			this.APSIM.Cells.Get(14, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(14, 0).Text = "SoilCN";
			this.APSIM.Cells.Get(15, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
			this.APSIM.Cells.Get(15, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(15, 0).Text = "Erosion";
			this.APSIM.Cells.Get(15, 1).Locked = true;
			this.APSIM.Cells.Get(16, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(16, 0).Text = "EnrACoeff";
			this.APSIM.Cells.Get(17, 0).ParseFormatString = "G";
			this.APSIM.Cells.Get(17, 0).Text = "EnrBCoeff";
			this.APSIM.ColumnHeader.Visible = false;
			this.APSIM.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.APSIM.Columns.Get(0).Locked = true;
			this.APSIM.Columns.Get(0).Width = 101F;
			this.APSIM.Columns.Get(1).Width = 100F;
			this.APSIM.RowHeader.Columns.Default.Resizable = false;
			this.APSIM.SheetName = "APSIM";
			this.APSIM.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.APSIM_CellChanged);
			this.APSIM.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// Phosphorus
			// 
			this.Phosphorus.Reset();
			// Formulas and custom names must be loaded with R1C1 reference style
			this.Phosphorus.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
			this.Phosphorus.ColumnCount = 5;
			this.Phosphorus.ColumnHeader.RowCount = 2;
			this.Phosphorus.RowCount = 17;
			this.Phosphorus.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
			this.Phosphorus.ColumnHeader.Cells.Get(0, 0).Text = "Depth";
			this.Phosphorus.ColumnHeader.Cells.Get(0, 1).Text = "Labile P";
			this.Phosphorus.ColumnHeader.Cells.Get(0, 2).Text = "Banded P";
			this.Phosphorus.ColumnHeader.Cells.Get(0, 3).Text = "Rock P";
			this.Phosphorus.ColumnHeader.Cells.Get(0, 4).Text = "Sorption";
			this.Phosphorus.ColumnHeader.Cells.Get(1, 0).Text = "(cm)";
			this.Phosphorus.ColumnHeader.Cells.Get(1, 1).Text = "(mg/kg)";
			this.Phosphorus.ColumnHeader.Cells.Get(1, 2).Text = "(kg/ha)";
			this.Phosphorus.ColumnHeader.Cells.Get(1, 3).Text = "(kg/ha)";
			this.Phosphorus.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.Columns.Get(0).Label = "(cm)";
			this.Phosphorus.Columns.Get(0).Width = 70F;
			textCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			textCellType1.DropDownButton = false;
			this.Phosphorus.Columns.Get(1).CellType = textCellType1;
			this.Phosphorus.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.Columns.Get(1).Label = "(mg/kg)";
			this.Phosphorus.Columns.Get(1).Width = 71F;
			textCellType2.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			textCellType2.DropDownButton = false;
			this.Phosphorus.Columns.Get(2).CellType = textCellType2;
			this.Phosphorus.Columns.Get(2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.Columns.Get(2).Label = "(kg/ha)";
			textCellType3.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			textCellType3.DropDownButton = false;
			this.Phosphorus.Columns.Get(3).CellType = textCellType3;
			this.Phosphorus.Columns.Get(3).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.Columns.Get(3).Label = "(kg/ha)";
			textCellType4.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
			textCellType4.DropDownButton = false;
			this.Phosphorus.Columns.Get(4).CellType = textCellType4;
			this.Phosphorus.Columns.Get(4).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.RowHeader.Columns.Default.Resizable = false;
			this.Phosphorus.Rows.Get(14).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
			this.Phosphorus.SheetName = "Phosphorus";
			this.Phosphorus.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.Phosphorus_CellChanged);
			this.Phosphorus.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
			// 
			// splitter1
			// 
			this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
			this.splitter1.Location = new System.Drawing.Point(0, 356);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(1145, 3);
			this.splitter1.TabIndex = 13;
			this.splitter1.TabStop = false;
			// 
			// WaterChartControl
			// 
			this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.WaterChartControl.LinkedSoil = null;
			this.WaterChartControl.Location = new System.Drawing.Point(0, 359);
			this.WaterChartControl.Name = "WaterChartControl";
			this.WaterChartControl.ShowSoilWaterLine = false;
			this.WaterChartControl.Size = new System.Drawing.Size(1145, 342);
			this.WaterChartControl.TabIndex = 14;
			// 
			// PrintForm
			// 
			this.PrintForm.AutoFit = TMGDevelopment.Windows.Forms.PageElement.Body;
			this.PrintForm.BodyContainer = this;
			this.PrintForm.CenterStyle = TMGDevelopment.Windows.Forms.CenterStyle.None;
			this.PrintForm.PreDraw += new TMGDevelopment.Windows.Forms.PreDrawEventHandler(this.printForm1_PreDraw);
			// 
			// PrintPreviewDialog
			// 
			this.PrintPreviewDialog.AutoScrollMargin = new System.Drawing.Size(0, 0);
			this.PrintPreviewDialog.AutoScrollMinSize = new System.Drawing.Size(0, 0);
			this.PrintPreviewDialog.ClientSize = new System.Drawing.Size(400, 300);
			this.PrintPreviewDialog.Document = this.printDocument1;
			this.PrintPreviewDialog.Enabled = true;
			this.PrintPreviewDialog.Icon = ((System.Drawing.Icon)(resources.GetObject("PrintPreviewDialog.Icon")));
			this.PrintPreviewDialog.Location = new System.Drawing.Point(272, 17);
			this.PrintPreviewDialog.MinimumSize = new System.Drawing.Size(375, 250);
			this.PrintPreviewDialog.Name = "PrintPreviewDialog";
			this.PrintPreviewDialog.TransparencyKey = System.Drawing.Color.Empty;
			this.PrintPreviewDialog.Visible = false;
			// 
			// printDocument1
			// 
			this.printDocument1.BeginPrint += new System.Drawing.Printing.PrintEventHandler(this.printDocument1_BeginPrint);
			this.printDocument1.QueryPageSettings += new System.Drawing.Printing.QueryPageSettingsEventHandler(this.printDocument1_QueryPageSettings);
			this.printDocument1.PrintPage += new System.Drawing.Printing.PrintPageEventHandler(this.printDocument1_PrintPage);
			// 
			// printDialog1
			// 
			this.printDialog1.Document = this.printDocument1;
			// 
			// UnitPanel
			// 
			this.UnitPanel.Controls.Add(this.label1);
			this.UnitPanel.Controls.Add(this.GravimetricCheck);
			this.UnitPanel.Controls.Add(this.VolumetricCheck);
			this.UnitPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.UnitPanel.Location = new System.Drawing.Point(0, 40);
			this.UnitPanel.Name = "UnitPanel";
			this.UnitPanel.Size = new System.Drawing.Size(1145, 24);
			this.UnitPanel.TabIndex = 15;
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(8, 4);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(139, 16);
			this.label1.TabIndex = 2;
			this.label1.Text = "Format for water variables:";
			// 
			// GravimetricCheck
			// 
			this.GravimetricCheck.Location = new System.Drawing.Point(304, 2);
			this.GravimetricCheck.Name = "GravimetricCheck";
			this.GravimetricCheck.Size = new System.Drawing.Size(104, 20);
			this.GravimetricCheck.TabIndex = 1;
			this.GravimetricCheck.Text = "Gravimetric %";
			this.GravimetricCheck.Click += new System.EventHandler(this.VolGravChecked);
			// 
			// VolumetricCheck
			// 
			this.VolumetricCheck.Checked = true;
			this.VolumetricCheck.Location = new System.Drawing.Point(168, 2);
			this.VolumetricCheck.Name = "VolumetricCheck";
			this.VolumetricCheck.Size = new System.Drawing.Size(104, 20);
			this.VolumetricCheck.TabIndex = 0;
			this.VolumetricCheck.TabStop = true;
			this.VolumetricCheck.Text = "Volumetric %";
			this.VolumetricCheck.Click += new System.EventHandler(this.VolGravChecked);
			// 
			// SoilUI
			// 
			this.Controls.Add(this.WaterChartControl);
			this.Controls.Add(this.splitter1);
			this.Controls.Add(this.Grid);
			this.Controls.Add(this.UnitPanel);
			this.Name = "SoilUI";
			this.Size = new System.Drawing.Size(1145, 701);
			this.Controls.SetChildIndex(this.UnitPanel, 0);
			this.Controls.SetChildIndex(this.Grid, 0);
			this.Controls.SetChildIndex(this.splitter1, 0);
			this.Controls.SetChildIndex(this.WaterChartControl, 0);
			((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.General)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.Water)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.APSIM)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.Phosphorus)).EndInit();
			this.UnitPanel.ResumeLayout(false);
			this.ResumeLayout(false);

		}
		#endregion


		// ------------------------
		// Refresh ourselves
		// ------------------------
		override public void Refresh()
			{
			try
				{
				if (MySoil == null)
					{
					FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused); 
					InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), 
								FarPoint.Win.Spread.SpreadActions.ClipboardCut); 
					InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), 
								FarPoint.Win.Spread.SpreadActions.MoveToNextRow); 
					Grid_ActiveSheetChanged(null, null);
					}
				MySoil = new CSGeneral.Soil(Controller.Data);
				ApsoilController Apsoil = Controller as ApsoilController;
				Apsoil.AddCropEvent -= new BaseController.NotifyEventHandler(Refresh);
				Apsoil.AddCropEvent += new BaseController.NotifyEventHandler(Refresh);
				Apsoil.DeleteCropEvent -= new BaseController.NotifyEventHandler(Refresh);
				Apsoil.DeleteCropEvent += new BaseController.NotifyEventHandler(Refresh);
				Apsoil.ReorderCropEvent -= new BaseController.NotifyEventHandler(Refresh);
				Apsoil.ReorderCropEvent += new BaseController.NotifyEventHandler(Refresh);
				Apsoil.AddNoteEvent -= new BaseController.NotifyEventHandler(AddNote);
				Apsoil.AddNoteEvent += new BaseController.NotifyEventHandler(AddNote);
				Apsoil.DeleteNoteEvent -= new BaseController.NotifyEventHandler(DeleteNote);
				Apsoil.DeleteNoteEvent += new BaseController.NotifyEventHandler(DeleteNote);
				Apsoil.PrintEvent -= new BaseController.NotifyEventHandler(Print);
				Apsoil.PrintEvent += new BaseController.NotifyEventHandler(Print);

				HelpText = "The 5 sheets below contain all the soil properties required for APSIM.";
				WaterChartControl.LinkedSoil = MySoil;
				PopulateGeneralGrid();
				PopulateWaterGrid();
				PopulateProfileGrid();
				PopulateAPSIMGrid();
				PopulatePhosphorusGrid();
				SetupCellNotes();
				OperationMode mode = OperationMode.Normal;
				
				if (!Apsoil.AllowChanges)
					mode = OperationMode.ReadOnly;
					
				General.OperationMode = mode;
				Water.OperationMode = mode;
				SoilProfile.OperationMode = mode;
				APSIM.OperationMode = mode;
				Phosphorus.OperationMode = mode;
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}


		// ----------------------------
		// Populate the general grid
		// ----------------------------
		private void PopulateGeneralGrid()
			{
			UserChange = false;
			General.ClearRange(0, 1, General.RowCount, 1, true);
			General.Cells[0, 1].Value = MySoil.Region;
			General.Cells[1, 1].Value = MySoil.Site;
			General.Cells[2, 1].Value = MySoil.Name;
			General.Cells[3, 1].Value = MySoil.Order;
			General.Cells[4, 1].Value = MySoil.NearestTown;
			General.Cells[5, 1].Value = MySoil.Comment;
			General.Cells[6, 1].Value = MySoil.GPS;
			General.Cells[7, 1].Value = MySoil.GPSDatum;
			General.Cells[8, 1].Value = MySoil.MapId;
			General.Cells[9, 1].Value = MySoil.NaturalVegetation;
			UserChange = true;
			//GeneralGrid.Columns[1].Width = GeneralGrid.DisplayRectangle.Width - GeneralGrid.Columns[0].Width;
			}


		// ---------------------
		// Save the general grid
		// ---------------------
		private void SaveGeneralGrid()
			{
			MySoil.Region = GridUtils.GetCellAsString(General, 1, 0);
			MySoil.Site = GridUtils.GetCellAsString(General, 1, 1);
			MySoil.Order = GridUtils.GetCellAsString(General, 1, 3); 
			MySoil.NearestTown = GridUtils.GetCellAsString(General, 1, 4);
			MySoil.Comment = GridUtils.GetCellAsString(General, 1, 5);
			MySoil.GPS = GridUtils.GetCellAsString(General, 1, 6);
			MySoil.GPSDatum = GridUtils.GetCellAsString(General, 1, 7);
			MySoil.MapId = GridUtils.GetCellAsString(General, 1, 8);
			MySoil.NaturalVegetation = GridUtils.GetCellAsString(General, 1, 9);
			}


		// -----------------------------------------------
		// Populate the water grid from the specified soil
		// -----------------------------------------------
		private void PopulateWaterGrid()
			{
			UserChange = false;
			Water.RowCount = 1;
			VolumetricCheck.Checked = (MySoil.StoredWaterFormat == Soil.StoredWaterFormatType.VolumetricPercent);
			GravimetricCheck.Checked = !VolumetricCheck.Checked;

			GridUtils.SetColumnAsStrings(Water, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(Water, 1, MySoil.BD);
			string WaterUnits;
			if (VolumetricCheck.Checked)
				{
				GridUtils.SetColumnAsDoubles(Water, 2, MySoil.SAT);
				GridUtils.SetColumnAsDoubles(Water, 3, MySoil.DUL);
				GridUtils.SetColumnAsDoubles(Water, 4, MySoil.Airdry);
				GridUtils.SetColumnAsDoubles(Water, 5, MySoil.LL15);
				WaterUnits = "(%vol)";
				}
			else
				{
				GridUtils.SetColumnAsDoubles(Water, 2, MySoil.SATGrav);
				GridUtils.SetColumnAsDoubles(Water, 3, MySoil.DULGrav);
				GridUtils.SetColumnAsDoubles(Water, 4, MySoil.AirdryGrav);
				GridUtils.SetColumnAsDoubles(Water, 5, MySoil.LL15Grav);
				WaterUnits = "(%grav)";
				}
			Water.ColumnHeader.Cells[2, 2].Text = WaterUnits;
			Water.ColumnHeader.Cells[2, 3].Text = WaterUnits;
			Water.ColumnHeader.Cells[2, 4].Text = WaterUnits;
			Water.ColumnHeader.Cells[2, 5].Text = WaterUnits;

			// Make sure we have the right number of crop columns.
			string[] CropNames = MySoil.Crops;
			int RequiredNumberOfColumns = CropNames.Length*4 + NUMBER_OF_STATIC_COLS;
			Water.ColumnCount = RequiredNumberOfColumns;

			// Fill all crop columns with numbers.
			for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
				{
				int CropCol = NUMBER_OF_STATIC_COLS + CropNumber*4;
				string CropName = CropNames[CropNumber];
				Color CropColor = Color.GreenYellow;
				if (CropNumber / 2.0 != CropNumber / 2)
					CropColor = Color.PaleGreen;
				Color PredCropColor = Color.Pink;
				if (CropNumber / 2.0 != CropNumber / 2)
					PredCropColor = Color.DeepPink;

				// setup the LL column.
				bool Predicted = MySoil.CropIsPredicted(CropName);
				if (Predicted)
					Water.ColumnHeader.Cells[0, CropCol].Text = "Predicted " + CropName;
				else
					Water.ColumnHeader.Cells[0, CropCol].Text = CropName;

				Water.ColumnHeader.Cells[0, CropCol].ColumnSpan = 4;
				Water.ColumnHeader.Cells[0, CropCol].HorizontalAlignment = CellHorizontalAlignment.Center;
				Water.ColumnHeader.Cells[1, CropCol].Text = "LL";
				Water.ColumnHeader.Cells[2, CropCol].Text = WaterUnits;
				if (VolumetricCheck.Checked)
					GridUtils.SetColumnAsDoubles(Water, CropCol, MySoil.LL(CropName));
				else
					GridUtils.SetColumnAsDoubles(Water, CropCol, MySoil.LLGrav(CropName));

				if (Predicted)
					{
					Water.Columns[CropCol].BackColor = PredCropColor;
					Water.Columns[CropCol].Locked = true;
					}
				else
					Water.Columns[CropCol].BackColor = CropColor;
				Water.Columns[CropCol].Width = 50;
				FarPoint.Win.Spread.CellType.NumberCellType LLFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
				LLFormatter.DecimalPlaces = 2;
				Water.Columns[CropCol].CellType = LLFormatter;

				// setup the PAWC column.
				Water.ColumnHeader.Cells[1,CropCol+1].Text = "PAWC";
				Water.ColumnHeader.Cells[2,CropCol+1].Text = "(mm)";
				Water.Columns[CropCol+1].Locked = true;
				if (Predicted)
					Water.Columns[CropCol+1].BackColor = PredCropColor;
				else
					Water.Columns[CropCol+1].BackColor = CropColor;
				Water.Columns[CropCol+1].Width = 45;
				RefreshPAWCColumn(CropCol+1);
				FarPoint.Win.Spread.CellType.NumberCellType PAWCFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
				PAWCFormatter.DecimalPlaces = 1;
				Water.Columns[CropCol+1].CellType = PAWCFormatter;

				// setup the KL column
				Water.ColumnHeader.Cells[1, CropCol+2].Text = "KL";
				Water.ColumnHeader.Cells[2, CropCol+2].Text = " ";
				GridUtils.SetColumnAsDoubles(Water, CropCol+2, MySoil.KL(CropName));
				if (Predicted)
					{
					Water.Columns[CropCol+2].BackColor = PredCropColor;
					Water.Columns[CropCol+2].Locked = true;
					}
				else
					Water.Columns[CropCol+2].BackColor = CropColor;
				Water.Columns[CropCol+2].Width = 45;
				FarPoint.Win.Spread.CellType.NumberCellType KLFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
				KLFormatter.DecimalPlaces = 2;
				Water.Columns[CropCol+2].CellType = KLFormatter;

				// setup the XF column
				Water.ColumnHeader.Cells[1, CropCol+3].Text = "XF";
				Water.ColumnHeader.Cells[2, CropCol+3].Text = " ";
				GridUtils.SetColumnAsDoubles(Water, CropCol+3, MySoil.XF(CropName));
				if (Predicted)
					{
					Water.Columns[CropCol+3].BackColor = PredCropColor;
					Water.Columns[CropCol+3].Locked = true;
					}
				else
					Water.Columns[CropCol+3].BackColor = CropColor;
				Water.Columns[CropCol+3].Width = 45;
				FarPoint.Win.Spread.CellType.NumberCellType XFFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
				XFFormatter.DecimalPlaces = 1;
				Water.Columns[CropCol+3].CellType = XFFormatter;
				}

			AddSummaryRow();
			UserChange = true;
			}


		// --------------------------
		// Save the water grid
		// --------------------------
		private void SaveWaterGrid()
			{	
			for (int col = 0; col != Water.ColumnCount; col++)
				SaveWaterGridColumn(col);
			}

		
		// --------------------------
		// Save the water grid
		// --------------------------
		private void SaveWaterGridColumn(int ColumnIndex)
			{
			if (ColumnIndex == 0)
				AddSummaryRow();
			int NumLayers = GridUtils.FindFirstBlankCell(Water, 0);
			switch (ColumnIndex)
				{
				case 0: MySoil.DepthStrings = GridUtils.GetColumnAsStrings(Water, 0, NumLayers); break;
				case 1: MySoil.BD = GridUtils.GetColumnAsDoubles(Water, 1, NumLayers); break;
				case 2: if (VolumetricCheck.Checked)
							MySoil.SAT = GridUtils.GetColumnAsDoubles(Water, 2, NumLayers); 
						else
							MySoil.SATGrav = GridUtils.GetColumnAsDoubles(Water, 2, NumLayers); 
						break;
				case 3: if (VolumetricCheck.Checked)
							MySoil.DUL = GridUtils.GetColumnAsDoubles(Water, 3, NumLayers);
						else
							MySoil.DULGrav = GridUtils.GetColumnAsDoubles(Water, 3, NumLayers);
						RefreshPAWCColumns();
						break;
				case 4: if (VolumetricCheck.Checked)
							MySoil.Airdry = GridUtils.GetColumnAsDoubles(Water, 4, NumLayers);
						else
							MySoil.AirdryGrav = GridUtils.GetColumnAsDoubles(Water, 4, NumLayers); 
						break;
				case 5: if (VolumetricCheck.Checked)
							MySoil.LL15 = GridUtils.GetColumnAsDoubles(Water, 5, NumLayers);
						else
							MySoil.LL15Grav = GridUtils.GetColumnAsDoubles(Water, 5, NumLayers); 
						RefreshPAWCColumns(); 
						break;
				default:
					{
					int CropCol = (ColumnIndex - NUMBER_OF_STATIC_COLS) / 4 * 4 + NUMBER_OF_STATIC_COLS;
					string CropName = Water.ColumnHeader.Cells[0, CropCol].Text;
					if (!CropName.StartsWith("Predicted "))
						{
						string[] CropNames = MySoil.Crops;
						// Save all crop columns
						double[] ll = GridUtils.GetColumnAsDoubles(Water, CropCol, NumLayers);
						double[] kl = GridUtils.GetColumnAsDoubles(Water, CropCol+2, NumLayers);
						double[] xf = GridUtils.GetColumnAsDoubles(Water, CropCol+3, NumLayers);
						if (VolumetricCheck.Checked)
							MySoil.SetCrop(CropName, ll, kl, xf);
						else
							MySoil.SetCropGrav(CropName, ll, kl, xf);
						RefreshPAWCColumn(CropCol+1);
						}
					break;
					}
				}
			WaterChartControl.Refresh();
			}


		// --------------------------
		// Populate the profile grid.
		// --------------------------
		private void PopulateProfileGrid()
			{
			UserChange = false;
			SoilProfile.ClearRange(0, 0, SoilProfile.RowCount, SoilProfile.ColumnCount, true);
			GridUtils.SetColumnAsStrings(SoilProfile, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(SoilProfile, 1, MySoil.SWCON);
			GridUtils.SetColumnAsDoubles(SoilProfile, 2, MySoil.MWCON);
			GridUtils.SetColumnAsDoubles(SoilProfile, 3, MySoil.FBIOM);
			GridUtils.SetColumnAsDoubles(SoilProfile, 4, MySoil.FINERT);
			GridUtils.SetColumnAsDoubles(SoilProfile, 5, MySoil.OC);
			GridUtils.SetColumnAsDoubles(SoilProfile, 6, MySoil.EC);
			GridUtils.SetColumnAsDoubles(SoilProfile, 7, MySoil.PH);
			GridUtils.SetColumnAsDoubles(SoilProfile, 8, MySoil.CL);
			GridUtils.SetColumnAsDoubles(SoilProfile, 9, MySoil.CEC);
			GridUtils.SetColumnAsDoubles(SoilProfile, 10, MySoil.Ca);
			GridUtils.SetColumnAsDoubles(SoilProfile, 11, MySoil.Mg);
			GridUtils.SetColumnAsDoubles(SoilProfile, 12, MySoil.Na);
			GridUtils.SetColumnAsDoubles(SoilProfile, 13, MySoil.K);
			GridUtils.SetColumnAsDoubles(SoilProfile, 14, MySoil.ESP);
			GridUtils.SetColumnAsDoubles(SoilProfile, 15, MySoil.ParticleSizeSand);
			GridUtils.SetColumnAsDoubles(SoilProfile, 16, MySoil.ParticleSizeSilt);
			GridUtils.SetColumnAsDoubles(SoilProfile, 17, MySoil.ParticleSizeClay);
			UserChange = true;
			}


		// --------------------------
		// Save the profile grid.
		// --------------------------
		private void SaveProfileGrid(int ColumnIndex)
			{
			int NumLayers = GridUtils.FindFirstBlankCell(SoilProfile, 0);
			switch (ColumnIndex)
				{
				case 1: MySoil.SWCON  = GridUtils.GetColumnAsDoubles(SoilProfile, 1, NumLayers); break;
				case 2: MySoil.MWCON  = GridUtils.GetColumnAsDoubles(SoilProfile, 2, NumLayers); break;
				case 3: MySoil.FBIOM  = GridUtils.GetColumnAsDoubles(SoilProfile, 3, NumLayers);  break;
				case 4: MySoil.FINERT = GridUtils.GetColumnAsDoubles(SoilProfile, 4, NumLayers);  break;
				case 5: MySoil.OC     = GridUtils.GetColumnAsDoubles(SoilProfile, 5, NumLayers);  break;
				case 6: MySoil.EC     = GridUtils.GetColumnAsDoubles(SoilProfile, 6, NumLayers);  break;
				case 7: MySoil.PH     = GridUtils.GetColumnAsDoubles(SoilProfile, 7, NumLayers);  break;
				case 8: MySoil.CL     = GridUtils.GetColumnAsDoubles(SoilProfile, 8, NumLayers);  break;
				case 9: MySoil.CEC    = GridUtils.GetColumnAsDoubles(SoilProfile, 9, NumLayers);  break;
				case 10: MySoil.Ca     = GridUtils.GetColumnAsDoubles(SoilProfile, 10, NumLayers);  break;
				case 11: MySoil.Mg     = GridUtils.GetColumnAsDoubles(SoilProfile, 11, NumLayers); break;
				case 12: MySoil.Na     = GridUtils.GetColumnAsDoubles(SoilProfile, 12, NumLayers); break;
				case 13: MySoil.K      = GridUtils.GetColumnAsDoubles(SoilProfile, 13, NumLayers); break;
				case 14: MySoil.ESP    = GridUtils.GetColumnAsDoubles(SoilProfile, 14, NumLayers); break;
				case 15: MySoil.ParticleSizeSand = GridUtils.GetColumnAsDoubles(SoilProfile, 15, NumLayers); break;
				case 16: MySoil.ParticleSizeSilt = GridUtils.GetColumnAsDoubles(SoilProfile, 16, NumLayers); break;
				case 17: MySoil.ParticleSizeClay = GridUtils.GetColumnAsDoubles(SoilProfile, 17, NumLayers); break;
				}
			}

		// ----------------------------
		// Populate the APSIM grid
		// ----------------------------
		private void PopulateAPSIMGrid()
			{
			UserChange = false;
			APSIM.ClearRange(0, 1, APSIM.RowCount, 1, true);
			GridUtils.SetCellAsDouble(APSIM, 1, 1, MySoil.U);
			GridUtils.SetCellAsDouble(APSIM, 1, 2, MySoil.Cona);
			GridUtils.SetCellAsDouble(APSIM, 1, 3, MySoil.Salb);
			GridUtils.SetCellAsDouble(APSIM, 1, 5, MySoil.DiffusConst);
			GridUtils.SetCellAsDouble(APSIM, 1, 6, MySoil.DiffusSlope);
			GridUtils.SetCellAsDouble(APSIM, 1, 8, MySoil.CN2Bare);
			GridUtils.SetCellAsDouble(APSIM, 1, 9, MySoil.CNRed);
			GridUtils.SetCellAsDouble(APSIM, 1, 10, MySoil.CNCov);
			GridUtils.SetCellAsDouble(APSIM, 1, 12, MySoil.RootCN);
			GridUtils.SetCellAsDouble(APSIM, 1, 13, MySoil.RootWT);
			GridUtils.SetCellAsDouble(APSIM, 1, 14, MySoil.SoilCN);
			GridUtils.SetCellAsDouble(APSIM, 1, 16, MySoil.EnrACoeff);
			GridUtils.SetCellAsDouble(APSIM, 1, 17, MySoil.EnrBCoeff);
			UserChange = true;
			
			}


		// ---------------------
		// Save the APSIM grid
		// ---------------------
		private void SaveAPSIMGrid()
			{
			MySoil.U = GridUtils.GetCellAsDouble(APSIM, 1, 1);
			MySoil.Cona = GridUtils.GetCellAsDouble(APSIM, 1, 2);
			MySoil.Salb = GridUtils.GetCellAsDouble(APSIM, 1, 3);
			MySoil.DiffusConst = GridUtils.GetCellAsDouble(APSIM, 1, 5);
			MySoil.DiffusSlope = GridUtils.GetCellAsDouble(APSIM, 1, 6);
			MySoil.CN2Bare = GridUtils.GetCellAsDouble(APSIM, 1, 8);
			MySoil.CNRed = GridUtils.GetCellAsDouble(APSIM, 1, 9);
			MySoil.CNCov = GridUtils.GetCellAsDouble(APSIM, 1, 10);
			MySoil.RootCN = GridUtils.GetCellAsDouble(APSIM, 1, 12);
			MySoil.RootWT = GridUtils.GetCellAsDouble(APSIM, 1, 13);
			MySoil.SoilCN = GridUtils.GetCellAsDouble(APSIM, 1, 14);
			MySoil.EnrACoeff = GridUtils.GetCellAsDouble(APSIM, 1, 16);
			MySoil.EnrBCoeff = GridUtils.GetCellAsDouble(APSIM, 1, 17);
			}


		// --------------------------
		// Populate the phosphorus grid.
		// --------------------------
		private void PopulatePhosphorusGrid()
			{
			UserChange = false;
			Phosphorus.ClearRange(0, 0, Phosphorus.RowCount, Phosphorus.ColumnCount, true);
			GridUtils.SetColumnAsStrings(Phosphorus, 0, MySoil.DepthStrings);
			GridUtils.SetColumnAsDoubles(Phosphorus, 1, MySoil.LabileP);
			GridUtils.SetColumnAsDoubles(Phosphorus, 2, MySoil.BandedP);
			GridUtils.SetColumnAsDoubles(Phosphorus, 3, MySoil.RockP);
			GridUtils.SetColumnAsDoubles(Phosphorus, 4, MySoil.Sorption);

			int NumLayers = GridUtils.FindFirstBlankCell(Phosphorus, 0);
			Phosphorus.RowCount = NumLayers + 4;
			int FirstStaticRow = NumLayers + 1;
			Phosphorus.Cells[FirstStaticRow,0].Value = "Residue CP:";
			GridUtils.SetCellAsDouble(Phosphorus, 1, FirstStaticRow, MySoil.ResidueCP);

			Phosphorus.Cells[FirstStaticRow+1,0].Value = "Root CP:";
			GridUtils.SetCellAsDouble(Phosphorus, 1, FirstStaticRow+1, MySoil.RootCP);

			Phosphorus.Cells[FirstStaticRow+2,0].Value = "RateDissolRock:";
			GridUtils.SetCellAsDouble(Phosphorus, 1, FirstStaticRow+2, MySoil.RateDissolRock);
			UserChange = true;
			}


		// --------------------------
		// Save the phosphorus grid.
		// --------------------------
		private void SavePhosphorusGrid()
			{
			int NumLayers = GridUtils.FindFirstBlankCell(Phosphorus, 0);
			MySoil.LabileP  = GridUtils.GetColumnAsDoubles(Phosphorus, 1, NumLayers);
			MySoil.BandedP  = GridUtils.GetColumnAsDoubles(Phosphorus, 2, NumLayers);
			MySoil.RockP  = GridUtils.GetColumnAsDoubles(Phosphorus, 3, NumLayers);
			MySoil.Sorption  = GridUtils.GetColumnAsDoubles(Phosphorus, 4, NumLayers);

			int FirstStaticRow = NumLayers + 1;
			MySoil.ResidueCP = GridUtils.GetCellAsDouble(Phosphorus, 1, FirstStaticRow);
			MySoil.RootCP = GridUtils.GetCellAsDouble(Phosphorus, 1, FirstStaticRow+1);
			MySoil.RateDissolRock = GridUtils.GetCellAsDouble(Phosphorus, 1, FirstStaticRow+2);
			}


		// ------------------------
		// Refresh all PAWC columns
		// ------------------------
		private void RefreshPAWCColumns()
			{
			for (int col = 0; col != Water.ColumnCount; col++)
				if (Water.ColumnHeader.Cells[1,col].Text == "PAWC")
					RefreshPAWCColumn(col);
			}

		// ------------------------------------------
		// Refresh the summary row on the water grid.
		// ------------------------------------------
		private void RefreshPAWCColumn(int ColumnIndex)
			{
			string CropName = Water.ColumnHeader.Cells[0, ColumnIndex-1].Text.Replace("Predicted ", "");
			double[] paw = MySoil.PAWC(CropName);
			GridUtils.SetColumnAsDoubles(Water, ColumnIndex, paw);
			Water.Columns[ColumnIndex].Locked = true;
			AddSummaryRow();
			}


		// ------------------------------------------
		// Add a summary row to the water table.
		// ------------------------------------------
		private void AddSummaryRow()
			{
			int NumDepths = GridUtils.FindFirstBlankCell(Water, 0);
			if (NumDepths > 0)
				{
				if (Water.Cells[NumDepths-1, 0].Text == "Totals")
					NumDepths--;
				Water.RowCount = NumDepths + 2;
	            
				// set the blank row.
				Water.Cells[Water.RowCount-2, 0, Water.RowCount-2, Water.ColumnCount-1].Formula = "";
				Water.Cells[Water.RowCount-2, 0, Water.RowCount-2, Water.ColumnCount-1].Text = "";
				Water.Rows[Water.RowCount-2].BackColor = Color.White;

				// set the summary row.
				int SummaryRow = Water.RowCount-1;
				Water.Cells[SummaryRow, 0].Text = "Totals";
				Water.Rows[SummaryRow].Locked = true;
				for (int col = NUMBER_OF_STATIC_COLS + 1; col < Water.ColumnCount; col += 4)
					{
					int ColForFormula = col + 1;
					int EndRowForFormula = SummaryRow - 1;
					string SumFormula = "SUM(R1C" + ColForFormula.ToString() + ":R" + EndRowForFormula.ToString() + "C" + ColForFormula.ToString() + ")";
					Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
					Water.Cells[SummaryRow, col].Formula = SumFormula;
					}
				Water.Rows[SummaryRow].BackColor = Color.Yellow;
				}
			}

		// --------------------------------
		// User wants to add a new crop.
		// --------------------------------
		public void AddCropMenuItem_Click(object sender, System.EventArgs e)
			{
			ApsoilController Apsoil = Controller as ApsoilController;
			Apsoil.AddCrop();
			}


		// --------------------------------
		// User wants to delete a new crop.
		// --------------------------------
		public void DeleteCropMenuItem_Click(object sender, System.EventArgs e)
			{
			ApsoilController Apsoil = Controller as ApsoilController;
			Apsoil.DeleteCrop();
			}

		// --------------------------------
		// user wants to reorder crops.
		// --------------------------------
		public void ReorderCropsMenuItem_Click(object sender, System.EventArgs e)
			{
			ApsoilController Apsoil = Controller as ApsoilController;
			Apsoil.ReorderCrops();
			}

		// --------------------------------
		// Active sheet has changed.
		// --------------------------------
		private void Grid_ActiveSheetChanged(object sender, System.EventArgs e)
			{
			if (Grid.ActiveSheet.SheetName == "Water") 
				Grid.ContextMenu = WaterMenu;
			else
				Grid.ContextMenu = null;
			}



		// -----------------------------------
		// User has changed a water grid cell
		// -----------------------------------
		private void Water_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				SaveWaterGridColumn(e.Column);
				UserChange = true;
				}
			}

		// -----------------------------------------
		// User has changed a general grid cell.
		// -----------------------------------------
		private void General_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				try
					{
					SaveGeneralGrid();
					if (e.Row == 3)
						PopulateWaterGrid();
					}
				catch (Exception err)
					{
					MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
					}

				UserChange = true;
				}
			}

		private void SoilProfile_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				SaveProfileGrid(e.Column);
				UserChange = true;
				}
			}

		private void APSIM_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				SaveAPSIMGrid();
				UserChange = true;
				}
			}

		private void Phosphorus_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				SavePhosphorusGrid();
				UserChange = true;
				}
			}

		
		private void SetupCellNotes()
			{
			foreach (Soil.Note note in MySoil.GetNotes())
				{
				if (note.GridName == "General")
					{
					General.Cells[note.Row, note.Col].Note = note.Text;
					General.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "Water")
					{
					Water.Cells[note.Row, note.Col].Note = note.Text;
					Water.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "Soil profile")
					{	
					SoilProfile.Cells[note.Row, note.Col].Note = note.Text;
					SoilProfile.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "APSIM")
					{
					APSIM.Cells[note.Row, note.Col].Note = note.Text;
					APSIM.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}

				else if (note.GridName == "Phosphorus")
					{
					Phosphorus.Cells[note.Row, note.Col].Note = note.Text;
					Phosphorus.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				}
			}

		private void AddNote()
			{
			string NoteText = InputDialog.InputBox("Enter text:", "New note", "");
			if (NoteText != "")
				{
				FarPoint.Win.Spread.Model.CellRange Range = Grid.ActiveSheet.GetSelection(0);
				for (int Row = Range.Row; Row < Range.Row + Range.RowCount; Row++)
					for (int Col = Range.Column; Col < Range.Column + Range.ColumnCount; Col++)
						MySoil.AddNote(Grid.ActiveSheet.SheetName, Col, Row, NoteText);
				Refresh();
				}
			}

		private void DeleteNote()
			{
			if (Grid.ActiveSheet.SelectionCount > 0)
				{
				FarPoint.Win.Spread.Model.CellRange Range = Grid.ActiveSheet.GetSelection(0);
				for (int Row = Range.Row; Row < Range.Row + Range.RowCount; Row++)
					for (int Col = Range.Column; Col < Range.Column + Range.ColumnCount; Col++)
						MySoil.DeleteNote(Grid.ActiveSheet.SheetName, Col, Row);
				Grid.ActiveSheet.Cells[Range.Row, Range.Column, Range.Row+Range.RowCount-1, Range.Column+Range.ColumnCount-1].Note = "";
				Grid.ActiveSheet.Cells[Range.Row, Range.Column, Range.Row+Range.RowCount-1, Range.Column+Range.ColumnCount-1].BackColor = Color.White;
				}
			}

		private void PrintClick(object sender, System.EventArgs e)
			{
			Print();
			}

		private int CurrentStartCol;
		private int SlotHeight;
		private int NumSlots = 3;
		private int NumFixedCols = 6;
		private int NumFreeColsToPrint = 8;
		private int MarginBetweenSlots = 0;
		private bool Previewing;
		private void Print()
			{
			Previewing = true;
			PrintPreviewDialog.ShowDialog();
			}

		private void printDocument1_BeginPrint(object sender, System.Drawing.Printing.PrintEventArgs e)
			{
			e.Cancel = (!Previewing && printDialog1.ShowDialog() != DialogResult.OK);

			CurrentStartCol = NumFixedCols;
			SlotHeight = 0;
			Previewing = false;
			}

		private void printDocument1_PrintPage(object sender, System.Drawing.Printing.PrintPageEventArgs e)
			{
			if (SlotHeight == 0)
				SlotHeight = CalcSlotHeight(e);

			e.Graphics.Clip = new Region(e.MarginBounds);
			e.HasMorePages = true;
			for (int slot = 0; slot < NumSlots && e.HasMorePages; slot++)
				{
				Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top + slot * (SlotHeight + MarginBetweenSlots),
											e.MarginBounds.Right-e.MarginBounds.Left,
											SlotHeight);
				if (CurrentStartCol >= Water.ColumnCount)
					{
					// print the graph.
					PrintForm.PrintControl(e.Graphics, r, WaterChartControl.WaterChart, 1.0F);
					
					e.HasMorePages = false;
					}
				else
					{
					PrintForm.PrintControl(e.Graphics, r, Grid, 1.0F);
					CurrentStartCol += NumFreeColsToPrint;
					}
				}
			e.Graphics.ResetClip();
			}

		private int CalcSlotHeight(System.Drawing.Printing.PrintPageEventArgs e)
			{
			Water.PrintInfo.ColStart = 0;
			Water.PrintInfo.ColEnd = 6;
			Water.PrintInfo.RowStart = 0;
			Water.PrintInfo.RowEnd = Water.RowCount-1;
			Water.PrintInfo.PrintType = PrintType.CellRange;
			Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top,
										e.MarginBounds.Width, 200);
			do 
				{
				r.Height = r.Height + 10;
				}
			while (Grid.GetOwnerPrintPageCount(e.Graphics, r, 1) > 1);
			return r.Height;			
			}

		private void printForm1_PreDraw(object sender, TMGDevelopment.Windows.Forms.PreDrawEventArgs e)
			{
			if (e.Control == Grid)
				{
				Water.PrintInfo.ColStart = CurrentStartCol;
				Water.PrintInfo.ColEnd = CurrentStartCol + NumFreeColsToPrint - 1;
				Water.PrintInfo.PrintType = PrintType.CellRange;
				Grid.OwnerPrintDraw(e.Graphics, e.Bounds, 1, 1);
				e.OwnerDrawn = true;
				}
			}

		private void printDocument1_QueryPageSettings(object sender, System.Drawing.Printing.QueryPageSettingsEventArgs e)
			{
			e.PageSettings.Margins.Left = 50;
			e.PageSettings.Margins.Top = 50;
			e.PageSettings.Margins.Right = 50;
			e.PageSettings.Margins.Bottom = 50;
			}

		private void VolGravChecked(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
				SaveWaterGrid();
				PopulateWaterGrid();
				UserChange = true;
				}
			}

		private void AddCropMenuItem_Click_1(object sender, System.EventArgs e)
			{
			ApsoilController Apsoil = Controller as ApsoilController;
			Apsoil.AddCrop();
			}

		private void DeleteCropMenuItem_Click_1(object sender, System.EventArgs e)
			{
			ApsoilController Apsoil = Controller as ApsoilController;
			Apsoil.DeleteCrop();
			}

		private void CheckSoilMenuItem_Click(object sender, System.EventArgs e)
			{
			string msg = MySoil.CheckForErrors();
			if (msg == "")
				MessageBox.Show("No errors encountered", "For your information", 
								MessageBoxButtons.OK, MessageBoxIcon.Information);
			else
				MessageBox.Show(msg, "Errors", MessageBoxButtons.OK, MessageBoxIcon.Error);
			}


		}
	}

