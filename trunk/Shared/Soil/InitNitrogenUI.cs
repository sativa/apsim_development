using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using CSGeneral;
using VBGeneral;


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
        public Steema.TeeChart.TChart NitrogenChart;
        private Steema.TeeChart.Styles.Line No3KgHaLine;
        private Steema.TeeChart.Styles.Line Nh4PpmLine;
        private Steema.TeeChart.Styles.Line Nh4KgHaLine;
        private Steema.TeeChart.Styles.Line No3PpmLine;
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
        FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType1 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType2 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType3 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType4 = new FarPoint.Win.Spread.CellType.NumberCellType();
        this.panel1 = new System.Windows.Forms.Panel();
        this.NitrogenChart = new Steema.TeeChart.TChart();
        this.No3KgHaLine = new Steema.TeeChart.Styles.Line();
        this.Nh4KgHaLine = new Steema.TeeChart.Styles.Line();
        this.No3PpmLine = new Steema.TeeChart.Styles.Line();
        this.Nh4PpmLine = new Steema.TeeChart.Styles.Line();
        this.Grid = new FarPoint.Win.Spread.FpSpread();
        this.NitrogenGrid = new FarPoint.Win.Spread.SheetView();
        this.panel1.SuspendLayout();
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).BeginInit();
        this.SuspendLayout();
        // 
        // panel1
        // 
        this.panel1.Controls.Add(this.NitrogenChart);
        this.panel1.Controls.Add(this.Grid);
        this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
        this.panel1.Location = new System.Drawing.Point(0, 40);
        this.panel1.Name = "panel1";
        this.panel1.Size = new System.Drawing.Size(908, 725);
        this.panel1.TabIndex = 36;
        // 
        // NitrogenChart
        // 
        // 
        // 
        // 
        this.NitrogenChart.Aspect.ElevationFloat = 345;
        this.NitrogenChart.Aspect.RotationFloat = 345;
        this.NitrogenChart.Aspect.View3D = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Bottom.Grid.Visible = false;
        this.NitrogenChart.Axes.Bottom.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Depth.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.DepthTop.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.AutomaticMinimum = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.AxisPen.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Left.Grid.Visible = false;
        this.NitrogenChart.Axes.Left.Grid.ZPosition = 0;
        this.NitrogenChart.Axes.Left.Increment = 20;
        this.NitrogenChart.Axes.Left.Inverted = true;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Labels.Shadow.Visible = false;
        this.NitrogenChart.Axes.Left.MaximumOffset = 2;
        this.NitrogenChart.Axes.Left.Minimum = 0;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.MinorTicks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Ticks.Length = 0;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.TicksInner.Length = 5;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Caption = "Depth (cm)";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Font.Shadow.Visible = false;
        this.NitrogenChart.Axes.Left.Title.Lines = new string[] {
        "Depth (cm)"};
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Right.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.AxisPen.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Top.Grid.Visible = false;
        this.NitrogenChart.Axes.Top.Grid.ZPosition = 0;
        this.NitrogenChart.Axes.Top.Increment = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Labels.Shadow.Visible = false;
        this.NitrogenChart.Axes.Top.MaximumOffset = 2;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.MinorTicks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Ticks.Length = 0;
        this.NitrogenChart.Axes.Top.Ticks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.TicksInner.Length = 5;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Caption = "Nitrogen (kg/ha)";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Font.Shadow.Visible = false;
        this.NitrogenChart.Axes.Top.Title.Lines = new string[] {
        "Nitrogen (kg/ha)"};
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Shadow.Visible = false;
        this.NitrogenChart.Cursor = System.Windows.Forms.Cursors.Default;
        this.NitrogenChart.Dock = System.Windows.Forms.DockStyle.Fill;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Footer.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Footer.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Header.Font.Shadow.Visible = false;
        this.NitrogenChart.Header.Lines = new string[] {
        ""};
        // 
        // 
        // 
        this.NitrogenChart.Header.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.CheckBoxes = true;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Legend.Font.Shadow.Visible = false;
        this.NitrogenChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Pen.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Font.Bold = true;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Pen.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Shadow.Visible = false;
        this.NitrogenChart.Location = new System.Drawing.Point(319, 0);
        this.NitrogenChart.Name = "NitrogenChart";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
        // 
        // 
        // 
        this.NitrogenChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        // 
        // 
        // 
        this.NitrogenChart.Panel.ImageBevel.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Panel.Shadow.Visible = false;
        this.NitrogenChart.Series.Add(this.No3KgHaLine);
        this.NitrogenChart.Series.Add(this.Nh4KgHaLine);
        this.NitrogenChart.Series.Add(this.No3PpmLine);
        this.NitrogenChart.Series.Add(this.Nh4PpmLine);
        this.NitrogenChart.Size = new System.Drawing.Size(589, 725);
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.SubFooter.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.SubFooter.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.SubHeader.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.SubHeader.Shadow.Visible = false;
        this.NitrogenChart.TabIndex = 38;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Walls.Back.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Back.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Bottom.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Bottom.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Left.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Left.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Right.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Right.Shadow.Visible = false;
        this.NitrogenChart.Walls.Visible = false;
        this.NitrogenChart.ClickLegend += new System.Windows.Forms.MouseEventHandler(this.NitrogenChart_ClickLegend);
        // 
        // No3KgHaLine
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.No3KgHaLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.No3KgHaLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.No3KgHaLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.No3KgHaLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.No3KgHaLine.Marks.Callout.Distance = 0;
        this.No3KgHaLine.Marks.Callout.Draw3D = false;
        this.No3KgHaLine.Marks.Callout.Length = 10;
        this.No3KgHaLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.No3KgHaLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.No3KgHaLine.Title = "no3 (kg/ha)";
        // 
        // 
        // 
        this.No3KgHaLine.XValues.DataMember = "X";
        this.No3KgHaLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.No3KgHaLine.YValues.DataMember = "Y";
        // 
        // Nh4KgHaLine
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.Nh4KgHaLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.Nh4KgHaLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.Nh4KgHaLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.Nh4KgHaLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.Nh4KgHaLine.Marks.Callout.Distance = 0;
        this.Nh4KgHaLine.Marks.Callout.Draw3D = false;
        this.Nh4KgHaLine.Marks.Callout.Length = 10;
        this.Nh4KgHaLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.Nh4KgHaLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.Nh4KgHaLine.Title = "nh4 (kg/ha)";
        // 
        // 
        // 
        this.Nh4KgHaLine.XValues.DataMember = "X";
        this.Nh4KgHaLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.Nh4KgHaLine.YValues.DataMember = "Y";
        // 
        // No3PpmLine
        // 
        // 
        // 
        // 
        this.No3PpmLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.No3PpmLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.No3PpmLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.No3PpmLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.No3PpmLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.No3PpmLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.No3PpmLine.Marks.Callout.Distance = 0;
        this.No3PpmLine.Marks.Callout.Draw3D = false;
        this.No3PpmLine.Marks.Callout.Length = 10;
        this.No3PpmLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.No3PpmLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.No3PpmLine.Title = "no3 (ppm)";
        this.No3PpmLine.Visible = false;
        // 
        // 
        // 
        this.No3PpmLine.XValues.DataMember = "X";
        this.No3PpmLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.No3PpmLine.YValues.DataMember = "Y";
        // 
        // Nh4PpmLine
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.Nh4PpmLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.Nh4PpmLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.Nh4PpmLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.Nh4PpmLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.Nh4PpmLine.Marks.Callout.Distance = 0;
        this.Nh4PpmLine.Marks.Callout.Draw3D = false;
        this.Nh4PpmLine.Marks.Callout.Length = 10;
        this.Nh4PpmLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.Nh4PpmLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.Nh4PpmLine.Title = "nh4 (ppm)";
        this.Nh4PpmLine.Visible = false;
        // 
        // 
        // 
        this.Nh4PpmLine.XValues.DataMember = "X";
        this.Nh4PpmLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.Nh4PpmLine.YValues.DataMember = "Y";
        // 
        // Grid
        // 
        this.Grid.AccessibleDescription = "";
        this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
        this.Grid.Dock = System.Windows.Forms.DockStyle.Left;
        this.Grid.EditModeReplace = true;
        this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        this.Grid.Location = new System.Drawing.Point(0, 0);
        this.Grid.Name = "Grid";
        this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.NitrogenGrid});
        this.Grid.Size = new System.Drawing.Size(319, 725);
        this.Grid.TabIndex = 36;
        tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
        tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
        this.Grid.TextTipAppearance = tipAppearance1;
        this.Grid.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        // 
        // NitrogenGrid
        // 
        this.NitrogenGrid.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.NitrogenGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.NitrogenGrid.ColumnCount = 5;
        this.NitrogenGrid.ColumnHeader.RowCount = 2;
        this.NitrogenGrid.AutoUpdateNotes = true;
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 1).Value = "NO3";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 2).Value = "NH4";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 3).Value = "NO3";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 4).Value = "NH4";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 1).Value = "(kg/ha)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 2).Value = "(kg/ha)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 3).Value = "(ppm)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 4).Value = "(ppm)";
        this.NitrogenGrid.Columns.Get(0).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
        this.NitrogenGrid.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(0).Label = "(cm)";
        this.NitrogenGrid.Columns.Get(0).Locked = true;
        numberCellType1.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(1).CellType = numberCellType1;
        this.NitrogenGrid.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(1).Label = "(kg/ha)";
        numberCellType2.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(2).CellType = numberCellType2;
        this.NitrogenGrid.Columns.Get(2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(2).Label = "(kg/ha)";
        this.NitrogenGrid.Columns.Get(3).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
        numberCellType3.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(3).CellType = numberCellType3;
        this.NitrogenGrid.Columns.Get(3).Label = "(ppm)";
        this.NitrogenGrid.Columns.Get(4).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
        numberCellType4.DecimalPlaces = 3;
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
        this.Controls.Add(this.panel1);
        this.Name = "InitNitrogenUI";
        this.Size = new System.Drawing.Size(908, 765);
        this.Controls.SetChildIndex(this.panel1, 0);
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
            double[] CumThicknessMidPoints = MathUtility.Divide_Value(SoilData.CumThicknessMidPoints, 10);
           
            No3KgHaLine.Add(InitialNitrogen.NO3KgHa, CumThicknessMidPoints);
            Nh4KgHaLine.Add(InitialNitrogen.NH4KgHa, CumThicknessMidPoints);
            No3PpmLine.Add(InitialNitrogen.NO3, CumThicknessMidPoints);
            Nh4PpmLine.Add(InitialNitrogen.NH4, CumThicknessMidPoints);
			}

        private void NitrogenChart_ClickLegend(object sender, MouseEventArgs e)
            // User has clicked legend
            {
            int index = NitrogenChart.Legend.Clicked(e.X, e.Y);

            if (index == 0 || index == 1)
                {
                NitrogenChart.Axes.Top.Title.Text = "Nitrogen (kg/ha)";
                No3KgHaLine.Active = true;
                Nh4KgHaLine.Active = true;
                No3PpmLine.Active = false;
                Nh4PpmLine.Active = false;
                }
            else
                {
                NitrogenChart.Axes.Top.Title.Text = "Nitrogen (ppm)";
                No3KgHaLine.Active = false;
                Nh4KgHaLine.Active = false;
                No3PpmLine.Active = true;
                Nh4PpmLine.Active = true;
                }

            }


	}
}

