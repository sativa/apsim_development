namespace CSUserInterface
    {
    partial class WaterUI
        {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
            {
            if (disposing && (components != null))
                {
                components.Dispose();
                }
            base.Dispose(disposing);
            }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
            {
            this.components = new System.ComponentModel.Container();
            FarPoint.Win.Spread.TipAppearance tipAppearance3 = new FarPoint.Win.Spread.TipAppearance();
            FarPoint.Win.BevelBorder bevelBorder5 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
            FarPoint.Win.BevelBorder bevelBorder6 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
            this.Grid = new FarPoint.Win.Spread.FpSpread();
            this.Water = new FarPoint.Win.Spread.SheetView();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.WaterChartControl = new CSUserInterface.WaterChartControl();
            this.PrintForm = new TMGDevelopment.Windows.Forms.PrintForm(this.components);
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.Water)).BeginInit();
            this.SuspendLayout();
            // 
            // Grid
            // 
            this.Grid.AccessibleDescription = "Grid, Water, Row 0, Column 0, ";
            this.Grid.AllowDragDrop = true;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Top;
            this.Grid.EditModeReplace = true;
            this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Grid.Location = new System.Drawing.Point(0, 40);
            this.Grid.Name = "Grid";
            this.Grid.SelectionBlockOptions = ((FarPoint.Win.Spread.SelectionBlockOptions)(((FarPoint.Win.Spread.SelectionBlockOptions.Cells | FarPoint.Win.Spread.SelectionBlockOptions.Rows)
                        | FarPoint.Win.Spread.SelectionBlockOptions.Sheet)));
            this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.Water});
            this.Grid.Size = new System.Drawing.Size(655, 292);
            this.Grid.TabIndex = 13;
            this.Grid.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
            this.Grid.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never;
            this.Grid.TabStripRatio = 0.512295081967213;
            tipAppearance3.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance3.ForeColor = System.Drawing.SystemColors.InfoText;
            this.Grid.TextTipAppearance = tipAppearance3;
            this.Grid.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating;
            this.Grid.SetViewportLeftColumn(0, 6);
            this.Grid.SetActiveViewport(0, -1);
            // 
            // Water
            // 
            this.Water.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.Water.ColumnCount = 7;
            this.Water.ColumnHeader.RowCount = 3;
            this.Water.RowCount = 100;
            this.Water.AutoUpdateNotes = true;
            this.Water.ColumnHeader.Cells.Get(0, 0).Border = bevelBorder5;
            this.Water.ColumnHeader.Cells.Get(0, 0).ColumnSpan = 7;
            this.Water.ColumnHeader.Cells.Get(0, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.Water.ColumnHeader.Cells.Get(0, 0).Value = "Soil properties";
            this.Water.ColumnHeader.Cells.Get(0, 6).VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.General;
            this.Water.ColumnHeader.Cells.Get(1, 0).Border = bevelBorder6;
            this.Water.ColumnHeader.Cells.Get(1, 0).Value = "Depth";
            this.Water.ColumnHeader.Cells.Get(1, 1).Value = "BD";
            this.Water.ColumnHeader.Cells.Get(1, 2).Value = "% Rocks";
            this.Water.ColumnHeader.Cells.Get(1, 3).Value = "SAT";
            this.Water.ColumnHeader.Cells.Get(1, 4).Value = " DUL";
            this.Water.ColumnHeader.Cells.Get(1, 5).Value = "AirDry";
            this.Water.ColumnHeader.Cells.Get(1, 6).Value = "LL15";
            this.Water.ColumnHeader.Cells.Get(2, 0).Value = "(cm)";
            this.Water.ColumnHeader.Cells.Get(2, 1).Value = " (g/cc)";
            this.Water.ColumnHeader.Cells.Get(2, 2).Value = "(%)";
            this.Water.ColumnHeader.Cells.Get(2, 3).Value = "(mm/mm)";
            this.Water.ColumnHeader.Cells.Get(2, 4).Value = "(mm/mm)";
            this.Water.ColumnHeader.Cells.Get(2, 5).Value = "(mm/mm)";
            this.Water.ColumnHeader.Cells.Get(2, 6).Value = "(mm/mm)";
            this.Water.Columns.Get(0).Label = "(cm)";
            this.Water.Columns.Get(0).Width = 52F;
            this.Water.Columns.Get(1).Label = " (g/cc)";
            this.Water.Columns.Get(1).Width = 45F;
            this.Water.Columns.Get(2).Label = "(%)";
            this.Water.Columns.Get(2).Width = 52F;
            this.Water.Columns.Get(3).Label = "(mm/mm)";
            this.Water.Columns.Get(3).Width = 55F;
            this.Water.Columns.Get(4).Label = "(mm/mm)";
            this.Water.Columns.Get(4).Width = 56F;
            this.Water.Columns.Get(5).Label = "(mm/mm)";
            this.Water.Columns.Get(5).Width = 56F;
            this.Water.Columns.Get(6).Label = "(mm/mm)";
            this.Water.Columns.Get(6).Width = 56F;
            this.Water.FrozenColumnCount = 6;
            this.Water.FrozenTrailingRowCount = 1;
            this.Water.RowHeader.Columns.Default.Resizable = false;
            this.Water.RowHeader.Visible = false;
            this.Water.SheetName = "Water";
            this.Water.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnWaterCellChanged);
            this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // splitter1
            // 
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter1.Location = new System.Drawing.Point(0, 332);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(655, 3);
            this.splitter1.TabIndex = 14;
            this.splitter1.TabStop = false;
            // 
            // WaterChartControl
            // 
            this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.WaterChartControl.LinkedSoil = null;
            this.WaterChartControl.Location = new System.Drawing.Point(0, 335);
            this.WaterChartControl.Name = "WaterChartControl";
            this.WaterChartControl.ShowSoilWaterLine = false;
            this.WaterChartControl.Size = new System.Drawing.Size(655, 252);
            this.WaterChartControl.TabIndex = 17;
            // 
            // PrintForm
            // 
            this.PrintForm.AutoFit = TMGDevelopment.Windows.Forms.PageElement.Body;
            this.PrintForm.BodyContainer = this;
            this.PrintForm.CenterStyle = TMGDevelopment.Windows.Forms.CenterStyle.None;
            this.PrintForm.PreDraw += new TMGDevelopment.Windows.Forms.PreDrawEventHandler(this.OnPreDraw);
            // 
            // WaterUI
            // 
            this.Controls.Add(this.WaterChartControl);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.Grid);
            this.Name = "WaterUI";
            this.Size = new System.Drawing.Size(655, 587);
            this.Controls.SetChildIndex(this.Grid, 0);
            this.Controls.SetChildIndex(this.splitter1, 0);
            this.Controls.SetChildIndex(this.WaterChartControl, 0);
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.Water)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private FarPoint.Win.Spread.FpSpread Grid;
        private FarPoint.Win.Spread.SheetView Water;
        private System.Windows.Forms.Splitter splitter1;
        private WaterChartControl WaterChartControl;
        private TMGDevelopment.Windows.Forms.PrintForm PrintForm;
        }
    }
