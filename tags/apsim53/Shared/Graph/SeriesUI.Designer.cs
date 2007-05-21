namespace Graph
    {
    partial class SeriesUI
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
            FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType1 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType2 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType3 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType4 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType5 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType1 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType2 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            this.Spreadsheet = new FarPoint.Win.Spread.FpSpread();
            this.Grid = new FarPoint.Win.Spread.SheetView();
            this.ColorDialog = new System.Windows.Forms.ColorDialog();
            this.PopupMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.DeleteSeriesMenu = new System.Windows.Forms.ToolStripMenuItem();
            ((System.ComponentModel.ISupportInitialize)(this.Spreadsheet)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.PopupMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // Spreadsheet
            // 
            this.Spreadsheet.AccessibleDescription = "Spreadsheet, Sheet1, Row 0, Column 0, ";
            this.Spreadsheet.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Spreadsheet.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Spreadsheet.Location = new System.Drawing.Point(0, 40);
            this.Spreadsheet.Name = "Spreadsheet";
            this.Spreadsheet.ScrollBarShowMax = false;
            this.Spreadsheet.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.Grid});
            this.Spreadsheet.Size = new System.Drawing.Size(752, 501);
            this.Spreadsheet.TabIndex = 7;
            this.Spreadsheet.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
            this.Spreadsheet.TabStripPlacement = FarPoint.Win.Spread.TabStripPlacement.Bottom;
            this.Spreadsheet.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Always;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.Spreadsheet.TextTipAppearance = tipAppearance1;
            this.Spreadsheet.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Spreadsheet.CellClick += new FarPoint.Win.Spread.CellClickEventHandler(this.Spreadsheet_CellClick);
            // 
            // Grid
            // 
            this.Grid.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.Grid.ColumnCount = 8;
            this.Grid.RowCount = 20;
            this.Grid.AutoUpdateNotes = true;
            this.Grid.Cells.Get(0, 3).Value = "Line";
            this.Grid.Cells.Get(0, 4).Value = "None";
            this.Grid.Cells.Get(0, 7).BackColor = System.Drawing.Color.Red;
            this.Grid.Cells.Get(1, 3).Value = "Line";
            this.Grid.Cells.Get(1, 4).Value = "None";
            this.Grid.Cells.Get(1, 7).BackColor = System.Drawing.Color.Green;
            this.Grid.Cells.Get(2, 3).Value = "Line";
            this.Grid.Cells.Get(2, 4).Value = "None";
            this.Grid.Cells.Get(2, 7).BackColor = System.Drawing.Color.Blue;
            this.Grid.Cells.Get(3, 3).Value = "Line";
            this.Grid.Cells.Get(3, 4).Value = "None";
            this.Grid.Cells.Get(3, 7).BackColor = System.Drawing.Color.Magenta;
            this.Grid.Cells.Get(4, 3).Value = "Line";
            this.Grid.Cells.Get(4, 4).Value = "None";
            this.Grid.Cells.Get(4, 7).BackColor = System.Drawing.Color.SlateGray;
            this.Grid.Cells.Get(5, 3).Value = "Line";
            this.Grid.Cells.Get(5, 4).Value = "None";
            this.Grid.Cells.Get(5, 7).BackColor = System.Drawing.Color.LightSeaGreen;
            this.Grid.Cells.Get(6, 3).Value = "Line";
            this.Grid.Cells.Get(6, 4).Value = "None";
            this.Grid.Cells.Get(6, 7).BackColor = System.Drawing.Color.LawnGreen;
            this.Grid.Cells.Get(7, 3).Value = "Line";
            this.Grid.Cells.Get(7, 4).Value = "None";
            this.Grid.Cells.Get(7, 7).BackColor = System.Drawing.Color.Yellow;
            this.Grid.Cells.Get(8, 3).Value = "Line";
            this.Grid.Cells.Get(8, 4).Value = "None";
            this.Grid.Cells.Get(8, 7).BackColor = System.Drawing.Color.Orange;
            this.Grid.Cells.Get(9, 3).Value = "Line";
            this.Grid.Cells.Get(9, 4).Value = "None";
            this.Grid.Cells.Get(9, 7).BackColor = System.Drawing.Color.Chocolate;
            this.Grid.Cells.Get(10, 3).Value = "Line";
            this.Grid.Cells.Get(10, 4).Value = "None";
            this.Grid.Cells.Get(10, 7).BackColor = System.Drawing.Color.Silver;
            this.Grid.Cells.Get(11, 3).Value = "Line";
            this.Grid.Cells.Get(11, 4).Value = "None";
            this.Grid.Cells.Get(11, 7).BackColor = System.Drawing.Color.Black;
            this.Grid.Cells.Get(12, 3).Value = "Line";
            this.Grid.Cells.Get(12, 4).Value = "None";
            this.Grid.Cells.Get(12, 7).BackColor = System.Drawing.Color.HotPink;
            this.Grid.Cells.Get(13, 3).Value = "Line";
            this.Grid.Cells.Get(13, 4).Value = "None";
            this.Grid.Cells.Get(13, 7).BackColor = System.Drawing.Color.DarkViolet;
            this.Grid.Cells.Get(14, 3).Value = "Line";
            this.Grid.Cells.Get(14, 4).Value = "None";
            this.Grid.Cells.Get(14, 7).BackColor = System.Drawing.Color.DarkBlue;
            this.Grid.Cells.Get(15, 3).Value = "Line";
            this.Grid.Cells.Get(15, 4).Value = "None";
            this.Grid.Cells.Get(15, 7).BackColor = System.Drawing.Color.LightSteelBlue;
            this.Grid.Cells.Get(16, 3).Value = "Line";
            this.Grid.Cells.Get(16, 4).Value = "None";
            this.Grid.Cells.Get(16, 7).BackColor = System.Drawing.Color.DeepSkyBlue;
            this.Grid.Cells.Get(17, 3).Value = "Line";
            this.Grid.Cells.Get(17, 4).Value = "None";
            this.Grid.Cells.Get(17, 7).BackColor = System.Drawing.Color.MediumAquamarine;
            this.Grid.Cells.Get(18, 3).Value = "Line";
            this.Grid.Cells.Get(18, 4).Value = "None";
            this.Grid.Cells.Get(18, 7).BackColor = System.Drawing.Color.DarkOliveGreen;
            this.Grid.Cells.Get(19, 3).Value = "Line";
            this.Grid.Cells.Get(19, 4).Value = "None";
            this.Grid.Cells.Get(19, 7).BackColor = System.Drawing.Color.DarkKhaki;
            this.Grid.ColumnHeader.Cells.Get(0, 0).Value = "DataSource";
            this.Grid.ColumnHeader.Cells.Get(0, 1).Value = "X";
            this.Grid.ColumnHeader.Cells.Get(0, 2).Value = "Y";
            this.Grid.ColumnHeader.Cells.Get(0, 3).Value = "Type";
            this.Grid.ColumnHeader.Cells.Get(0, 4).Value = "Point";
            this.Grid.ColumnHeader.Cells.Get(0, 5).Value = "X (top)";
            this.Grid.ColumnHeader.Cells.Get(0, 6).Value = "Y (right)";
            this.Grid.ColumnHeader.Cells.Get(0, 7).Value = "Colour";
            comboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType1.MaxDrop = 20;
            this.Grid.Columns.Get(0).CellType = comboBoxCellType1;
            this.Grid.Columns.Get(0).Label = "DataSource";
            this.Grid.Columns.Get(0).Width = 182F;
            comboBoxCellType2.AutoSearch = FarPoint.Win.AutoSearch.MultipleCharacter;
            comboBoxCellType2.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType2.MaxDrop = 20;
            this.Grid.Columns.Get(1).CellType = comboBoxCellType2;
            this.Grid.Columns.Get(1).Label = "X";
            this.Grid.Columns.Get(1).Width = 111F;
            comboBoxCellType3.AutoSearch = FarPoint.Win.AutoSearch.MultipleCharacter;
            comboBoxCellType3.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType3.MaxDrop = 20;
            this.Grid.Columns.Get(2).CellType = comboBoxCellType3;
            this.Grid.Columns.Get(2).Label = "Y";
            this.Grid.Columns.Get(2).Width = 111F;
            comboBoxCellType4.AutoSearch = FarPoint.Win.AutoSearch.MultipleCharacter;
            comboBoxCellType4.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType4.Items = new string[] {
        "None",
        "Line",
        "Bar",
        "Dash",
        "Dashdot",
        "Dashdotdot",
        "Dot"};
            this.Grid.Columns.Get(3).CellType = comboBoxCellType4;
            this.Grid.Columns.Get(3).Label = "Type";
            this.Grid.Columns.Get(3).Width = 70F;
            comboBoxCellType5.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType5.Items = new string[] {
        "None",
        "Circle",
        "Triangle",
        "DownTriangle",
        "LeftTriangle",
        "RightTriangle",
        "Diamond",
        "Rectangle",
        "Cross",
        "DiagCross",
        "SmallDot"};
            comboBoxCellType5.MaxDrop = 20;
            this.Grid.Columns.Get(4).CellType = comboBoxCellType5;
            this.Grid.Columns.Get(4).Label = "Point";
            this.Grid.Columns.Get(4).Width = 76F;
            this.Grid.Columns.Get(5).CellType = checkBoxCellType1;
            this.Grid.Columns.Get(5).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.Grid.Columns.Get(5).Label = "X (top)";
            this.Grid.Columns.Get(5).VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Top;
            this.Grid.Columns.Get(5).Width = 44F;
            this.Grid.Columns.Get(6).CellType = checkBoxCellType2;
            this.Grid.Columns.Get(6).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.Grid.Columns.Get(6).Label = "Y (right)";
            this.Grid.Columns.Get(6).Width = 49F;
            this.Grid.Columns.Get(7).Label = "Colour";
            this.Grid.Columns.Get(7).Width = 42F;
            this.Grid.RowHeader.Columns.Default.Resizable = false;
            this.Grid.SheetName = "Sheet1";
            this.Grid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.Grid_CellChanged);
            this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // ColorDialog
            // 
            this.ColorDialog.AnyColor = true;
            this.ColorDialog.FullOpen = true;
            // 
            // PopupMenu
            // 
            this.PopupMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.DeleteSeriesMenu});
            this.PopupMenu.Name = "PopupMenu";
            this.PopupMenu.Size = new System.Drawing.Size(164, 26);
            // 
            // DeleteSeriesMenu
            // 
            this.DeleteSeriesMenu.Name = "DeleteSeriesMenu";
            this.DeleteSeriesMenu.Size = new System.Drawing.Size(163, 22);
            this.DeleteSeriesMenu.Text = "Delete series";
            this.DeleteSeriesMenu.Click += new System.EventHandler(this.DeleteSeriesMenu_Click);
            // 
            // SeriesUI
            // 
            this.ContextMenuStrip = this.PopupMenu;
            this.Controls.Add(this.Spreadsheet);
            this.Name = "SeriesUI";
            this.Size = new System.Drawing.Size(752, 541);
            this.Controls.SetChildIndex(this.Spreadsheet, 0);
            ((System.ComponentModel.ISupportInitialize)(this.Spreadsheet)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.PopupMenu.ResumeLayout(false);
            this.ResumeLayout(false);

            }

        #endregion

        private FarPoint.Win.Spread.FpSpread Spreadsheet;
        private FarPoint.Win.Spread.SheetView Grid;
        private System.Windows.Forms.ColorDialog ColorDialog;
        private System.Windows.Forms.ContextMenuStrip PopupMenu;
        private System.Windows.Forms.ToolStripMenuItem DeleteSeriesMenu;
        }
    }
