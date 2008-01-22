namespace Graph
    {
    partial class SOIUI
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
            FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType1 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.fpSpread1 = new FarPoint.Win.Spread.FpSpread();
            this.PhaseList = new FarPoint.Win.Spread.SheetView();
            this.label3 = new System.Windows.Forms.Label();
            this.MonthDropDown = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.FileNameEdit = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.GroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.PhaseList)).BeginInit();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.fpSpread1);
            this.GroupBox.Controls.Add(this.label3);
            this.GroupBox.Controls.Add(this.MonthDropDown);
            this.GroupBox.Controls.Add(this.label2);
            this.GroupBox.Controls.Add(this.FileNameEdit);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(127, 245);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // fpSpread1
            // 
            this.fpSpread1.AccessibleDescription = "fpSpread1, Sheet1, Row 0, Column 0, ";
            this.fpSpread1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fpSpread1.ColumnSplitBoxPolicy = FarPoint.Win.Spread.SplitBoxPolicy.Never;
            this.fpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.Never;
            this.fpSpread1.Location = new System.Drawing.Point(8, 140);
            this.fpSpread1.Name = "fpSpread1";
            this.fpSpread1.RowSplitBoxPolicy = FarPoint.Win.Spread.SplitBoxPolicy.Never;
            this.fpSpread1.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.PhaseList});
            this.fpSpread1.Size = new System.Drawing.Size(111, 94);
            this.fpSpread1.TabIndex = 6;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.fpSpread1.TextTipAppearance = tipAppearance1;
            // 
            // PhaseList
            // 
            this.PhaseList.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.PhaseList.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.PhaseList.ColumnCount = 1;
            this.PhaseList.RowHeader.ColumnCount = 0;
            this.PhaseList.AutoUpdateNotes = true;
            this.PhaseList.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
            comboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType1.Editable = true;
            comboBoxCellType1.Items = new string[] {
        "Negative",
        "Positive",
        "Falling",
        "Rising",
        "Zero",
        "AllOtherYears"};
            this.PhaseList.Columns.Get(0).CellType = comboBoxCellType1;
            this.PhaseList.Columns.Get(0).Locked = false;
            this.PhaseList.Columns.Get(0).Width = 89F;
            this.PhaseList.RowHeader.Columns.Default.Resizable = false;
            this.PhaseList.SheetName = "Sheet1";
            this.PhaseList.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(3, 124);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(40, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "Phase:";
            // 
            // MonthDropDown
            // 
            this.MonthDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.MonthDropDown.FormattingEnabled = true;
            this.MonthDropDown.Items.AddRange(new object[] {
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"});
            this.MonthDropDown.Location = new System.Drawing.Point(6, 91);
            this.MonthDropDown.Name = "MonthDropDown";
            this.MonthDropDown.Size = new System.Drawing.Size(114, 21);
            this.MonthDropDown.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 75);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(40, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Month:";
            // 
            // FileNameEdit
            // 
            this.FileNameEdit.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FileNameEdit.Location = new System.Drawing.Point(6, 44);
            this.FileNameEdit.Name = "FileNameEdit";
            this.FileNameEdit.Size = new System.Drawing.Size(114, 20);
            this.FileNameEdit.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(3, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(82, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Phase filename:";
            // 
            // SOIUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "SOIUI";
            this.Size = new System.Drawing.Size(127, 263);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.PhaseList)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.TextBox FileNameEdit;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox MonthDropDown;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private FarPoint.Win.Spread.FpSpread fpSpread1;
        private FarPoint.Win.Spread.SheetView PhaseList;
        }
    }
