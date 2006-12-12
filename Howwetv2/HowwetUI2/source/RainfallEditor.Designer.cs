namespace APSRU.Howwet
    {
    partial class RainfallEditor
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(RainfallEditor));
            this.fpSpread1 = new FarPoint.Win.Spread.FpSpread();
            this.fpSpread1_Sheet1 = new FarPoint.Win.Spread.SheetView();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.yearSelectUpDown = new System.Windows.Forms.NumericUpDown();
            this.AddYearButton = new System.Windows.Forms.Button();
            this.closeButton = new System.Windows.Forms.Button();
            this.saveCloseButton = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread1_Sheet1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.yearSelectUpDown)).BeginInit();
            this.SuspendLayout();
            // 
            // fpSpread1
            // 
            this.fpSpread1.AccessibleDescription = "fpSpread1, Sheet1, Row 0, Column 0, ";
            this.fpSpread1.AllowUserZoom = false;
            this.fpSpread1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.Never;
            this.fpSpread1.Location = new System.Drawing.Point(12, 12);
            this.fpSpread1.Name = "fpSpread1";
            this.fpSpread1.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.fpSpread1_Sheet1});
            this.fpSpread1.Size = new System.Drawing.Size(759, 663);
            this.fpSpread1.TabIndex = 1;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.fpSpread1.TextTipAppearance = tipAppearance1;
            this.fpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.Never;
            this.fpSpread1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.fpSpread1_KeyDown);
            this.fpSpread1.Change += new FarPoint.Win.Spread.ChangeEventHandler(this.fpSpread1_Change);
            // 
            // fpSpread1_Sheet1
            // 
            this.fpSpread1_Sheet1.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.fpSpread1_Sheet1.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.fpSpread1_Sheet1.ColumnCount = 13;
            this.fpSpread1_Sheet1.RowCount = 32;
            this.fpSpread1_Sheet1.AutoUpdateNotes = true;
            this.fpSpread1_Sheet1.Cells.Get(0, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(1, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(2, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(3, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(4, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(5, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(6, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(7, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(8, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(9, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(10, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(11, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(12, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(13, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(14, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(15, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(16, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(17, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(18, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(19, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(20, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(21, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(22, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(23, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(24, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(25, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(26, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(27, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(28, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(29, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(30, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(31, 0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Cells.Get(31, 1).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 1).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 2).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 2).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 3).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 3).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 4).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 4).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 5).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 5).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 6).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 6).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 7).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 7).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 8).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 8).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 9).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 9).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 10).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 10).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 11).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 11).Value = 0;
            this.fpSpread1_Sheet1.Cells.Get(31, 12).Formula = "SUM(R[-31]C:R[-1]C)";
            this.fpSpread1_Sheet1.Cells.Get(31, 12).Value = 0;
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 0).Value = "Day";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 1).Value = "Jan";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 2).Value = "Feb";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 3).Value = "Mar";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 4).Value = "Apr";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 5).Value = "May";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 6).Value = "Jun";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 7).Value = "Jul";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 8).Value = "Aug";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 9).Value = "Sep";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 10).Value = "Oct";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 11).Value = "Nov";
            this.fpSpread1_Sheet1.ColumnHeader.Cells.Get(0, 12).Value = "Dec";
            this.fpSpread1_Sheet1.Columns.Get(0).ForeColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Columns.Get(0).Label = "Day";
            this.fpSpread1_Sheet1.Columns.Get(0).Visible = false;
            this.fpSpread1_Sheet1.Columns.Get(1).CellType = numberCellType1;
            this.fpSpread1_Sheet1.Columns.Get(1).Label = "Jan";
            this.fpSpread1_Sheet1.Columns.Get(2).CellType = numberCellType2;
            this.fpSpread1_Sheet1.Columns.Get(2).Label = "Feb";
            this.fpSpread1_Sheet1.Columns.Get(3).CellType = numberCellType3;
            this.fpSpread1_Sheet1.Columns.Get(3).Label = "Mar";
            this.fpSpread1_Sheet1.Columns.Get(4).CellType = numberCellType4;
            this.fpSpread1_Sheet1.Columns.Get(4).Label = "Apr";
            this.fpSpread1_Sheet1.Columns.Get(5).CellType = numberCellType5;
            this.fpSpread1_Sheet1.Columns.Get(5).Label = "May";
            this.fpSpread1_Sheet1.Columns.Get(6).CellType = numberCellType6;
            this.fpSpread1_Sheet1.Columns.Get(6).Label = "Jun";
            this.fpSpread1_Sheet1.Columns.Get(7).CellType = numberCellType7;
            this.fpSpread1_Sheet1.Columns.Get(7).Label = "Jul";
            this.fpSpread1_Sheet1.Columns.Get(8).CellType = numberCellType8;
            this.fpSpread1_Sheet1.Columns.Get(8).Label = "Aug";
            this.fpSpread1_Sheet1.Columns.Get(9).CellType = numberCellType9;
            this.fpSpread1_Sheet1.Columns.Get(9).Label = "Sep";
            this.fpSpread1_Sheet1.Columns.Get(10).CellType = numberCellType10;
            this.fpSpread1_Sheet1.Columns.Get(10).Label = "Oct";
            this.fpSpread1_Sheet1.Columns.Get(11).CellType = numberCellType11;
            this.fpSpread1_Sheet1.Columns.Get(11).Label = "Nov";
            this.fpSpread1_Sheet1.Columns.Get(12).CellType = numberCellType12;
            this.fpSpread1_Sheet1.Columns.Get(12).Label = "Dec";
            this.fpSpread1_Sheet1.RowHeader.Columns.Default.Resizable = false;
            this.fpSpread1_Sheet1.Rows.Get(31).BackColor = System.Drawing.Color.Silver;
            this.fpSpread1_Sheet1.Rows.Get(31).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold);
            this.fpSpread1_Sheet1.SheetName = "Sheet1";
            this.fpSpread1_Sheet1.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(140, 688);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(32, 13);
            this.label1.TabIndex = 6;
            this.label1.Text = "Year:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(238, 688);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(35, 13);
            this.label2.TabIndex = 7;
            this.label2.Text = "label2";
            // 
            // yearSelectUpDown
            // 
            this.yearSelectUpDown.Location = new System.Drawing.Point(169, 684);
            this.yearSelectUpDown.Name = "yearSelectUpDown";
            this.yearSelectUpDown.Size = new System.Drawing.Size(63, 20);
            this.yearSelectUpDown.TabIndex = 3;
            this.yearSelectUpDown.ValueChanged += new System.EventHandler(this.yearSelectUpDown_ValueChanged);
            // 
            // AddYearButton
            // 
            this.AddYearButton.Location = new System.Drawing.Point(12, 681);
            this.AddYearButton.Name = "AddYearButton";
            this.AddYearButton.Size = new System.Drawing.Size(88, 23);
            this.AddYearButton.TabIndex = 2;
            this.AddYearButton.Text = "&Add Year";
            this.AddYearButton.UseVisualStyleBackColor = true;
            this.AddYearButton.Click += new System.EventHandler(this.AddYearButton_Click);
            // 
            // closeButton
            // 
            this.closeButton.Location = new System.Drawing.Point(683, 681);
            this.closeButton.Name = "closeButton";
            this.closeButton.Size = new System.Drawing.Size(88, 23);
            this.closeButton.TabIndex = 5;
            this.closeButton.Text = "&Close";
            this.closeButton.UseVisualStyleBackColor = true;
            this.closeButton.Click += new System.EventHandler(this.closeButton_Click);
            // 
            // saveCloseButton
            // 
            this.saveCloseButton.Location = new System.Drawing.Point(589, 681);
            this.saveCloseButton.Name = "saveCloseButton";
            this.saveCloseButton.Size = new System.Drawing.Size(88, 23);
            this.saveCloseButton.TabIndex = 4;
            this.saveCloseButton.Text = "&Save && Close";
            this.saveCloseButton.UseVisualStyleBackColor = true;
            this.saveCloseButton.Click += new System.EventHandler(this.saveCloseButton_Click);
            // 
            // RainfallEditor
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(783, 710);
            this.Controls.Add(this.saveCloseButton);
            this.Controls.Add(this.closeButton);
            this.Controls.Add(this.AddYearButton);
            this.Controls.Add(this.yearSelectUpDown);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.fpSpread1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "RainfallEditor";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Rainfall Editor";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.RainfallEditor_FormClosing);
                      ((System.ComponentModel.ISupportInitialize)(this.fpSpread1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread1_Sheet1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.yearSelectUpDown)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        

       

        #endregion

        private FarPoint.Win.Spread.FpSpread fpSpread1;
        private FarPoint.Win.Spread.SheetView fpSpread1_Sheet1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.NumericUpDown yearSelectUpDown;
        private System.Windows.Forms.Button AddYearButton;
        private System.Windows.Forms.Button closeButton;
        private System.Windows.Forms.Button saveCloseButton;
        
        }
    }