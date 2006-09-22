namespace Graph
    {
    partial class GraphUI
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
            this.ToolStripContainer = new System.Windows.Forms.ToolStripContainer();
            this.ChartPanel = new System.Windows.Forms.Panel();
            this.Splitter = new System.Windows.Forms.Splitter();
            this.Spread = new FarPoint.Win.Spread.FpSpread();
            this.ToolStrip = new System.Windows.Forms.ToolStrip();
            this.ToolStripLabel = new System.Windows.Forms.ToolStripLabel();
            this.NumChartsCombo = new System.Windows.Forms.ToolStripComboBox();
            this.ToolStripSeparator = new System.Windows.Forms.ToolStripSeparator();
            this.RefreshButton = new System.Windows.Forms.ToolStripButton();
            this.ColourDialog = new System.Windows.Forms.ColorDialog();
            this.ToolStripContainer.ContentPanel.SuspendLayout();
            this.ToolStripContainer.TopToolStripPanel.SuspendLayout();
            this.ToolStripContainer.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Spread)).BeginInit();
            this.ToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolStripContainer
            // 
            this.ToolStripContainer.BottomToolStripPanelVisible = false;
            // 
            // ToolStripContainer.ContentPanel
            // 
            this.ToolStripContainer.ContentPanel.Controls.Add(this.ChartPanel);
            this.ToolStripContainer.ContentPanel.Controls.Add(this.Splitter);
            this.ToolStripContainer.ContentPanel.Controls.Add(this.Spread);
            this.ToolStripContainer.ContentPanel.Size = new System.Drawing.Size(1105, 497);
            this.ToolStripContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ToolStripContainer.LeftToolStripPanelVisible = false;
            this.ToolStripContainer.Location = new System.Drawing.Point(0, 40);
            this.ToolStripContainer.Name = "ToolStripContainer";
            this.ToolStripContainer.RightToolStripPanelVisible = false;
            this.ToolStripContainer.Size = new System.Drawing.Size(1105, 522);
            this.ToolStripContainer.TabIndex = 7;
            this.ToolStripContainer.Text = "toolStripContainer1";
            // 
            // ToolStripContainer.TopToolStripPanel
            // 
            this.ToolStripContainer.TopToolStripPanel.Controls.Add(this.ToolStrip);
            // 
            // ChartPanel
            // 
            this.ChartPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ChartPanel.Location = new System.Drawing.Point(0, 156);
            this.ChartPanel.Name = "ChartPanel";
            this.ChartPanel.Size = new System.Drawing.Size(1105, 341);
            this.ChartPanel.TabIndex = 8;
            this.ChartPanel.Resize += new System.EventHandler(this.ChartPanel_Resize);
            // 
            // Splitter
            // 
            this.Splitter.Dock = System.Windows.Forms.DockStyle.Top;
            this.Splitter.Location = new System.Drawing.Point(0, 153);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(1105, 3);
            this.Splitter.TabIndex = 7;
            this.Splitter.TabStop = false;
            // 
            // Spread
            // 
            this.Spread.AccessibleDescription = "Spread";
            this.Spread.Dock = System.Windows.Forms.DockStyle.Top;
            this.Spread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Spread.Location = new System.Drawing.Point(0, 0);
            this.Spread.Name = "Spread";
            this.Spread.ScrollBarShowMax = false;
            this.Spread.Size = new System.Drawing.Size(1105, 153);
            this.Spread.TabIndex = 6;
            this.Spread.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
            this.Spread.TabStripPlacement = FarPoint.Win.Spread.TabStripPlacement.Bottom;
            this.Spread.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Always;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.Spread.TextTipAppearance = tipAppearance1;
            this.Spread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Spread.CellClick += new FarPoint.Win.Spread.CellClickEventHandler(this.Spread_CellClick);
            this.Spread.ActiveSheetIndex = -1;
            // 
            // ToolStrip
            // 
            this.ToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.ToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ToolStripLabel,
            this.NumChartsCombo,
            this.ToolStripSeparator,
            this.RefreshButton});
            this.ToolStrip.Location = new System.Drawing.Point(3, 0);
            this.ToolStrip.Name = "ToolStrip";
            this.ToolStrip.Size = new System.Drawing.Size(304, 25);
            this.ToolStrip.TabIndex = 0;
            // 
            // ToolStripLabel
            // 
            this.ToolStripLabel.Name = "ToolStripLabel";
            this.ToolStripLabel.Size = new System.Drawing.Size(112, 22);
            this.ToolStripLabel.Text = "Number of charts:";
            // 
            // NumChartsCombo
            // 
            this.NumChartsCombo.Items.AddRange(new object[] {
            "One chart",
            "Two charts",
            "Three charts",
            "Four charts",
            "Five charts",
            "Six charts",
            "Seven charts",
            "Eight charts"});
            this.NumChartsCombo.Name = "NumChartsCombo";
            this.NumChartsCombo.Size = new System.Drawing.Size(100, 25);
            this.NumChartsCombo.Text = "One chart";
            this.NumChartsCombo.TextChanged += new System.EventHandler(this.NumChartsCombo_TextChanged);
            // 
            // ToolStripSeparator
            // 
            this.ToolStripSeparator.Name = "ToolStripSeparator";
            this.ToolStripSeparator.Size = new System.Drawing.Size(6, 25);
            // 
            // RefreshButton
            // 
            this.RefreshButton.Image = global::Graph.Properties.Resources.refresh;
            this.RefreshButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(72, 22);
            this.RefreshButton.Text = "Refresh";
            // 
            // GraphUI
            // 
            this.Controls.Add(this.ToolStripContainer);
            this.Name = "GraphUI";
            this.Size = new System.Drawing.Size(1105, 562);
            this.Controls.SetChildIndex(this.ToolStripContainer, 0);
            this.ToolStripContainer.ContentPanel.ResumeLayout(false);
            this.ToolStripContainer.TopToolStripPanel.ResumeLayout(false);
            this.ToolStripContainer.TopToolStripPanel.PerformLayout();
            this.ToolStripContainer.ResumeLayout(false);
            this.ToolStripContainer.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Spread)).EndInit();
            this.ToolStrip.ResumeLayout(false);
            this.ToolStrip.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ToolStripContainer ToolStripContainer;
        private System.Windows.Forms.Splitter Splitter;
        private FarPoint.Win.Spread.FpSpread Spread;
        private System.Windows.Forms.ToolStrip ToolStrip;
        private System.Windows.Forms.ToolStripLabel ToolStripLabel;
        private System.Windows.Forms.ToolStripComboBox NumChartsCombo;
        private System.Windows.Forms.ToolStripSeparator ToolStripSeparator;
        private System.Windows.Forms.ToolStripButton RefreshButton;
        private System.Windows.Forms.Panel ChartPanel;
        private System.Windows.Forms.ColorDialog ColourDialog;

        }
    }
