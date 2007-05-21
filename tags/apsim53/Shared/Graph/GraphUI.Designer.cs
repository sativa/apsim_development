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
            this.ToolStripContainer = new System.Windows.Forms.ToolStripContainer();
            this.ChartPanel = new System.Windows.Forms.Panel();
            this.GraphControl = new Graph.GraphControl();
            this.Splitter = new System.Windows.Forms.Splitter();
            this.ToolStrip = new System.Windows.Forms.ToolStrip();
            this.ColourDialog = new System.Windows.Forms.ColorDialog();
            this.RefreshButton = new System.Windows.Forms.ToolStripButton();
            this.PropertiesButton = new System.Windows.Forms.ToolStripButton();
            this.ToolStripContainer.ContentPanel.SuspendLayout();
            this.ToolStripContainer.TopToolStripPanel.SuspendLayout();
            this.ToolStripContainer.SuspendLayout();
            this.ChartPanel.SuspendLayout();
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
            this.ChartPanel.Controls.Add(this.GraphControl);
            this.ChartPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ChartPanel.Location = new System.Drawing.Point(0, 3);
            this.ChartPanel.Name = "ChartPanel";
            this.ChartPanel.Size = new System.Drawing.Size(1105, 494);
            this.ChartPanel.TabIndex = 8;
            // 
            // GraphControl
            // 
            this.GraphControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GraphControl.Location = new System.Drawing.Point(0, 0);
            this.GraphControl.Name = "GraphControl";
            this.GraphControl.Size = new System.Drawing.Size(1105, 494);
            this.GraphControl.TabIndex = 0;
            // 
            // Splitter
            // 
            this.Splitter.Dock = System.Windows.Forms.DockStyle.Top;
            this.Splitter.Location = new System.Drawing.Point(0, 0);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(1105, 3);
            this.Splitter.TabIndex = 7;
            this.Splitter.TabStop = false;
            // 
            // ToolStrip
            // 
            this.ToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.ToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.RefreshButton,
            this.PropertiesButton});
            this.ToolStrip.Location = new System.Drawing.Point(3, 0);
            this.ToolStrip.Name = "ToolStrip";
            this.ToolStrip.Size = new System.Drawing.Size(178, 25);
            this.ToolStrip.TabIndex = 0;
            // 
            // RefreshButton
            // 
            this.RefreshButton.Image = global::Graph.Properties.Resources.refresh;
            this.RefreshButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RefreshButton.Name = "RefreshButton";
            this.RefreshButton.Size = new System.Drawing.Size(66, 22);
            this.RefreshButton.Text = "Refresh";
            this.RefreshButton.Click += new System.EventHandler(this.RefreshButton_Click);
            // 
            // PropertiesButton
            // 
            this.PropertiesButton.Image = global::Graph.Properties.Resources.preferences;
            this.PropertiesButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.PropertiesButton.Name = "PropertiesButton";
            this.PropertiesButton.Size = new System.Drawing.Size(69, 22);
            this.PropertiesButton.Text = "Settings";
            this.PropertiesButton.Click += new System.EventHandler(this.PropertiesButton_Click);
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
            this.ChartPanel.ResumeLayout(false);
            this.ToolStrip.ResumeLayout(false);
            this.ToolStrip.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ToolStripContainer ToolStripContainer;
        private System.Windows.Forms.Splitter Splitter;
        private System.Windows.Forms.ToolStrip ToolStrip;
        private System.Windows.Forms.ToolStripButton RefreshButton;
        private System.Windows.Forms.Panel ChartPanel;
        private System.Windows.Forms.ColorDialog ColourDialog;
        private GraphControl GraphControl;
        private System.Windows.Forms.ToolStripButton PropertiesButton;

        }
    }
