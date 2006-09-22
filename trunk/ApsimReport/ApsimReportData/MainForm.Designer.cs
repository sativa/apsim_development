namespace ApsimReportData
    {
    partial class MainForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.BottomPanel = new System.Windows.Forms.Panel();
            this.SmallImages = new System.Windows.Forms.ImageList(this.components);
            this.RightPanel = new System.Windows.Forms.Panel();
            this.CancelBut = new System.Windows.Forms.Button();
            this.OkButton = new System.Windows.Forms.Button();
            this.Splitter = new System.Windows.Forms.Splitter();
            this.MainPanel = new System.Windows.Forms.Panel();
            this.BottomToolStrip = new System.Windows.Forms.ToolStrip();
            this.ShowHideToolboxButton = new System.Windows.Forms.ToolStripButton();
            this.ToolboxLabel = new System.Windows.Forms.Panel();
            this.label1 = new System.Windows.Forms.Label();
            this.ToolboxPanel = new System.Windows.Forms.Panel();
            this.BottomPanel.SuspendLayout();
            this.RightPanel.SuspendLayout();
            this.BottomToolStrip.SuspendLayout();
            this.ToolboxLabel.SuspendLayout();
            this.SuspendLayout();
            // 
            // BottomPanel
            // 
            this.BottomPanel.Controls.Add(this.ToolboxPanel);
            this.BottomPanel.Controls.Add(this.ToolboxLabel);
            this.BottomPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.BottomPanel.Location = new System.Drawing.Point(0, 488);
            this.BottomPanel.Name = "BottomPanel";
            this.BottomPanel.Size = new System.Drawing.Size(894, 154);
            this.BottomPanel.TabIndex = 2;
            this.BottomPanel.Visible = false;
            // 
            // SmallImages
            // 
            this.SmallImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("SmallImages.ImageStream")));
            this.SmallImages.TransparentColor = System.Drawing.Color.Transparent;
            this.SmallImages.Images.SetKeyName(0, "data.png");
            this.SmallImages.Images.SetKeyName(1, "ApsimFileReader.png");
            this.SmallImages.Images.SetKeyName(2, "XmlFileReader.png");
            this.SmallImages.Images.SetKeyName(3, "Rems.png");
            this.SmallImages.Images.SetKeyName(4, "ExcelReader.png");
            this.SmallImages.Images.SetKeyName(5, "die.png");
            this.SmallImages.Images.SetKeyName(6, "PredObs.png");
            this.SmallImages.Images.SetKeyName(7, "funnel.png");
            this.SmallImages.Images.SetKeyName(8, "Cumulative.png");
            this.SmallImages.Images.SetKeyName(9, "Depth.png");
            this.SmallImages.Images.SetKeyName(10, "Diff.png");
            // 
            // RightPanel
            // 
            this.RightPanel.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
            this.RightPanel.Controls.Add(this.CancelBut);
            this.RightPanel.Controls.Add(this.OkButton);
            this.RightPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.RightPanel.Location = new System.Drawing.Point(894, 0);
            this.RightPanel.Name = "RightPanel";
            this.RightPanel.Size = new System.Drawing.Size(98, 667);
            this.RightPanel.TabIndex = 3;
            // 
            // CancelBut
            // 
            this.CancelBut.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.CancelBut.Location = new System.Drawing.Point(15, 50);
            this.CancelBut.Name = "CancelBut";
            this.CancelBut.Size = new System.Drawing.Size(75, 23);
            this.CancelBut.TabIndex = 3;
            this.CancelBut.Text = "Cancel";
            this.CancelBut.UseVisualStyleBackColor = true;
            this.CancelBut.Click += new System.EventHandler(this.CancelBut_Click);
            // 
            // OkButton
            // 
            this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.OkButton.Location = new System.Drawing.Point(15, 12);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 23);
            this.OkButton.TabIndex = 2;
            this.OkButton.Text = "Ok";
            this.OkButton.UseVisualStyleBackColor = true;
            this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
            // 
            // Splitter
            // 
            this.Splitter.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.Splitter.Location = new System.Drawing.Point(0, 485);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(894, 3);
            this.Splitter.TabIndex = 4;
            this.Splitter.TabStop = false;
            this.Splitter.Visible = false;
            // 
            // MainPanel
            // 
            this.MainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.MainPanel.Location = new System.Drawing.Point(0, 0);
            this.MainPanel.Name = "MainPanel";
            this.MainPanel.Size = new System.Drawing.Size(894, 485);
            this.MainPanel.TabIndex = 5;
            // 
            // BottomToolStrip
            // 
            this.BottomToolStrip.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.BottomToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ShowHideToolboxButton});
            this.BottomToolStrip.Location = new System.Drawing.Point(0, 642);
            this.BottomToolStrip.Name = "BottomToolStrip";
            this.BottomToolStrip.Size = new System.Drawing.Size(894, 25);
            this.BottomToolStrip.TabIndex = 21;
            this.BottomToolStrip.Text = "toolStrip1";
            // 
            // ShowHideToolboxButton
            // 
            this.ShowHideToolboxButton.CheckOnClick = true;
            this.ShowHideToolboxButton.Image = global::ApsimReportData.Properties.Resources.toolbox;
            this.ShowHideToolboxButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ShowHideToolboxButton.Name = "ShowHideToolboxButton";
            this.ShowHideToolboxButton.Size = new System.Drawing.Size(143, 22);
            this.ShowHideToolboxButton.Text = "Show / Hide toolbox";
            this.ShowHideToolboxButton.Click += new System.EventHandler(this.ShowHideToolboxButton_Click);
            // 
            // ToolboxLabel
            // 
            this.ToolboxLabel.BackColor = System.Drawing.SystemColors.GradientInactiveCaption;
            this.ToolboxLabel.Controls.Add(this.label1);
            this.ToolboxLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.ToolboxLabel.Location = new System.Drawing.Point(0, 0);
            this.ToolboxLabel.Name = "ToolboxLabel";
            this.ToolboxLabel.Size = new System.Drawing.Size(894, 30);
            this.ToolboxLabel.TabIndex = 21;
            this.ToolboxLabel.Visible = false;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.label1.Location = new System.Drawing.Point(5, 4);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(51, 15);
            this.label1.TabIndex = 0;
            this.label1.Text = "Toolbox";
            // 
            // ToolboxPanel
            // 
            this.ToolboxPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ToolboxPanel.Location = new System.Drawing.Point(0, 30);
            this.ToolboxPanel.Name = "ToolboxPanel";
            this.ToolboxPanel.Size = new System.Drawing.Size(894, 124);
            this.ToolboxPanel.TabIndex = 22;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(992, 667);
            this.Controls.Add(this.MainPanel);
            this.Controls.Add(this.Splitter);
            this.Controls.Add(this.BottomPanel);
            this.Controls.Add(this.BottomToolStrip);
            this.Controls.Add(this.RightPanel);
            this.Name = "MainForm";
            this.Text = "ApsimReport Data Manager";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.BottomPanel.ResumeLayout(false);
            this.RightPanel.ResumeLayout(false);
            this.BottomToolStrip.ResumeLayout(false);
            this.BottomToolStrip.PerformLayout();
            this.ToolboxLabel.ResumeLayout(false);
            this.ToolboxLabel.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private System.Windows.Forms.Panel BottomPanel;
        private System.Windows.Forms.ImageList SmallImages;
        private System.Windows.Forms.Panel RightPanel;
        private System.Windows.Forms.Button CancelBut;
        private System.Windows.Forms.Button OkButton;
        private System.Windows.Forms.Splitter Splitter;
        private System.Windows.Forms.Panel MainPanel;
        private System.Windows.Forms.ToolStrip BottomToolStrip;
        private System.Windows.Forms.ToolStripButton ShowHideToolboxButton;
        private System.Windows.Forms.Panel ToolboxPanel;
        private System.Windows.Forms.Panel ToolboxLabel;
        private System.Windows.Forms.Label label1;
        }
    }

