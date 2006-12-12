namespace APSRU.Howwet
    {
    partial class PageTemplate
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
            {
            this.secondaryLabel = new System.Windows.Forms.Label();
            this.colorBox1 = new APSRU.UIControls.ColorBox();
            this.textHeader1 = new APSRU.UIControls.TextHeader();
            this.colorBox2 = new APSRU.UIControls.ColorBox();
            this.startupLabel = new System.Windows.Forms.Label();
            this.resultsLabel = new System.Windows.Forms.Label();
            this.fallowSettingsLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // secondaryLabel
            // 
            this.secondaryLabel.AutoSize = true;
            this.secondaryLabel.BackColor = System.Drawing.Color.DarkGreen;
            this.secondaryLabel.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.secondaryLabel.ForeColor = System.Drawing.Color.White;
            this.secondaryLabel.Location = new System.Drawing.Point(12, 108);
            this.secondaryLabel.Name = "secondaryLabel";
            this.secondaryLabel.Size = new System.Drawing.Size(58, 18);
            this.secondaryLabel.TabIndex = 6;
            this.secondaryLabel.Text = "label2";
            // 
            // colorBox1
            // 
            this.colorBox1.BoxColor = System.Drawing.Color.DarkGreen;
            this.colorBox1.Dock = System.Windows.Forms.DockStyle.Left;
            this.colorBox1.Location = new System.Drawing.Point(0, 0);
            this.colorBox1.Name = "colorBox1";
            this.colorBox1.Size = new System.Drawing.Size(150, 768);
            this.colorBox1.TabIndex = 1;
            // 
            // textHeader1
            // 
            this.textHeader1.Font = new System.Drawing.Font("Verdana", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textHeader1.ForeColor = System.Drawing.Color.White;
            this.textHeader1.HeaderText = "";
            this.textHeader1.LeftColor = System.Drawing.Color.DarkGreen;
            this.textHeader1.Location = new System.Drawing.Point(150, 38);
            this.textHeader1.Name = "textHeader1";
            this.textHeader1.RightColor = System.Drawing.SystemColors.Control;
            this.textHeader1.Size = new System.Drawing.Size(868, 62);
            this.textHeader1.TabIndex = 0;
            this.textHeader1.TextAlignmentX = 50;
            // 
            // colorBox2
            // 
            this.colorBox2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.colorBox2.BoxColor = System.Drawing.Color.DarkGreen;
            this.colorBox2.Location = new System.Drawing.Point(0, 0);
            this.colorBox2.Name = "colorBox2";
            this.colorBox2.Size = new System.Drawing.Size(1024, 38);
            this.colorBox2.TabIndex = 7;
            // 
            // startupLabel
            // 
            this.startupLabel.AutoSize = true;
            this.startupLabel.BackColor = System.Drawing.Color.DarkGreen;
            this.startupLabel.Font = new System.Drawing.Font("Verdana", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.startupLabel.ForeColor = System.Drawing.Color.White;
            this.startupLabel.Location = new System.Drawing.Point(177, 3);
            this.startupLabel.Name = "startupLabel";
            this.startupLabel.Size = new System.Drawing.Size(115, 23);
            this.startupLabel.TabIndex = 8;
            this.startupLabel.Text = "1 - Startup";
            this.startupLabel.Click += new System.EventHandler(this.startupLabel_Click);
            // 
            // resultsLabel
            // 
            this.resultsLabel.AutoSize = true;
            this.resultsLabel.BackColor = System.Drawing.Color.DarkGreen;
            this.resultsLabel.Font = new System.Drawing.Font("Verdana", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.resultsLabel.ForeColor = System.Drawing.Color.White;
            this.resultsLabel.Location = new System.Drawing.Point(594, 3);
            this.resultsLabel.Name = "resultsLabel";
            this.resultsLabel.Size = new System.Drawing.Size(114, 23);
            this.resultsLabel.TabIndex = 10;
            this.resultsLabel.Text = "3 - Results";
            this.resultsLabel.Click += new System.EventHandler(this.resultsLabel_Click);
            // 
            // fallowSettingsLabel
            // 
            this.fallowSettingsLabel.AutoSize = true;
            this.fallowSettingsLabel.BackColor = System.Drawing.Color.DarkGreen;
            this.fallowSettingsLabel.Font = new System.Drawing.Font("Verdana", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.fallowSettingsLabel.ForeColor = System.Drawing.Color.White;
            this.fallowSettingsLabel.Location = new System.Drawing.Point(351, 3);
            this.fallowSettingsLabel.Name = "fallowSettingsLabel";
            this.fallowSettingsLabel.Size = new System.Drawing.Size(192, 23);
            this.fallowSettingsLabel.TabIndex = 11;
            this.fallowSettingsLabel.Text = "2 - Fallow Settings";
            this.fallowSettingsLabel.Click += new System.EventHandler(this.fallowSettingsLabel_Click);
            // 
            // PageTemplate
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.startupLabel);
            this.Controls.Add(this.fallowSettingsLabel);
            this.Controls.Add(this.resultsLabel);
            this.Controls.Add(this.secondaryLabel);
            this.Controls.Add(this.colorBox1);
            this.Controls.Add(this.textHeader1);
            this.Controls.Add(this.colorBox2);
            this.Name = "PageTemplate";
            this.Size = new System.Drawing.Size(1024, 768);
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private APSRU.UIControls.TextHeader textHeader1;
        private APSRU.UIControls.ColorBox colorBox1;
        private System.Windows.Forms.Label secondaryLabel;
        private APSRU.UIControls.ColorBox colorBox2;
        private System.Windows.Forms.Label startupLabel;
        private System.Windows.Forms.Label resultsLabel;
        private System.Windows.Forms.Label fallowSettingsLabel;
        }
    }
