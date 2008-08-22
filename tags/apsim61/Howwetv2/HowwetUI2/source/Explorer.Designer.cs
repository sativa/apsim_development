namespace APSRU.Howwet
    {
    partial class Explorer
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
            this.components = new System.ComponentModel.Container();
            Janus.Windows.Common.JanusColorScheme janusColorScheme1 = ((Janus.Windows.Common.JanusColorScheme)(new Janus.Windows.Common.JanusColorScheme()));
            this.pageSite = new System.Windows.Forms.Panel();
            this.backButton = new System.Windows.Forms.Button();
            this.forwardButton = new System.Windows.Forms.Button();
            this.homeButton = new System.Windows.Forms.Button();
            this.visualStyleManager1 = new Janus.Windows.Common.VisualStyleManager(this.components);
            this.SuspendLayout();
            // 
            // pageSite
            // 
            this.pageSite.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pageSite.Location = new System.Drawing.Point(0, 0);
            this.pageSite.Name = "pageSite";
            this.pageSite.Size = new System.Drawing.Size(1016, 734);
            this.pageSite.TabIndex = 0;
            // 
            // backButton
            // 
            this.backButton.Location = new System.Drawing.Point(12, 14);
            this.backButton.Name = "backButton";
            this.backButton.Size = new System.Drawing.Size(42, 23);
            this.backButton.TabIndex = 1;
            this.backButton.Text = "<<";
            this.backButton.UseVisualStyleBackColor = true;
            this.backButton.Click += new System.EventHandler(this.backButton_Click);
            // 
            // forwardButton
            // 
            this.forwardButton.Location = new System.Drawing.Point(60, 14);
            this.forwardButton.Name = "forwardButton";
            this.forwardButton.Size = new System.Drawing.Size(37, 23);
            this.forwardButton.TabIndex = 2;
            this.forwardButton.Text = ">>";
            this.forwardButton.UseVisualStyleBackColor = true;
            this.forwardButton.Click += new System.EventHandler(this.forwardButton_Click);
            // 
            // homeButton
            // 
            this.homeButton.Location = new System.Drawing.Point(103, 14);
            this.homeButton.Name = "homeButton";
            this.homeButton.Size = new System.Drawing.Size(43, 23);
            this.homeButton.TabIndex = 3;
            this.homeButton.Text = "Home";
            this.homeButton.UseVisualStyleBackColor = true;
            this.homeButton.Click += new System.EventHandler(this.homeButton_Click);
            // 
            // visualStyleManager1
            // 
            janusColorScheme1.ActiveCaptionColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(76)))), ((int)(((byte)(0)))));
            janusColorScheme1.ActiveCaptionTextColor = System.Drawing.Color.White;
            janusColorScheme1.ControlColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(116)))), ((int)(((byte)(0)))));
            janusColorScheme1.ControlDarkColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(76)))), ((int)(((byte)(0)))));
            janusColorScheme1.ControlTextColor = System.Drawing.Color.Black;
            janusColorScheme1.DisabledColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(116)))), ((int)(((byte)(0)))));
            janusColorScheme1.GrayTextColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(76)))), ((int)(((byte)(0)))));
            janusColorScheme1.HighlightColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(76)))), ((int)(((byte)(0)))));
            janusColorScheme1.HighlightTextColor = System.Drawing.Color.White;
            janusColorScheme1.InfoColor = System.Drawing.Color.White;
            janusColorScheme1.InfoTextColor = System.Drawing.Color.Black;
            janusColorScheme1.MenuColor = System.Drawing.Color.White;
            janusColorScheme1.MenuTextColor = System.Drawing.Color.Black;
            janusColorScheme1.Name = "Scheme0";
            janusColorScheme1.UseThemes = false;
            janusColorScheme1.WindowColor = System.Drawing.Color.White;
            janusColorScheme1.WindowTextColor = System.Drawing.Color.Black;
            this.visualStyleManager1.ColorSchemes.Add(janusColorScheme1);
            // 
            // Explorer
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.White;
            this.ClientSize = new System.Drawing.Size(1016, 734);
            this.Controls.Add(this.homeButton);
            this.Controls.Add(this.forwardButton);
            this.Controls.Add(this.backButton);
            this.Controls.Add(this.pageSite);
            this.Name = "Explorer";
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.Panel pageSite;
        private System.Windows.Forms.Button backButton;
        private System.Windows.Forms.Button forwardButton;
        private System.Windows.Forms.Button homeButton;
        private Janus.Windows.Common.VisualStyleManager visualStyleManager1;
        }
    }
