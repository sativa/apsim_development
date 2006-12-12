namespace APSRU.UIControls
    {
    partial class HowwetPageTemplate
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
            this.leftLabel = new System.Windows.Forms.Label();
            this.button1 = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.button3 = new System.Windows.Forms.Button();
            this.textHeader1 = new APSRU.UIControls.TextHeader();
            this.colorBox1 = new APSRU.UIControls.ColorBox();
            this.SuspendLayout();
            // 
            // leftLabel
            // 
            this.leftLabel.AutoSize = true;
            this.leftLabel.BackColor = System.Drawing.Color.DarkGreen;
            this.leftLabel.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.leftLabel.ForeColor = System.Drawing.Color.White;
            this.leftLabel.Location = new System.Drawing.Point(4, 48);
            this.leftLabel.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.leftLabel.Name = "leftLabel";
            this.leftLabel.Size = new System.Drawing.Size(80, 18);
            this.leftLabel.TabIndex = 5;
            this.leftLabel.Text = "leftLabel";
            // 
            // button1
            // 
            this.button1.Image = global::APSRU.UIControls.Properties.Resources.nav_left_green1;
            this.button1.Location = new System.Drawing.Point(6, 3);
            this.button1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(40, 40);
            this.button1.TabIndex = 6;
            this.button1.UseVisualStyleBackColor = true;
            // 
            // button2
            // 
            this.button2.Image = global::APSRU.UIControls.Properties.Resources.home1;
            this.button2.Location = new System.Drawing.Point(99, 3);
            this.button2.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(40, 40);
            this.button2.TabIndex = 7;
            this.button2.UseVisualStyleBackColor = true;
            // 
            // button3
            // 
            this.button3.Image = global::APSRU.UIControls.Properties.Resources.nav_right_green1;
            this.button3.Location = new System.Drawing.Point(52, 3);
            this.button3.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(40, 40);
            this.button3.TabIndex = 8;
            this.button3.UseVisualStyleBackColor = true;
            // 
            // textHeader1
            // 
            this.textHeader1.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar;
            this.textHeader1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textHeader1.Font = new System.Drawing.Font("Verdana", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textHeader1.ForeColor = System.Drawing.Color.White;
            this.textHeader1.HeaderText = "TextHeader";
            this.textHeader1.LeftColor = System.Drawing.Color.DarkGreen;
            this.textHeader1.Location = new System.Drawing.Point(147, 0);
            this.textHeader1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.textHeader1.Name = "textHeader1";
            this.textHeader1.RightColor = System.Drawing.SystemColors.Control;
            this.textHeader1.Size = new System.Drawing.Size(849, 45);
            this.textHeader1.TabIndex = 1;
            this.textHeader1.TextAlignmentX = 50;
            // 
            // colorBox1
            // 
            this.colorBox1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.colorBox1.BackColor = System.Drawing.Color.White;
            this.colorBox1.BoxColor = System.Drawing.Color.DarkGreen;
            this.colorBox1.Location = new System.Drawing.Point(0, 45);
            this.colorBox1.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.colorBox1.Name = "colorBox1";
            this.colorBox1.Size = new System.Drawing.Size(149, 642);
            this.colorBox1.TabIndex = 0;
            // 
            // HowwetPageTemplate
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.White;
            this.Controls.Add(this.button3);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.leftLabel);
            this.Controls.Add(this.textHeader1);
            this.Controls.Add(this.colorBox1);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.Name = "HowwetPageTemplate";
            this.Size = new System.Drawing.Size(1004, 685);
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private ColorBox colorBox1;
        private TextHeader textHeader1;
        private System.Windows.Forms.Label leftLabel;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button button3;
        }
    }
