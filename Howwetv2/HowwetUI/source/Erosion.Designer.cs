namespace APSRU.Howwet
    {
    partial class Erosion
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
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.slope = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.erodibilty = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.slopeLength = new System.Windows.Forms.TextBox();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.slope);
            this.groupBox2.Controls.Add(this.label11);
            this.groupBox2.Controls.Add(this.label8);
            this.groupBox2.Controls.Add(this.label10);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.erodibilty);
            this.groupBox2.Controls.Add(this.label7);
            this.groupBox2.Controls.Add(this.slopeLength);
            this.groupBox2.Location = new System.Drawing.Point(12, 12);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(355, 49);
            this.groupBox2.TabIndex = 30;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Erosion";
            // 
            // slope
            // 
            this.slope.Location = new System.Drawing.Point(42, 16);
            this.slope.Name = "slope";
            this.slope.Size = new System.Drawing.Size(30, 20);
            this.slope.TabIndex = 0;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(195, 18);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(15, 13);
            this.label11.TabIndex = 9;
            this.label11.Text = "m";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(228, 20);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(52, 13);
            this.label8.TabIndex = 6;
            this.label8.Text = "Erodibilty:";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(71, 22);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(15, 13);
            this.label10.TabIndex = 8;
            this.label10.Text = "%";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(6, 20);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(37, 13);
            this.label6.TabIndex = 4;
            this.label6.Text = "Slope:";
            // 
            // erodibilty
            // 
            this.erodibilty.Location = new System.Drawing.Point(286, 15);
            this.erodibilty.Name = "erodibilty";
            this.erodibilty.Size = new System.Drawing.Size(30, 20);
            this.erodibilty.TabIndex = 2;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(92, 18);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(73, 13);
            this.label7.TabIndex = 5;
            this.label7.Text = "Slope Length:";
            // 
            // slopeLength
            // 
            this.slopeLength.Location = new System.Drawing.Point(171, 15);
            this.slopeLength.Name = "slopeLength";
            this.slopeLength.Size = new System.Drawing.Size(30, 20);
            this.slopeLength.TabIndex = 1;
            // 
            // Erosion
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(378, 79);
            this.Controls.Add(this.groupBox2);
            this.Name = "Erosion";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Erosion Options";
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.TextBox slope;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox erodibilty;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox slopeLength;
        }
    }