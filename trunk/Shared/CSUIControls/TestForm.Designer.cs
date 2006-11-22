namespace APSRUUIControls
    {
    partial class TestForm
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
            this.metGraphControl1 = new APSRU.UIControls.MetGraphControl();
            this.textHeader1 = new APSRU.UIControls.TextHeader();
            this.SuspendLayout();
            // 
            // metGraphControl1
            // 
            this.metGraphControl1.Location = new System.Drawing.Point(23, 139);
            this.metGraphControl1.Name = "metGraphControl1";
            this.metGraphControl1.Size = new System.Drawing.Size(475, 327);
            this.metGraphControl1.TabIndex = 1;
            // 
            // textHeader1
            // 
            this.textHeader1.Font = new System.Drawing.Font("Verdana", 13F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textHeader1.HeaderText = "TextHeader";
            this.textHeader1.LeftColor = System.Drawing.SystemColors.ActiveCaption;
            this.textHeader1.Location = new System.Drawing.Point(186, 30);
            this.textHeader1.Name = "textHeader1";
            this.textHeader1.RightColor = System.Drawing.SystemColors.Control;
            this.textHeader1.Size = new System.Drawing.Size(150, 58);
            this.textHeader1.TabIndex = 2;
            this.textHeader1.TextAlignmentX = 0;
            // 
            // TestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(585, 478);
            this.Controls.Add(this.textHeader1);
            this.Controls.Add(this.metGraphControl1);
            this.Name = "TestForm";
            this.Text = "TestForm";
            this.Load += new System.EventHandler(this.TestForm_Load);
            this.ResumeLayout(false);

            }

        #endregion

        private APSRU.UIControls.MetGraphControl metGraphControl1;
        private APSRU.UIControls.TextHeader textHeader1;

        }
    }