namespace Graph
    {
    partial class SmartPanel
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SmartPanel));
            this.Canvas = new CSUserInterface.DragDropCanvas();
            this.button1 = new System.Windows.Forms.Button();
            this.formDesigner1 = new Spire.Designer.FormDesigner();
            this.SuspendLayout();
            // 
            // Canvas
            // 
            this.Canvas.AllowDrop = true;
            this.Canvas.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.Canvas.AutoScroll = true;
            this.Canvas.BackColor = System.Drawing.SystemColors.Window;
            this.Canvas.ForeColor = System.Drawing.SystemColors.ControlText;
            this.Canvas.Location = new System.Drawing.Point(3, 56);
            this.Canvas.Name = "Canvas";
            this.Canvas.Size = new System.Drawing.Size(162, 133);
            this.Canvas.TabIndex = 4;
            this.Canvas.Paint += new System.Windows.Forms.PaintEventHandler(this.OnCanvasPaint);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(58, 26);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 5;
            this.button1.Text = "button1";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // formDesigner1
            // 
            this.formDesigner1.DesignedForm = this;
            this.formDesigner1.GridSize = new System.Drawing.Size(8, 8);
            this.formDesigner1.PropertyGrid = null;
            this.formDesigner1.XMLForm = resources.GetString("formDesigner1.XMLForm");
            // 
            // SmartPanel
            // 
            this.Controls.Add(this.button1);
            this.Controls.Add(this.Canvas);
            this.Name = "SmartPanel";
            this.Controls.SetChildIndex(this.Canvas, 0);
            this.Controls.SetChildIndex(this.button1, 0);
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private System.Windows.Forms.Button button1;
        private Spire.Designer.FormDesigner formDesigner1;
        }
    }
