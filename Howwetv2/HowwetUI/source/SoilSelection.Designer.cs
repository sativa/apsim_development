namespace APSRU.Howwet
    {
    partial class SoilSelection
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SoilSelection));
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.dataTree1 = new VBGeneral.DataTree();
            this.SuspendLayout();
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "shovel.png");
            this.imageList1.Images.SetKeyName(1, "folder_document.png");
            // 
            // dataTree1
            // 
            this.dataTree1.AllowDrop = true;
            this.dataTree1.AutoScroll = true;
            this.dataTree1.BackColor = System.Drawing.SystemColors.Control;
            this.dataTree1.Controller = null;
            this.dataTree1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dataTree1.HelpText = "";
            this.dataTree1.Location = new System.Drawing.Point(0, 0);
            this.dataTree1.Name = "dataTree1";
            this.dataTree1.Size = new System.Drawing.Size(369, 354);
            this.dataTree1.Sorted = false;
            this.dataTree1.TabIndex = 0;
            this.dataTree1.DoubleClickEvent += new VBGeneral.DataTree.NotifyEventHandler(this.dataTree1_DoubleClickEvent);
            // 
            // SoilSelection
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(369, 354);
            this.Controls.Add(this.dataTree1);
            this.Name = "SoilSelection";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Select Soil";
            this.Load += new System.EventHandler(this.SoilSelection_Load);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ImageList imageList1;
        private VBGeneral.DataTree dataTree1;


        }
    }