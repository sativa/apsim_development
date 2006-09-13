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
            this.SaveCloseButton = new System.Windows.Forms.Button();
            this.SoilCloseButton = new System.Windows.Forms.Button();
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
            // SaveCloseButton
            // 
            this.SaveCloseButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.SaveCloseButton.Location = new System.Drawing.Point(175, 326);
            this.SaveCloseButton.Name = "SaveCloseButton";
            this.SaveCloseButton.Size = new System.Drawing.Size(88, 23);
            this.SaveCloseButton.TabIndex = 1;
            this.SaveCloseButton.Text = "&Select && Close";
            this.SaveCloseButton.UseVisualStyleBackColor = true;
            this.SaveCloseButton.Click += new System.EventHandler(this.SaveCloseButton_Click);
            // 
            // SoilCloseButton
            // 
            this.SoilCloseButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.SoilCloseButton.Location = new System.Drawing.Point(269, 326);
            this.SoilCloseButton.Name = "SoilCloseButton";
            this.SoilCloseButton.Size = new System.Drawing.Size(88, 23);
            this.SoilCloseButton.TabIndex = 2;
            this.SoilCloseButton.Text = "&Close";
            this.SoilCloseButton.UseVisualStyleBackColor = true;
            this.SoilCloseButton.Click += new System.EventHandler(this.SoilCloseButton_Click);
            // 
            // dataTree1
            // 
            this.dataTree1.AllowDrop = true;
            this.dataTree1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dataTree1.AutoScroll = true;
            this.dataTree1.BackColor = System.Drawing.SystemColors.Control;
            this.dataTree1.Controller = null;
            this.dataTree1.HelpText = "";
            this.dataTree1.Location = new System.Drawing.Point(0, 0);
            this.dataTree1.Name = "dataTree1";
            this.dataTree1.Size = new System.Drawing.Size(369, 320);
            this.dataTree1.Sorted = false;
            this.dataTree1.TabIndex = 0;
            this.dataTree1.DoubleClickEvent += new VBGeneral.DataTree.NotifyEventHandler(this.dataTree1_DoubleClickEvent);
            // 
            // SoilSelection
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(369, 352);
            this.Controls.Add(this.SoilCloseButton);
            this.Controls.Add(this.SaveCloseButton);
            this.Controls.Add(this.dataTree1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "SoilSelection";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Select a Soil";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.SoilSelection_FormClosing);
            this.Load += new System.EventHandler(this.SoilSelection_Load);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ImageList imageList1;
        private VBGeneral.DataTree dataTree1;
        private System.Windows.Forms.Button SaveCloseButton;
        private System.Windows.Forms.Button SoilCloseButton;


        }
    }