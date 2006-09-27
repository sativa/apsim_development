namespace Graph
    {
    partial class GraphDataUI
        {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;



        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
            {
            this.GenericUI = new VBGeneral.GenericUI();
            this.splitter2 = new System.Windows.Forms.Splitter();
            this.DataPanel = new System.Windows.Forms.Panel();
            this.SuspendLayout();
            // 
            // GenericUI
            // 
            this.GenericUI.AutoScroll = true;
            this.GenericUI.BackColor = System.Drawing.SystemColors.Control;
            this.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GenericUI.HelpText = "";
            this.GenericUI.Location = new System.Drawing.Point(0, 40);
            this.GenericUI.Name = "GenericUI";
            this.GenericUI.Size = new System.Drawing.Size(1020, 416);
            this.GenericUI.TabIndex = 4;
            // 
            // splitter2
            // 
            this.splitter2.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.splitter2.Location = new System.Drawing.Point(0, 456);
            this.splitter2.Name = "splitter2";
            this.splitter2.Size = new System.Drawing.Size(1020, 3);
            this.splitter2.TabIndex = 5;
            this.splitter2.TabStop = false;
            // 
            // DataPanel
            // 
            this.DataPanel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.DataPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.DataPanel.Location = new System.Drawing.Point(0, 459);
            this.DataPanel.Name = "DataPanel";
            this.DataPanel.Size = new System.Drawing.Size(1020, 354);
            this.DataPanel.TabIndex = 6;
            this.DataPanel.Resize += new System.EventHandler(this.DataPanel_Resize);
            // 
            // GraphDataUI
            // 
            this.Controls.Add(this.GenericUI);
            this.Controls.Add(this.splitter2);
            this.Controls.Add(this.DataPanel);
            this.Name = "GraphDataUI";
            this.Size = new System.Drawing.Size(1020, 813);
            this.Controls.SetChildIndex(this.DataPanel, 0);
            this.Controls.SetChildIndex(this.splitter2, 0);
            this.Controls.SetChildIndex(this.GenericUI, 0);
            this.ResumeLayout(false);

            }

        #endregion

        private VBGeneral.GenericUI GenericUI;
        private System.Windows.Forms.Splitter splitter2;
        private System.Windows.Forms.Panel DataPanel;
        }
    }
