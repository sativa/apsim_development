namespace APSRU.Howwet
    {
    partial class Home
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
            this.newSimulation = new System.Windows.Forms.LinkLabel();
            this.label22 = new System.Windows.Forms.Label();
            this.label36 = new System.Windows.Forms.Label();
            this.soilName = new System.Windows.Forms.TextBox();
            this.label37 = new System.Windows.Forms.Label();
            this.browseSoilFile = new System.Windows.Forms.LinkLabel();
            this.label82 = new System.Windows.Forms.Label();
            this.selectSoil = new System.Windows.Forms.LinkLabel();
            this.soilFileName = new System.Windows.Forms.TextBox();
            this.browseMetfile = new System.Windows.Forms.LinkLabel();
            this.txtMetFile = new System.Windows.Forms.TextBox();
            this.label83 = new System.Windows.Forms.Label();
            this.label84 = new System.Windows.Forms.Label();
            this.label85 = new System.Windows.Forms.Label();
            this.label86 = new System.Windows.Forms.Label();
            this.selectRegion = new System.Windows.Forms.ComboBox();
            this.label87 = new System.Windows.Forms.Label();
            this.nextStepButton = new System.Windows.Forms.Button();
            this.openSimulation = new System.Windows.Forms.LinkLabel();
            this.editMetfile = new System.Windows.Forms.LinkLabel();
            this.SuspendLayout();
            // 
            // newSimulation
            // 
            this.newSimulation.AutoSize = true;
            this.newSimulation.BackColor = System.Drawing.Color.DarkGreen;
            this.newSimulation.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.newSimulation.LinkColor = System.Drawing.Color.White;
            this.newSimulation.Location = new System.Drawing.Point(3, 188);
            this.newSimulation.Name = "newSimulation";
            this.newSimulation.Size = new System.Drawing.Size(123, 18);
            this.newSimulation.TabIndex = 0;
            this.newSimulation.TabStop = true;
            this.newSimulation.Text = "New Simulation";
            this.newSimulation.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.newSimulation_LinkClicked);
            // 
            // label22
            // 
            this.label22.AutoSize = true;
            this.label22.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label22.Location = new System.Drawing.Point(252, 293);
            this.label22.Name = "label22";
            this.label22.Size = new System.Drawing.Size(96, 18);
            this.label22.TabIndex = 103;
            this.label22.Text = "Soil name:";
            this.label22.UseWaitCursor = true;
            // 
            // label36
            // 
            this.label36.AutoSize = true;
            this.label36.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label36.Location = new System.Drawing.Point(253, 365);
            this.label36.Name = "label36";
            this.label36.Size = new System.Drawing.Size(77, 18);
            this.label36.TabIndex = 104;
            this.label36.Text = "Met file:";
            this.label36.UseWaitCursor = true;
            // 
            // soilName
            // 
            this.soilName.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilName.Location = new System.Drawing.Point(255, 317);
            this.soilName.Margin = new System.Windows.Forms.Padding(3, 1, 3, 1);
            this.soilName.Name = "soilName";
            this.soilName.Size = new System.Drawing.Size(382, 27);
            this.soilName.TabIndex = 105;
            this.soilName.UseWaitCursor = true;
            // 
            // label37
            // 
            this.label37.AutoSize = true;
            this.label37.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label37.ForeColor = System.Drawing.Color.DarkGreen;
            this.label37.Location = new System.Drawing.Point(224, 250);
            this.label37.Name = "label37";
            this.label37.Size = new System.Drawing.Size(26, 29);
            this.label37.TabIndex = 118;
            this.label37.Text = "1";
            this.label37.UseWaitCursor = true;
            // 
            // browseSoilFile
            // 
            this.browseSoilFile.AutoSize = true;
            this.browseSoilFile.Font = new System.Drawing.Font("Verdana", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.browseSoilFile.Location = new System.Drawing.Point(643, 250);
            this.browseSoilFile.Name = "browseSoilFile";
            this.browseSoilFile.Size = new System.Drawing.Size(241, 18);
            this.browseSoilFile.TabIndex = 117;
            this.browseSoilFile.TabStop = true;
            this.browseSoilFile.Text = "Click here to browse for soil file";
            this.browseSoilFile.UseWaitCursor = true;
            this.browseSoilFile.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.browseSoilFile_LinkClicked);
            // 
            // label82
            // 
            this.label82.AutoSize = true;
            this.label82.BackColor = System.Drawing.Color.White;
            this.label82.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label82.Location = new System.Drawing.Point(253, 216);
            this.label82.Name = "label82";
            this.label82.Size = new System.Drawing.Size(77, 18);
            this.label82.TabIndex = 115;
            this.label82.Text = "Soil file:";
            this.label82.UseWaitCursor = true;
            // 
            // selectSoil
            // 
            this.selectSoil.AutoSize = true;
            this.selectSoil.Font = new System.Drawing.Font("Verdana", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.selectSoil.Location = new System.Drawing.Point(643, 317);
            this.selectSoil.Name = "selectSoil";
            this.selectSoil.Size = new System.Drawing.Size(192, 18);
            this.selectSoil.TabIndex = 109;
            this.selectSoil.TabStop = true;
            this.selectSoil.Text = "Click here to select a soil";
            this.selectSoil.UseWaitCursor = true;
            this.selectSoil.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.selectSoil_LinkClicked);
            // 
            // soilFileName
            // 
            this.soilFileName.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilFileName.Location = new System.Drawing.Point(256, 247);
            this.soilFileName.Margin = new System.Windows.Forms.Padding(3, 1, 3, 1);
            this.soilFileName.Name = "soilFileName";
            this.soilFileName.Size = new System.Drawing.Size(381, 27);
            this.soilFileName.TabIndex = 116;
            this.soilFileName.UseWaitCursor = true;
            // 
            // browseMetfile
            // 
            this.browseMetfile.AutoSize = true;
            this.browseMetfile.Font = new System.Drawing.Font("Verdana", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.browseMetfile.Location = new System.Drawing.Point(643, 399);
            this.browseMetfile.Name = "browseMetfile";
            this.browseMetfile.Size = new System.Drawing.Size(223, 18);
            this.browseMetfile.TabIndex = 110;
            this.browseMetfile.TabStop = true;
            this.browseMetfile.Text = "Click here to select a met file";
            this.browseMetfile.UseWaitCursor = true;
            this.browseMetfile.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.browseMetfile_LinkClicked);
            // 
            // txtMetFile
            // 
            this.txtMetFile.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtMetFile.Location = new System.Drawing.Point(256, 396);
            this.txtMetFile.Margin = new System.Windows.Forms.Padding(3, 1, 3, 1);
            this.txtMetFile.Name = "txtMetFile";
            this.txtMetFile.Size = new System.Drawing.Size(381, 27);
            this.txtMetFile.TabIndex = 106;
            this.txtMetFile.UseWaitCursor = true;
            // 
            // label83
            // 
            this.label83.AutoSize = true;
            this.label83.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label83.ForeColor = System.Drawing.Color.DarkGreen;
            this.label83.Location = new System.Drawing.Point(223, 317);
            this.label83.Name = "label83";
            this.label83.Size = new System.Drawing.Size(26, 29);
            this.label83.TabIndex = 111;
            this.label83.Text = "2";
            this.label83.UseWaitCursor = true;
            // 
            // label84
            // 
            this.label84.AutoSize = true;
            this.label84.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label84.ForeColor = System.Drawing.Color.DarkGreen;
            this.label84.Location = new System.Drawing.Point(223, 394);
            this.label84.Name = "label84";
            this.label84.Size = new System.Drawing.Size(26, 29);
            this.label84.TabIndex = 112;
            this.label84.Text = "3";
            this.label84.UseWaitCursor = true;
            // 
            // label85
            // 
            this.label85.AutoSize = true;
            this.label85.Font = new System.Drawing.Font("Microsoft Sans Serif", 18F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label85.ForeColor = System.Drawing.Color.DarkGreen;
            this.label85.Location = new System.Drawing.Point(223, 462);
            this.label85.Name = "label85";
            this.label85.Size = new System.Drawing.Size(26, 29);
            this.label85.TabIndex = 113;
            this.label85.Text = "4";
            this.label85.UseWaitCursor = true;
            // 
            // label86
            // 
            this.label86.AutoSize = true;
            this.label86.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label86.Location = new System.Drawing.Point(643, 470);
            this.label86.Name = "label86";
            this.label86.Size = new System.Drawing.Size(295, 18);
            this.label86.TabIndex = 114;
            this.label86.Text = "Select your closest region from the list";
            this.label86.UseWaitCursor = true;
            // 
            // selectRegion
            // 
            this.selectRegion.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.selectRegion.FormattingEnabled = true;
            this.selectRegion.Location = new System.Drawing.Point(255, 467);
            this.selectRegion.Margin = new System.Windows.Forms.Padding(3, 1, 3, 1);
            this.selectRegion.Name = "selectRegion";
            this.selectRegion.Size = new System.Drawing.Size(382, 26);
            this.selectRegion.TabIndex = 108;
            this.selectRegion.UseWaitCursor = true;
            // 
            // label87
            // 
            this.label87.AutoSize = true;
            this.label87.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label87.Location = new System.Drawing.Point(253, 443);
            this.label87.Name = "label87";
            this.label87.Size = new System.Drawing.Size(71, 18);
            this.label87.TabIndex = 107;
            this.label87.Text = "Region:";
            this.label87.UseWaitCursor = true;
            // 
            // nextStepButton
            // 
            this.nextStepButton.Location = new System.Drawing.Point(782, 610);
            this.nextStepButton.Name = "nextStepButton";
            this.nextStepButton.Size = new System.Drawing.Size(109, 23);
            this.nextStepButton.TabIndex = 119;
            this.nextStepButton.Text = "Next Step >>";
            this.nextStepButton.UseVisualStyleBackColor = true;
            this.nextStepButton.Click += new System.EventHandler(this.nextStepButton_Click);
            // 
            // openSimulation
            // 
            this.openSimulation.AutoSize = true;
            this.openSimulation.BackColor = System.Drawing.Color.DarkGreen;
            this.openSimulation.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.openSimulation.LinkColor = System.Drawing.Color.White;
            this.openSimulation.Location = new System.Drawing.Point(3, 227);
            this.openSimulation.Name = "openSimulation";
            this.openSimulation.Size = new System.Drawing.Size(127, 18);
            this.openSimulation.TabIndex = 120;
            this.openSimulation.TabStop = true;
            this.openSimulation.Text = "Open simulation";
            this.openSimulation.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.openSimulation_LinkClicked);
            // 
            // editMetfile
            // 
            this.editMetfile.AutoSize = true;
            this.editMetfile.BackColor = System.Drawing.Color.DarkGreen;
            this.editMetfile.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.editMetfile.LinkColor = System.Drawing.Color.White;
            this.editMetfile.Location = new System.Drawing.Point(3, 261);
            this.editMetfile.Name = "editMetfile";
            this.editMetfile.Size = new System.Drawing.Size(129, 18);
            this.editMetfile.TabIndex = 121;
            this.editMetfile.TabStop = true;
            this.editMetfile.Text = "Edit your metfile";
            this.editMetfile.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.editMetfile_LinkClicked);
            // 
            // Home
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.Controls.Add(this.editMetfile);
            this.Controls.Add(this.openSimulation);
            this.Controls.Add(this.nextStepButton);
            this.Controls.Add(this.label22);
            this.Controls.Add(this.label36);
            this.Controls.Add(this.soilName);
            this.Controls.Add(this.label37);
            this.Controls.Add(this.browseSoilFile);
            this.Controls.Add(this.label82);
            this.Controls.Add(this.selectSoil);
            this.Controls.Add(this.soilFileName);
            this.Controls.Add(this.browseMetfile);
            this.Controls.Add(this.txtMetFile);
            this.Controls.Add(this.label83);
            this.Controls.Add(this.label84);
            this.Controls.Add(this.label85);
            this.Controls.Add(this.label86);
            this.Controls.Add(this.selectRegion);
            this.Controls.Add(this.label87);
            this.Controls.Add(this.newSimulation);
            this.Name = "Home";
            this.Controls.SetChildIndex(this.newSimulation, 0);
            this.Controls.SetChildIndex(this.label87, 0);
            this.Controls.SetChildIndex(this.selectRegion, 0);
            this.Controls.SetChildIndex(this.label86, 0);
            this.Controls.SetChildIndex(this.label85, 0);
            this.Controls.SetChildIndex(this.label84, 0);
            this.Controls.SetChildIndex(this.label83, 0);
            this.Controls.SetChildIndex(this.txtMetFile, 0);
            this.Controls.SetChildIndex(this.browseMetfile, 0);
            this.Controls.SetChildIndex(this.soilFileName, 0);
            this.Controls.SetChildIndex(this.selectSoil, 0);
            this.Controls.SetChildIndex(this.label82, 0);
            this.Controls.SetChildIndex(this.browseSoilFile, 0);
            this.Controls.SetChildIndex(this.label37, 0);
            this.Controls.SetChildIndex(this.soilName, 0);
            this.Controls.SetChildIndex(this.label36, 0);
            this.Controls.SetChildIndex(this.label22, 0);
            this.Controls.SetChildIndex(this.nextStepButton, 0);
            this.Controls.SetChildIndex(this.openSimulation, 0);
            this.Controls.SetChildIndex(this.editMetfile, 0);
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private System.Windows.Forms.LinkLabel newSimulation;
        private System.Windows.Forms.Label label22;
        private System.Windows.Forms.Label label36;
        private System.Windows.Forms.TextBox soilName;
        private System.Windows.Forms.Label label37;
        private System.Windows.Forms.LinkLabel browseSoilFile;
        private System.Windows.Forms.Label label82;
        private System.Windows.Forms.LinkLabel selectSoil;
        private System.Windows.Forms.TextBox soilFileName;
        private System.Windows.Forms.LinkLabel browseMetfile;
        private System.Windows.Forms.TextBox txtMetFile;
        private System.Windows.Forms.Label label83;
        private System.Windows.Forms.Label label84;
        private System.Windows.Forms.Label label85;
        private System.Windows.Forms.Label label86;
        private System.Windows.Forms.ComboBox selectRegion;
        private System.Windows.Forms.Label label87;
        private System.Windows.Forms.Button nextStepButton;
        private System.Windows.Forms.LinkLabel openSimulation;
        private System.Windows.Forms.LinkLabel editMetfile;
        }
    }
