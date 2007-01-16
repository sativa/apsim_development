namespace APSRU.Howwet
    {
    partial class FallowSettings
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
            this.label1 = new System.Windows.Forms.Label();
            this.label29 = new System.Windows.Forms.Label();
            this.runSimulation = new System.Windows.Forms.Button();
            this.label9 = new System.Windows.Forms.Label();
            this.label73 = new System.Windows.Forms.Label();
            this.coverEndPercent = new System.Windows.Forms.NumericUpDown();
            this.label78 = new System.Windows.Forms.Label();
            this.label23 = new System.Windows.Forms.Label();
            this.proposedCropList = new System.Windows.Forms.ComboBox();
            this.label77 = new System.Windows.Forms.Label();
            this.EndDatePicker = new System.Windows.Forms.DateTimePicker();
            this.coverCropList = new System.Windows.Forms.ComboBox();
            this.coverStartPercent = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.StartDatePicker = new System.Windows.Forms.DateTimePicker();
            this.initialWaterPercent = new System.Windows.Forms.NumericUpDown();
            this.label4 = new System.Windows.Forms.Label();
            this.label26 = new System.Windows.Forms.Label();
            this.initialWater = new System.Windows.Forms.TextBox();
            this.label13 = new System.Windows.Forms.Label();
            this.label31 = new System.Windows.Forms.Label();
            this.label21 = new System.Windows.Forms.Label();
            this.initialNitrogen = new System.Windows.Forms.TextBox();
            this.label61 = new System.Windows.Forms.Label();
            this.label20 = new System.Windows.Forms.Label();
            this.label18 = new System.Windows.Forms.Label();
            this.label19 = new System.Windows.Forms.Label();
            this.PAWC = new System.Windows.Forms.TextBox();
            this.label74 = new System.Windows.Forms.Label();
            this.ocDepthLabel = new System.Windows.Forms.Label();
            this.soilDepth = new System.Windows.Forms.TextBox();
            this.soilDepthLabel = new System.Windows.Forms.Label();
            this.organicCarbonContent = new System.Windows.Forms.TextBox();
            this.soilName = new System.Windows.Forms.Label();
            this.label17 = new System.Windows.Forms.Label();
            this.erosionOptions = new System.Windows.Forms.LinkLabel();
            this.editMetfile = new System.Windows.Forms.LinkLabel();
            this.rainfallAnalysis = new System.Windows.Forms.LinkLabel();
            this.line1 = new APSRU.UIControls.Line();
            this.viewMetdata = new System.Windows.Forms.LinkLabel();
            this.label3 = new System.Windows.Forms.Label();
            this.soilRegion = new System.Windows.Forms.Label();
            this.line2 = new APSRU.UIControls.Line();
            this.line3 = new APSRU.UIControls.Line();
            this.label6 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.coverEndPercent)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.coverStartPercent)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.initialWaterPercent)).BeginInit();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(95, 181);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(68, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "fallow setting";
            // 
            // label29
            // 
            this.label29.AutoSize = true;
            this.label29.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label29.Location = new System.Drawing.Point(188, 390);
            this.label29.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label29.Name = "label29";
            this.label29.Size = new System.Drawing.Size(156, 18);
            this.label29.TabIndex = 219;
            this.label29.Text = "Initial Soil Water:";
            // 
            // runSimulation
            // 
            this.runSimulation.Location = new System.Drawing.Point(803, 604);
            this.runSimulation.Margin = new System.Windows.Forms.Padding(2);
            this.runSimulation.Name = "runSimulation";
            this.runSimulation.Size = new System.Drawing.Size(114, 36);
            this.runSimulation.TabIndex = 231;
            this.runSimulation.Text = "Run Simulation";
            this.runSimulation.UseVisualStyleBackColor = true;
            this.runSimulation.Click += new System.EventHandler(this.runSimulation_Click);
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label9.Location = new System.Drawing.Point(188, 350);
            this.label9.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(173, 18);
            this.label9.TabIndex = 197;
            this.label9.Text = "Initial soil nitrogen:";
            // 
            // label73
            // 
            this.label73.AutoSize = true;
            this.label73.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label73.Location = new System.Drawing.Point(460, 236);
            this.label73.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label73.Name = "label73";
            this.label73.Size = new System.Drawing.Size(23, 20);
            this.label73.TabIndex = 225;
            this.label73.Text = "%";
            // 
            // coverEndPercent
            // 
            this.coverEndPercent.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.coverEndPercent.Location = new System.Drawing.Point(519, 578);
            this.coverEndPercent.Margin = new System.Windows.Forms.Padding(2);
            this.coverEndPercent.Name = "coverEndPercent";
            this.coverEndPercent.Size = new System.Drawing.Size(121, 26);
            this.coverEndPercent.TabIndex = 224;
            this.coverEndPercent.ValueChanged += new System.EventHandler(this.coverEndPercent_ValueChanged);
            // 
            // label78
            // 
            this.label78.AutoSize = true;
            this.label78.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label78.Location = new System.Drawing.Point(190, 507);
            this.label78.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label78.Name = "label78";
            this.label78.Size = new System.Drawing.Size(174, 18);
            this.label78.TabIndex = 229;
            this.label78.Text = "Stubble Cover Type:";
            // 
            // label23
            // 
            this.label23.AutoSize = true;
            this.label23.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label23.Location = new System.Drawing.Point(189, 451);
            this.label23.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label23.Name = "label23";
            this.label23.Size = new System.Drawing.Size(103, 20);
            this.label23.TabIndex = 227;
            this.label23.Text = "Crop to grow:";
            // 
            // proposedCropList
            // 
            this.proposedCropList.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.proposedCropList.FormattingEnabled = true;
            this.proposedCropList.Location = new System.Drawing.Point(382, 451);
            this.proposedCropList.Margin = new System.Windows.Forms.Padding(2);
            this.proposedCropList.Name = "proposedCropList";
            this.proposedCropList.Size = new System.Drawing.Size(74, 28);
            this.proposedCropList.TabIndex = 226;
            // 
            // label77
            // 
            this.label77.AutoSize = true;
            this.label77.Font = new System.Drawing.Font("Verdana", 13F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label77.Location = new System.Drawing.Point(161, 102);
            this.label77.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label77.Name = "label77";
            this.label77.Size = new System.Drawing.Size(141, 22);
            this.label77.TabIndex = 228;
            this.label77.Text = "Soil Properties";
            // 
            // EndDatePicker
            // 
            this.EndDatePicker.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.EndDatePicker.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.EndDatePicker.Location = new System.Drawing.Point(519, 614);
            this.EndDatePicker.Margin = new System.Windows.Forms.Padding(2);
            this.EndDatePicker.Name = "EndDatePicker";
            this.EndDatePicker.Size = new System.Drawing.Size(122, 26);
            this.EndDatePicker.TabIndex = 212;
            this.EndDatePicker.ValueChanged += new System.EventHandler(this.EndDatePicker_ValueChanged);
            // 
            // coverCropList
            // 
            this.coverCropList.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.coverCropList.FormattingEnabled = true;
            this.coverCropList.Location = new System.Drawing.Point(382, 503);
            this.coverCropList.Margin = new System.Windows.Forms.Padding(2);
            this.coverCropList.Name = "coverCropList";
            this.coverCropList.Size = new System.Drawing.Size(73, 28);
            this.coverCropList.TabIndex = 210;
            this.coverCropList.SelectedValueChanged += new System.EventHandler(this.coverCropList_SelectedValueChanged);
            // 
            // coverStartPercent
            // 
            this.coverStartPercent.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.coverStartPercent.Location = new System.Drawing.Point(382, 578);
            this.coverStartPercent.Margin = new System.Windows.Forms.Padding(2);
            this.coverStartPercent.Name = "coverStartPercent";
            this.coverStartPercent.Size = new System.Drawing.Size(116, 26);
            this.coverStartPercent.TabIndex = 209;
            this.coverStartPercent.ValueChanged += new System.EventHandler(this.coverStartPercent_ValueChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(192, 580);
            this.label2.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(126, 18);
            this.label2.TabIndex = 216;
            this.label2.Text = "Stubble cover:";
            // 
            // StartDatePicker
            // 
            this.StartDatePicker.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.StartDatePicker.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.StartDatePicker.Location = new System.Drawing.Point(382, 614);
            this.StartDatePicker.Margin = new System.Windows.Forms.Padding(2);
            this.StartDatePicker.Name = "StartDatePicker";
            this.StartDatePicker.Size = new System.Drawing.Size(116, 26);
            this.StartDatePicker.TabIndex = 211;
            this.StartDatePicker.ValueChanged += new System.EventHandler(this.StartDatePicker_ValueChanged);
            // 
            // initialWaterPercent
            // 
            this.initialWaterPercent.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.initialWaterPercent.Location = new System.Drawing.Point(382, 384);
            this.initialWaterPercent.Margin = new System.Windows.Forms.Padding(2);
            this.initialWaterPercent.Name = "initialWaterPercent";
            this.initialWaterPercent.Size = new System.Drawing.Size(74, 26);
            this.initialWaterPercent.TabIndex = 206;
            this.initialWaterPercent.ValueChanged += new System.EventHandler(this.initialWaterPercent_ValueChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(192, 614);
            this.label4.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(110, 18);
            this.label4.TabIndex = 213;
            this.label4.Text = "Fallow date:";
            // 
            // label26
            // 
            this.label26.AutoSize = true;
            this.label26.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label26.Location = new System.Drawing.Point(487, 386);
            this.label26.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label26.Name = "label26";
            this.label26.Size = new System.Drawing.Size(23, 20);
            this.label26.TabIndex = 222;
            this.label26.Text = "or";
            // 
            // initialWater
            // 
            this.initialWater.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.initialWater.Location = new System.Drawing.Point(519, 384);
            this.initialWater.Margin = new System.Windows.Forms.Padding(2);
            this.initialWater.Name = "initialWater";
            this.initialWater.Size = new System.Drawing.Size(68, 26);
            this.initialWater.TabIndex = 207;
            this.initialWater.TextChanged += new System.EventHandler(this.initialWater_TextChanged);
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label13.Location = new System.Drawing.Point(459, 349);
            this.label13.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(48, 20);
            this.label13.TabIndex = 202;
            this.label13.Text = "kg/ha";
            // 
            // label31
            // 
            this.label31.AutoSize = true;
            this.label31.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label31.Location = new System.Drawing.Point(459, 274);
            this.label31.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label31.Name = "label31";
            this.label31.Size = new System.Drawing.Size(35, 20);
            this.label31.TabIndex = 221;
            this.label31.Text = "mm";
            // 
            // label21
            // 
            this.label21.AutoSize = true;
            this.label21.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label21.Location = new System.Drawing.Point(459, 307);
            this.label21.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label21.Name = "label21";
            this.label21.Size = new System.Drawing.Size(35, 20);
            this.label21.TabIndex = 218;
            this.label21.Text = "mm";
            // 
            // initialNitrogen
            // 
            this.initialNitrogen.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.initialNitrogen.Location = new System.Drawing.Point(382, 343);
            this.initialNitrogen.Margin = new System.Windows.Forms.Padding(2);
            this.initialNitrogen.Name = "initialNitrogen";
            this.initialNitrogen.Size = new System.Drawing.Size(73, 26);
            this.initialNitrogen.TabIndex = 203;
            // 
            // label61
            // 
            this.label61.AutoSize = true;
            this.label61.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label61.Location = new System.Drawing.Point(188, 154);
            this.label61.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label61.Name = "label61";
            this.label61.Size = new System.Drawing.Size(62, 18);
            this.label61.TabIndex = 192;
            this.label61.Text = "Name:";
            // 
            // label20
            // 
            this.label20.AutoSize = true;
            this.label20.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label20.Location = new System.Drawing.Point(594, 387);
            this.label20.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label20.Name = "label20";
            this.label20.Size = new System.Drawing.Size(35, 20);
            this.label20.TabIndex = 217;
            this.label20.Text = "mm";
            // 
            // label18
            // 
            this.label18.AutoSize = true;
            this.label18.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label18.Location = new System.Drawing.Point(460, 390);
            this.label18.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label18.Name = "label18";
            this.label18.Size = new System.Drawing.Size(23, 20);
            this.label18.TabIndex = 204;
            this.label18.Text = "%";
            // 
            // label19
            // 
            this.label19.AutoSize = true;
            this.label19.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label19.Location = new System.Drawing.Point(188, 309);
            this.label19.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label19.Name = "label19";
            this.label19.Size = new System.Drawing.Size(64, 18);
            this.label19.TabIndex = 208;
            this.label19.Text = "PAWC:";
            // 
            // PAWC
            // 
            this.PAWC.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.PAWC.Location = new System.Drawing.Point(382, 305);
            this.PAWC.Margin = new System.Windows.Forms.Padding(2);
            this.PAWC.Name = "PAWC";
            this.PAWC.Size = new System.Drawing.Size(73, 26);
            this.PAWC.TabIndex = 201;
            // 
            // label74
            // 
            this.label74.AutoSize = true;
            this.label74.Location = new System.Drawing.Point(607, 384);
            this.label74.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label74.Name = "label74";
            this.label74.Size = new System.Drawing.Size(0, 13);
            this.label74.TabIndex = 193;
            // 
            // ocDepthLabel
            // 
            this.ocDepthLabel.AutoSize = true;
            this.ocDepthLabel.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ocDepthLabel.Location = new System.Drawing.Point(322, 236);
            this.ocDepthLabel.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.ocDepthLabel.Name = "ocDepthLabel";
            this.ocDepthLabel.Size = new System.Drawing.Size(48, 18);
            this.ocDepthLabel.TabIndex = 200;
            this.ocDepthLabel.Text = "layer";
            // 
            // soilDepth
            // 
            this.soilDepth.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilDepth.Location = new System.Drawing.Point(382, 268);
            this.soilDepth.Margin = new System.Windows.Forms.Padding(2);
            this.soilDepth.Name = "soilDepth";
            this.soilDepth.Size = new System.Drawing.Size(73, 26);
            this.soilDepth.TabIndex = 199;
            // 
            // soilDepthLabel
            // 
            this.soilDepthLabel.AutoSize = true;
            this.soilDepthLabel.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilDepthLabel.Location = new System.Drawing.Point(188, 272);
            this.soilDepthLabel.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.soilDepthLabel.Name = "soilDepthLabel";
            this.soilDepthLabel.Size = new System.Drawing.Size(98, 18);
            this.soilDepthLabel.TabIndex = 205;
            this.soilDepthLabel.Text = "Soil depth:";
            // 
            // organicCarbonContent
            // 
            this.organicCarbonContent.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.organicCarbonContent.Location = new System.Drawing.Point(382, 232);
            this.organicCarbonContent.Margin = new System.Windows.Forms.Padding(2);
            this.organicCarbonContent.Name = "organicCarbonContent";
            this.organicCarbonContent.Size = new System.Drawing.Size(73, 26);
            this.organicCarbonContent.TabIndex = 198;
            // 
            // soilName
            // 
            this.soilName.AutoSize = true;
            this.soilName.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilName.Location = new System.Drawing.Point(378, 154);
            this.soilName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.soilName.Name = "soilName";
            this.soilName.Size = new System.Drawing.Size(31, 20);
            this.soilName.TabIndex = 194;
            this.soilName.Text = "n/a";
            // 
            // label17
            // 
            this.label17.AutoSize = true;
            this.label17.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label17.Location = new System.Drawing.Point(188, 236);
            this.label17.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label17.Name = "label17";
            this.label17.Size = new System.Drawing.Size(130, 18);
            this.label17.TabIndex = 196;
            this.label17.Text = "Organic carbon";
            // 
            // erosionOptions
            // 
            this.erosionOptions.AutoSize = true;
            this.erosionOptions.BackColor = System.Drawing.Color.DarkGreen;
            this.erosionOptions.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.erosionOptions.LinkColor = System.Drawing.Color.White;
            this.erosionOptions.Location = new System.Drawing.Point(12, 203);
            this.erosionOptions.Name = "erosionOptions";
            this.erosionOptions.Size = new System.Drawing.Size(125, 18);
            this.erosionOptions.TabIndex = 232;
            this.erosionOptions.TabStop = true;
            this.erosionOptions.Text = "Erosion Options";
            this.erosionOptions.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.erosionOptions_LinkClicked);
            // 
            // editMetfile
            // 
            this.editMetfile.AutoSize = true;
            this.editMetfile.BackColor = System.Drawing.Color.DarkGreen;
            this.editMetfile.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.editMetfile.LinkColor = System.Drawing.Color.White;
            this.editMetfile.Location = new System.Drawing.Point(12, 244);
            this.editMetfile.Name = "editMetfile";
            this.editMetfile.Size = new System.Drawing.Size(90, 18);
            this.editMetfile.TabIndex = 233;
            this.editMetfile.TabStop = true;
            this.editMetfile.Text = "Edit metfile";
            this.editMetfile.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.editMetfile_LinkClicked);
            // 
            // rainfallAnalysis
            // 
            this.rainfallAnalysis.AutoSize = true;
            this.rainfallAnalysis.BackColor = System.Drawing.Color.DarkGreen;
            this.rainfallAnalysis.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rainfallAnalysis.LinkColor = System.Drawing.Color.White;
            this.rainfallAnalysis.Location = new System.Drawing.Point(12, 286);
            this.rainfallAnalysis.Name = "rainfallAnalysis";
            this.rainfallAnalysis.Size = new System.Drawing.Size(122, 18);
            this.rainfallAnalysis.TabIndex = 234;
            this.rainfallAnalysis.TabStop = true;
            this.rainfallAnalysis.Text = "Rainfall analysis";
            // 
            // line1
            // 
            this.line1.LineColour = System.Drawing.Color.DarkGreen;
            this.line1.LineThickness = 5;
            this.line1.LineTransparent = 50;
            this.line1.Location = new System.Drawing.Point(166, 127);
            this.line1.Name = "line1";
            this.line1.Size = new System.Drawing.Size(842, 10);
            this.line1.TabIndex = 235;
            // 
            // viewMetdata
            // 
            this.viewMetdata.AutoSize = true;
            this.viewMetdata.BackColor = System.Drawing.Color.DarkGreen;
            this.viewMetdata.Font = new System.Drawing.Font("Verdana", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.viewMetdata.ForeColor = System.Drawing.Color.Black;
            this.viewMetdata.LinkColor = System.Drawing.Color.White;
            this.viewMetdata.Location = new System.Drawing.Point(12, 330);
            this.viewMetdata.Name = "viewMetdata";
            this.viewMetdata.Size = new System.Drawing.Size(134, 18);
            this.viewMetdata.TabIndex = 236;
            this.viewMetdata.TabStop = true;
            this.viewMetdata.Text = "View Met Graphs";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(188, 192);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(71, 18);
            this.label3.TabIndex = 238;
            this.label3.Text = "Region:";
            // 
            // soilRegion
            // 
            this.soilRegion.AutoSize = true;
            this.soilRegion.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.soilRegion.Location = new System.Drawing.Point(379, 192);
            this.soilRegion.Name = "soilRegion";
            this.soilRegion.Size = new System.Drawing.Size(35, 18);
            this.soilRegion.TabIndex = 239;
            this.soilRegion.Text = "n/a";
            // 
            // line2
            // 
            this.line2.LineColour = System.Drawing.Color.DarkGreen;
            this.line2.LineThickness = 5;
            this.line2.LineTransparent = 50;
            this.line2.Location = new System.Drawing.Point(166, 436);
            this.line2.Name = "line2";
            this.line2.Size = new System.Drawing.Size(842, 10);
            this.line2.TabIndex = 240;
            // 
            // line3
            // 
            this.line3.LineColour = System.Drawing.Color.DarkGreen;
            this.line3.LineThickness = 5;
            this.line3.LineTransparent = 50;
            this.line3.Location = new System.Drawing.Point(166, 488);
            this.line3.Name = "line3";
            this.line3.Size = new System.Drawing.Size(842, 10);
            this.line3.TabIndex = 241;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(379, 549);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(48, 18);
            this.label6.TabIndex = 242;
            this.label6.Text = "Start";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Font = new System.Drawing.Font("Verdana", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(516, 549);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(38, 18);
            this.label7.TabIndex = 243;
            this.label7.Text = "End";
            // 
            // FallowSettings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.line3);
            this.Controls.Add(this.line2);
            this.Controls.Add(this.soilRegion);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.viewMetdata);
            this.Controls.Add(this.line1);
            this.Controls.Add(this.rainfallAnalysis);
            this.Controls.Add(this.editMetfile);
            this.Controls.Add(this.erosionOptions);
            this.Controls.Add(this.label29);
            this.Controls.Add(this.runSimulation);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.label73);
            this.Controls.Add(this.coverEndPercent);
            this.Controls.Add(this.label78);
            this.Controls.Add(this.label23);
            this.Controls.Add(this.proposedCropList);
            this.Controls.Add(this.label77);
            this.Controls.Add(this.EndDatePicker);
            this.Controls.Add(this.coverCropList);
            this.Controls.Add(this.coverStartPercent);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.StartDatePicker);
            this.Controls.Add(this.initialWaterPercent);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label26);
            this.Controls.Add(this.initialWater);
            this.Controls.Add(this.label13);
            this.Controls.Add(this.label31);
            this.Controls.Add(this.label21);
            this.Controls.Add(this.initialNitrogen);
            this.Controls.Add(this.label61);
            this.Controls.Add(this.label20);
            this.Controls.Add(this.label18);
            this.Controls.Add(this.label19);
            this.Controls.Add(this.PAWC);
            this.Controls.Add(this.label74);
            this.Controls.Add(this.ocDepthLabel);
            this.Controls.Add(this.soilDepth);
            this.Controls.Add(this.soilDepthLabel);
            this.Controls.Add(this.organicCarbonContent);
            this.Controls.Add(this.soilName);
            this.Controls.Add(this.label17);
            this.Name = "FallowSettings";
            this.Controls.SetChildIndex(this.label17, 0);
            this.Controls.SetChildIndex(this.soilName, 0);
            this.Controls.SetChildIndex(this.organicCarbonContent, 0);
            this.Controls.SetChildIndex(this.soilDepthLabel, 0);
            this.Controls.SetChildIndex(this.soilDepth, 0);
            this.Controls.SetChildIndex(this.ocDepthLabel, 0);
            this.Controls.SetChildIndex(this.label74, 0);
            this.Controls.SetChildIndex(this.PAWC, 0);
            this.Controls.SetChildIndex(this.label19, 0);
            this.Controls.SetChildIndex(this.label18, 0);
            this.Controls.SetChildIndex(this.label20, 0);
            this.Controls.SetChildIndex(this.label61, 0);
            this.Controls.SetChildIndex(this.initialNitrogen, 0);
            this.Controls.SetChildIndex(this.label21, 0);
            this.Controls.SetChildIndex(this.label31, 0);
            this.Controls.SetChildIndex(this.label13, 0);
            this.Controls.SetChildIndex(this.initialWater, 0);
            this.Controls.SetChildIndex(this.label26, 0);
            this.Controls.SetChildIndex(this.label4, 0);
            this.Controls.SetChildIndex(this.initialWaterPercent, 0);
            this.Controls.SetChildIndex(this.StartDatePicker, 0);
            this.Controls.SetChildIndex(this.label2, 0);
            this.Controls.SetChildIndex(this.coverStartPercent, 0);
            this.Controls.SetChildIndex(this.coverCropList, 0);
            this.Controls.SetChildIndex(this.EndDatePicker, 0);
            this.Controls.SetChildIndex(this.label77, 0);
            this.Controls.SetChildIndex(this.proposedCropList, 0);
            this.Controls.SetChildIndex(this.label23, 0);
            this.Controls.SetChildIndex(this.label78, 0);
            this.Controls.SetChildIndex(this.coverEndPercent, 0);
            this.Controls.SetChildIndex(this.label73, 0);
            this.Controls.SetChildIndex(this.label9, 0);
            this.Controls.SetChildIndex(this.runSimulation, 0);
            this.Controls.SetChildIndex(this.label29, 0);
            this.Controls.SetChildIndex(this.erosionOptions, 0);
            this.Controls.SetChildIndex(this.editMetfile, 0);
            this.Controls.SetChildIndex(this.rainfallAnalysis, 0);
            this.Controls.SetChildIndex(this.line1, 0);
            this.Controls.SetChildIndex(this.viewMetdata, 0);
            this.Controls.SetChildIndex(this.label3, 0);
            this.Controls.SetChildIndex(this.soilRegion, 0);
            this.Controls.SetChildIndex(this.line2, 0);
            this.Controls.SetChildIndex(this.line3, 0);
            this.Controls.SetChildIndex(this.label6, 0);
            this.Controls.SetChildIndex(this.label7, 0);
            ((System.ComponentModel.ISupportInitialize)(this.coverEndPercent)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.coverStartPercent)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.initialWaterPercent)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label29;
        private System.Windows.Forms.Button runSimulation;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label73;
        private System.Windows.Forms.NumericUpDown coverEndPercent;
        private System.Windows.Forms.Label label78;
        private System.Windows.Forms.Label label23;
        private System.Windows.Forms.ComboBox proposedCropList;
        private System.Windows.Forms.Label label77;
        private System.Windows.Forms.DateTimePicker EndDatePicker;
        private System.Windows.Forms.ComboBox coverCropList;
        private System.Windows.Forms.NumericUpDown coverStartPercent;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.DateTimePicker StartDatePicker;
        private System.Windows.Forms.NumericUpDown initialWaterPercent;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label26;
        private System.Windows.Forms.TextBox initialWater;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label label31;
        private System.Windows.Forms.Label label21;
        private System.Windows.Forms.TextBox initialNitrogen;
        private System.Windows.Forms.Label label61;
        private System.Windows.Forms.Label label20;
        private System.Windows.Forms.Label label18;
        private System.Windows.Forms.Label label19;
        private System.Windows.Forms.TextBox PAWC;
        private System.Windows.Forms.Label label74;
        private System.Windows.Forms.Label ocDepthLabel;
        private System.Windows.Forms.TextBox soilDepth;
        private System.Windows.Forms.Label soilDepthLabel;
        private System.Windows.Forms.TextBox organicCarbonContent;
        private System.Windows.Forms.Label soilName;
        private System.Windows.Forms.Label label17;
        private System.Windows.Forms.LinkLabel erosionOptions;
        private System.Windows.Forms.LinkLabel editMetfile;
        private System.Windows.Forms.LinkLabel rainfallAnalysis;
        private APSRU.UIControls.Line line1;
        private System.Windows.Forms.LinkLabel viewMetdata;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label soilRegion;
        private APSRU.UIControls.Line line2;
        private APSRU.UIControls.Line line3;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label7;

        }
    }
