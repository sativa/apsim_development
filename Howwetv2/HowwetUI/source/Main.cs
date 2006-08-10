using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using CSGeneral;
using APSRU.Model.Howwet;
using APSRU.Error;
using Xceed.Chart.Utilities;
using Xceed.Chart.Standard;
using Xceed.Chart.GraphicsCore;
using Xceed.Chart;
using Xceed.Chart.Core;
using System.Diagnostics;
using APSRU.Translator.Howwet;


namespace APSRU.Howwet
    {
    
    public partial class Main : Form
        {
        public Main()
            {
            Xceed.Chart.Licenser.LicenseKey = "CHT40-N4AAF-77UTD-6ANA";
            InitializeComponent();
            }
        #region init
        private SimulationIn simulationObject;
        private HowwetUtility util;
        private HowwetConfiguration config;
        public MetData metObject;
        public String soilsFileName="";
        private InitWater initialWater;
        private InitNitrogen initialNitrogen;
        private ArrayList coverCrops;
        int rowCount = 0;
        private double soilPAWCSumOriginal;
        private double soilDepthSumOriginal;
        private double soilNitrogenSumOriginal;
        private double[] thicknessOriginal;
        private double[] LL15Original;

        DateTime[] date;
        DataTable dt;
        double[] esw,rainfall, som, runoff, evaporation, soilWaterTopLayer, no3Total, maxTemp, soilLoss;
        Xceed.Chart.Core.Chart chart;
        ToolTip toolTip1 = new System.Windows.Forms.ToolTip();
        BarSeries rainBar;

        #endregion

        #region Startup

        private void Main_Load(object sender, EventArgs e)
            {

            

            toolStripStatusLabel1.Text = "Select a Soil";
            config = new HowwetConfiguration();
            util = new HowwetUtility();
            simulationObject = new SimulationIn(util.ReadTemplateFile(config.TemplateFileName));
            simulationObject.FileName = "untitled";
            
            FileInfo fileInfo = new FileInfo(config.DefaultSoilFileName);
            if (fileInfo.Exists)
                {
                soilFileName.Text = fileInfo.Name;
                this.soilsFileName = fileInfo.FullName;
                toolStripStatusLabel1.Text = "Select a Soil";
                toolTip1.Show("tes", this, 1000);
                }
            else
                {
                soilFileName.Text = "<Select a Soil file>";
                toolStripStatusLabel1.Text = "Select a Soil file";
                }
            resetFormValues();
            }
        
        private void resetFormValues()
            {
                        
            //setup default vaule on form
            slope.Text = simulationObject.ErosionSlope;
            slopeLength.Text = simulationObject.ErosionSlopeLength;
            erodibilty.Text = simulationObject.ErosionErodibilty;
           
            this.Text = simulationObject.FileName;

            //setup chart defaults
            chart = chartControl1.Charts[0];
            chart.View.SetPredefinedProjection(PredefinedProjection.Orthogonal);
            chart.MarginMode = MarginMode.Stretch;
            chart.Margins = new RectangleF(10, 10, 80, 80);
            //chart.Axis(StandardAxis.PrimaryX).ScaleMode = AxisScaleMode.DateTime;
            Axis axis = chart.Axis(StandardAxis.PrimaryX);
            axis.DateTimeScale.MajorTickMode = MajorTickModeDateTime.Months;
            axis.DateTimeScale.MonthsStep = 3;
            rainBar = (BarSeries)chart.Series.Add(SeriesType.Bar);
            //  area.UseXValues = true;
            rainBar.Name = "Rainfall";
            rainBar.DataLabels.Mode = DataLabelsMode.None;
            rainBar.Values.ValueFormatting.Format = ValueFormat.CustomNumber;
            rainBar.Values.ValueFormatting.CustomFormat = "0.0";
            //  bar.AreaBorder.Color = Color.DarkBlue;
            rainBar.Appearance.FillMode = AppearanceFillMode.Series;
            //  bar.AreaFillEffect.SetSolidColor(Color.Aqua);
            rainBar.Values.EmptyDataPoints.ValueMode = EmptyDataPointsValueMode.Skip;

            
            toolTip1.SetToolTip(selectSoilButton, "This is tooltip test");
            toolTip1.Active = true;
           
            
            }
        #endregion

        #region Tool Strip events

        private void RunButton_Click(object sender, EventArgs e)
            {
            displayProposedCropList();
            if (saveAllData())
                {
                //check if apsrun is already running
                Process[] processes = Process.GetProcesses();
                foreach (Process proc in processes)
                    {
                    if (Path.GetFileName(proc.ProcessName) == "apsrun")
                        {
                        proc.Kill();
                        }
                    }

                 //run apsun
                //String ApsRunFileName = Path.GetDirectoryName(Application.ExecutablePath) + "\\apsrun.exe";
                String ApsRunFileName = "c:\\Program Files\\APSIM51\\bin\\apsrun.exe";
                try
                    {
                    if (File.Exists(ApsRunFileName))
                        {
                        Process.Start(ApsRunFileName, "" + simulationObject.FileName + "");
                        }
                    toolStripStatusLabel1.Text = "Output values and charts from the simluation are displayed on the right side of the page.";
                    }
                catch (FileNotFoundException e1)
                    {
                    MessageBox.Show(e1.Message);
                    }
                }
            }


        private void toolStripButton1_Click(object sender, EventArgs e)
            {
            if (saveAllData())
                {
                String ApsimToSimPath = "c:\\Program Files\\APSIM51\\bin\\apsimtosim.exe";
                String Apsim = "c:\\Program Files\\APSIM51\\bin\\apsim.exe";
                String test = "c:\\robert\\data\\" + simulationObject.FileName;       
                try
                    {
                    if (File.Exists(ApsimToSimPath))
                        {
                        Process.Start(ApsimToSimPath, "" + test + "");
                        }

                    Process[] processes = Process.GetProcesses();
                    foreach (Process proc in processes)
                        {
                        if (Path.GetFileName(proc.ProcessName) == "apsimtosim")
                            {
                            while(proc.WaitForExit(300000)); //5 min
                            if (!proc.Responding) proc.Kill();
                            }
                        }


                    if (File.Exists(Apsim))
                        {
                        Process.Start(Apsim, "" + simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim") - 1) + ".sim" + "");
                        }
                    processes = Process.GetProcesses();
                    foreach (Process proc in processes)
                        {
                        if (Path.GetFileName(proc.ProcessName) == "apsim")
                            {
                            while (proc.WaitForExit(300000)) ; //5 min
                            if (!proc.Responding) proc.Kill();
                            }
                        }
                    }
                catch (FileNotFoundException e1)
                    {
                    MessageBox.Show(e1.Message);
                    }
                }

            }
        private void ReportButton_Click_1(object sender, EventArgs e)
            {
            //TODO change path to a application variable
            String path = "C:\\Development\\Howwetv2\\HowwetUI\\source";
            System.Diagnostics.Process.Start("IExplore.exe", path + "\\HowwetReport.xml");
            }

        private void openToolStripButton_Click(object sender, EventArgs e)
            {
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.Title = "Browse for Howwet File";
            openDialog.Filter = "Howwet files (*.hwt)|*.hwt";
            openDialog.ShowDialog();
            if (!(openDialog.FileName == ""))
                {
                APSIMData appsimDataObject = new APSIMData();
                appsimDataObject.LoadFromFile(openDialog.FileName);
                simulationObject.Data = appsimDataObject;
                }
            }

        private void saveToolStripButton_Click(object sender, EventArgs e)
            {
            APSIMData appsimDataObject = new APSIMData();
            appsimDataObject = simulationObject.Data;
            if (simulationObject.FileName == "untitled")
                {
                saveAs();
                }
            else
                {
                String fileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim") - 1) + ".hwt";
                appsimDataObject.SaveToFile(fileName);

                }
            }

        private void saveAsToolStripButton_Click(object sender, EventArgs e)
            {
            saveAs();
            }

        private void saveAs()
            {
            SaveFileDialog saveDialog = new SaveFileDialog();
            saveDialog.Title = "Save Howwet File";
            saveDialog.Filter = "Howwet files (*.hwt)|*.hwt";
            if (saveDialog.ShowDialog() == DialogResult.OK)
                {
                String howwetFile = saveDialog.FileName;
                APSIMData dataObject = new APSIMData();
                dataObject = simulationObject.Data;
                dataObject.SaveToFile(howwetFile);
                }
            }


                #endregion

        #region Events Inputside of Form

        private void browseSoilFileButton_Click(object sender, EventArgs e)
            {
            try
                {
                OpenFileDialog openDialog = new OpenFileDialog();
                openDialog.Title = "Browse for Soil File";
                openDialog.Filter = "Soils files (*.soils)|*.soils";
                openDialog.ShowDialog();
                if (!(openDialog.FileName == ""))
                    {
                    FileInfo fileInfo = new FileInfo(openDialog.FileName);
                    soilFileName.Text = fileInfo.Name;
                    this.soilsFileName = fileInfo.Name;

                    SoilSelection soilForm = new SoilSelection(this.soilsFileName);
                    soilForm.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                    soilForm.Show();
                    }
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }
        
        private void selectSoilButton_Click(object sender, EventArgs e)
            {
            try
                {
                if (!(this.soilsFileName == ""))
                    {
                    SoilSelection soilForm = new SoilSelection(this.soilsFileName);
                    soilForm.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                    soilForm.Show();
                    }
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }

        void soilForm_SoilSelectedEvent(APSIMData soil)
            {
            try
                {
                //add soil to simulation object
                simulationObject.AddSoil(soil);
                initialWater = simulationObject.Soil.InitialWater;
                initialNitrogen = simulationObject.Soil.InitialNitrogen;

                selectedSoilLabel.Text = soil.Name;
                String[] layers = simulationObject.Soil.DepthStrings;
                ocDepthLabel.Text = layers[0];//top layer string
                organicCarbonContent.Text = simulationObject.Soil.OC.GetValue(0).ToString();

                soilDepthSumOriginal = MathUtility.Sum(simulationObject.Soil.Thickness);
                soilPAWCSumOriginal = MathUtility.Sum(simulationObject.Soil.PAWC());
                soilNitrogenSumOriginal = MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa);

                LL15Original = simulationObject.Soil.LL15;
                thicknessOriginal = simulationObject.Soil.Thickness;

                soilDepth.Text = soilDepthSumOriginal.ToString("f0");
                initialWaterCapacity.Text = soilPAWCSumOriginal.ToString("f0");
                waterCapacity.Text = soilPAWCSumOriginal.ToString("f0");
                initialSoilNitrogen.Text = soilNitrogenSumOriginal.ToString("f0");
                displayCoverCropList();

                initialSoilWaterPercent.Value = 100;
                coverPercent.Maximum = 99;
                coverPercent.Minimum = 0;
                coverPercent.Value = 99;
                toolStripStatusLabel1.Text = "Select a Met file";
                
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }


        private void soilDepth_Leave(object sender, EventArgs e)
            {
            try
                {
                if (!(soilDepth.Text == ""))
                    {
                    if (Convert.ToDouble(soilDepth.Text) > soilDepthSumOriginal)
                        {
                        soilDepth.Text = Convert.ToString(soilDepthSumOriginal);
                        simulationObject.Soil.LL15 = LL15Original;
                        simulationObject.Soil.Thickness = thicknessOriginal;
                        }
                    //cut off soil
                    simulationObject.Soil.ApplyMaxSoilDepth(Convert.ToInt16(soilDepth.Text));

                    waterCapacity.Text = MathUtility.Sum(simulationObject.Soil.PAWC()).ToString("f0");
                    initialWaterCapacity.Text = MathUtility.Sum(simulationObject.Soil.PAWC()).ToString("f0");
                    }
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }

        private void waterCapacity_Leave(object sender, EventArgs e)
            {
            try
                {
                if (!(waterCapacity.Text == ""))
                    {
                    if (Convert.ToDouble(waterCapacity.Text) > soilPAWCSumOriginal)
                        {
                        waterCapacity.Text = soilPAWCSumOriginal.ToString("f0");
                        simulationObject.Soil.LL15 = LL15Original;
                        simulationObject.Soil.Thickness = thicknessOriginal;
                        }
                    //adjust water capacity
                    simulationObject.Soil.ApplyMaxWaterCapacity(Convert.ToInt16(waterCapacity.Text));
                    //display new values
                    initialWaterCapacity.Text = MathUtility.Sum(simulationObject.Soil.PAWC()).ToString("f0");
                    }
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }

        void initialSoilWaterPercent_ValueChanged(object sender, System.EventArgs e)
            {
            try
                {
                int percent = Convert.ToInt32(initialSoilWaterPercent.Value);
                initialWater.SetUsingPercent(percent,true);

                double Proportion = Convert.ToInt32(initialSoilWaterPercent.Value) / 100.0;
                double AmountWater = MathUtility.Sum(simulationObject.Soil.PAWC()) * Proportion;

                this.initialWaterCapacity.TextChanged -= new System.EventHandler(this.initialWaterCapacity_TextChanged);
                initialWaterCapacity.Text = AmountWater.ToString("f0");
                this.initialWaterCapacity.TextChanged += new System.EventHandler(this.initialWaterCapacity_TextChanged);
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }

        void initialWaterCapacity_TextChanged(object sender, System.EventArgs e)
            {
            try
                {
                double TotalPAWC = MathUtility.Sum(simulationObject.Soil.PAWC());
                int percent = 0;
                if (initialWaterCapacity.Text != "")
                    percent = Convert.ToInt32(Convert.ToDouble(initialWaterCapacity.Text) / TotalPAWC * 100);
                percent = Math.Min(percent, 100);
                percent = Math.Max(percent, 0);

                this.initialSoilWaterPercent.ValueChanged -= new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
                initialSoilWaterPercent.Value = percent;
                this.initialSoilWaterPercent.ValueChanged += new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
                initialWater.SetUsingPercent(percent, true);
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }

        private void initialSoilNitrogen_Leave(object sender, EventArgs e)
            {
            if (!(initialSoilNitrogen.Text == ""))
                {
                if (Convert.ToDouble(initialSoilNitrogen.Text) > soilNitrogenSumOriginal)
                    {
                    initialSoilNitrogen.Text = soilNitrogenSumOriginal.ToString("f0");
                    }
                else
                    {
                    initialNitrogen.TotalNO3KgHa = Convert.ToDouble(initialSoilNitrogen.Text);
                    }
                }
            }

        private void browseMetButton_Click(object sender, EventArgs e)
            {
            try
              {
                //Select a met file
                OpenFileDialog fdlg = new OpenFileDialog();
                fdlg.Title = "Browse for Met File";
                fdlg.Filter = "Met files (*.met)|*.met";
                fdlg.ShowDialog();
                if (!(fdlg.FileName == ""))
                {
                    //hydrate metObject       
                    metObject = new MetData(fdlg.FileName);
                    txtMetFile.Text = metObject.FileName;
                    //set metfileName in simulation object
                    simulationObject.MetFileName = metObject.FileName;
                    //set datetime picker
                    StartDatePicker.MinDate = metObject.StartDate;
                    if (!(metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0)) < metObject.StartDate))//400 days
                        {
                        StartDatePicker.Value = metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0));//400 days
                        }
                    else
                        {
                        StartDatePicker.Value = metObject.StartDate;
                        }
                    EndDatePicker.MaxDate = metObject.EndDate;
                    EndDatePicker.Value = metObject.EndDate;
                    toolStripStatusLabel1.Text = "Check soil Water and Nitrogen values are correct, then run the simulation";
                }
               }
            
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                }
            }


        //Populate combo list with corps from the setup  file
        private void displayCoverCropList()
            {
            coverCropList.Items.Clear();
            coverCrops  = config.CropList;
            for (int i = 0; i < coverCrops.Count; i++)
                {
                CoverCrop crop = (CoverCrop)coverCrops[i];
                coverCropList.Items.Add(crop.Name);
                }
            this.coverCropList.SelectedValueChanged -= new System.EventHandler(this.coverCropList_SelectedValueChanged);
            coverCropList.SelectedIndex = 0;
            this.coverCropList.SelectedValueChanged += new System.EventHandler(this.coverCropList_SelectedValueChanged);
            }
        

        
        //On selecting the cover crop 
        private void coverCropList_SelectedValueChanged(object sender, EventArgs e)
            {
            String selectedCrop = (String)coverCropList.SelectedItem;
            CoverCrop crop = util.GetCrop(coverCrops, selectedCrop);
            simulationObject.SOMType = crop.Name;
            simulationObject.SOMMass = Convert.ToString(util.ConvertCoverPercentToKg((coverPercent.Value/100), crop.SpecificArea));
            simulationObject.SOMCNRatio = Convert.ToString(crop.Cnr);
            }

        private void coverPercent_ValueChanged(object sender, EventArgs e)
            {
            decimal percent = coverPercent.Value;
       //     MessageBox.Show(percent.ToString());
            String selectedCrop = (String)coverCropList.SelectedItem;
            CoverCrop crop = util.GetCrop(coverCrops, selectedCrop);
            simulationObject.SOMMass = Convert.ToString(util.ConvertCoverPercentToKg(percent / 100, crop.SpecificArea));
            }
       
      

        private void editRainfallButton_Click(object sender, EventArgs e)
            {
            if (!(this.metObject.FileName == ""))
                {
                RainfallEditor form = new RainfallEditor();
                form.displayData(this.metObject);
                form.Show();
                }
            else
                {
                MessageBox.Show("Please select a Met file to edit");
                }
            }
       
        private void StartDatePicker_ValueChanged(object sender, EventArgs e)
            {
            simulationObject.StartDate = StartDatePicker.Value.ToShortDateString();
            }

        private void EndDatePicker_ValueChanged(object sender, EventArgs e)
            {
            simulationObject.EndDate = EndDatePicker.Value.ToShortDateString();
            }

        #endregion

        #region Events Outputside of Form

        //Populate combo list with corps from the soil file
        private void displayProposedCropList()
            {
            proposedCropList.Items.Clear();
            String[] crops = simulationObject.Soil.Crops;
            proposedCropList.Items.Add("<Select a Crop>");
            for (int i = 0; i < crops.Length; i++)
                {
                proposedCropList.Items.Add(crops[i]);
                }
            this.proposedCropList.SelectedValueChanged -= new System.EventHandler(this.proposedCropList_SelectedValueChanged);
            proposedCropList.SelectedIndex = 0;
            this.proposedCropList.SelectedValueChanged += new System.EventHandler(this.proposedCropList_SelectedValueChanged);
            }

        //After selecting a proposed corp subtract the corp from the esw to get PAWC 
        private void proposedCropList_SelectedValueChanged(object sender, System.EventArgs e)
            {
            if (!((String)proposedCropList.SelectedItem == "<Select a Crop>"))
                {
                String selectedCrop = (String)proposedCropList.SelectedItem;
                }
            }

        private void showAPSIMFileButton_Click(object sender, EventArgs e)
            {
            APSIMData test = new APSIMData();
            test = simulationObject.Data;
            MessageBox.Show(test.XML);
            }

        private void tabControl1_Click(object sender, EventArgs e)
            {
                TabControl tabControl= (TabControl) sender;
                switch (tabControl.SelectedIndex)
                    {
                    case 0:
                        displayRainfallChart();
                        break;
                    case 1:
                       // displayTestChart();
                        break;
                    }
            }

       private void button1_Click(object sender, EventArgs e)
            {
            SimulationOut outputObject = new SimulationOut(simulationObject.OutputFileName);
            gainNitrate.Text = outputObject.Nitrate.ToString();
            gainSoilWater.Text = outputObject.SoilWater.ToString();
            
            dt = outputObject.Data;
            int i = 0;
            date = new DateTime[dt.Rows.Count ];
            rainfall = new double[dt.Rows.Count];
            esw = new double[dt.Rows.Count];
            som = new double[dt.Rows.Count];
            runoff = new double[dt.Rows.Count];
            evaporation = new double[dt.Rows.Count];
            soilWaterTopLayer = new double[dt.Rows.Count];
            no3Total = new double[dt.Rows.Count];
            maxTemp = new double[dt.Rows.Count];
           
            soilLoss = new double[dt.Rows.Count];

            foreach(DataRow row in dt.Rows)
                {
                esw[i] = Convert.ToDouble(row["ExtractableSoilWater"]);
                DateTime d = new DateTime();
                d = (DateTime)row["Date"];
                date[i] = d;
                rainfall[i] = Convert.ToDouble(row["Rainfall"]);
                som[i] = Convert.ToDouble(row["SurfaceOrganicMatter"]);
                runoff[i] = Convert.ToDouble(row["Runoff"]);
                evaporation[i] = Convert.ToDouble(row["Evapoation"]);
                soilWaterTopLayer[i] = Convert.ToDouble(row["SoilWaterTopLayer"]);
                no3Total[i] = Convert.ToDouble(row["NO3Total"]);
                maxTemp[i] = Convert.ToDouble(row["MaxTemp"]);
                soilLoss[i] = Convert.ToDouble(row["SoilLoss"]);
                i++;
                }
            rowCount=0;
            timer1.Interval = 100;
            timer1.Start();
            chartControl1.Refresh();
            chartControl1.Visible = true;
            }

        public void timer1_Tick(object sender, EventArgs e)
            {
            //Console.WriteLine("tick");
            if (rowCount <= (dt.Rows.Count-1 ))
                {
              //  rainBar.Add(rainfall[rowCount], date[rowCount].ToShortDateString());
                rainBar.Add(rainfall[rowCount], date[rowCount].ToString());
                rowCount++;
                chartControl1.Refresh();
                }
            else
                {
                timer1.Stop();
                }
            }
        #endregion

        #region General Functions

        private void showExceptionMessages(Exception err)
            {
            if (config.Debug)
                {
                MessageBox.Show("An Error occurred: "+err.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            else
                {
                MessageBox.Show("An Error occurred","Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        private void showCustomExceptionMessages(CustomException err)
            {
            String displayString = "";
            if (config.Debug)
                {//show all info
                IEnumerator errorList = (IEnumerator)err.getErrors().GetEnumerator();
                while (errorList.MoveNext())
                    {
                    CustomError error = (CustomError)errorList.Current;
                    displayString = displayString + "\nError #: " + error.ErrorNumber + "\nPub Message: " + error.PublicMessage;
                    displayString = displayString + "\nTech Message: " + error.TechMessage + "\nFunction: " + error.FunctionName;
                    displayString = displayString + "\nClass: " + error.ClassName;
                    }
                }
            else
                {//only show last public message
                IEnumerator errorList = (IEnumerator)err.getErrors().GetEnumerator();
                CustomError lastError=new CustomError("","","","","",false);
                while (errorList.MoveNext())
                    {
                    lastError = (CustomError)errorList.Current;
                    }
                displayString = lastError.PublicMessage;
                }
            MessageBox.Show(displayString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        private bool saveAllData()
            {
            Boolean saved = false;
            //write form values
            if (simulationObject.FileName == "untitled")
                {
                SaveFileDialog saveDialog = new SaveFileDialog();
                saveDialog.Filter = "APSIM files (*.apsim)|*.apsim";
                //saveDialog.ShowDialog();

                try
                    {
                    if (saveDialog.ShowDialog() == DialogResult.OK)
                        {
                        simulationObject.FileName = saveDialog.FileName;

                        simulationObject.OutputFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim") - 1) + ".out";
                        simulationObject.SummaryFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim") - 1) + ".sum";

                        APSIMData t = new APSIMData();
                        t = simulationObject.Data;
                        t.SaveToFile(simulationObject.OutputFileName);
                        t.SaveToFile(simulationObject.FileName);
                        this.Text = simulationObject.FileName;
                        saved = true;
                        }
                    }
                catch (Exception e)
                    {
                    showExceptionMessages(e);
                    }
                }
            else
                {
                APSIMData t = new APSIMData();
                t = simulationObject.Data;
                t.SaveToFile(simulationObject.OutputFileName);
                t.SaveToFile(simulationObject.FileName);
                this.Text = simulationObject.FileName;
                saved = true;
                }
            return saved;
            }

        private void displayRainfallChart()
            {
            timer1.Interval = 200;
            timer1.Start();
            chartControl1.Refresh();
            chartControl1.Visible = true;
            }

        #endregion

        

       

      
      

        

        

       
       

       

       
       

       


        }
    }