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
using System.Diagnostics;
using APSRU.Translator.Howwet;


namespace APSRU.Howwet
    {
    
    public partial class Main : Form
        {
        #region init
        private SimulationIn simulationObject;
        private HowwetUtility util;
        private HowwetConfiguration config;
        public MetData metObject;
        private SimulationOut outputObject;
        public String soilsFileName = "";
        private ArrayList coverCrops;
        int rowCount = 0;
        private double soilPAWCSumOriginal;
        private double soilDepthSumOriginal;
        private double soilNitrogenSumOriginal;
        private double[] thicknessOriginal;
        private double[] lL15Original;
        private double[] cLL;
        DataTable dt;
        DataTable chartDataTable;
        private String selectedFileName="";

        ToolTip toolTip1 = new System.Windows.Forms.ToolTip();
        #endregion

        public Main(string[] args)
            {
                    InitializeComponent();
                    if (args.Length>0)
                        {
                        this.selectedFileName = args[1];
                        }
            }
        
        #region Startup

        private void Main_Load(object sender, EventArgs e)
            {
            toolStripStatusLabel1.Text = "Select a Soil";
            config = new HowwetConfiguration();
            util = new HowwetUtility();
            if (this.selectedFileName == "")
                {
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
            else
                {
                APSIMData appsimDataObject = new APSIMData();
                appsimDataObject.LoadFromFile(this.selectedFileName);
                simulationObject=new SimulationIn(appsimDataObject);
                resetFormValues();
                }
            }
        
        private void resetFormValues()
            {
                        
            //setup default values on form
            this.Text = simulationObject.FileName;

            //soil file
 
            //soil name
            //oc/soil depth/water cap/ init water/init n/slope/slope length/erodibilty/ cover/met/start/end
          //  soilForm_SoilSelectedEvent(

           // organicCarbonContent.Text = simulationObject.Soil.OC;
           


            //Erosion
        //    slope.Text = simulationObject.ErosionSlope;
        //    slopeLength.Text = simulationObject.ErosionSlopeLength;
        //    erodibilty.Text = simulationObject.ErosionErodibilty;
           
            
           // toolTip1.SetToolTip(selectSoilButton, "This is tooltip test");
           // toolTip1.Active = true;
            RainfallSWChart.Visible = false;
  
            
            }
        #endregion

        #region Tool Strip events

        private void RunButton_Click(object sender, EventArgs e)
            {
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
            RainfallSWChart.Export.Image.GIF.Save(path+"\\RainfallSWChart.gif"); 
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
                simulationObject = new SimulationIn(appsimDataObject);
                resetFormValues();
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

        #region Events Input side of Form

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

                    if (!SoilSelection.Instance.isLoaded)
                        {
                        SoilSelection.Instance.loadObject(this.soilsFileName);
                        SoilSelection.Instance.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                        }
                    SoilSelection.Instance.Focus();
                    SoilSelection.Instance.Show();

                  //  SoilSelection soilForm = new SoilSelection(this.soilsFileName);
                  //  soilForm.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                  //  soilForm.Show();
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
                    if (!SoilSelection.Instance.isLoaded)
                        {
                        SoilSelection.Instance.loadObject(this.soilsFileName);
                        SoilSelection.Instance.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                        }
                    SoilSelection.Instance.Focus();
                    SoilSelection.Instance.Show();
                    
                 //   SoilSelection soilForm = new SoilSelection(this.soilsFileName);
                 //   soilForm.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                 //   soilForm.Show();
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
            

                selectedSoilLabel.Text = soil.Name;
                String[] layers = simulationObject.Soil.DepthStrings;
                ocDepthLabel.Text = layers[0];//top layer string
                organicCarbonContent.Text = simulationObject.Soil.OC.GetValue(0).ToString();

                soilDepthSumOriginal = MathUtility.Sum(simulationObject.Soil.Thickness);
                soilPAWCSumOriginal = MathUtility.Sum(simulationObject.Soil.PAWC());
                soilNitrogenSumOriginal = MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa);

                lL15Original = simulationObject.Soil.LL15;
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
                        simulationObject.Soil.LL15 = lL15Original;
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


        private void erosionButton_Click(object sender, EventArgs e)
            {
            Erosion  erosionForm = new Erosion(simulationObject);
            erosionForm.Show();

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
                        simulationObject.Soil.LL15 = lL15Original;
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
                
                simulationObject.Soil.InitialWater.SetUsingPercent(percent,true);
                
                double Proportion = Convert.ToInt32(initialSoilWaterPercent.Value) / 100.0;
                double AmountWater = MathUtility.Sum(simulationObject.Soil.PAWC()) * Proportion;

                this.initialWaterCapacity.TextChanged -= new System.EventHandler(this.initialWaterCapacity_TextChanged);
                initialWaterCapacity.Text = AmountWater.ToString("f0");
                this.initialWaterCapacity.TextChanged += new System.EventHandler(this.initialWaterCapacity_TextChanged);
                double[] pawc =simulationObject.Soil.PAWC();
               
                String test1 = "";
               
                double[] sw = simulationObject.Soil.InitialWater.SW;
                
                for (int i = 0; i < sw.Length; i++)
                    {
                    test1 = test1 + " " + sw[i];
                    }
                Console.WriteLine("sw " + test1 + "\n");
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
                simulationObject.Soil.InitialWater.SetUsingPercent(percent, true);
                double[] sw=simulationObject.Soil.InitialWater.SW;
                String test="";
                for (int i = 0; i < sw.Length; i++)
                    {
                    test = test + " " + sw[i];
                    }
                Console.WriteLine("sw "+test + "\n");
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
                    simulationObject.Soil.InitialNitrogen.TotalNO3KgHa = Convert.ToDouble(initialSoilNitrogen.Text);
                    }
                }
            }

        private void browseMetButton_Click(object sender, EventArgs e)
            {
            try
              {
                //Select a met file
              OpenFileDialog openDialog = new OpenFileDialog();
              openDialog.Title = "Browse for Met File";
              openDialog.Filter = "Met files (*.met)|*.met";
              openDialog.ShowDialog();
              if (!(openDialog.FileName == ""))
                {
                    FileInfo fileInfo = new FileInfo(openDialog.FileName);
                    //hydrate metObject       
                    metObject = new MetData(openDialog.FileName);
                    txtMetFile.Text = fileInfo.Name;
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
            label36.Text = simulationObject.SOMMass;
            }
       
      

        private void editRainfallButton_Click(object sender, EventArgs e)
            {
            if (!(this.metObject.FileName == ""))
                {
                //RainfallEditor form = new RainfallEditor();
                
                toolStripStatusLabel2.Text = "Please wait: Loading Met file";
               
                if (!RainfallEditor.Instance.isLoaded)
                    {
                    this.metObject.BuildAverages();
                    RainfallEditor.Instance.loadObject(this.metObject);
                    }
                RainfallEditor.Instance.Focus();
                RainfallEditor.Instance.Show();
                }
            else
                {
                MessageBox.Show("Please select a Met file to edit");
                }
            }

        private void timerProgresBar_Tick(object sender, EventArgs e)
            {
            toolStripProgressBar1.PerformStep();
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

        //Populate combo list with crops from the soil file
        private void displayProposedCropList()
            {
            proposedCropList.Items.Clear();
            String[] crops = simulationObject.Soil.Crops;
            for (int i = 0; i < crops.Length; i++)
                {
                proposedCropList.Items.Add(crops[i]);
                }
            proposedCropList.SelectedIndex = 0;
            }

        //After selecting a proposed corp subtract the corp from the esw to get PAWC 
        private void proposedCropList_SelectedValueChanged(object sender, System.EventArgs e)
            {
                String selectedCrop = (String)proposedCropList.SelectedItem;
                cLL=simulationObject.Soil.LL(selectedCrop);
               
                ArrayList soilWaterEndLayers = (ArrayList)outputObject.SoilWaterEndByLayer;
                double pAWEnd = 0;
                for (int layer = 0; layer < soilWaterEndLayers.Count; layer++)
                    {
                    pAWEnd = pAWEnd + (Math.Abs(Convert.ToDouble(soilWaterEndLayers[layer]) - cLL[layer]) * simulationObject.Soil.Thickness[layer]); ;
                    }
                endPAW.Text = pAWEnd.ToString("f0");
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
                        RainfallSWChart.Visible=true;
                        SoilNitrogenChart.Visible = false;
                        ErosionChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        break;
                    case 1:
                        SoilNitrogenChart.Visible = true;
                        RainfallSWChart.Visible = false;
                        ErosionChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        break;
                    case 2:
                        ErosionChart.Visible = true;
                        SoilNitrogenChart.Visible = false;
                        RainfallSWChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        break;
                    case 3:
                        LTRainfallChart.Visible = true;
                        SoilNitrogenChart.Visible = false;
                        RainfallSWChart.Visible = false;
                        ErosionChart.Visible = false;
                        break;
                    }
            }

       private void button1_Click(object sender, EventArgs e)
            {
            try
                {
                Console.WriteLine("button clicked");
                RainfallBar.Clear();
                RunoffBar.Clear();
                PAWCLine.Clear();
                NitrateLine.Clear();
                SurfaceMoistureLine.Clear();
                MaxTemperatureLine.Clear();
                EroisionRunoffBar.Clear();
                ErosionSoilLossBar.Clear();
                LTRainfallBar.Clear();
                LTAvRainfallLine.Clear();

                outputObject = new SimulationOut(simulationObject.OutputFileName);
                displayProposedCropList();

                //output summary results
                double soilWaterStart = MathUtility.Sum(simulationObject.Soil.PAWC());
                startSoilWater.Text = soilWaterStart.ToString("f0");
                fallowRainfall.Text = outputObject.RainfallTotal.ToString("f0");
                fallowEvaporation.Text = outputObject.EvaporationTotal.ToString("f0");
                fallowRunoff.Text = outputObject.RunoffTotal.ToString("f0");
                drainage.Text = outputObject.DrainTotal.ToString("f0");
                double soilWaterEnd = outputObject.SoilWaterEnd(simulationObject.Soil);
                endSoilWater.Text = soilWaterEnd.ToString("f0");

                double fallowWaterGain = soilWaterEnd - soilWaterStart;
                gainSoilWater.Text = fallowWaterGain.ToString("f0");
                double fallowWaterEfficiency = ((fallowWaterGain / soilWaterStart) * 100);
                waterEfficiency.Text = fallowWaterEfficiency.ToString("f0");

                startSoilNitrate.Text = MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa).ToString("f0");
                endSoilNitrate.Text = outputObject.NitrateEnd.ToString("f0");
                double nitrateGain = outputObject.NitrateEnd - MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa);
                gainNitrate.Text = nitrateGain.ToString("f0");
                double nitrateEfficiencySum = (nitrateGain / MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa)) * 100;
                nitrateEfficiency.Text = nitrateEfficiencySum.ToString("f0");
                ArrayList soilWaterEndLayers = outputObject.SoilWaterEndByLayer;
                double pAWEnd = 0;
                for (int layer = 0; layer < soilWaterEndLayers.Count; layer++)
                    {
                    pAWEnd = pAWEnd + (Math.Abs(Convert.ToDouble(soilWaterEndLayers[layer]) - cLL[layer]) * simulationObject.Soil.Thickness[layer]);
                    }
                endPAW.Text = pAWEnd.ToString("f0");
                
                dt = outputObject.Data;
                chartDataTable = dt;
                //add new column for the sum of PAWC
                chartDataTable.Columns.Add("PAWC", typeof(double));
                //set x axis range
                RainfallSWChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
                RainfallSWChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
                SoilNitrogenChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
                SoilNitrogenChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
                ErosionChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
                ErosionChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
                LTRainfallChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
                LTRainfallChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
    
                //set left y axis range
                double maxRainfallSW = 0, minRainfallSW = 0, maxPAWCSW = 0, minPAWCSW = 10000;
                double maxNitrate = 0, minNitrate = 10000, maxSurfaceMoisture = 0, minSurfaceMoisture = 10000, maxMaxTemp = 0, minMaxTemp = 10000; ;
                double maxSoilLoss = 0, minSoilLoss = 0, maxRunoff = 0, minRunoff = 0;
                double maxLTRainfall = 0, minLTRainfall = 0, maxLTAvRainfall = 0, minLTAvRainfall = 0;
                Console.WriteLine("finding axes max min");
                foreach (DataRow row in chartDataTable.Rows)
                    {
                    //Rainfall soil water; subtract cll from soilwater and sum the absolute values to got pawc
                    if (Convert.ToDouble(row["Rainfall"]) > maxRainfallSW) maxRainfallSW = Convert.ToDouble(row["Rainfall"]);
                    ArrayList tmpSoilWaterLayers = (ArrayList)row["SoilWaterLayers"];
                    double pawc=0;
                    for (int layer = 0; layer < tmpSoilWaterLayers.Count; layer++)
                        {
                        pawc=pawc+(simulationObject.Soil.Thickness[layer]*(Math.Abs(Convert.ToDouble(tmpSoilWaterLayers[layer]) - simulationObject.Soil.LL15[layer])));
                        }
                    row["PAWC"] = pawc;
                    if (pawc > maxPAWCSW) maxPAWCSW = pawc;
                    if (pawc < minPAWCSW) minPAWCSW = pawc;
                    //Soil Nitrogen
                    if (Convert.ToDouble(row["NO3Total"]) > maxNitrate) maxNitrate = Convert.ToDouble(row["NO3Total"]);
                    if (Convert.ToDouble(row["NO3Total"]) < minNitrate) minNitrate = Convert.ToDouble(row["NO3Total"]);
                    if (Convert.ToDouble(row["SoilWaterTopLayer"]) > maxSurfaceMoisture) maxSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
                    if (Convert.ToDouble(row["SoilWaterTopLayer"]) < minSurfaceMoisture) minSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
                    if (Convert.ToDouble(row["MaxTemp"]) > maxMaxTemp) maxMaxTemp = Convert.ToDouble(row["MaxTemp"]);
                    if (Convert.ToDouble(row["MaxTemp"]) < minMaxTemp) minMaxTemp = Convert.ToDouble(row["MaxTemp"]);

                    //Erosion
                    if (Convert.ToDouble(row["SoilLoss"]) > maxSoilLoss) maxSoilLoss = Convert.ToDouble(row["SoilLoss"]);
                    if (Convert.ToDouble(row["Runoff"]) > maxRunoff) maxRunoff = Convert.ToDouble(row["Runoff"]); 
                    //Long term rainfall
                    if (Convert.ToDouble(row["Rainfall"]) > maxLTRainfall) maxLTRainfall = Convert.ToDouble(row["Rainfall"]);
                    //  if (Convert.ToDouble(row["Rainfall"]) > maxLTAvRainfall) maxLTAvRainfall = Convert.ToDouble(row["Rainfall"]);
                    }
                Console.WriteLine("found max min");
                //Rainfall and soil water 
                RainfallSWChart.Axes.Left.Maximum = maxRainfallSW;
                RainfallSWChart.Axes.Left.Minimum = minRainfallSW;
                RainfallSWChart.Axes.Right.Maximum = maxPAWCSW;
                RainfallSWChart.Axes.Right.Minimum = minPAWCSW;
                //soil nitrogen
                SoilNitrogenChart.Axes.Left.Maximum = maxNitrate;
                SoilNitrogenChart.Axes.Left.Minimum = minNitrate;
                SoilNitrogenChart.Axes.Right.Maximum = maxSurfaceMoisture;
                SoilNitrogenChart.Axes.Right.Minimum = minSurfaceMoisture;
                SoilNitrogenChart.Axes.Custom[0].Maximum = maxMaxTemp;
                SoilNitrogenChart.Axes.Custom[0].Minimum = minMaxTemp;
                //Erosion
                ErosionChart.Axes.Left.Maximum = maxSoilLoss;
                ErosionChart.Axes.Left.Minimum = minSoilLoss;
                ErosionChart.Axes.Right.Maximum = maxRunoff;
                ErosionChart.Axes.Right.Minimum = minRunoff;
                //Long term rainfall
                LTRainfallChart.Axes.Left.Maximum = maxLTRainfall;
                LTRainfallChart.Axes.Left.Minimum = minLTRainfall;
            //    LTRainfallChart.Axes.Right.Maximum = maxLTAvRainfall;
            //    LTRainfallChart.Axes.Right.Minimum = minLTAvRainfall;
               
               
             //   if (!(speedBar.Value == 0))
                 //   {
                rowCount = 0;
                    timer1.Interval =2;
                    timer1.Start();
                    RainfallSWChart.Refresh();
                    SoilNitrogenChart.Refresh();
                    ErosionChart.Refresh();
                    LTRainfallChart.Refresh();
                  //  }
              
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

        public void timer1_Tick(object sender, EventArgs e)
            {
            
            if (rowCount <= (chartDataTable.Rows.Count - 1))
                {
                DataRow row = dt.Rows[rowCount];
                Console.WriteLine("Date "+row["Date"]+ " Runoff " + row["Runoff"] + " SoilLoss " + row["SoilLoss"]);

                //Rainfall and soil water graph
                RainfallBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
                RunoffBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Runoff"]));
                PAWCLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["PAWC"]));
                //Soil Nitrogen graph
                NitrateLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["NO3Total"]));
                SurfaceMoistureLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["SoilWaterTopLayer"]));
                MaxTemperatureLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["MaxTemp"]));
                //Erosion graph
                EroisionRunoffBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Runoff"]));
                ErosionSoilLossBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["SoilLoss"]));
                //Long term rainfall
                LTRainfallBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
         //       LTAvRainfallLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
                rowCount++;
              
                RainfallSWChart.Refresh();
                SoilNitrogenChart.Refresh();
                ErosionChart.Refresh();
                LTRainfallChart.Refresh();
                }
            else
                {
                timer1.Stop();
                }
            }

        private void speedBar_ValueChanged(object sender, EventArgs e)
            {
            textBox1.Text=speedBar.Value.ToString();
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
                        simulationObject.OutputFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".out";
                        simulationObject.SummaryFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".sum";

                        APSIMData apsimObject = new APSIMData();
                        apsimObject = simulationObject.Data;
                        apsimObject.SaveToFile(simulationObject.OutputFileName);
                        apsimObject.SaveToFile(simulationObject.FileName);
                        
                        String fileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".hwt";
                        apsimObject.SaveToFile(fileName);
                        this.Text = fileName;
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
            RainfallSWChart.Refresh();
            RainfallSWChart.Visible = true;
            }

        #endregion

       

       

      

       

      
       
       
        }
    }