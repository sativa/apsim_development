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
using System.Xml;
using System.Net;
using System.Threading;

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
        private Results result;
        public String soilsFileName = "";
        private ArrayList coverCrops;
        int rowCount = 0;
        private double soilPAWCSumOriginal;
        private double soilDepthSumOriginal;
        private double soilNitrogenSumOriginal;
        private double[] thicknessOriginal;
        private double[] lL15Original;
        private String erosionSlopeOriginal;
        private String erosionSlopeLengthOriginal;
        private String erosionErodibiltyOriginal;
        private double[] cLL;
        DataTable chartDataTable;
        private String selectedFileName="";

        private String apsimPath = "\\bin\\apsim.exe";
        private String apsimToSimPath = "\\bin\\apsimtosim.exe";
        private String howwetReportFileName = "\\howwetv2\\HowwetReport.xml";
        private String howwetSetupFileName = "\\howwetv2\\HowwetSetup.xml";
        private String howwetRegionFileName="\\howwetv2\\HowwetRegions.xml";
        private const int WUEDefault = 3;
        private const int ThresholdWaterDefault = 100;

        ToolTip toolTip1 = new System.Windows.Forms.ToolTip();
        #endregion

        public Main()
            {
               InitializeComponent();
            }
        
        #region Startup

        private void Main_Load(object sender, EventArgs e)
            {
            resetSimulationValues();
            }

        private void resetSimulationValues()
            {
          //  toolStripStatusLabel1.Text = "Select a Soil";
            util = new HowwetUtility(Application.ExecutablePath, howwetSetupFileName);
            config = new HowwetConfiguration(util.ApplicationDirectory + howwetSetupFileName, util.ApplicationDirectory + howwetRegionFileName);
            version.Text=config.Version;
            if (this.selectedFileName == "")
                {
                simulationObject = new SimulationIn(util.ReadTemplateFile(util.ApplicationDirectory + "\\howwetv2\\" + config.TemplateFileName));
                erosionSlopeOriginal = simulationObject.ErosionSlope;
                erosionSlopeLengthOriginal = simulationObject.ErosionSlopeLength;
                erosionErodibiltyOriginal = simulationObject.ErosionErodibilty;
                simulationObject.FileName = "untitled";
                if (!(config.DefaultSoilFileName == ""))
                    {
                    if (LoadSoilFile(config.DefaultSoilFileName))
                        {
                        if (!(config.DefaultSoilName == ""))
                            {
                            APSIMData soils = new APSIMData();
                            soils.LoadFromFile(config.DefaultSoilFileName);
                            APSIMData soil = soils.GetNode("name", config.DefaultSoilName);
                            LoadSoil(soil);
                            }
                        }
                    }
               if (!(config.DefaultMetfile == ""))
                    {
                    LoadMetFile(config.DefaultMetfile);
                    }
              //  if (!(config.DefaultRegionName == ""))
               //     {
               //     }
             //   loadDefaultSoilFile();
                resetFormValues();
                }
            else
                {
                APSIMData apsimDataObject = new APSIMData();
                apsimDataObject.LoadFromFile(this.selectedFileName);
                simulationObject = new SimulationIn(apsimDataObject);
                resetFormValues();
                }
            }

        public void LoadMetFile(String metFileName)
            {
            metObject = new MetData(metFileName);
            txtMetFile.Text = metFileName;

            simulationObject.MetFileName = metObject.FileName;
            //set datetime picker
            if (!(metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0)) < metObject.StartDate))
                {
                StartDatePicker.Value = metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0));
                }
            else
                {
                StartDatePicker.Value = metObject.StartDate;
                }
            EndDatePicker.MaxDate = metObject.EndDate;
            EndDatePicker.Value = metObject.EndDate;
            config.DefaultMetfile = txtMetFile.Text;
            }

        public Boolean LoadSoilFile(String fileName)
            {
            Boolean isLoaded = false;
            FileInfo fileInfo = new FileInfo(fileName);
            if (fileInfo.Exists)
                {
                soilFileName.Text = fileInfo.Name;
                this.soilsFileName = fileInfo.FullName;
                config.DefaultSoilFileName = this.soilsFileName;
                isLoaded = true;
                }
            else
                {
                soilFileName.Text = "<Select a Soil file>";
                }
            return isLoaded;
            }


        public void LoadSoil(APSIMData soil)
            {
            simulationObject.AddSoil(soil);
            config.DefaultSoilName = simulationObject.Soil.Name;
            displayProposedCropList();
            }

        private void resetFormValues()
            {
            TrainingModeCheckBox.Checked = config.TrainingMode;
            this.Text = simulationObject.FileName;
            tabControl1.Visible = false;
            RainfallSWChart.Visible = false;
            SoilNitrogenChart.Visible = false;
            ErosionChart.Visible = false;
            LTRainfallChart.Visible = false;
            ProfileChart.Visible = false;
            ReportButton.Enabled = false;
            WaterPanel.Visible = false;
            CoverPanel.Visible = false;
            NitrogenPanel.Visible = false;
            NRequirementPanel.Visible = false;

            label61.Visible = false;
            //check if simluationObject has a soil
            if (!(simulationObject.Soil == null))
                {
                updateFormSoilValues();
                }
            else
                {
                clearFormSoilValues();
                }
            //check if simulationObject has a met file
            if (!(simulationObject.MetFileName == ""))
                {
                metObject = new MetData(simulationObject.MetFileName);
                metObject.BuildAverages();
                updateFormMetValues();
                }
            else
                {
                clearFormMetValues();
                }
            updateFormOutputValues();
            }

        private void clearFormSoilValues()
            {
            selectedSoilName.Text = "";
            selectedSoilName.ToolTipText = "";
            ocDepthLabel.Text = "";
            organicCarbonContent.Text = "";
            soilDepth.Text = "";
            this.initialWaterCapacity.TextChanged -= new System.EventHandler(this.initialWaterCapacity_TextChanged);
            initialWaterCapacity.Text = "";
            this.initialWaterCapacity.TextChanged += new System.EventHandler(this.initialWaterCapacity_TextChanged);
            waterCapacity.Text = "";
            initialSoilNitrogen.Text = "";
            displayCoverCropList();
           // this.initialSoilWaterPercent.ValueChanged -= new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
            initialSoilWaterPercent.Value = 20;
          //  this.initialSoilWaterPercent.ValueChanged += new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
            coverPercent.Maximum = 99;
            coverPercent.Minimum = 0;
            coverPercent.Value = 30;
            }

        private void updateFormSoilValues()
            {
            selectedSoilName.Text = simulationObject.Soil.Name;
            String soilString = "Soil Name: "+simulationObject.Soil.Name+ "\nRegion: " + simulationObject.Soil.Region;
            selectedSoilName.ToolTipText = soilString;
            String[] layers = simulationObject.Soil.DepthStrings;
            ocDepthLabel.Text = layers[0];//top layer string
            organicCarbonContent.Text = simulationObject.Soil.OC.GetValue(0).ToString();
            soilDepthSumOriginal = MathUtility.Sum(simulationObject.Soil.Thickness);
            soilPAWCSumOriginal = MathUtility.Sum(simulationObject.Soil.PAWC());
            soilNitrogenSumOriginal = MathUtility.Sum(simulationObject.Soil.InitialNitrogen.NO3KgHa);
            lL15Original = simulationObject.Soil.LL15;
            thicknessOriginal = simulationObject.Soil.Thickness;
            soilDepth.Text = soilDepthSumOriginal.ToString("f0");
            waterCapacity.Text = soilPAWCSumOriginal.ToString("f0");
            initialSoilNitrogen.Text = soilNitrogenSumOriginal.ToString("f0");
            displayCoverCropList();
           // this.initialSoilWaterPercent.ValueChanged -= new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
            initialSoilWaterPercent.Value = 20;
          //  this.initialSoilWaterPercent.ValueChanged += new System.EventHandler(this.initialSoilWaterPercent_ValueChanged);
            coverPercent.Maximum = 99;
            coverPercent.Minimum = 0;
            coverPercent.Value = 30;
          //  toolStripStatusLabel1.Text = "Select a Met file";
            }

        private void updateFormMetValues()
            {
            FileInfo fileInfo = new FileInfo(metObject.FileName);
            txtMetFile.Text = fileInfo.Name;
            //set metfileName in simulation object
            simulationObject.MetFileName = metObject.FileName;
            //set datetime picker
            StartDatePicker.MinDate = metObject.StartDate;
            if (!(metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0)) < metObject.StartDate)) 
                {
                StartDatePicker.Value = metObject.EndDate.Subtract(new TimeSpan(400, 0, 0, 0, 0)); 
                }
            else
                {
                StartDatePicker.Value = metObject.StartDate;
                }
            EndDatePicker.MaxDate = metObject.EndDate;
            EndDatePicker.Value = metObject.EndDate;
          //  toolStripStatusLabel1.Text = "Check soil Water and Nitrogen values are correct, then run the simulation";
            }

        private void clearFormMetValues()
            {
            txtMetFile.Text = "";
            StartDatePicker.MaxDate = DateTime.Now;
            StartDatePicker.Value = DateTime.Today;
            EndDatePicker.MaxDate = DateTime.Now;
            EndDatePicker.Value = DateTime.Today;
            }
        
        private void updateFormOutputValues()
            {//todo
            // clear the proposed crop combo and other bits
            this.daystoMaturityUpDown.ValueChanged -= new EventHandler(daystoMaturityUpDown_ValueChanged);
            daystoMaturityUpDown.Minimum = 1;
            daystoMaturityUpDown.Maximum = 180;
            daystoMaturityUpDown.Increment = 1;
            daystoMaturityUpDown.Value = 1;
            this.daystoMaturityUpDown.ValueChanged +=new EventHandler(daystoMaturityUpDown_ValueChanged);
            }

        #endregion

        #region Tool Strip events

        private void TrainingModeCheckBox_CheckedChanged(object sender, EventArgs e)
            {
            config.TrainingMode = TrainingModeCheckBox.Checked;
            }
               
        private void RunButton_Click(object sender, EventArgs e)
            {
            saveAllData();
            tabControl1.Visible = false;
            RainfallSWChart.Visible = false;
            SoilNitrogenChart.Visible = false;
            ErosionChart.Visible = false;
            LTRainfallChart.Visible = false;
            label61.Visible = true;
            if (ExecuteAPSIM())
                {
                ShowGraph();
                }
            else
                {

                }
            label61.Visible=false;
            }

        private bool ExecuteAPSIM()
            {
            String errString = "";
            const String FUNCTION_NAME = "ExecuteAPSIM";
            bool success = false;
            try
                {
                String ApsimToSimPath = util.ApplicationDirectory +  apsimToSimPath;
                String Apsim = util.ApplicationDirectory+ apsimPath;
                String simulationFileName = simulationObject.FileName;
                String simulationParentPath = Directory.GetParent(simulationFileName).ToString();
                //remove old .sum, .out, and .sim files if they exist
                if (File.Exists(simulationObject.OutputFileName)) File.Delete(simulationObject.OutputFileName);
                if (File.Exists(simulationObject.SummaryFileName)) File.Delete(simulationObject.SummaryFileName);
                if (File.Exists(simulationObject.SimulationName + ".sim")) File.Delete(simulationObject.SimulationName + ".sim");
                ProgressBar1.Minimum = 0;
                ProgressBar1.Maximum = 2;
                ProgressBar1.Step = 1;
                StatusLabel2.Text = "Running APSIM";
                errString = "ApsimToSimPath=" + ApsimToSimPath;
                if (File.Exists(ApsimToSimPath))
                    {
                    ProgressBar1.PerformStep();
                    Process p = Process.Start("\""+ApsimToSimPath+"\"", "\"" + simulationFileName + "\"");
                    p.WaitForExit();
                    this.Refresh();
                  
                    String simFileName = simulationParentPath+"\\"+simulationObject.SimulationName + ".sim";
                    errString = "Apsim=" + Apsim+ " Sim="+simFileName;
                    if(File.Exists(simFileName))
                        {
                        if (File.Exists(Apsim))
                            {
                            ProgressBar1.PerformStep();
                            p = Process.Start("\""+ Apsim +"\"", "\""+ simFileName +"\"");
                            p.WaitForExit();
                            this.Refresh();

                            String outFileName = simulationObject.SimulationName + ".out";
                            errString = "Output file=" + outFileName;
                            if (File.Exists(outFileName))
                                {
                                success = true;
                                }
                            else
                                {
                                new CustomException(new CustomError("", "Cannot find simulation output file", errString, FUNCTION_NAME, this.GetType().FullName, false));
                                }
                            }
                        else
                            {
                            new CustomException(new CustomError("", "Cannot find Apsim file", errString, FUNCTION_NAME, this.GetType().FullName, false));
                            }
                        }
                    }
                else
                    {
                    new CustomException(new CustomError("", "Cannot find ApsimToSim file", errString, FUNCTION_NAME, this.GetType().FullName, false));
                    }
                ProgressBar1.Value = 0;
                StatusLabel2.Text = "";
                return success;
                }
            catch (CustomException err)
                {
                showCustomExceptionMessages(err);
                return success;
                }
            catch (Exception err)
                {
                showExceptionMessages(err);
                return success;
                }
            }
 
        private void ReportButton_Click(object sender, EventArgs e)
            {
            String fileName = util.ApplicationDirectory + howwetReportFileName;
            StreamWriter stream = new StreamWriter(fileName);
            stream.WriteLine("<?xml-stylesheet type=\"text/xsl\" href=\"HowwetReport.xsl\"?>");
            System.Xml.Serialization.XmlSerializer xmlStream = new System.Xml.Serialization.XmlSerializer(result.GetType());
            xmlStream.Serialize(stream, result);
            RainfallSWChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\RainfallSWChart.gif");
            SoilNitrogenChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\SoilNitorgenChart.gif");
            ErosionChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\ErosionChart.gif");
            LTRainfallChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\LTRainfallChart.gif");
            ProfileChart.Export.Image.GIF.Save(util.ApplicationDirectory + "\\howwetv2\\ProfileChart.gif");
            System.Diagnostics.Process.Start("IExplore.exe", util.ApplicationDirectory + howwetReportFileName);
            }

        private void newToolStripButton_Click(object sender, EventArgs e)
            {
            resetSimulationValues();
            }

        private void openToolStripButton_Click(object sender, EventArgs e)
            {
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.Title = "Browse for Howwet File";
            openDialog.Filter = "Howwet files (*.apsim)|*.apsim";
            openDialog.ShowDialog();
            if (!(openDialog.FileName == ""))
                {
                APSIMData appsimDataObject = new APSIMData();
                appsimDataObject.LoadFromFile(openDialog.FileName);
                simulationObject = new SimulationIn(appsimDataObject);
                simulationObject.FileName=openDialog.FileName;
               // loadDefaultSoilFile();
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
                String fileName = simulationObject.FileName;
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
            saveDialog.Filter = "Howwet files (*.apsim)|*.apsim";
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
                    }
                else
                    {
                    MessageBox.Show("Please select a Soil File first");
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
                LoadSoil(soil);
                updateFormSoilValues();
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
        
        //***********
        private void updateOrganicCarbonContent()
            {
            if (!(organicCarbonContent.Text == ""))
                {
                simulationObject.Soil.OC.SetValue(Convert.ToDouble(organicCarbonContent.Text), 0);
                }
            }
        private void organicCarbonContent_KeyPress(object sender, KeyPressEventArgs e)
            {
            if (e.KeyChar == Convert.ToChar(13)) updateOrganicCarbonContent();
            }
        private void organicCarbonContent_Leave(object sender, EventArgs e)
            {
            updateOrganicCarbonContent();
            }
        //**********
        //**********
        private void updateSoilDepth()
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
        private void soilDepth_KeyPress(object sender, KeyPressEventArgs e)
            {
            if (e.KeyChar == Convert.ToChar(13))updateSoilDepth();
            }
        private void soilDepth_Leave(object sender, EventArgs e)
            {
            updateSoilDepth();
            }
        //*********
        private void erosionButton_Click(object sender, EventArgs e)
            {
                {
                if (!Erosion.Instance.isLoaded)
                    {
                    Erosion.Instance.loadObject(simulationObject);
                    Erosion.Instance.ErosionChangedEvent += new Erosion.ErosionValuesChanged(Instance_ErosionChangedEvent);
                    }
                Erosion.Instance.Focus();
                Erosion.Instance.Show();
                }
            }

        void Instance_ErosionChangedEvent(string slope, string slopeLength, string erodibilty)
            {
            simulationObject.ErosionSlope = slope;
            simulationObject.ErosionSlopeLength = slopeLength;
            simulationObject.ErosionErodibilty = erodibilty;
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
                if (!(simulationObject.Soil == null))
                    {
                    int percent = Convert.ToInt32(initialSoilWaterPercent.Value);
                    simulationObject.Soil.InitialWater.SetUsingPercent(percent, true);
                    double Proportion = Convert.ToInt32(initialSoilWaterPercent.Value) / 100.0;
                    double AmountWater = MathUtility.Sum(simulationObject.Soil.PAWC()) * Proportion;
                    this.initialWaterCapacity.TextChanged -= new System.EventHandler(this.initialWaterCapacity_TextChanged);
                    initialWaterCapacity.Text = AmountWater.ToString("f0");
                    this.initialWaterCapacity.TextChanged += new System.EventHandler(this.initialWaterCapacity_TextChanged);
                    double[] pawc = simulationObject.Soil.PAWC();
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
                //hydrate metObject       
               // metObject = new MetData(openDialog.FileName);
               // metObject.BuildAverages();
                LoadMetFile(openDialog.FileName);
                updateFormMetValues();
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
            String selectedCrop = (String)coverCropList.SelectedItem;
            CoverCrop crop = util.GetCrop(coverCrops, selectedCrop);
            simulationObject.SOMMass = Convert.ToString(util.ConvertCoverPercentToKg(percent / 100, crop.SpecificArea));
            }

        private void editRainfallButton_Click(object sender, EventArgs e)
            {
            if (!(this.metObject == null))
                {
                StatusLabel2.Text = "Please wait: Loading Met file";
                if (!RainfallEditor.Instance.isLoaded)
                    {
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
            ProgressBar1.PerformStep();
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

        #region Events Output side of Form

        //Populate combo list with crops from the soil file
        private void displayProposedCropList()
            {
            proposedCropList.Items.Clear();
            String[] crops = simulationObject.Soil.Crops;
            if (!(crops.Length == 0))
                {
                for (int i = 0; i < crops.Length; i++)
                    {
                    proposedCropList.Items.Add(crops[i]);
                    }
                proposedCropList.SelectedIndex = 0;
                }
            else
                {
                proposedCropList.Text = "Using LL15";
                }
            }

        //After selecting a proposed corp subtract the corp from the esw to get PAWC 
        private void proposedCropList_SelectedValueChanged(object sender, System.EventArgs e)
            {
            String selectedCrop = (String)proposedCropList.SelectedItem;
            if (selectedCrop == "Using LL15")
                {
                cLL=simulationObject.Soil.LL15;
                }
            else
                {
                cLL = simulationObject.Soil.LL(selectedCrop);
                }
        //        endPAW.Text = result.calcPAWEnd(cLL).ToString("f0");
          //  calculateNitrogenRequirement();
            }

        private void daystoMaturityUpDown_ValueChanged(object sender, EventArgs e)
            {
            inCropRainfall.Text = metObject.averageRainInNext(EndDatePicker.Value, Convert.ToInt16(daystoMaturityUpDown.Value)).ToString("f0");
            calculateNitrogenRequirement();
            }

        //************
        private void calculateNitrogenRequirement()
            {
            double expectedYield = result.calcYield(Convert.ToDouble(inCropRainfall.Text), Convert.ToDouble(thresholdWater.Text), Convert.ToDouble(WUE.Text));
            cropYield.Text = String.Format("{0:##.##}", expectedYield);
            double nDemand=result.calcNitrogenDemand();
            nitrateDemand.Text = String.Format("{0:##.##}", nDemand);
            double nGap=result.calcNitrogenGap();
            nitrateGap.Text = nGap.ToString("f0");
            }
        private void NRequirement_Leave(object sender, EventArgs e)
            {
            calculateNitrogenRequirement();
            }
        private void NRequirement_KeyPress(object sender, KeyPressEventArgs e)
            {
            if (e.KeyChar == Convert.ToChar(13)) calculateNitrogenRequirement();
            }
        //************
        
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
                        ProfileChart.Visible = false;
                        break;
                    case 1:
                        SoilNitrogenChart.Visible = true;
                        RainfallSWChart.Visible = false;
                        ErosionChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        ProfileChart.Visible = false;
                        break;
                    case 2:
                        ErosionChart.Visible = true;
                        SoilNitrogenChart.Visible = false;
                        RainfallSWChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        ProfileChart.Visible = false;
                        break;
                    case 3:
                        LTRainfallChart.Visible = true;
                        SoilNitrogenChart.Visible = false;
                        RainfallSWChart.Visible = false;
                        ErosionChart.Visible = false;
                        ProfileChart.Visible = false;
                        break;
                    case 4:
                        ProfileChart.Visible = true;
                        SoilNitrogenChart.Visible = false;
                        LTRainfallChart.Visible = false;
                        RainfallSWChart.Visible = false;
                        ErosionChart.Visible = false;
                        break;
                    }
            }

         private void ShowGraph()
            {
            try
                {
                RainfallBar.Clear();
                RunoffBar.Clear();
                SWLine.Clear();
                NitrateLine.Clear();
                SurfaceMoistureLine.Clear();
                MaxTemperatureLine.Clear();
                ErosionRunoffCumLine.Clear();
                ErosionSoilLossCumLine.Clear();
                LTRainfallBar.Clear();
                LTAvRainfallLine.Clear();
                ProfileCLLLine.Clear();
                ProfileDULLine.Clear();
                ProfileLL15Line.Clear();
                ProfileSWLine.Clear();

                outputObject = new SimulationOut(simulationObject);
                result = new Results();
                result.loadResults(simulationObject,outputObject);
                result.softwareVersion = config.Version;
               
                //output summary results
                //water
                startSoilWater.Text = result.soilWaterStart.ToString("f0");
                fallowRainfall.Text = result.rainfall.ToString("f0");
                fallowEvaporation.Text = result.evaporation.ToString("f0");
                fallowRunoff.Text = result.runoff.ToString("f0");
                drainage.Text = result.drain.ToString("f0");
                endSoilWater.Text = result.soilWaterEnd.ToString("f0");
                gainSoilWater.Text = result.fallowWaterGain.ToString("f0");
                waterEfficiency.Text = result.fallowWaterEfficiency.ToString("f0");
                //cover
                CoverCrop crop = util.GetCrop(coverCrops, simulationObject.SOMType);
                decimal startCoverPercent = util.ConvertCoverKgToPercent(result.startCover, crop.SpecificArea);
                ToolTip a=new ToolTip();
                a.Show(result.startCover.ToString("f0") + " kg/ha", startingCover, 1000);
                startingCover.Text = startCoverPercent.ToString("f0");
                decimal endCoverPrecent=util.ConvertCoverKgToPercent(result.endCover, crop.SpecificArea);
                ToolTip b = new ToolTip();
                b.Show(result.endCover.ToString("f0") + " kg/ha", endCover, 1000);
                endCover.Text = endCoverPrecent.ToString("f0");
                
                //Nitrogen
                startSoilNitrate.Text = result.nitrateStart.ToString("f0");
                endSoilNitrate.Text = result.nitrateEnd.ToString("f0");
                gainNitrate.Text = result.nitrateGain.ToString("f0");

                //n Requirement
                thresholdWater.Text = ThresholdWaterDefault.ToString("f0");
                WUE.Text = WUEDefault.ToString("f0");
                inCropRainfall.Text = metObject.averageRainInNext(EndDatePicker.Value, Convert.ToInt16(daystoMaturityUpDown.Value)).ToString("f0");
                displayProposedCropList();
                chartDataTable = outputObject.Data;
              
                RainfallSWChart.Axes.Left.Automatic = false;
                RainfallSWChart.Axes.Right.Automatic = false;
                RainfallSWChart.Axes.Bottom.Automatic = false;
                ErosionChart.Axes.Left.Automatic = false;
                ErosionChart.Axes.Right.Automatic = false;
                ErosionChart.Axes.Bottom.Automatic = false;
                SoilNitrogenChart.Axes.Left.Automatic = false;
                SoilNitrogenChart.Axes.Right.Automatic = false;
                SoilNitrogenChart.Axes.Bottom.Automatic = false;
                axis1.Automatic = false;
                LTRainfallChart.Axes.Left.Automatic = false;
                LTRainfallChart.Axes.Right.Automatic = false;
                LTRainfallChart.Axes.Bottom.Automatic = false;
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
                double maxRainfallSW = 0, minRainfallSW = 0, maxSW = 0, minSW = 10000;
                double maxNitrate = 0, minNitrate = 10000, maxSurfaceMoisture = 0, minSurfaceMoisture = 10000, maxMaxTemp = 0, minMaxTemp = 10000; ;
                double maxSoilLoss = 0, minSoilLoss = 0, maxRunoff = 0, minRunoff = 0;
                double maxLTRainfall = 0, minLTRainfall = 0;
                
                if (config.TrainingMode)
                    {
                    StatusLabel2.Text = "Parsing  chart data";
                    ProgressBar1.Minimum = 0;
                    ProgressBar1.Maximum = chartDataTable.Rows.Count;
                    ProgressBar1.Step = 1;
                   // Console.WriteLine("finding axes max min");
                    foreach (DataRow row in chartDataTable.Rows)
                        {
                        //Rainfall soil water; subtract cll from soilwater and sum the absolute values to get sw
                        if (Convert.ToDouble(row["Rainfall"]) > maxRainfallSW) maxRainfallSW = Convert.ToDouble(row["Rainfall"]);
                        if(Convert.ToDouble(row["SoilWater"]) > maxSW)maxSW=Convert.ToDouble(row["SoilWater"]);
                        if(Convert.ToDouble(row["SoilWater"]) < minSW)minSW=Convert.ToDouble(row["SoilWater"]);
                        //Soil Nitrogen
                        if (Convert.ToDouble(row["NO3Total"]) > maxNitrate) maxNitrate = Convert.ToDouble(row["NO3Total"]);
                        if (Convert.ToDouble(row["NO3Total"]) < minNitrate) minNitrate = Convert.ToDouble(row["NO3Total"]);
                        if (Convert.ToDouble(row["SoilWaterTopLayer"]) > maxSurfaceMoisture) maxSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
                        if (Convert.ToDouble(row["SoilWaterTopLayer"]) < minSurfaceMoisture) minSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
                        if (Convert.ToDouble(row["MaxTemp"]) > maxMaxTemp) maxMaxTemp = Convert.ToDouble(row["MaxTemp"]);
                        if (Convert.ToDouble(row["MaxTemp"]) < minMaxTemp) minMaxTemp = Convert.ToDouble(row["MaxTemp"]);

                        //Erosion
                        if (Convert.ToDouble(row["SoilLossCum"]) > maxSoilLoss) maxSoilLoss = Convert.ToDouble(row["SoilLossCum"]);
                        if (Convert.ToDouble(row["RunoffCum"]) > maxRunoff) maxRunoff = Convert.ToDouble(row["RunoffCum"]);
                        //Long term rainfall
                        if (Convert.ToDouble(row["Rainfall"]) > maxLTRainfall) maxLTRainfall = Convert.ToDouble(row["Rainfall"]);
                        //  if (Convert.ToDouble(row["Rainfall"]) > maxLTAvRainfall) maxLTAvRainfall = Convert.ToDouble(row["Rainfall"]);
                        ProgressBar1.PerformStep();
                        }
                    StatusLabel2.Text = "";
                    ProgressBar1.Value = 0;
                  //  Console.WriteLine("found max min");
                    //Rainfall and soil water 
                    RainfallSWChart.Axes.Left.Maximum = maxRainfallSW;
                    RainfallSWChart.Axes.Left.Minimum = minRainfallSW;
                    RainfallSWChart.Axes.Right.Maximum = maxSW;
                    //   RainfallSWChart.Axes.Right.Minimum = minSW;
                    RainfallSWChart.Axes.Right.Minimum = 0;
                    //soil nitrogen
                    SoilNitrogenChart.Axes.Left.Maximum = maxNitrate;
                    SoilNitrogenChart.Axes.Left.Minimum = minNitrate;
                    SoilNitrogenChart.Axes.Right.Maximum = maxSurfaceMoisture * 150;
                    SoilNitrogenChart.Axes.Right.Minimum = 0;
                 //   SoilNitrogenChart.Axes.Right.Minimum = minSurfaceMoisture * 150;
                    SoilNitrogenChart.Axes.Custom[0].Maximum = maxMaxTemp;
                    //  SoilNitrogenChart.Axes.Custom[0].Minimum = minMaxTemp;
                    SoilNitrogenChart.Axes.Custom[0].Minimum = 0;
                    //Erosion
                    ErosionChart.Axes.Left.Maximum = maxSoilLoss;
                    ErosionChart.Axes.Left.Minimum = minSoilLoss;
                    ErosionChart.Axes.Right.Maximum = maxRunoff;
                    ErosionChart.Axes.Right.Minimum = minRunoff;
                    //Long term rainfall
                    LTRainfallChart.Axes.Left.Maximum = maxLTRainfall;
                    LTRainfallChart.Axes.Left.Minimum = 0;
                    LTRainfallChart.Axes.Right.Maximum = maxLTRainfall;
                    LTRainfallChart.Axes.Right.Minimum = 0;
                    StatusLabel2.Text = "Building chart";
                    ProgressBar1.Minimum = 0;
                    ProgressBar1.Maximum = chartDataTable.Rows.Count;
                    ProgressBar1.Step = 1;

                    rowCount = 0;
                    timer1.Interval = 2;
                    timer1.Start();
                    RainfallSWChart.Refresh();
                    SoilNitrogenChart.Refresh();
                    ErosionChart.Refresh();
                    LTRainfallChart.Refresh();
                    }
                else
                    {
                    RainfallSWChart.Axes.Left.Automatic = true;
                    RainfallSWChart.Axes.Right.AutomaticMaximum = true;
                    RainfallSWChart.Axes.Right.Minimum = 0;
                    RainfallSWChart.Axes.Bottom.Automatic = true;
                    ErosionChart.Axes.Left.Automatic = true;
                    ErosionChart.Axes.Right.Automatic = true;
                    ErosionChart.Axes.Bottom.Automatic = true;
                    SoilNitrogenChart.Axes.Left.Automatic = true;
                    SoilNitrogenChart.Axes.Right.Automatic = true;
                    SoilNitrogenChart.Axes.Bottom.Automatic = true;
                    axis1.AutomaticMaximum = true;
                    axis1.Minimum = 0;
                    LTRainfallChart.Axes.Left.Automatic = false;
                    LTRainfallChart.Axes.Right.Automatic = false;
                    LTRainfallChart.Axes.Bottom.Automatic = true;
                    LTRainfallChart.Axes.Left.Minimum = 0;
                    LTRainfallChart.Axes.Right.Minimum = 0;
                    StatusLabel2.Text = "Building chart";
                    ProgressBar1.Minimum = 0;
                    ProgressBar1.Maximum = chartDataTable.Rows.Count;
                    ProgressBar1.Step = 1;
                    DataTable longTermRain = metObject.RainMonthlyAverage;

                    foreach (DataRow row in chartDataTable.Rows)
                        {
                        DateTime date = new DateTime();
                        date = Convert.ToDateTime(row["Date"]);
                        //Rainfall and soil water graph
                        RainfallBar.Add(date, Convert.ToDouble(row["Rainfall"]));
                        RunoffBar.Add(date, Convert.ToDouble(row["Runoff"]));
                        SWLine.Add(date, Convert.ToDouble(row["SoilWater"]));
                        //Soil Nitrogen graph
                        NitrateLine.Add(date, Convert.ToDouble(row["NO3Total"]));
                        SurfaceMoistureLine.Add(date, (Convert.ToDouble(row["SoilWaterTopLayer"]) * 150));
                        MaxTemperatureLine.Add(date, Convert.ToDouble(row["MaxTemp"]));
                        //Erosion graph
                        ErosionRunoffCumLine.Add(date, Convert.ToDouble(row["RunoffCum"]));
                        ErosionSoilLossCumLine.Add(date, Convert.ToDouble(row["SoilLossCum"]));
                        //Long term rainfall
                        if (date.Day == 1)
                            {
                            DataRow yearRow=null;
                            foreach (DataRow tableRow in longTermRain.Rows)
                                {
                                if (Convert.ToInt16(tableRow[0]) == date.Year)
                                    {
                                    yearRow = tableRow;
                                    break;
                                    }
                                }
                            double monthlyAverageRain = Convert.ToDouble(yearRow[date.Month]);
                            LTRainfallBar.Add(date, monthlyAverageRain);
                            LTAvRainfallLine.Add(date, metObject.RainMonthlyYearlyAverage[date.Month]);
                            }
                        ProgressBar1.PerformStep();
                        }
                    StatusLabel2.Text = "";
                    ProgressBar1.Value = 0;
                    RainfallSWChart.Refresh();
                    SoilNitrogenChart.Refresh();
                    ErosionChart.Refresh();
                    LTRainfallChart.Refresh();
                    }
                //Profile chart
                ProfileChart.Axes.Top.Minimum = 0;
                ProfileChart.Axes.Left.Minimum = 0;
                ProfileCLLLine.Add(cLL, simulationObject.Soil.CumThickness);
                ProfileLL15Line.Add(simulationObject.Soil.LL15, simulationObject.Soil.CumThickness);
                ProfileSWLine.Add(result.soilWaterEndByLayer, simulationObject.Soil.CumThickness);
                ProfileDULLine.Add(simulationObject.Soil.DUL, simulationObject.Soil.CumThickness);
                //make everthing visable;
                tabControl1.Visible = true;
                tabControl1.SelectedIndex = 0;
                RainfallSWChart.Visible = true;
                SoilNitrogenChart.Visible = false;
                ErosionChart.Visible = false;
                LTRainfallChart.Visible = false;
                ProfileChart.Visible = false;
                WaterPanel.Visible = true;
                CoverPanel.Visible = true;
                NitrogenPanel.Visible = true;
                NRequirementPanel.Visible = true;
                ReportButton.Enabled = true;
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
                DataRow row = chartDataTable.Rows[rowCount];
                //Rainfall and soil water graph
                RainfallBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
                RunoffBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Runoff"]));
                SWLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["SoilWater"]));
                //Soil Nitrogen graph
                NitrateLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["NO3Total"]));
                SurfaceMoistureLine.Add(Convert.ToDateTime(row["Date"]), (Convert.ToDouble(row["SoilWaterTopLayer"])*150));
                MaxTemperatureLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["MaxTemp"]));
                //Erosion graph
                ErosionRunoffCumLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["RunoffCum"]));
               ErosionSoilLossCumLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["SoilLossCum"]));
                //Long term rainfall
                LTRainfallBar.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
         //       LTAvRainfallLine.Add(Convert.ToDateTime(row["Date"]), Convert.ToDouble(row["Rainfall"]));
                rowCount++;
              
                RainfallSWChart.Refresh();
                SoilNitrogenChart.Refresh();
                ErosionChart.Refresh();
                LTRainfallChart.Refresh();
                ProgressBar1.PerformStep();
                }
            else
                {
                StatusLabel2.Text = "";
                ProgressBar1.Value = 0;
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

        private void saveAllData()
            {
            config.SaveDefaults();
            //write form values
            if (simulationObject.FileName == "untitled")
                {
                SaveFileDialog saveDialog = new SaveFileDialog();
                saveDialog.Filter = "APSIM files (*.apsim)|*.apsim";
                if (saveDialog.ShowDialog() == DialogResult.OK)
                    {
                    simulationObject.FileName = saveDialog.FileName;
                    }
                }
            FileInfo fileInfo = new FileInfo(simulationObject.FileName);
            simulationObject.SimulationName = fileInfo.Name.Substring(0,fileInfo.Name.IndexOf(".apsim"));
            simulationObject.OutputFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".out";
            simulationObject.SummaryFileName = simulationObject.FileName.Substring(0, simulationObject.FileName.IndexOf(".apsim")) + ".sum";

            APSIMData apsimObject = new APSIMData();
            apsimObject = simulationObject.Data;
                             
            apsimObject.SaveToFile(simulationObject.FileName);
            this.Text = simulationObject.FileName;
            }
        private void STATest()
            {
            SaveFileDialog saveDialog = new SaveFileDialog();
            saveDialog.Filter = "APSIM files (*.apsim)|*.apsim";
            if (saveDialog.ShowDialog() == DialogResult.OK)
                {
                simulationObject.FileName = saveDialog.FileName;
                }
            }
        #endregion

        

       

       

        

        

        

       

        

        

        

        
        }
    }