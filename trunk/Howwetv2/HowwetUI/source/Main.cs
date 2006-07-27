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
            InitializeComponent();
            }
        #region init

        private SimulationIn simulationObject;
        private HowwetUtility util;
        public MetData metObject;
        int rowCount = 0;
        private double soilPAWCSum = 0;
       
        DateTime[] date;
        DataTable dt;
        double[] esw,rainfall, som, runoff, evaporation, soilWaterTopLayer, no3Total, maxTemp, soilLoss;
        Xceed.Chart.Core.Chart chart;
        BarSeries rainBar;

        #endregion

        #region Startup

        private void Main_Load(object sender, EventArgs e)
            {
            resetFormValues();
            }
        
        private void resetFormValues()
            {
            util = new HowwetUtility();
            simulationObject = new SimulationIn(util.ReadTemplateFile());
            simulationObject.FileName = "untitled";

            //setup default vaule on form
            slope.Text = simulationObject.ErosionSlope;
            slopeLength.Text = simulationObject.ErosionSlopeLength;
            erodibilty.Text = simulationObject.ErosionErodibilty;
            // bedDepth.Text = simulationObject.ErosionBedDepth;
            typeName.Text = simulationObject.SOMType;
            mass.Text = simulationObject.SOMMass;
            // cnRatio.Text = simulationObject.SOMCNRatio;
            this.Text = simulationObject.FileName;
            soilFileName.Text = "<Select a Soil file>";

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
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.Title = "Browse for Soil File";
            openDialog.Filter = "Soils files (*.soils)|*.soils";
            openDialog.ShowDialog();
            if (!(openDialog.FileName == ""))
                {
                //openDialog.InitialDirectory=
                FileInfo fileInfo = new FileInfo(openDialog.FileName);
                soilFileName.Text = fileInfo.Name;
                soilsList.Items.Clear();

                soilsList.Items.Add("<Select a Soil>");
                StringCollection soilCollection = util.GetListOfSoils(openDialog.FileName);
                foreach (String soil in soilCollection)
                    {
                    soilsList.Items.Add(soil);
                    }
                this.soilsList.SelectedValueChanged -= new System.EventHandler(this.soilsList_SelectedValueChanged);
                soilsList.SelectedIndex = 0;
                this.soilsList.SelectedValueChanged += new System.EventHandler(this.soilsList_SelectedValueChanged);

                }
            }
        
        private void selectSoilButton_Click(object sender, EventArgs e)
            {
            SoilSelection soilForm = new SoilSelection();
            soilForm.Show();
            }

        private void soilsList_SelectedValueChanged(object sender, System.EventArgs e)
            {
            if (!((String)soilsList.SelectedItem == "<Select a Soil>"))
                {
                APSIMData selectedSoil = new APSIMData();
                selectedSoil = util.GetSoil((String)soilsList.SelectedItem);
                //add soil to simulation object
                simulationObject.AddSoil(selectedSoil);
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
                
                organicCarbonContent.Text = simulationObject.Soil.OC.GetValue(0).ToString();
                }
            }

        void proposedCropList_SelectedValueChanged(object sender, System.EventArgs e)
            {
            if (!((String)proposedCropList.SelectedItem == "<Select a Crop>"))
                {
                String selectedCrop=(String)proposedCropList.SelectedItem;
                //TODO not right ask Dean, crop should be added to paddock level
                simulationObject.Soil.AddCrop(selectedCrop); 
                //sum water layers for crop
                double soilPAWCSum = 0;
                double soilDepthSum = 0;
                int count = 0;
                double[] thicknessEachLayer = simulationObject.Soil.Thickness;
                foreach (double layer in simulationObject.Soil.PAWC(selectedCrop))
                    {
                    soilDepthSum = soilDepthSum + thicknessEachLayer[count++];
                    soilPAWCSum = soilPAWCSum + layer;
                    }
                this.soilPAWCSum = soilPAWCSum;
                String[] layers = simulationObject.Soil.DepthStrings;
                ocDepthLabel.Text = layers[0];//top layer string
                soilDepth.Text = soilDepthSum.ToString();

                initialWater.Text = soilPAWCSum.ToString();
                initialSoilWaterPercent.Value = 100;
                }
            }

        void soilDepth_TextChanged(object sender, System.EventArgs e)
            {//update the XF value
            
            }

        void initialSoilWaterPercent_ValueChanged(object sender, System.EventArgs e)
            {
            decimal newValue = (Convert.ToDecimal(this.soilPAWCSum) * initialSoilWaterPercent.Value) / 100;
            initialWater.Text = newValue.ToString();
            }

        void initialWater_TextChanged(object sender, System.EventArgs e)
            {
            double newValue = (Convert.ToDouble(initialWater.Text) / this.soilPAWCSum) * 100;

            initialSoilWaterPercent.Value = Convert.ToDecimal(newValue);

            }

        private void browseMetButton_Click(object sender, EventArgs e)
            {
            //Select a met file
            OpenFileDialog fdlg = new OpenFileDialog();
            fdlg.Title = "Browse for Met File";
            fdlg.Filter = "Met files (*.met)|*.met";
            fdlg.ShowDialog();
            if (!(fdlg.FileName == ""))
                {
                try
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
                    }
                catch (CustomException err)
                    {
                    showMessages(err);
                    }
                }
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

        private void button2_Click(object sender, EventArgs e)
            {
            SoilSelection form = new SoilSelection();
           
            form.Show();
         
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

        private void showMessages(CustomException err)
            {
            bool isDebug = true;
            String displayString = "";
            if (isDebug)
                {//show all info
                IEnumerator errorList = (IEnumerator)err.getErrors().GetEnumerator();
                while (errorList.MoveNext())
                    {
                    CustomError error = (CustomError)errorList.Current;
                    displayString = displayString + "Err #:" + error.ErrorNumber + "\n Pub Mess:" + error.PublicMessage;
                    }
                }
            else
                {//only show last public message
                IEnumerator errorList = (IEnumerator)err.getErrors().GetEnumerator();
                errorList.MoveNext();
                CustomError firstError = (CustomError)errorList.Current;
                displayString = firstError.PublicMessage;
                }
            MessageBox.Show(displayString, "caption", MessageBoxButtons.OK, MessageBoxIcon.Error);
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
                    MessageBox.Show("Message " + e.Message + " Source " + e.Source);
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



        private void typeName_SelectedIndexChanged(object sender, EventArgs e)
            {

            }
     
        }
    }