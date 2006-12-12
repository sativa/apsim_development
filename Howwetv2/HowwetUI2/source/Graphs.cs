using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace APSRU.Howwet
    {
    public partial class Graphs : APSRU.Howwet.PageTemplate
        {
        private DataTable chartData;
        private DateTime fallowStartDate;
        private DateTime fallowEndDate;

        public Graphs()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Graphs";
            this.SecondaryHeadingLabel = "Graphs";   
            }

        private void Graphs_Load(object sender, EventArgs e)
            {
            
            }

        private void DisplayCharts()
            {

            RainfallBar.Clear();
            RunoffBar.Clear();
            SWLine.Clear();
            // NitrateLine.Clear();
            //  SurfaceMoistureLine.Clear();
            //  MaxTemperatureLine.Clear();
            //   ErosionRunoffCumLine.Clear();
            //   ErosionSoilLossCumLine.Clear();
            //   LTRainfallBar.Clear();
            //   LTAvRainfallLine.Clear();
            //   ProfileCLLLine.Clear();
            //    ProfileDULLine.Clear();
            //   ProfileLL15Line.Clear();
            //  ProfileSWLine.Clear();


            RainfallSWChart.Axes.Left.Automatic = false;
            RainfallSWChart.Axes.Right.Automatic = false;
            RainfallSWChart.Axes.Bottom.Automatic = false;
            //    ErosionChart.Axes.Left.Automatic = false;
            //    ErosionChart.Axes.Right.Automatic = false;
            //   ErosionChart.Axes.Bottom.Automatic = false;
            //    SoilNitrogenChart.Axes.Left.Automatic = false;
            //    SoilNitrogenChart.Axes.Right.Automatic = false;
            //    SoilNitrogenChart.Axes.Bottom.Automatic = false;
            //    axis1.Automatic = false;
            //    LTRainfallChart.Axes.Left.Automatic = false;
            //    LTRainfallChart.Axes.Right.Automatic = false;
            //    LTRainfallChart.Axes.Bottom.Automatic = false;
            //set x axis range

            RainfallSWChart.Axes.Bottom.Maximum = fallowStartDate.ToOADate();
            RainfallSWChart.Axes.Bottom.Minimum = fallowEndDate.ToOADate();
            //     SoilNitrogenChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
            //     SoilNitrogenChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
            //     ErosionChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
            //     ErosionChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();
            //    LTRainfallChart.Axes.Bottom.Maximum = Convert.ToDateTime(simulationObject.EndDate).ToOADate();
            //   LTRainfallChart.Axes.Bottom.Minimum = Convert.ToDateTime(simulationObject.StartDate).ToOADate();

            //set left y axis range
            double maxRainfallSW = 0, minRainfallSW = 0, maxSW = 0, minSW = 10000;
            //   double maxNitrate = 0, minNitrate = 10000, maxSurfaceMoisture = 0, minSurfaceMoisture = 10000, maxMaxTemp = 0, minMaxTemp = 10000; ;
            //   double maxSoilLoss = 0, minSoilLoss = 0, maxRunoff = 0, minRunoff = 0;
            //   double maxLTRainfall = 0, minLTRainfall = 0;

            // if (config.TrainingMode)
            //     {
            //     foreach (DataRow row in chartDataTable.Rows)
            ///         {
            //Rainfall soil water; subtract cll from soilwater and sum the absolute values to get sw
            //         if (Convert.ToDouble(row["Rainfall"]) > maxRainfallSW) maxRainfallSW = Convert.ToDouble(row["Rainfall"]);
            //         if(Convert.ToDouble(row["SoilWater"]) > maxSW)maxSW=Convert.ToDouble(row["SoilWater"]);
            //         if(Convert.ToDouble(row["SoilWater"]) < minSW)minSW=Convert.ToDouble(row["SoilWater"]);
            //Soil Nitrogen
            //        if (Convert.ToDouble(row["NO3Total"]) > maxNitrate) maxNitrate = Convert.ToDouble(row["NO3Total"]);
            //          if (Convert.ToDouble(row["NO3Total"]) < minNitrate) minNitrate = Convert.ToDouble(row["NO3Total"]);
            //          if (Convert.ToDouble(row["SoilWaterTopLayer"]) > maxSurfaceMoisture) maxSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
            //          if (Convert.ToDouble(row["SoilWaterTopLayer"]) < minSurfaceMoisture) minSurfaceMoisture = Convert.ToDouble(row["SoilWaterTopLayer"]);
            //          if (Convert.ToDouble(row["MaxTemp"]) > maxMaxTemp) maxMaxTemp = Convert.ToDouble(row["MaxTemp"]);
            //          if (Convert.ToDouble(row["MaxTemp"]) < minMaxTemp) minMaxTemp = Convert.ToDouble(row["MaxTemp"]);

            //Erosion
            //            if (Convert.ToDouble(row["SoilLossCum"]) > maxSoilLoss) maxSoilLoss = Convert.ToDouble(row["SoilLossCum"]);
            //           if (Convert.ToDouble(row["RunoffCum"]) > maxRunoff) maxRunoff = Convert.ToDouble(row["RunoffCum"]);
            //Long term rainfall
            //           if (Convert.ToDouble(row["Rainfall"]) > maxLTRainfall) maxLTRainfall = Convert.ToDouble(row["Rainfall"]);
            //  if (Convert.ToDouble(row["Rainfall"]) > maxLTAvRainfall) maxLTAvRainfall = Convert.ToDouble(row["Rainfall"]);
            //     }
            //Rainfall and soil water 
            //    RainfallSWChart.Axes.Left.Maximum = maxRainfallSW;
            //      RainfallSWChart.Axes.Left.Minimum = minRainfallSW;
            //      RainfallSWChart.Axes.Right.Maximum = maxSW;
            //   RainfallSWChart.Axes.Right.Minimum = minSW;
            //       RainfallSWChart.Axes.Right.Minimum = 0;
            //soil nitrogen
            //       SoilNitrogenChart.Axes.Left.Maximum = maxNitrate;
            //         SoilNitrogenChart.Axes.Left.Minimum = minNitrate;
            //         SoilNitrogenChart.Axes.Right.Maximum = maxSurfaceMoisture * 150;
            ///         SoilNitrogenChart.Axes.Right.Minimum = 0;
            //   SoilNitrogenChart.Axes.Right.Minimum = minSurfaceMoisture * 150;
            //         SoilNitrogenChart.Axes.Custom[0].Maximum = maxMaxTemp;
            //  SoilNitrogenChart.Axes.Custom[0].Minimum = minMaxTemp;
            //         SoilNitrogenChart.Axes.Custom[0].Minimum = 0;
            //Erosion
            //         ErosionChart.Axes.Left.Maximum = maxSoilLoss;
            //          ErosionChart.Axes.Left.Minimum = minSoilLoss;
            //          ErosionChart.Axes.Right.Maximum = maxRunoff;
            //         ErosionChart.Axes.Right.Minimum = minRunoff;
            //Long term rainfall
            //         LTRainfallChart.Axes.Left.Maximum = maxLTRainfall;
            //          LTRainfallChart.Axes.Left.Minimum = minLTRainfall;
            //    LTRainfallChart.Axes.Right.Maximum = maxLTAvRainfall;
            //    LTRainfallChart.Axes.Right.Minimum = minLTAvRainfall;

            //                rowCount = 0;
            //                timer1.Interval = 2;
            //                timer1.Start();
            //                RainfallSWChart.Refresh();
            ////               SoilNitrogenChart.Refresh();
            //               ErosionChart.Refresh();
            //               LTRainfallChart.Refresh();
            //               }
            //           else
            //              {
            //             

            RainfallSWChart.Axes.Left.Automatic = true;
            RainfallSWChart.Axes.Right.AutomaticMaximum = true;
            RainfallSWChart.Axes.Right.Minimum = 0;
            RainfallSWChart.Axes.Bottom.Automatic = true;
            //        ErosionChart.Axes.Left.Automatic = true;
            //        ErosionChart.Axes.Right.Automatic = true;
            //        ErosionChart.Axes.Bottom.Automatic = true;
            //        SoilNitrogenChart.Axes.Left.Automatic = true;
            //        SoilNitrogenChart.Axes.Right.Automatic = true;
            //        SoilNitrogenChart.Axes.Bottom.Automatic = true;
            //        axis1.AutomaticMaximum = true;
            //        axis1.Minimum = 0;
            //         LTRainfallChart.Axes.Left.Automatic = true;
            //         LTRainfallChart.Axes.Right.Automatic = true;
            //         LTRainfallChart.Axes.Bottom.Automatic = true;
            //          DataTable longTermRain = metObject.RainMonthlyAverage;
            if (!(chartData == null))
                {

                foreach (DataRow row in chartData.Rows)
                    {
                    DateTime date = new DateTime();
                    date = Convert.ToDateTime(row["Date"]);
                    //Rainfall and soil water graph
                    RainfallBar.Add(date, Convert.ToDouble(row["Rainfall"]));
                    RunoffBar.Add(date, Convert.ToDouble(row["Runoff"]));
                    SWLine.Add(date, Convert.ToDouble(row["SoilWater"]));
                    //Soil Nitrogen graph
                    //   NitrateLine.Add(date, Convert.ToDouble(row["NO3Total"]));
                    //   SurfaceMoistureLine.Add(date, (Convert.ToDouble(row["SoilWaterTopLayer"]) * 150));
                    //   MaxTemperatureLine.Add(date, Convert.ToDouble(row["MaxTemp"]));
                    //Erosion graph
                    //   ErosionRunoffCumLine.Add(date, Convert.ToDouble(row["RunoffCum"]));
                    //   ErosionSoilLossCumLine.Add(date, Convert.ToDouble(row["SoilLossCum"]));
                    //Long term rainfall
                    //   if (date.Day == 1)
                    //       {
                    //      DataRow yearRow=null;
                    //      foreach (DataRow tableRow in longTermRain.Rows)
                    //          {
                    //          if (Convert.ToInt16(tableRow[0]) == date.Year)
                    //              {
                    //              yearRow = tableRow;
                    //              break;
                    //             }
                    //         }
                    //     double monthlyAverageRain = Convert.ToDouble(yearRow[date.Month]);
                    //     LTRainfallBar.Add(date, monthlyAverageRain);
                    //       LTAvRainfallLine.Add(date, metObject.RainMonthlyYearlyAverage[date.Month]);
                    //       }
                    //                 }
                    RainfallSWChart.Refresh();
                    //        SoilNitrogenChart.Refresh();
                    //        ErosionChart.Refresh();
                    //       LTRainfallChart.Refresh();
                    }
                //Profile chart
                //     ProfileChart.Axes.Top.Minimum = 0;
                //    ProfileChart.Axes.Left.Minimum = 0;
                //    ProfileCLLLine.Add(cLL, simulationObject.Soil.CumThickness);
                //    ProfileLL15Line.Add(simulationObject.Soil.LL15, simulationObject.Soil.CumThickness);
                ////     ProfileSWLine.Add(result.soilWaterEndByLayer, simulationObject.Soil.CumThickness);
                //     ProfileDULLine.Add(simulationObject.Soil.DUL, simulationObject.Soil.CumThickness);
                //make everthing visable;
                //      tabControl1.Visible = true;
                //     tabControl1.SelectedIndex = 0;
                RainfallSWChart.Visible = true;
                //      SoilNitrogenChart.Visible = false;
                //      ErosionChart.Visible = false;
                ///      LTRainfallChart.Visible = false;
                //      ProfileChart.Visible = false;
                //      WaterPanel.Visible = true;
                //       CoverPanel.Visible = true;
                //       NitrogenPanel.Visible = true;
                //       PawPanel.Visible = true;
                //       NRequirementPanel.Visible = true;
                //       ReportButton.Enabled = true;
                }
            }


#region IEventListener Members

        public override void OnNotification(IHowwetModel publisher)
            {
            chartData=publisher.ChartData;
            fallowStartDate=publisher.FallowDateStart;
            fallowEndDate = publisher.FallowDateEnd;
            DisplayCharts();
            }

        #endregion

        
        }
    }

