using System;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Data.SqlClient;

using System.Drawing;
using System.Text;
using System.Windows.Forms;
using FarPoint.Win.Spread;
using FarPoint.Win.Spread.CellType;
using APSRU.Translator.Howwet;
using APSRU.Model.Howwet;
using CSGeneral;


namespace APSRU.Howwet
    {
    public partial class RainfallEditor : Form
        {
        public RainfallEditor()
            {
            InitializeComponent();
            }
        private MetData metObject;

        public void displayData(MetData metObject)
            {
            this.metObject = metObject;
            label3.Text = metObject.FileName;
            DateTime firstYear = (DateTime)metObject.Data.Rows[0]["date"];
            DateTime lastYear = (DateTime)metObject.Data.Rows[metObject.Data.Rows.Count - 1]["date"];
            yearSelectUpDown.Minimum = firstYear.Year;
            yearSelectUpDown.Maximum = lastYear.Year;
            yearSelectUpDown.Increment = 1;
            yearSelectUpDown.Value = lastYear.Year;
            }

        private void yearSelectUpDown_ValueChanged(object sender, EventArgs e)
            {
            int selectedYear = (int)yearSelectUpDown.Value; ;
            String sql = "Date >= '1-1-" + selectedYear + "' AND Date < '31-12-" + selectedYear + "'";
            DataRow[] yearRows = this.metObject.Data.Select(sql);
            if(!(yearRows.Length==0))
                {
           
                int lastMonth = 0;
                fpSpread1.Sheets[0].ClearRange(0, 1, 31, 13, true);
                foreach (DataRow row in yearRows)
                    {
                    DateTime dateTime = (DateTime)row["date"];
                    int month = dateTime.Month;
                    int day = dateTime.Day;
                    if (!(month == lastMonth))
                        {
                        int daysInMonth = DateTime.DaysInMonth(selectedYear, month);
                        //lock the free cells
                        for (int i = daysInMonth; i < 31; i++)
                            {
                            fpSpread1.Sheets[0].Cells[i, month].Locked = true;
                            fpSpread1.Sheets[0].Cells[i, month].BackColor = Color.Gray;
                            }
                        lastMonth = month;
                        }
                 
                    if (!(Convert.ToInt16(row["rain"])==0))
                        {
                        fpSpread1.Sheets[0].Cells[day-1, month].Value = row["rain"];
                        }
                    }
                }
                else
                {
                fpSpread1.Sheets[0].ClearRange(0, 1, 31, 13, true);
                }
                
            updateRainfallTotal();
            }
            
        private void updateRainfallTotal()
            {
            int totalRainfall = 0;
            for (int i = 1; i <= 12; i++)
                {
                totalRainfall = totalRainfall + Convert.ToInt16(fpSpread1.Sheets[0].Cells[31, i].Value);
                }
            label2.Text = "Total Rainfall: " + totalRainfall.ToString() + "mm";
            }

        //cell changed
        private void fpSpread1_Change(object sender, ChangeEventArgs e)
            {
            DateTime selectedDate=new DateTime((int)yearSelectUpDown.Value,(int)e.Column,(int)e.Row+1);
            int year = selectedDate.Year;
            int month = selectedDate.Month;
            int day = selectedDate.Day;
            
            String sql = "Date = '"+day+"-"+month+"-"+ year+"'";
            DataRow[] rows = this.metObject.Data.Select(sql);

            if (!(rows.Length == 0))
                {//update
                DataRow row = (DataRow)rows.GetValue(0);
                row["rain"] = Convert.ToInt16(fpSpread1.Sheets[0].Cells[e.Row, e.Column].Value);
                updateRainfallTotal();
                }
            else
                {
                DataRow newRow = this.metObject.Data.NewRow();
                newRow["site"] = metObject.Site;
                newRow["Date"] = selectedDate;
                newRow["radn"] = metObject.RadnYearlyAverage[selectedDate.Month];
                newRow["maxt"] = metObject.MaxtYearlyAverage[selectedDate.Month];
                newRow["mint"] = metObject.MintYearlyAverage[selectedDate.Month];
                newRow["evap"] = metObject.EvapYearlyAverage[selectedDate.Month];
                newRow["rain"] = Convert.ToInt16(fpSpread1.Sheets[0].Cells[e.Row, e.Column].Value);
                this.metObject.Data.Rows.Add(newRow);
                }
            }
                
        void RainfallEditor_FormClosing(object sender, System.Windows.Forms.FormClosingEventArgs e)
            {
            //write new data
            metObject.overWriteMetFile();
            //generate dataChangedEvent
            }
       
        private void AddYearButton_Click(object sender, EventArgs e)
            {
            if(yearSelectUpDown.Maximum <(decimal)DateTime.Today.Year)
                {
                yearSelectUpDown.Maximum++;
                yearSelectUpDown.Value = yearSelectUpDown.Maximum;
                //default all values in the year to averages and zero the rain
                DateTime startDate=new DateTime((int)yearSelectUpDown.Value,1,1);
                DateTime endDate =new DateTime ((int)yearSelectUpDown.Value,12,31);
                while (startDate <= endDate)
                    {
                    DataRow newRow = this.metObject.Data.NewRow();
                    newRow["site"] = metObject.Site;
                    newRow["Date"] = startDate;
                    newRow["radn"] = metObject.RadnYearlyAverage[startDate.Month];
                    newRow["maxt"] = metObject.MaxtYearlyAverage[startDate.Month];
                    newRow["mint"] = metObject.MintYearlyAverage[startDate.Month];
                    newRow["evap"] = metObject.EvapYearlyAverage[startDate.Month];
                    newRow["rain"] = 0;
                    this.metObject.Data.Rows.Add(newRow);
                    startDate = startDate.AddDays(1);
                    }
                }
            }
        
        }
    }