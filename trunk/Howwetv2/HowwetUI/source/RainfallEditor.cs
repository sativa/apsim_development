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
using APSRU.Model.Howwet;
using CSGeneral;


namespace APSRU.Howwet
    {
    public partial class RainfallEditor : Form
        {
        private APSRU.Model.Howwet.Region selectedRegion;
        private MetData tempMetObject=null;
        private static RainfallEditor instance = null;
        public bool isLoaded = false;
        private DataRow lastRowInFile;
        public delegate void RainfallEditorSave(MetData met);
        public event RainfallEditorSave RainfallEditorSaveEvent;

        public RainfallEditor()
            {
            InitializeComponent();
            }

        public static RainfallEditor Instance
            {
            get
              {
              if(instance==null)instance = new RainfallEditor();
              return instance;
              }
            }

        public void loadObject(MetData data, APSRU.Model.Howwet.Region region)
            {
            this.tempMetObject = data;
            this.selectedRegion = region;
            isLoaded = true;
            fpSpread1.Sheets[0].ClearRange(0, 1, 31, 13, true);
            DateTime firstDate = (DateTime)this.tempMetObject.Data.Rows[0]["date"];
            lastRowInFile = this.tempMetObject.Data.Rows[this.tempMetObject.Data.Rows.Count-1];
            DateTime lastDate = (DateTime)lastRowInFile["date"];
            yearSelectUpDown.Minimum = firstDate.Year;
            yearSelectUpDown.Maximum = lastDate.Year;
            yearSelectUpDown.Increment = 1;
            yearSelectUpDown.Value = lastDate.Year;
            this.Text = "Rainfall Editor         Rainfall File:" +tempMetObject.FileName+"      "+firstDate.Year+" to "+lastDate.Year ;
            }

        private void yearSelectUpDown_ValueChanged(object sender, EventArgs e)
            {
            int selectedYear = (int)yearSelectUpDown.Value; ;
            String sql = "Date >= '1-1-" + selectedYear + "' AND Date <= '31-12-" + selectedYear + "'";
            DataRow[] yearRows = this.tempMetObject.Data.Select(sql);
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
            DataRow[] rows = this.tempMetObject.Data.Select(sql);

            if (!(rows.Length == 0))
                {//update
                DataRow row = (DataRow)rows.GetValue(0);
                row["rain"] = Convert.ToInt16(fpSpread1.Sheets[0].Cells[e.Row, e.Column].Value);
                updateRainfallTotal();
                }
            else
                {
                DataRow newRow = this.tempMetObject.Data.NewRow();
                if (!(this.tempMetObject.Site == ""))
                    {
                    newRow["site"] = this.tempMetObject.Site;
                    }
                newRow["Date"] = selectedDate;
                newRow["radn"] = selectedRegion.AverageMonthlyRadiation[selectedDate.Month-1];
                newRow["maxt"] = selectedRegion.AverageMonthlyMaxT[selectedDate.Month-1];
                newRow["mint"] = selectedRegion.AverageMonthlyMinT[selectedDate.Month-1];
                newRow["rain"] = Convert.ToInt16(fpSpread1.Sheets[0].Cells[e.Row, e.Column].Value);
                this.tempMetObject.Data.Rows.Add(newRow);
                }
            }
                
        void RainfallEditor_FormClosing(object sender, System.Windows.Forms.FormClosingEventArgs e)
            {
            isLoaded = false;
            instance = null;
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
                }
            }

        private void saveCloseButton_Click(object sender, EventArgs e)
            {
            RainfallEditorSaveEvent(this.tempMetObject);
            this.Close();
            }
 

        private void closeButton_Click(object sender, EventArgs e)
            {
            this.Close();
            }

        private void fpSpread1_KeyDown(object sender, KeyEventArgs e)
            {
            if (e.KeyCode == Keys.Delete)
                {
                fpSpread1.Sheets[0].ActiveCell.Value = null;
                }
            }
        
        }
    }