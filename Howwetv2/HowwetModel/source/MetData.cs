using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Collections.Generic;
using System.Text;
using System.Data;
using CSGeneral;
using APSRU.Error;

namespace APSRU.Model.Howwet
    {
    //this class encapsulates and expands the functionalilty of the APSIMInputFile class
    //given a met file name this class will read and hydrate values into an Object Entity structure
    //plus calculate monthly averages and yearly averages from monthly averages.
    //to will also write back to the met file.
    public class MetData
        {
        
        private DataTable data;
        private String site;
        private String fileName;
        private DateTime startDate;
        private DateTime endDate;
        private int[] radnYearlyAverage;
        private DataTable radnMonthlyAverage;
        private int[] maxtYearlyAverage;
        private DataTable maxtMonthlyAverage;
        private int[] mintYearlyAverage;
        private DataTable mintMonthlyAverage;
        private int[] rainYearlyAverage;
        private DataTable rainMonthlyAverage;
        private int[] evapYearlyAverage;
        private DataTable evapMonthlyAverage;
        private ArrayList constants;
        private StringCollection headings;
        private StringCollection units;
        

        public MetData(String fileName)
            {
            String errString = "";
            const String FUNCTION_NAME = "MetData";
            try
                {
                APSIMInputFile metInput = new APSIMInputFile();
                DateTime startDate = new DateTime();
                DateTime endDate = new DateTime();
                //get values from met file
                errString = "reading Met file";
                APSIMInputFile tempMetInput=new APSIMInputFile();
                tempMetInput.GetStartEndDate(fileName, ref startDate, ref endDate);
                metInput.ReadFromFile(fileName);
                constants = metInput.Constants;
                headings=metInput.Headings;
                units = metInput.Units;
                //hydrate this object
                errString = "hydrating MetData Object";
                this.Data = metInput.Data;
                this.FileName = fileName;
                this.StartDate = startDate;
                this.EndDate = endDate;
                this.Site = "";
                foreach (String heading in headings)
                    {
                    if(heading=="site")
                        {
                        DataRowCollection rows=metInput.Data.Rows;
                        DataRow firstRow = (DataRow)rows[0];
                        this.Site = (String)firstRow["site"];
                        break;
                        }
                    }
                }
            catch (CustomException err)
                {
                err.addError(new CustomError("", "Cannot read Met file", errString + "\n Exception:" + err.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                throw err;
                }
            catch (Exception err)
                {
                throw new CustomException(new CustomError("", "Cannot read Met file", errString + "\n Exception:" + err.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }

        public bool BuildAverages()
            {
            const String FUNCTION_NAME = "MetData";
            String errString = "calculating averages";
            try
                {
                bool success = false;
                this.RadnMonthlyAverage = createMonthlyAverages("radn");
                this.RadnYearlyAverage = getMonthlyYearlyAverages(this.RadnMonthlyAverage);
                this.MaxtMonthlyAverage = createMonthlyAverages("maxt");
                this.MaxtYearlyAverage = getMonthlyYearlyAverages(this.MaxtMonthlyAverage);
                this.MintMonthlyAverage = createMonthlyAverages("mint");
                this.MintYearlyAverage = getMonthlyYearlyAverages(this.MintMonthlyAverage);
                this.RainMonthlyAverage = createMonthlyAverages("rain");
                this.RainYearlyAverage = getMonthlyYearlyAverages(this.RainMonthlyAverage);
                success = true;
                return success;
                }
            catch (CustomException err)
                {
                err.addError(new CustomError("", "Cannot build met averages", errString + "\n Exception:" + err.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                throw err;
                }
            catch (Exception err)
                {
                throw new CustomException(new CustomError("", "Cannot build met averages", errString + "\n Exception:" + err.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }

        public String Site
            {
            set { site = value; }
            get { return site; }
            }

        public DateTime StartDate
            {
            set { startDate = value; }
            get { return startDate; }
            }

        public DateTime EndDate
            {
            set { endDate = value; }
            get { return endDate; }
            }

        public String FileName
            {
            set { fileName = value; }
            get { return fileName; }
            }

        public int[] RadnYearlyAverage
            {
            set { radnYearlyAverage = value; }
            get { return radnYearlyAverage; }
            }

        public DataTable RadnMonthlyAverage
            {
            set { radnMonthlyAverage = value; }
            get { return radnMonthlyAverage; }
            }

        public int[] MaxtYearlyAverage
            {
            set { maxtYearlyAverage = value; }
            get { return maxtYearlyAverage; }
            }

        public DataTable MaxtMonthlyAverage
            {
            set { maxtMonthlyAverage = value; }
            get { return maxtMonthlyAverage; }
            }

        public int[] MintYearlyAverage
            {
            set { mintYearlyAverage = value; }
            get { return mintYearlyAverage; }
            }

        public DataTable MintMonthlyAverage
            {
            set { mintMonthlyAverage = value; }
            get { return mintMonthlyAverage; }
            }

        public int[] RainYearlyAverage
            {
            set { rainYearlyAverage = value; }
            get { return rainYearlyAverage; }
            }

        public DataTable RainMonthlyAverage
            {
            set { rainMonthlyAverage = value; }
            get { return rainMonthlyAverage; }
            }
     
        public DataTable Data
            {
            set { data = value; }
            get { return data; }
            }

        //write met data to existing met file
        public void overWriteMetFile()
            {
            String errString = "";
            const String FUNCTION_NAME = "overWriteMetFile";
            try
                {
                StreamWriter outStream = new StreamWriter(fileName);
                errString = "writing " + fileName;
                DateTime firstYear = (DateTime)this.Data.Rows[0]["date"];
                DateTime lastYear = (DateTime)this.Data.Rows[Data.Rows.Count - 1]["date"];
                String title = "!Title =" + fileName + " " + firstYear.Year + " - " + lastYear.Year;
                outStream.WriteLine(title);
                //write out the original file headers and comments + any new ones
                foreach (APSIMConstant con in constants)
                    {
                    outStream.WriteLine(con.Name + " = " + con.Value + " (" + con.Units + ") " + " ! " + con.Comment);
                    }
                //headings
                String headingString = "";
                int headingCount = headings.Count;
                for (int i = 7; i < headingCount; i++)
                    {
                    headings.RemoveAt(i);
                    }
                foreach(String heading in headings)
                    {
                    headingString = headingString + " " + heading;
                    }
             
                outStream.WriteLine(headingString);
                //Units
                int unitCount = units.Count;
                for (int j = 7; j < unitCount; j++)
                    {
                    units.RemoveAt(j);
                    }
                String unitString = "";
                foreach (String unit in units)
                    {
                    unitString = unitString + " " + unit;
                    }
                outStream.WriteLine(unitString);
                //Data
                foreach (DataRow row in Data.Rows)
                    {
                    foreach (String heading in headings)
                        {
                        DateTime date = new DateTime();
                        date = (DateTime)row["date"];
                        switch (heading)
                            {
                            case "day":
                                outStream.Write(date.DayOfYear);
                                break;
                            case "year":
                                outStream.Write(date.Year);
                                break;
                            default:
                                outStream.Write(row[heading].ToString());
                                break;
                            }
                        outStream.Write(" ");
                        }
                    outStream.Write("\r\n");
                //    outStream.WriteLine(row["site"].ToString() + " " + date.Year + " " + date.DayOfYear + " " + row["radn"].ToString() + " " + row["maxt"].ToString() + " " + row["mint"].ToString() + " " + row["rain"].ToString());
                    }
                outStream.Close();
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Cannot write Met file", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }
        //create monthly averages for the given field across all years
        private DataTable createMonthlyAverages(String field)
            {
            String errString = "";
            const String FUNCTION_NAME = "createMonthlyAverages";
            try
                {
                DataTable yearlySum = new DataTable("MetData");
                yearlySum.Columns.Add("Year");
                yearlySum.Columns.Add("Jan");
                yearlySum.Columns.Add("Feb");
                yearlySum.Columns.Add("Mar");
                yearlySum.Columns.Add("Apr");
                yearlySum.Columns.Add("May");
                yearlySum.Columns.Add("Jun");
                yearlySum.Columns.Add("Jul");
                yearlySum.Columns.Add("Aug");
                yearlySum.Columns.Add("Sep");
                yearlySum.Columns.Add("Oct");
                yearlySum.Columns.Add("Nov");
                yearlySum.Columns.Add("Dec");
                //for each year
                DateTime firstYear = (DateTime)this.Data.Rows[0]["date"];
                DateTime lastYear = (DateTime)this.Data.Rows[this.Data.Rows.Count - 1]["date"];

                for (int year = firstYear.Year; year <= lastYear.Year; year++)
                    {
                    int[] monthlySum = new int[13];
                    int[] dayCount = new int[13];
                    //for the selected year sum the daily totals to a month total
                    String sql = "Date >= '1-1-" + year + "' AND Date < '31-12-" + year + "'";
                    DataRow[] yearRows = this.Data.Select(sql);
                    if (!(yearRows.Length == 0))
                        {
                        foreach (DataRow row in yearRows)
                            {
                            DateTime date = (DateTime)row["Date"];
                            if (!(Convert.ToInt16(row[field]) == 0))
                                {
                                monthlySum[date.Month] = monthlySum[date.Month] + Convert.ToInt16(row[field]);
                                dayCount[date.Month] = date.Day;
                                }
                            }
                        }
                    //generate the averages for each month within the selected year
                    int[] monthlyAverage = new int[13];
                    int[] yearSumAverage = new int[13];
                    int itemsInArray = 1;
                    for (int loop = 1; loop < dayCount.Length; loop++)
                        {
                        if (!(dayCount[loop] == 0)) itemsInArray++;
                        }
                    for (int month = 1; month < itemsInArray; month++)
                        {
                        if (dayCount[month] == 0)//test divide by zero
                            {
                            monthlyAverage[month] = 0;
                            }
                        else
                            {
                            monthlyAverage[month] = monthlySum[month] / dayCount[month];
                            }
                        }
                    //put the monthly averages per year in a data table
                    DataRow newRow = yearlySum.NewRow();
                    newRow["Year"] = year;
                    for (int col = 1; col < monthlyAverage.Length; col++)
                        {
                        newRow[col] = monthlyAverage[col];
                        }
                    yearlySum.Rows.Add(newRow);
                    }
                return yearlySum;
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Cannot create monthly averages", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }

        //calculate yearly averages of monthly average 
        private int[] getMonthlyYearlyAverages(DataTable yearlySum)
            {
            String errString = "";
            const String FUNCTION_NAME="getMonthlyYearlyAverages";
            try
                {
                //generate the averages for each month across all years
                int[] monthlyYearlyAverages = new int[13];
                for (int columun = 1; columun < yearlySum.Columns.Count; columun++)
                    {
                    int rowCount = 0;
                    int yearSum = 0;
                    foreach (DataRow row in yearlySum.Rows)
                        {
                        yearSum = yearSum + Convert.ToInt16(row[columun]);
                        rowCount++;
                        }
                    monthlyYearlyAverages[columun] = yearSum / rowCount;
                    }
                return monthlyYearlyAverages;
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Cannot getMonthlyYearlyAverages", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }
        }
    }
