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
        private ArrayList constants;
        private StringCollection headings;
        private StringCollection units;


        public MetData(MetData met)
            {
            }
        
        public MetData(String fileName)
            {
            String errString = "";
            const String FUNCTION_NAME = "MetData";
            
            APSIMInputFile metInput = new APSIMInputFile();
            DateTime startDate = new DateTime();
            DateTime endDate = new DateTime();
            Boolean isAPSIMMetFile = true;
            //get values from met file
            errString = "reading Met file";
            APSIMInputFile tempMetInput=new APSIMInputFile();
            //test if is an APSIM format file or not
            try
                {
                tempMetInput.GetStartEndDate(fileName, ref startDate, ref endDate);
                }
            catch (Exception e)
                {
                isAPSIMMetFile = false;
                }
            if (isAPSIMMetFile)
                {
                ReadAPSIMMetFile(fileName);
                }
            else
                {
                ReadNonAPSIMMetFile(fileName);
                }
            }

        private void ReadNonAPSIMMetFile(String fileName)
            {
            }

        private void ReadAPSIMMetFile(String fileName)
            {
            String errString = "";
            const String FUNCTION_NAME = "MetData";
            try
                {
                APSIMInputFile metInput = new APSIMInputFile();
               // DateTime startDate = new DateTime();
               // DateTime endDate = new DateTime();
                //get values from met file
                errString = "reading Met file";
                APSIMInputFile tempMetInput=new APSIMInputFile();
               // tempMetInput.GetStartEndDate(fileName, ref startDate, ref endDate);
                metInput.ReadFromFile(fileName);
                constants = metInput.Constants;
                headings=metInput.Headings;
                units = metInput.Units;
                //hydrate this object
                errString = "hydrating MetData Object";
                this.Data = metInput.Data;
                this.FileName = fileName;
               // this.StartDate = startDate;
               // this.EndDate = endDate;
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
       
        public double averageRainInNext(DateTime startDate,int days)
            {
            double totalAverageRain = 0;
            TimeSpan daySpan = new TimeSpan(days, 0, 0, 0);
            int day=1;
         //   while (day <= daySpan.Days)
         //       {
         //       DateTime newStartDate = startDate.Add(new TimeSpan(day, 0, 0, 0));
         //       totalAverageRain = totalAverageRain + RainDailyYearlyAverage[newStartDate.Month];
         //       day++;
         //       }
           // return totalAverageRain;
            return 0;
            }

        public String Site
            {
            set { site = value; }
            get { return site; }
            }

        public DateTime StartDate()
            {
            return (DateTime)data.Rows[0]["Date"];
            }

        public DateTime EndDate()
            {
            return (DateTime)data.Rows[data.Rows.Count - 1]["Date"];
            }

        public String FileName
            {
            set { fileName = value; }
            get { return fileName; }
            }
     
        public DataTable Data
            {
            set { data = value; }
            get { return data; }
            }
        
        public ArrayList Constants
            {
            set { constants = value; }
            get { return constants; }
            }

        public StringCollection Headings
            {
            set { headings = value; }
            get { return headings; }
            }

        public StringCollection Units
            {
            set { units = value; }
            get { return units; }
            }
       
        }
    }
