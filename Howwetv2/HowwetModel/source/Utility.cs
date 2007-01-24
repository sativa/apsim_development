using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Data;
using VBGeneral;
using CSGeneral;
using System.Xml;
using APSRU.Error;
using APSRU.Model;
namespace APSRU.Model.Howwet
    {
    // This class encapsulates Howwet general functionality  
    public class Utility
        {
        private APSIMData apsimData=new APSIMData();
        private String applicationDirectory = "";

        public Utility(String appPath,String howwetSetupFileName)
            {
            String exeFolder = Path.GetDirectoryName(appPath);
            if (File.Exists(exeFolder + howwetSetupFileName))
                {
                applicationDirectory= exeFolder;
                }
            else
                {
                String newPath = Directory.GetParent(exeFolder).ToString();
                if (File.Exists(newPath + howwetSetupFileName))
                    {
                    applicationDirectory = newPath;
                    }
                }
            }

        public String ApplicationDirectory
            {
            get
                {
                return applicationDirectory;
                }
            }

        public CoverCrop GetCrop(ArrayList cropList,String crop)
            {
            CoverCrop cropOut=new CoverCrop();
            for (int i = 0; i < cropList.Count; i++)
                {
                CoverCrop cropTemp = (CoverCrop)cropList[i];
                if (cropTemp.Name == crop)
                    {
                    cropOut=cropTemp;
                    break;
                    }
                }
            return cropOut;
            }

        public decimal ConvertCoverKgToPercent(double mass,double specific_area)
            {
            decimal percent=Convert.ToDecimal((1-Math.Exp(-specific_area*mass))*100);
            return percent;
            }

        public double ConvertCoverPercentToKg(decimal percent,double specific_area)
            {
            double mass = -((Math.Log(1 - (Convert.ToDouble(percent)), 2.718281828459)) / specific_area);
            return mass;
            }

        public APSIMData GetSoil(string soilName)
            {
            APSIMData soil = new APSIMData();
            return apsimData.Child(soilName);
            }

        public APSIMData ReadTemplateFile(String fileLocation)
            {
            APSIMData template = new APSIMData();
            template.LoadFromFile(fileLocation);
            return template;
            }

        public APSIMData SoilData
            {
            get { return apsimData; }
            }

        //looks thru the metObject and checks sequence and that all rows have radn, temp/max/min data
        // returns a fixed MetData object
        public MetData TestMetObject(MetData metObject,APSRU.Model.Howwet.Region region)
            {
            DateTime currentDate = (DateTime)metObject.Data.Rows[0]["Date"];
            DateTime lastDate = currentDate.Subtract(new TimeSpan(1, 0, 0, 0));

            DataTable tempDataTable = metObject.Data.Copy();
            tempDataTable.Clear(); //clear data but keep schema
            //find the gaps and put into temp table
            foreach (DataRow row in metObject.Data.Rows)
                {
                currentDate = (DateTime)row["Date"];
                while (!(currentDate == lastDate.Add(new TimeSpan(1, 0, 0, 0))))
                    {
                    DataRow newRow = tempDataTable.NewRow();
                    lastDate = lastDate.Add(new TimeSpan(1, 0, 0, 0));
                    newRow["Date"] = lastDate;
                    newRow["Rain"] = 0;
                    tempDataTable.Rows.Add(newRow);
                    }
                lastDate = currentDate;
                }
            //add the temp table to the original metObject table
            foreach (DataRow row in tempDataTable.Rows)
                {
                DataRow newRow = metObject.Data.NewRow();
                DateTime date = (DateTime)row["Date"];
                newRow["Date"] = row["Date"];
                newRow["Site"] = metObject.Site;
                newRow["radn"] = region.AverageMonthlyRadiation[date.Month-1];
                newRow["maxt"] = region.AverageMonthlyMaxT[date.Month - 1]; 
                newRow["mint"] = region.AverageMonthlyMinT[date.Month -1];
                newRow["Rain"] = row["Rain"];
                metObject.Data.Rows.Add(newRow);
                
                }
            //reorder the rows on date
            DataRow[] dateOrderedRows = metObject.Data.Select("Date > '" + metObject.Data.Rows[0]["Date"] + "'", "Date ASC");
            DataTable newTempDataTable = metObject.Data.Copy();
            newTempDataTable.Rows.Clear();
            //put the reordered rows in to a datatable 
            foreach (DataRow row in dateOrderedRows)
                {
                DataRow newRow = newTempDataTable.NewRow();
                newRow["Date"] = row["Date"];
                newRow["Site"] = row["Site"];
                newRow["radn"] = row["radn"];
                newRow["maxt"] = row["maxt"];
                newRow["mint"] = row["mint"];
                newRow["Rain"] = row["Rain"];
                newTempDataTable.Rows.Add(newRow);
                }
            //copy the temp reordered datatable over the original metObject datatable
            metObject.Data = newTempDataTable;
            return metObject;
            }

        public void WriteAPSIMMetFile(MetData met)
            {
            String errString = "";
            const String FUNCTION_NAME = "WriteAPSIMMetFile";
            try
                {
                StreamWriter outStream = new StreamWriter(met.FileName);
                errString = "writing " + met.FileName;
                DateTime firstYear = (DateTime)met.Data.Rows[0]["date"];
                DateTime lastYear = (DateTime)met.Data.Rows[met.Data.Rows.Count - 1]["date"];
                String title = "!Title =" + met.FileName + " " + firstYear.Year + " - " + lastYear.Year;
                outStream.WriteLine(title);
                //write out the original file headers and comments + any new ones
                foreach (APSIMConstant con in met.Constants)
                    {
                    outStream.WriteLine(con.Name + " = " + con.Value + " (" + con.Units + ") " + " ! " + con.Comment);
                    }
                //headings
                errString = "Writting headings";
                String headingString = "";
                int headingCount = met.Headings.Count;
                for (int i = headingCount - 1; i >= 7; i--)
                    {
                    met.Headings.RemoveAt(i);
                    }
                foreach (String heading in met.Headings)
                    {
                    headingString = headingString + " " + heading;
                    }

                outStream.WriteLine(headingString);
                //Units
                errString = "Writting Unit";
                int unitCount = met.Units.Count;
                for (int j = unitCount - 1; j >= 7; j--)
                    {
                    met.Units.RemoveAt(j);
                    }
                String unitString = "";
                foreach (String unit in met.Units)
                    {
                    unitString = unitString + " " + unit;
                    }
                outStream.WriteLine(unitString);
                //Data
                errString = "Writting Rows";
                foreach (DataRow row in met.Data.Rows)
                    {
                    foreach (String heading in met.Headings)
                        {
                        DateTime date = new DateTime();
                        date = (DateTime)row["date"];
                        switch (heading)
                            {
                            case "day":
                                outStream.Write(date.DayOfYear);
                                break;
                            case "Day":
                                outStream.Write(date.DayOfYear);
                                break;
                            case "year":
                                outStream.Write(date.Year);
                                break;
                            case "Year":
                                outStream.Write(date.Year);
                                break;
                            default:
                                outStream.Write(row[heading].ToString());
                                break;
                            }
                        outStream.Write(" ");
                        }
                    outStream.Write("\r\n");
                    }
                outStream.Close();
                }
            catch (Exception e)
                {
                throw new CustomException(new CustomError("", "Cannot write APSIM Met file", errString + "\n Exception:" + e.ToString(), FUNCTION_NAME, this.GetType().FullName, true));
                }
            }
        }
    }
