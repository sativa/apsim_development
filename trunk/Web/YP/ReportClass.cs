using System;
using System.Data;
using System.IO;
using System.Web;
using System.Web.UI;
using CSGeneral;
using System.Xml;
using System.Collections;
using System.Collections.Specialized;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for ReportClass.
	/// </summary>

	public class ReportClass
	{
		public ReportClass()
		{}


		#region Public Variables
		//-------------------------------------------------------------------------
		public const string szAgronomicReport = "Agronomic report";
		public const string szClimateReport = "Climate report";
		public const string szNitrogenComparisonReport = "Nitrogen comparison report";
		public const string szSowingXVarietyReport = "Sowing X variety report";
		//-------------------------------------------------------------------------
		#endregion
		

		
		#region Manage Reports
		//-------------------------------------------------------------------------
		//Takes a users user name and password and creates a report directory for them
		//The directory will be in the following format:
		// YP/Reports/1/2005/  is the report directory for the user with the UserID 
		// 1 and for the year 2005
		//-------------------------------------------------------------------------
		public static void CreateUsersReportDirectory(string szUserName)
			{	
			string szDirectoryLocaton = HttpContext.Current.Server.MapPath("/YP/")+"Reports";
			//Creates the Reports directory if it doesn't exist
			if(System.IO.Directory.Exists(szDirectoryLocaton)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocaton);
				}
			//Creates the Users sub directory if it doesn't exist
			szDirectoryLocaton = szDirectoryLocaton+"\\"+szUserName;
			if(System.IO.Directory.Exists(szDirectoryLocaton)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocaton);
				}
			//Creates the user's report directory
			szDirectoryLocaton = szDirectoryLocaton+"\\"+
				DateTime.Today.Year.ToString();
			System.IO.Directory.CreateDirectory(szDirectoryLocaton);	
			}
		//-------------------------------------------------------------------------
		//Creates a user directory for a given year, same format as above
		//-------------------------------------------------------------------------
		public static void CreateUsersReportDirectory(string szUserName, int iYear)
			{	
			string szDirectoryLocaton = HttpContext.Current.Server.MapPath("/YP/")+"Reports";
			//Creates the Reports directory if it doesn't exist
			if(System.IO.Directory.Exists(szDirectoryLocaton)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocaton);
				}
			//Creates the Users sub directory if it doesn't exist
			szDirectoryLocaton = szDirectoryLocaton+"\\"+szUserName;
			if(System.IO.Directory.Exists(szDirectoryLocaton)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocaton);
				}
			//Creates the user's report directory
			szDirectoryLocaton = szDirectoryLocaton+"\\"+iYear.ToString();
			if(System.IO.Directory.Exists(szDirectoryLocaton)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocaton);	
				}
			}
		//-------------------------------------------------------------------------
		//Takes a UserID and a year and checks if there is a report directory for the
		//given values.
		//-------------------------------------------------------------------------
		public static bool DoesUsersReportDirectoryExisit(string szUserName, int iYear)
			{
			bool bUsersReportDirectoryExists = false;
		
			string szDirectoryLocaton = HttpContext.Current.Server.MapPath("/YP/")+"Reports";
			szDirectoryLocaton = szDirectoryLocaton+"\\"+szUserName+"\\"+iYear.ToString();
			bUsersReportDirectoryExists = System.IO.Directory.Exists(szDirectoryLocaton);
		
			return bUsersReportDirectoryExists;
			}
		//-------------------------------------------------------------------------
		//Takes a userID and deletes the users report directory including all
		//sub directories and files
		//-------------------------------------------------------------------------
		public static void DeleteUsersReportDirectory(string szUserName)
			{			
			string szFileLocation = HttpContext.Current.Server.MapPath("/YP/");
			szFileLocation = szFileLocation+"Reports//"+szUserName;
			if(System.IO.Directory.Exists(szFileLocation))
				{
				System.IO.Directory.Delete(szFileLocation, true);
				}
			}
		//-------------------------------------------------------------------------
		//Takes a UserID, a report name and a year a deletes the report found in
		//that location.
		//-------------------------------------------------------------------------
		public static void DeleteReport(string szUserName, string szReportName, int iYear)
			{
			string szFileLocation = HttpContext.Current.Server.MapPath("/YP/");
			szFileLocation = szFileLocation+"Reports//"+szUserName+
				"//"+iYear.ToString()+"//"+szReportName+".gif";
			if(System.IO.File.Exists(szFileLocation))
				{
				System.IO.File.Delete(szFileLocation);
				}
			}
		//-------------------------------------------------------------------------
		//Takes a UserID and a Year and returns all the report names from the
		//directory found in that location.
		//-------------------------------------------------------------------------
		public static DataTable GetReportsOfUser(string szUserName, int iYear)
			{
			DataTable dtUsersReports = new DataTable("Reports");
			dtUsersReports.Columns.Add("Name");

			DataRow drUsersReport;
			string szDirectory = HttpContext.Current.Server.MapPath("/YP/")+"Reports//"+
				szUserName+"//"+iYear.ToString()+"//";
			if(Directory.Exists(szDirectory) == true)
				{
				//Finds all the report files by looking for all .gif files
				string[] szReports = Directory.GetFiles(szDirectory, "*.gif");
				System.Array.Sort(szReports, new FileSortByDate());
				string szReportName = "";
				//Cleans up the file names so that only the file name is stored
				for(int iIndex = 0; iIndex < szReports.Length; iIndex++)
					{
					drUsersReport = dtUsersReports.NewRow();
					szReportName = szReports[iIndex];
					//removes the directory structure from the front of the file name
					szReportName = szReportName.Remove(0, (szReportName.LastIndexOf('/')+1));
					//removes the .gif from the end of the file name
					szReportName = szReportName.Replace(".gif", "");
					drUsersReport["Name"] = szReportName;
					dtUsersReports.Rows.Add(drUsersReport);
					}
				}
			return dtUsersReports;
			}
		//-------------------------------------------------------------------------
		//Takes an existing report name, a new report name, a UserID and a Year
		//and replaces the report name of the report found in the specified directory
		//with the new report name.
		//-------------------------------------------------------------------------
		public static bool RenameReport(string szOldReportName, 
			string szNewReportName, string szUserName, int iYear)
			{
			bool bRenamedSuccessfully = false;

			string szDirectory = HttpContext.Current.Server.MapPath("/YP/")+"Reports//"+
				szUserName+"//"+iYear.ToString()+"//";
			if(Directory.Exists(szDirectory) == true)
				{
				if(szOldReportName == szNewReportName)
					{
					bRenamedSuccessfully = true;
					}
				else
					{
					if(File.Exists(szDirectory+szNewReportName+".gif") == false)
						{
						//Copies the existing report and saves it with a new name
						File.Copy(szDirectory+szOldReportName+".gif", 
							szDirectory+szNewReportName+".gif", false);
						bRenamedSuccessfully = true;
						//Then deletes the existing report leaving only the 
						//newly named report.
						DeleteReport(szUserName, szOldReportName, iYear);
						}
					}
				}
			return bRenamedSuccessfully;
			}
		//-------------------------------------------------------------------------
		#endregion
						


		#region Collate Report Files
		//-------------------------------------------------------------------------
		//Takes a report type ID and a name to give the report and creates the
		//files needed for a agronomic report to be generated
		//-------------------------------------------------------------------------
		public static StringCollection PrepareReportFiles(string szReportType, 
			string szReportName, DataTable dtOtherValues)
			{
			StringCollection scAttachments = new StringCollection();
			string szDirectoryLocation = HttpContext.Current.Server.MapPath("/YP/")+"Temp";
			//Creates the directory used to store the files before they are sent to the
			//apsimrun machine, if the directory doesn't exist
			if(System.IO.Directory.Exists(szDirectoryLocation)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocation);
				}
		
			CreateReportFile(szDirectoryLocation, szReportType, 
				"apsimreport", szReportName, dtOtherValues, ref scAttachments);
			CreateReportFile(szDirectoryLocation, szReportType, 
				"con/par", szReportName, dtOtherValues, ref scAttachments);
			CreateRainfallInformationFile(szDirectoryLocation, ref scAttachments);
			CreateSoilFile(szDirectoryLocation, ref scAttachments);
			return scAttachments;	
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Report File Functions
		//-----------------------------------------------------------------------
		//Creates the .report file for an agronomic report
		//-----------------------------------------------------------------------
		public static void CreateReportFile(string szDirectoryLocation, 
			string szReportType, string szTemplateType, string szReportName, 
			DataTable dtOtherValues, ref StringCollection scAttachments)
			{
			//Gets the report template from the database
			string szReportTemplate = DataAccessClass.GetReportTypeTemplate(szReportType, szTemplateType);
			//Removes any place holders stored in the report template
			szReportTemplate = SetUpTemplateTextForSaveToFile(szReportTemplate);
			//Gets the data for the template, in XML format
			string szReportXml = CreateReportXML(szReportName, dtOtherValues);
			VBGeneral.APSIMData APSIMReportData = new VBGeneral.APSIMData(szReportXml);
			Macro mcReportFile = new Macro();
			//Fills the template with the data and stores the file location in 
			//a string collection
			StringCollection scReportFiles = mcReportFile.Go(APSIMReportData, szReportTemplate, szDirectoryLocation);
			for(int iIndex = 0; iIndex < scReportFiles.Count; iIndex++)
				{
				scAttachments.Add(scReportFiles[iIndex]);
				}
			}
		//-------------------------------------------------------------------------
		//Gets the data needed for the .con and .par files needed for any 
		//report and stores it in a xml string.
		//-------------------------------------------------------------------------
		public static string CreateReportXML(string szReportName, DataTable dtOtherValues)
			{	
			XmlDocument xmlDocSoilSample = new XmlDocument();
			xmlDocSoilSample.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocSoilSample.DocumentElement;
			//Gets the data needed for the xml string
			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());	
			string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());
			string szSowDate =  dtPaddocksDetails.Rows[0]["SowDate"].ToString();
			string szSowDateFull = "";
			string szSowDateDayMonth = "";
			if(szSowDate != "")
				{
				szSowDateFull = (DateTime.ParseExact(szSowDate, "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
				szSowDateDayMonth = (DateTime.ParseExact(szSowDate, "yyyy-MM-dd", null)).ToString("dd-MMM");
				}
			string szStartOfGrowingSeasonDate = dtPaddocksDetails.Rows[0]["StartOfGrowingSeasonDate"].ToString();
			string szStartOfGrowingSeasonDateFull = (DateTime.ParseExact(szStartOfGrowingSeasonDate, "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
			string szCultivar =  dtPaddocksDetails.Rows[0]["CultivarType"].ToString();
			string szYesterdayFull = DateTime.Today.AddDays(-1).ToString("dd/MM/yyyy");
			string szMetStationName =  dtPaddocksDetails.Rows[0]["MetStationName"].ToString();
			string szMetStationNumber = dtPaddocksDetails.Rows[0]["StationNumber"].ToString();
			DataTable dtSoilSample = DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, FunctionsClass.GetActiveUserName());
			string szResetDate = (DateTime.ParseExact(dtSoilSample.Rows[0]["SampleDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
			string szResetDateDayMonth = (DateTime.ParseExact(dtSoilSample.Rows[0]["SampleDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd-MMM");
			string szYesterdayDayMonth = DateTime.Today.AddDays(-1).ToString("dd-MMM");
			DataTable dtFertiliserApplications = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtClimateForcast = DataAccessClass.GetClimateForecast();
			DateTime dtSOIMonth = new DateTime(1, Convert.ToInt32(dtClimateForcast.Rows[0]["SoiMonth"].ToString()), 1);
			string szSOIPhase = dtClimateForcast.Rows[0]["SoiPhase"].ToString();

			//Creates the XML File
			XmlNode xmlPaddock = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "Paddock", "");  
			XmlNode xmlPaddockAttribute = xmlDocSoilSample.CreateNode(XmlNodeType.Attribute, "name", "");
			xmlPaddockAttribute.Value = szPaddockName;
			xmlPaddock.Attributes.SetNamedItem(xmlPaddockAttribute);
			//Add a report description node to the paddock
			XmlNode xmlReportDescription = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"reportdescription", "");
			xmlReportDescription.InnerText = szReportName;	
			xmlPaddock.AppendChild(xmlReportDescription);	
			//Create the grower node
			XmlNode xmlGrower = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "growername", "");  
			xmlGrower.InnerText = szUsersName;	
			xmlPaddock.AppendChild(xmlGrower);	
			//Create the yesterday full node
			XmlNode xmlYesterdayDateFull = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "yesterday_date", "");  
			xmlYesterdayDateFull.InnerText = szYesterdayFull;	
			xmlPaddock.AppendChild(xmlYesterdayDateFull);	
			//Create the sow date node
			XmlNode xmlSowDate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "sowdate", "");  
			xmlSowDate.InnerText = szSowDateFull;	
			xmlPaddock.AppendChild(xmlSowDate);	
			//Create the Start of Growing Season date node
			XmlNode xmlStartOfGrwoingSeasonDate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "start_growing_season_date", "");  
			xmlStartOfGrwoingSeasonDate.InnerText = szStartOfGrowingSeasonDateFull;	
			xmlPaddock.AppendChild(xmlStartOfGrwoingSeasonDate);		
			//Create the Start of Growing Season day month.
			string szStartOfGrowingSeasonDayMonth = (DateTime.ParseExact(szStartOfGrowingSeasonDate, "yyyy-MM-dd", null)).ToString("dd-MMM");
			XmlNode xmlStartOfGrwoingSeasonDayMonth = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "start_growing_season_daymonth", "");  
			xmlStartOfGrwoingSeasonDayMonth.InnerText = szStartOfGrowingSeasonDayMonth;	
			xmlPaddock.AppendChild(xmlStartOfGrwoingSeasonDayMonth);		
			//Create the cultivar node
			XmlNode xmlCultivar = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "cultivar", "");  
			xmlCultivar.InnerText = szCultivar;	
			xmlPaddock.AppendChild(xmlCultivar);	
			//Create the met station name 
			XmlNode xmlStationName = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "stationname", "");  
			xmlStationName.InnerText = szMetStationName;	
			xmlPaddock.AppendChild(xmlStationName);	
			//Create the met station number 
			XmlNode xmlStationNumber = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "stationNumber", "");  
			xmlStationNumber.InnerText = szMetStationNumber;	
			xmlPaddock.AppendChild(xmlStationNumber);	
			//Create the reset date day month node
			XmlNode xmlResetDate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "resetdate", "");  
			xmlResetDate.InnerText = szResetDate;	
			xmlPaddock.AppendChild(xmlResetDate);	
			//Create the reset date day month node
			XmlNode xmlResetDateDay = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "resetdaymonth", "");  
			xmlResetDateDay.InnerText = szResetDateDayMonth;	
			xmlPaddock.AppendChild(xmlResetDateDay);	
			//Create Sow Date Day Month node
			XmlNode xmlSowDateDayMonth = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "sowdaymonth", "");  
			xmlSowDateDayMonth.InnerText = szSowDateDayMonth;	
			xmlPaddock.AppendChild(xmlSowDateDayMonth);	
			//Create Yesterday day month node
			XmlNode xmlYesterdayMonth = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "yesterday_daymonth", "");  
			xmlYesterdayMonth.InnerText = szYesterdayDayMonth;	
			xmlPaddock.AppendChild(xmlYesterdayMonth);	
			//Add a soi month node to the paddock
			XmlNode xmlSOIMonth = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"soimonth", "");
			xmlSOIMonth.InnerText = dtSOIMonth.ToString("MMMM");	
			xmlPaddock.AppendChild(xmlSOIMonth);	
			//Add a soi phase node to the paddock
			XmlNode xmlSOIPhase = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"soiphase", "");
			xmlSOIPhase.InnerText = szSOIPhase;	
			xmlPaddock.AppendChild(xmlSOIPhase);	
			//Add a davids year 1 node to the paddock
			XmlNode xmlDavidsYear1 = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsyear1", "");
			xmlDavidsYear1.InnerText = dtClimateForcast.Rows[0]["DavidsYearOne"].ToString();		
			xmlPaddock.AppendChild(xmlDavidsYear1);	
			//Add a davids year 2 node to the paddock
			XmlNode xmlDavidsYear2 = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsyear2", "");
			xmlDavidsYear2.InnerText = dtClimateForcast.Rows[0]["DavidsYearTwo"].ToString();		
			xmlPaddock.AppendChild(xmlDavidsYear2);	
			//Add a davids year 3 node to the paddock
			XmlNode xmlDavidsYear3 = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsyear3", "");
			xmlDavidsYear3.InnerText = dtClimateForcast.Rows[0]["DavidsYearThree"].ToString();		
			xmlPaddock.AppendChild(xmlDavidsYear3);	
			//Add a davids year 4 node to the paddock
			XmlNode xmlDavidsYear4 = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsyear4", "");
			xmlDavidsYear4.InnerText = dtClimateForcast.Rows[0]["DavidsYearFour"].ToString();		
			xmlPaddock.AppendChild(xmlDavidsYear4);	
			//Add a davids year 5 node to the paddock
			XmlNode xmlDavidsYear5 = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsyear5", "");
			xmlDavidsYear5.InnerText = dtClimateForcast.Rows[0]["DavidsYearFive"].ToString();	
			xmlPaddock.AppendChild(xmlDavidsYear5);	
			//Add a soi description node to the paddock
			XmlNode xmlSOIDescription = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"soidescription", "");
			xmlSOIDescription.InnerText = dtClimateForcast.Rows[0]["SoiDescription"].ToString();	
			xmlPaddock.AppendChild(xmlSOIDescription);	
			//Add a davids description node to the paddock
			XmlNode xmlDavidsDescription = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"davidsdescription", "");
			xmlDavidsDescription.InnerText = dtClimateForcast.Rows[0]["DavidsDescription"].ToString();		
			xmlPaddock.AppendChild(xmlDavidsDescription);	
			//Creates the Nitrogen Application nodes (Fertiliser rate and Fertiliser date)
			int iIndex = 1;
			int iMaximumNumberOfNitrogenApplications = 5;
			foreach(DataRow drNitrogenApplication in dtFertiliserApplications.Rows)
				{
				XmlNode xmlFertiliserRate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "fert"+iIndex.ToString()+"rate", "");  
				xmlFertiliserRate.InnerText = drNitrogenApplication["Rate"].ToString();	
				xmlPaddock.AppendChild(xmlFertiliserRate);	
				
				XmlNode xmlFertiliserDate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "fert"+iIndex.ToString()+"daymonth", "");  
				xmlFertiliserDate.InnerText = (DateTime.ParseExact(drNitrogenApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd-MMM");	
				xmlPaddock.AppendChild(xmlFertiliserDate);	
				
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				XmlNode xmlFertiliserRate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "fert"+iIndex.ToString()+"rate", "");  
				xmlFertiliserRate.InnerText = "0";	
				xmlPaddock.AppendChild(xmlFertiliserRate);	
				
				XmlNode xmlFertiliserDate = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "fert"+iIndex.ToString()+"daymonth", "");  
				xmlPaddock.AppendChild(xmlFertiliserDate);	
				}
			//Adds any extra values that are report specific.
			if(dtOtherValues != null)
				{
				foreach(DataRow drOtherValue in dtOtherValues.Rows)
					{
					XmlNode xmlOtherValue = xmlDocSoilSample.CreateNode(XmlNodeType.Element, drOtherValue["Name"].ToString(), "");  
					xmlOtherValue.InnerText = drOtherValue["Value"].ToString();	
					xmlPaddock.AppendChild(xmlOtherValue);	
					}
				}
			xmlRoot.AppendChild(xmlPaddock);	
			string szConParXML = xmlDocSoilSample.OuterXml;
			return szConParXML;
			}	
		//-------------------------------------------------------------------------
		#endregion
		
		

		#region Soil File Functions
		//-------------------------------------------------------------------------
		//Create the .soil file needed for all reports
		//-------------------------------------------------------------------------
		static public void CreateSoilFile(string szDirectoryLocation, ref StringCollection scAttachments)
			{
			//Gets the data needed
			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
			string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());
			string szCropType = dtPaddocksDetails.Rows[0]["CropType"].ToString();
			//Gets the paddocks selected soil data in the form of an xml string and
			//converts that data into a APSIMData variable
			string szSoilXml = DataAccessClass.GetSoilData(dtPaddocksDetails.Rows[0]["SoilName"].ToString());
			VBGeneral.APSIMData APSIMSoilData = new VBGeneral.APSIMData(szSoilXml);
			Soil sPaddocksDefaultSoil = new Soil(APSIMSoilData);
			//Gets the soil sample data from the first grid
			DataTable dtSoilSampleOne = DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, FunctionsClass.GetActiveUserName());
			string szSoilSampleOneXml = dtSoilSampleOne.Rows[0]["Data"].ToString();
			if(szSoilSampleOneXml != "")
				{
				//Converts the soil sample data from the first grid into a APSIMData variable
				VBGeneral.APSIMData APSIMSoilSampleData = new VBGeneral.APSIMData(szSoilSampleOneXml);
				SoilSample ssPaddocksSoilSample = new SoilSample(APSIMSoilSampleData);
				//Maps this soil sample data with the default soil data
				ssPaddocksSoilSample.MapSampleToSoil(sPaddocksDefaultSoil);
				if(ssPaddocksSoilSample.HasData("Water", "sw"))
					{
					sPaddocksDefaultSoil.SetSw(ssPaddocksSoilSample.GetSw());
					}
				if(ssPaddocksSoilSample.HasData("Nitrogen", "no3"))
					{
					sPaddocksDefaultSoil.SetNo3(ssPaddocksSoilSample.GetNo3());
					}
				if(ssPaddocksSoilSample.HasData("Nitrogen", "nh4"))
					{
					sPaddocksDefaultSoil.SetNh4(ssPaddocksSoilSample.GetNh4());
					}
				}
			//Gets the soil sample data from the second grid
			DataTable dtSoilSampleTwo = DataAccessClass.GetPaddocksSoilSample("GridTwo", szPaddockName, FunctionsClass.GetActiveUserName());
			string szSoilSampleTwoXml = dtSoilSampleTwo.Rows[0]["Data"].ToString();
			if(szSoilSampleTwoXml != "")
				{
				//Converts the soil sample data from the first grid into a APSIMData variable
				VBGeneral.APSIMData APSIMSoilSampleData = new VBGeneral.APSIMData(szSoilSampleTwoXml);
				SoilSample ssPaddocksSoilSample = new SoilSample(APSIMSoilSampleData);
				//Maps this soil sample data with the default soil data
				ssPaddocksSoilSample.MapSampleToSoil(sPaddocksDefaultSoil);
				if(ssPaddocksSoilSample.HasData("Nitrogen", "oc"))
					{
					sPaddocksDefaultSoil.SetOc(ssPaddocksSoilSample.GetOc());
					}
				if(ssPaddocksSoilSample.HasData("Nitrogen", "ph"))
					{
					sPaddocksDefaultSoil.SetPh(ssPaddocksSoilSample.GetPh());
					}
				if(ssPaddocksSoilSample.HasData("Other", "esp"))
					{
					sPaddocksDefaultSoil.SetEsp(ssPaddocksSoilSample.GetEsp());
					}
				if(ssPaddocksSoilSample.HasData("Other", "ec"))
					{
					sPaddocksDefaultSoil.SetEc(ssPaddocksSoilSample.GetEc());
					}
				}
			// make sure the soil is valid.
			sPaddocksDefaultSoil.AutoCorrect();
			//Gets the file template
			string szSoilFileTemplate = CreateSoilFileTemplate();
			szSoilFileTemplate = szSoilFileTemplate.Replace("[soil.growername]", szUsersName);
			szSoilFileTemplate = szSoilFileTemplate.Replace("[crop.name]", szCropType.ToLower());
			Macro mcSoilFile = new Macro();
			sPaddocksDefaultSoil.RoundResultsTo3DecimalPlaces();
			//Fills the template with the data and stores the file location in 
			//a string collection
			StringCollection scSoilFiles = mcSoilFile.Go(sPaddocksDefaultSoil.ReturnData(), szSoilFileTemplate, szDirectoryLocation);
			for(int iIndex = 0; iIndex < scSoilFiles.Count; iIndex++)
				{
				scAttachments.Add(scSoilFiles[iIndex]);
				}	
			}
		//-------------------------------------------------------------------------
		//Creates the template for the soil file
		//-------------------------------------------------------------------------
		static public string CreateSoilFileTemplate()
			{
			string szSoilFileTemplate = "";
			
			szSoilFileTemplate = 
				"[file grower.soil]\n"+
				"[soil.soilwat2.parameters]\n"+//TITLE
				"[foreach Soil.Water as water]\n"+
				"   diffus_const = [water.DiffusConst]   ! coeffs for unsaturated water flow\n"+
				"   diffus_slope = [water.DiffusSlope]\n"+
				"   cn2_bare     = [water.Cn2Bare]    ! bare soil runoff curve number\n"+
				"   cn_red       = [water.CnRed]    ! potetial reduction in curve number due to residue\n"+
				"   cn_cov       = [water.CnCov]   ! cover for maximum reduction in curve number\n"+
				"   salb         = [water.Salb]  ! bare soil albedo\n"+
				"   cona         = [water.Cona]     ! stage 2 evap coef.\n"+
				"   u            = [water.U]     ! stage 1 soil evaporation coefficient (mm)\n"+
				"\n"+
				"   dlayer  =[foreach water.layer as Layer]\n      [Layer.thickness][endfor]   ! layer thickness mm soil\n"+
				"   air_dry =[foreach water.layer as Layer]\n    [Layer.airdry][endfor]   ! air dry mm water/mm soil\n"+
				"   ll15    =[foreach water.layer as Layer]\n    [Layer.ll15][endfor]   ! lower limit mm water/mm soil\n"+
				"   dul     =[foreach water.layer as Layer]\n    [Layer.dul][endfor]   ! drained upper limit mm water/mm soil\n"+
				"   sat     =[foreach water.layer as Layer]\n    [Layer.sat][endfor]   ! saturation mm water/mm soil\n"+
				"   sw      =[foreach water.layer as Layer]\n    [Layer.sw][endfor]   ! starting soil water mm water/mm soil\n"+
				"   swcon   =[foreach water.layer as Layer]\n    [Layer.swcon][endfor]   ! drainage coefficient\n"+
				"   bd      =[foreach water.layer as Layer]\n    [Layer.bd][endfor]   ! bulk density gm dry soil/cc moist soil\n"+
				"[endfor]\n"+//END OF WATER FOR LOOP
				"\n"+
				"[soil.soiln2.parameters]\n"+//TITLE
				"[foreach Soil.Nitrogen as nitrogen]\n"+
				"   root_cn      = [nitrogen.rootcn]     ! C:N ratio of initial root residues\n"+
				"   root_wt      = [nitrogen.rootwt]   ! root residues as biomass (kg/ha)\n"+
				"   soil_cn      = [nitrogen.soilcn]   ! C:N ratio of soil\n"+
				"   enr_a_coeff  = [nitrogen.enracoeff]\n"+
				"   enr_b_coeff  = [nitrogen.enrbcoeff]\n"+
				"   profile_reduction =  off\n"+ 
				"\n"+
				"   oc      =[foreach nitrogen.layer as Layer]\n      [Layer.oc][endfor]   ! Soil Organic Carbon\n"+
				"   ph      =[foreach nitrogen.layer as Layer]\n      [Layer.ph][endfor]   ! pH of soil\n"+
				"   fbiom   =[foreach nitrogen.layer as Layer]\n      [Layer.fbiom][endfor]   ! Organic C Biomass Fraction\n"+
				"   finert  =[foreach nitrogen.layer as Layer]\n      [Layer.finert][endfor]   ! Inert Organic C Fraction\n"+
				"   no3ppm  =[foreach nitrogen.layer as Layer]\n      [Layer.no3][endfor]   ! Nitrate Concentration\n"+
				"   nh4ppm  =[foreach nitrogen.layer as Layer]\n      [Layer.nh4][endfor]   ! Ammonium Concentration\n"+
				"[endfor]\n"+//END OF NITROGEN FOR LOOP
				"\n"+
				"[soil.[crop.name].parameters]\n"+//TITLE
				"[foreach Soil.SoilCrop as crop]\n"+
				"   ll      =[foreach crop.layer as Layer]\n      [Layer.ll][endfor]\n\n"+
				"   kl      =[foreach crop.layer as Layer]\n      [Layer.kl][endfor]\n\n"+
				"   xf      =[foreach crop.layer as Layer]\n      [Layer.xf][endfor]\n\n"+
				"[endfor]\n"+//END OF CROP FOR LOOP
				"[endfile]\n\n";

			return szSoilFileTemplate;
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Rainfall File Functions
		//-------------------------------------------------------------------------
		//Creates the .rai file needed for all report types
		//-------------------------------------------------------------------------
		public static void CreateRainfallInformationFile(string szDirectoryLocation, ref StringCollection scAttachments)
			{
			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
			string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
			string szRainfallPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szRainfallPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtSoilSample = DataAccessClass.GetPaddocksSoilSample("GridOne", szRainfallPaddockName, FunctionsClass.GetActiveUserName());

			int iValueCount = 0;
			int iPlacesToFill = 4;
			string szEventValue = "";
			System.Text.StringBuilder sbFileText = new System.Text.StringBuilder();
			//Write the text from the database to a temorary file
			string szFileLocation = szDirectoryLocation+"\\grower.rai";
			System.IO.StreamWriter swReportFile = System.IO.File.CreateText(szFileLocation);
			//Get the rain fall data from the database 
			string szSowingDate = dtPaddocksDetails.Rows[0]["SowDate"].ToString();
			string szInitialConditionsDate = dtSoilSample.Rows[0]["SampleDate"].ToString();

			// Work out the patch date. It is the earliest of sowing date and reset date.
			string szStartOfSowingSeasonDate = dtPaddocksDetails.Rows[0]["StartOfGrowingSeasonDate"].ToString();
			DateTime dtStartOfSowingSeasonDate = DateTime.ParseExact(szStartOfSowingSeasonDate, "yyyy-MM-dd", null);
			DateTime dtSowingDate  = new DateTime(9999, 12, 31);
			if(szSowingDate != "")
				{
				dtSowingDate = DateTime.ParseExact(szSowingDate, "yyyy-MM-dd", null);
				}
			DateTime dtInitialConditionsDate = DateTime.ParseExact(szInitialConditionsDate, "yyyy-MM-dd", null);
			DateTime dtPatchDate = dtInitialConditionsDate;
			if (dtSowingDate < dtInitialConditionsDate)
				dtPatchDate = dtSowingDate;
			string szPatchDate = dtPatchDate.ToString("yyyy-MM-dd");

			//Write the header information to the file
			sbFileText.Append("[grower.Rainfall.data]\n");
			sbFileText.Append("allow_sparse_data = true ()\n");
			sbFileText.Append("patch_all_years = true ()\n");
			sbFileText.Append("start_patching_from = "+szPatchDate+"\n");
			sbFileText.Append("patch_variables_long_term = maxt mint radn ()\n");
			sbFileText.Append("           date     patch_rain\n");
			sbFileText.Append("             ()             ()\n");

			//Finds out what the earliest date from which to start the rainfall file
			//As the Sowing Date doesn't have to be set for all reports we must test that it is a valid date, if 
			//it isn't then set it to the highest possible date so it won't be chosen.
			DateTime dtDateToRecord = new DateTime(DateTime.Today.Year, 01, 01);

			if(dtStartOfSowingSeasonDate <= dtSowingDate && dtStartOfSowingSeasonDate <= dtInitialConditionsDate)
				{
				dtDateToRecord = dtStartOfSowingSeasonDate;
				}
			else if(dtSowingDate <= dtStartOfSowingSeasonDate && dtSowingDate <= dtInitialConditionsDate)
				{
				dtDateToRecord = dtSowingDate;
				}
			else if(dtInitialConditionsDate <= dtStartOfSowingSeasonDate && dtInitialConditionsDate <= dtSowingDate)
				{
				dtDateToRecord = dtInitialConditionsDate;
				}

			//Get the rainfall data from the database for the specific period.
			string szLinkedTemporalPaddockName = dtPaddocksDetails.Rows[0]["LinkedRainfallPaddockName"].ToString();
			if(szLinkedTemporalPaddockName != "")
			{
				szRainfallPaddockName = szLinkedTemporalPaddockName;
			}
			DataTable dtRainfall = DataAccessClass.GetPaddocksTemporalEvents(szRainfallPaddockName, FunctionsClass.GetActiveUserName(), 
				"patch_rain", dtDateToRecord.ToString("yyyy-MM-dd"), DateTime.Today.ToString("yyyy-MM-dd"));


			//Add blank records, for every day in the period, to the file
			while (dtDateToRecord < DateTime.Today)
			{
				sbFileText.Append("     "+dtDateToRecord.ToString("yyyy-MM-dd")+"              0\n");
				dtDateToRecord = dtDateToRecord.AddDays(1);
			}
			//Add the rain fall information from the database
			for(int iIndex = 0; iIndex < dtRainfall.Rows.Count; iIndex++)
			{
				szEventValue = dtRainfall.Rows[iIndex]["EventValue"].ToString();
				iValueCount = szEventValue.Length;
				//Ensure that the rainfall event is correctly positioned.
				//Basically this just adds the correct amount of spaces
				//to ensure that it lines up with the other records
				while(iValueCount <= iPlacesToFill)
				{
					szEventValue = szEventValue.Insert(0, " ");
					iValueCount++;
				}
				//Replace the blank record with the actual rainfall event
				sbFileText = sbFileText.Replace(dtRainfall.Rows[iIndex]["EventDate"].ToString()+"              0",
					dtRainfall.Rows[iIndex]["EventDate"].ToString()+"          "+szEventValue);		
			}
			swReportFile.Write(sbFileText.ToString());
			swReportFile.Close();
			//Store the location of this file in a datatable
			scAttachments.Add(szFileLocation);
				
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Nitrogen Comparision Report Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static DataTable CreateNitrogenComparisonOtherValues(DataTable dtScenarioOne, 
			DataTable dtScenarioTwo, DataTable dtScenarioThree)
			{	
			DataTable dtOtherValues = new DataTable();
			dtOtherValues.Columns.Add("Name");
			dtOtherValues.Columns.Add("Value");
			DataRow drOtherValue;

			int iMaximumNumberOfNitrogenApplications = 5;
			//Gets all the data for the first scenario	
			int iIndex = 1;
			foreach(DataRow drScenarioOne in dtScenarioOne.Rows)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario1fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = drScenarioOne["Rate"].ToString();	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario1fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = ((DateTime)drScenarioOne["ApplicationDate"]).ToString("dd-MMM");
				dtOtherValues.Rows.Add(drOtherValue);
				
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario1fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = "0";	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario1fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = "";
				dtOtherValues.Rows.Add(drOtherValue);
				}
			//Gets all the data for the second scenario
			iIndex = 1;
			foreach(DataRow drScenarioTwo in dtScenarioTwo.Rows)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario2fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = drScenarioTwo["Rate"].ToString();	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario2fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = ((DateTime)drScenarioTwo["ApplicationDate"]).ToString("dd-MMM");
				dtOtherValues.Rows.Add(drOtherValue);
				
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario2fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = "0";	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario2fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = "";
				dtOtherValues.Rows.Add(drOtherValue);
				}
			//Gets all the data for the third scenario
			iIndex = 1;
			foreach(DataRow drScenarioThree in dtScenarioThree.Rows)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario3fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = drScenarioThree["Rate"].ToString();	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario3fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = ((DateTime)drScenarioThree["ApplicationDate"]).ToString("dd-MMM");
				dtOtherValues.Rows.Add(drOtherValue);
				
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario3fert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = "0";	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenario3fert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = "";
				dtOtherValues.Rows.Add(drOtherValue);
				}
			return dtOtherValues;
			}	
		//-------------------------------------------------------------------------
		#endregion
		


		#region Sowing X Variety Report Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static DataTable CreateSowingXVarietyReportOtherValues(DataTable dtNitrogen, 
			string szVarietyOne, string szSowingDateOne, string szVarietyTwo, string szSowingDateTwo,
			string szVarietyThree, string szSowingDateThree)
			{	
			DataTable dtOtherValues = new DataTable();
			dtOtherValues.Columns.Add("Name");
			dtOtherValues.Columns.Add("Value");
			DataRow drOtherValue;

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "variety1";  
			drOtherValue["Value"] = szVarietyOne;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdate1";  
			drOtherValue["Value"] = szSowingDateOne;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdaymonth1";  
			drOtherValue["Value"] = (DateTime.ParseExact(szSowingDateOne, "yyyy-MM-dd", null)).ToString("dd-MMM");
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "variety2";  
			drOtherValue["Value"] = szVarietyTwo;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdate2";  
			drOtherValue["Value"] = szSowingDateTwo;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdaymonth2";  
			drOtherValue["Value"] = (DateTime.ParseExact(szSowingDateTwo, "yyyy-MM-dd", null)).ToString("dd-MMM");
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "variety3";  
			drOtherValue["Value"] = szVarietyThree;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdate3";  
			drOtherValue["Value"] = szSowingDateThree;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdaymonth3";  
			drOtherValue["Value"] = (DateTime.ParseExact(szSowingDateThree, "yyyy-MM-dd", null)).ToString("dd-MMM");
			dtOtherValues.Rows.Add(drOtherValue);

			int iMaximumNumberOfNitrogenApplications = 5;
			//Gets all the data for the first scenario	
			int iIndex = 1;
			foreach(DataRow drNitrogen in dtNitrogen.Rows)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenariofert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = drNitrogen["Rate"].ToString();	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenariofert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = ((DateTime)drNitrogen["ApplicationDate"]).ToString("dd-MMM");
				dtOtherValues.Rows.Add(drOtherValue);
				
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenariofert"+iIndex.ToString()+"rate";  
				drOtherValue["Value"] = "0";	
				dtOtherValues.Rows.Add(drOtherValue);
				
				drOtherValue = dtOtherValues.NewRow();
				drOtherValue["Name"] = "scenariofert"+iIndex.ToString()+"daymonth"; 
				drOtherValue["Value"] = "";
				dtOtherValues.Rows.Add(drOtherValue);
				}
			return dtOtherValues;
			}	
		//-------------------------------------------------------------------------
		#endregion

		

		#region Auxiallary Functions
		//-------------------------------------------------------------------------
		//Replaces the place holders with the actual charcters
		//-------------------------------------------------------------------------
		private static string SetUpTemplateTextForSaveToFile(string szTemplateText)
			{
			szTemplateText = szTemplateText.Replace("\r", "");
			szTemplateText = szTemplateText.Replace("#Quote#", "'");
			szTemplateText = szTemplateText.Replace("#DQuote#", "\"");
			return szTemplateText;
			}
		//---------------------------------------------------------------------------
		#endregion
		
		
		//-------------------------------------------------------------------------	
		}//END CLASS
	//---------------------------------------------------------------------------
	//The FileSortByDate class sorts an array of files firstly by date then secondly
	//by name.
	//---------------------------------------------------------------------------
	public class FileSortByDate: IComparer
		{
		int IComparer.Compare(object objFirstFile, object objSecondFile)
			{
			FileInfo fiFirstFile = new FileInfo(objFirstFile.ToString());
			FileInfo fiSecondFile = new FileInfo(objSecondFile.ToString());
			TimeSpan difference = fiSecondFile.LastWriteTime - fiFirstFile.LastWriteTime;
			int iDifference = difference.Days;
			if(iDifference == 0)
				{
				iDifference = difference.Hours;
				if(iDifference == 0)
					{
					iDifference = difference.Minutes;
					if(iDifference == 0)
						{
						iDifference = (new CaseInsensitiveComparer()).Compare(objFirstFile, objSecondFile);
						}
					}
				}
			return iDifference;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END NAMESPACE
