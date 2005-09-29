using System;
using System.Data;
using System.IO;
using System.Web;
using System.Web.UI;
using CSGeneral;
using VBGeneral;
using System.Xml;
using Janus.Web.GridEX;
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
		public const string szFallowReport = "Fallow report";
		public const string szNitrogenProfitReport = "Nitrogen profit report";
		public const string szIrrigationComparisonReport = "Irrigation comparison report";
		public const string szIrrigationSchedulingReport = "Irrigation scheduling report";
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
			string szCropType, string szReportName, string szReportXML, DataTable dtOtherValues)
			{
			StringCollection scAttachments = new StringCollection();
			string szDirectoryLocation = HttpContext.Current.Server.MapPath("/YP/")+"Temp";
			//Creates the directory used to store the files before they are sent to the
			//apsimrun machine, if the directory doesn't exist
			if(System.IO.Directory.Exists(szDirectoryLocation)==false)
				{
				System.IO.Directory.CreateDirectory(szDirectoryLocation);
				}
		
			string szAPSIMReportName = DataAccessClass.GetAPSIMReportTemplateName(szCropType, szReportType);
			string szConParReportName = DataAccessClass.GetConParReportTemplateName(szCropType, szReportType);
			
			CreateReportFile(szDirectoryLocation, szAPSIMReportName, szReportName, szReportType, szReportXML, dtOtherValues, ref scAttachments);
			CreateReportFile(szDirectoryLocation, szConParReportName, szReportName, szReportType, szReportXML, dtOtherValues, ref scAttachments);

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
			string szTemplateName, string szReportName, string szReportType, 
			string szReportXML, DataTable dtOtherValues, 
			ref StringCollection scAttachments)
			{
			//Gets the report template from the database
			string szReportTemplate = DataAccessClass.GetReportTemplate(szTemplateName);
			//Removes any place holders stored in the report template
			szReportTemplate = SetUpTemplateTextForSaveToFile(szReportTemplate);
			//Gets the data for the template, in XML format
			string szReportXml = CreateReportXML(szReportName, szReportType, dtOtherValues, szReportXML);
			VBGeneral.APSIMData APSIMReportData = new VBGeneral.APSIMData(szReportXml);

			Macro mcReportFile = new Macro();
			//Fills the template with the data and stores the file location in 
			//a string collectdion
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
		public static string CreateReportXML(string szReportName, string szReportType, 
			DataTable dtOtherValues, string szNewReportXML)
			{	
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
			string szStartOfGrowingSeasonDayMonth = (DateTime.ParseExact(szStartOfGrowingSeasonDate, "yyyy-MM-dd", null)).ToString("dd-MMM");
			string szStartOfGrowingSeasonDateFull = (DateTime.ParseExact(szStartOfGrowingSeasonDate, "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
			string szCultivar =  dtPaddocksDetails.Rows[0]["CultivarType"].ToString();
			string szCropType = dtPaddocksDetails.Rows[0]["CropType"].ToString();
			string szRegion = dtPaddocksDetails.Rows[0]["RegionType"].ToString();
			string szRowConfiguration = dtPaddocksDetails.Rows[0]["RowConfigurationType"].ToString();
			string szYesterdayFull = DateTime.Today.AddDays(-1).ToString("dd/MM/yyyy");
			string szMetStationName =  dtPaddocksDetails.Rows[0]["MetStationName"].ToString();
			string szMetStationNumber = dtPaddocksDetails.Rows[0]["StationNumber"].ToString();
			DataTable dtSoilSample = DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, FunctionsClass.GetActiveUserName());
			string szResetDate = (DateTime.ParseExact(dtSoilSample.Rows[0]["SampleDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
			string szResetDateDayMonth = (DateTime.ParseExact(dtSoilSample.Rows[0]["SampleDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd-MMM");
			string szYesterdayDayMonth = DateTime.Today.AddDays(-1).ToString("dd-MMM");
			DataTable dtFertiliserApplications = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtIrrigationApplications = DataAccessClass.GetPaddocksIrrigationApplications(szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtClimateForcast = DataAccessClass.GetClimateForecast();
			DateTime dtSOIMonth = new DateTime(1, Convert.ToInt32(dtClimateForcast.Rows[0]["SoiMonth"].ToString()), 1);
			string szSOIPhase = dtClimateForcast.Rows[0]["SoiPhase"].ToString();

			int iTriazine = 0;
			if(dtPaddocksDetails.Rows[0]["Triazine"].ToString() != null &&
				dtPaddocksDetails.Rows[0]["Triazine"].ToString() != "")
				{
				iTriazine = Convert.ToInt32(dtPaddocksDetails.Rows[0]["Triazine"].ToString());
				}
			int iPopulation = 0;
			if(dtPaddocksDetails.Rows[0]["Population"].ToString() != null &&
				dtPaddocksDetails.Rows[0]["Population"].ToString() != "")
			{
				iPopulation = Convert.ToInt32(dtPaddocksDetails.Rows[0]["Population"].ToString());
			}
			int iMaxRootingDepth = 0;
			if(dtPaddocksDetails.Rows[0]["RootingDepth"].ToString() != null &&
				dtPaddocksDetails.Rows[0]["RootingDepth"].ToString() != "")
			{
				iMaxRootingDepth = Convert.ToInt32(dtPaddocksDetails.Rows[0]["RootingDepth"].ToString());
			}
			double dFertileTillerNumber = 0;
			if(dtPaddocksDetails.Rows[0]["TillerNumber"].ToString() != null &&
				dtPaddocksDetails.Rows[0]["TillerNumber"].ToString() != "")
			{
				dFertileTillerNumber = Convert.ToDouble(dtPaddocksDetails.Rows[0]["TillerNumber"].ToString());
			}
			double dRowSpacing = 0;
			if(dtPaddocksDetails.Rows[0]["RowSpacing"].ToString() != null &&
				dtPaddocksDetails.Rows[0]["RowSpacing"].ToString() != "")
			{
				dRowSpacing = Convert.ToDouble(dtPaddocksDetails.Rows[0]["RowSpacing"].ToString());
			}

			if(dtOtherValues == null)
			{
				dtOtherValues = new DataTable();
				dtOtherValues.Columns.Add("Name");
				dtOtherValues.Columns.Add("Value");
			}

			//Add standard report variables to the report specific variables
			AddValuesToTable("reporttype", szReportType, ref dtOtherValues);
			AddValuesToTable("reportdescription", szReportName, ref dtOtherValues);
			AddValuesToTable("growername", szUsersName, ref dtOtherValues);
			AddValuesToTable("yesterday_date", szYesterdayFull, ref dtOtherValues);
			AddValuesToTable("sowdate", szSowDateFull, ref dtOtherValues);
			AddValuesToTable("start_growing_season_date", szStartOfGrowingSeasonDateFull, ref dtOtherValues);		
			AddValuesToTable("start_growing_season_daymonth", szStartOfGrowingSeasonDayMonth, ref dtOtherValues);	
			AddValuesToTable("cultivar", szCultivar, ref dtOtherValues);	
			AddValuesToTable("crop", szCropType, ref dtOtherValues);
			AddValuesToTable("region", szRegion, ref dtOtherValues);
			AddValuesToTable("rowconfiguration", szRowConfiguration, ref dtOtherValues);	
			AddValuesToTable("rootingdepth", iMaxRootingDepth.ToString(), ref dtOtherValues);
			AddValuesToTable("population", iPopulation.ToString(), ref dtOtherValues);
			AddValuesToTable("ftn", dFertileTillerNumber.ToString(), ref dtOtherValues);
			AddValuesToTable("rowspacing", dRowSpacing.ToString(), ref dtOtherValues);
			AddValuesToTable("stationname", szMetStationName, ref dtOtherValues);
			AddValuesToTable("stationNumber", szMetStationNumber, ref dtOtherValues);
			AddValuesToTable("resetdate", szResetDate, ref dtOtherValues);
			AddValuesToTable("triazine", iTriazine.ToString(), ref dtOtherValues);
			AddValuesToTable("resetdaymonth", szResetDateDayMonth, ref dtOtherValues);
			AddValuesToTable("sowdaymonth", szSowDateDayMonth, ref dtOtherValues);
			AddValuesToTable("yesterday_daymonth", szYesterdayDayMonth, ref dtOtherValues);
			AddValuesToTable("soimonth", dtSOIMonth.ToString("MMMM"), ref dtOtherValues);
			AddValuesToTable("soiphase", szSOIPhase, ref dtOtherValues);
			AddValuesToTable("davidsyear1", dtClimateForcast.Rows[0]["DavidsYearOne"].ToString(), ref dtOtherValues);
			AddValuesToTable("davidsyear2", dtClimateForcast.Rows[0]["DavidsYearTwo"].ToString(), ref dtOtherValues);
			AddValuesToTable("davidsyear3", dtClimateForcast.Rows[0]["DavidsYearThree"].ToString(), ref dtOtherValues);
			AddValuesToTable("davidsyear4", dtClimateForcast.Rows[0]["DavidsYearFour"].ToString(), ref dtOtherValues);
			AddValuesToTable("davidsyear5", dtClimateForcast.Rows[0]["DavidsYearFive"].ToString(), ref dtOtherValues);
			AddValuesToTable("soidescription", dtClimateForcast.Rows[0]["SoiDescription"].ToString(), ref dtOtherValues);
			AddValuesToTable("davidsdescription", dtClimateForcast.Rows[0]["DavidsDescription"].ToString(), ref dtOtherValues);
			//Creates the Nitrogen Application nodes (Fertiliser rate and Fertiliser date)
			int iIndex = 1;
			int iMaximumNumberOfNitrogenApplications = 5;
			foreach(DataRow drNitrogenApplication in dtFertiliserApplications.Rows)
				{
				AddValuesToTable("fert"+iIndex.ToString()+"rate", drNitrogenApplication["Rate"].ToString(), ref dtOtherValues);
				AddValuesToTable("fert"+iIndex.ToString()+"daymonth", (DateTime.ParseExact(drNitrogenApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd-MMM"), ref dtOtherValues);
				iIndex++;
				}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
				{
				AddValuesToTable("fert"+iIndex.ToString()+"rate", "0", ref dtOtherValues);
				AddValuesToTable("fert"+iIndex.ToString()+"daymonth", "", ref dtOtherValues);
				}
			//Creates the Irrigation Application nodes (Amount, efficency and date)
			iIndex = 0;
			int iMaximumNumberOfIrrigationApplications = 10;
			foreach(DataRow drIrrigationApplication in dtIrrigationApplications.Rows)
			{
				AddValuesToTable("irr"+iIndex.ToString()+"amount", drIrrigationApplication["Amount"].ToString(), ref dtOtherValues);
				AddValuesToTable("irr"+iIndex.ToString()+"efficency", drIrrigationApplication["Efficency"].ToString(), ref dtOtherValues);
				AddValuesToTable("irr"+iIndex.ToString()+"daymonth", (DateTime.ParseExact(drIrrigationApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null)).ToString("dd-MMM"), ref dtOtherValues);
				iIndex++;
			}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfIrrigationApplications; iIndex++)
			{
				AddValuesToTable("irr"+iIndex.ToString()+"amount", "0", ref dtOtherValues);
				AddValuesToTable("irr"+iIndex.ToString()+"efficency", "0", ref dtOtherValues);
				AddValuesToTable("irr"+iIndex.ToString()+"daymonth", "", ref dtOtherValues);
			}
			//Creates the XML File
			XmlDocument xmlDocSoilSample = new XmlDocument();
			xmlDocSoilSample.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocSoilSample.DocumentElement;
			XmlNode xmlPaddock = xmlDocSoilSample.CreateNode(XmlNodeType.Element, "Paddock", "");  
			XmlNode xmlPaddockAttribute = xmlDocSoilSample.CreateNode(XmlNodeType.Attribute, "name", "");
			xmlPaddockAttribute.Value = szPaddockName;
			xmlPaddock.Attributes.SetNamedItem(xmlPaddockAttribute);
			//Adds all values to the XML File
			if(dtOtherValues != null)
				{
				foreach(DataRow drOtherValue in dtOtherValues.Rows)
					{
					XmlNode xmlOtherValue = xmlDocSoilSample.CreateNode(XmlNodeType.Element, drOtherValue["Name"].ToString(), "");  
					xmlOtherValue.InnerText = drOtherValue["Value"].ToString();	
					xmlPaddock.AppendChild(xmlOtherValue);	
					}
				}
			//Adds a PAW value from the initial conditions.
			string soilName = dtPaddocksDetails.Rows[0]["SoilName"].ToString();
			Soil PaddockSoil = DataAccessClass.GetSoil(soilName);
			DataTable dtPaddocksSoilSample =
				DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, dtUsersDetails.Rows[0]["UserName"].ToString());
			if(dtPaddocksSoilSample.Rows.Count > 0)
			{
				string CropType =  dtPaddocksDetails.Rows[0]["CropType"].ToString();
				if (CropType.ToLower() == "barley" || CropType.ToLower() == "none")
					CropType = "wheat";

				SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSample.Rows[0]["Data"].ToString()));
				Sample.LinkedSoil = PaddockSoil;
				double dPAW = MathUtility.Sum(Sample.PAW(CropType));
				
				string paw = Math.Round(dPAW, 2).ToString("F2");
				XmlNode xmlPAW = xmlDocSoilSample.CreateNode(XmlNodeType.Element,"initialconditionspaw", "");
				xmlPAW.InnerText = paw;
				xmlPaddock.AppendChild(xmlPAW);	
			}
			//Convert all the data to xml string form
			xmlRoot.AppendChild(xmlPaddock);	
			string szReportXML = xmlDocSoilSample.OuterXml;
			szReportXML = MergeXMLStrings(szReportXML, szNewReportXML);
			return szReportXML;
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static XmlNode CreatePaddockXML(string szReportName, string szReportType, 
			XmlDocument xmlDocPaddock, int NumScenarios)
		{
			XmlNode xmlPaddock = xmlDocPaddock.CreateNode(XmlNodeType.Element, "YPPaddock", "");  

			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			XmlNode xmlPaddockAttribute = xmlDocPaddock.CreateNode(XmlNodeType.Attribute, "name", "");
			xmlPaddockAttribute.Value = szPaddockName;
			xmlPaddock.Attributes.SetNamedItem(xmlPaddockAttribute);

			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());	
			string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtSoilSample = DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtFertiliserApplications = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtIrrigationApplications = DataAccessClass.GetPaddocksIrrigationApplications(szPaddockName, FunctionsClass.GetActiveUserName());
			DataTable dtClimateForcast = DataAccessClass.GetClimateForecast();
			
			AddStringNode("reporttype", szReportType, ref xmlPaddock, xmlDocPaddock);
			AddStringNode("reportdescription", szReportName, ref xmlPaddock, xmlDocPaddock);
			AddStringNode("growername", szUsersName, ref xmlPaddock, xmlDocPaddock);
			AddStringNode("yesterday_date", DateTime.Today.AddDays(-1).ToString("dd/MM/yyyy"), ref xmlPaddock, xmlDocPaddock);
			AddDateNode("start_growing_season_date", dtPaddocksDetails.Rows[0]["StartOfGrowingSeasonDate"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddDateNode("resetdate", dtSoilSample.Rows[0]["SampleDate"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("soiphase", dtClimateForcast.Rows[0]["SoiPhase"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("soimonth", 	(DateTime.ParseExact("2005-"+dtClimateForcast.Rows[0]["SoiMonth"].ToString()+"-1","yyyy-M-d", null)).ToString("MMMM"), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsyear1", dtClimateForcast.Rows[0]["DavidsYearOne"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsyear2", dtClimateForcast.Rows[0]["DavidsYearTwo"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsyear3", dtClimateForcast.Rows[0]["DavidsYearThree"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsyear4", dtClimateForcast.Rows[0]["DavidsYearFour"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsyear5", dtClimateForcast.Rows[0]["DavidsYearFive"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("soidescription", dtClimateForcast.Rows[0]["SoiDescription"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("davidsdescription", dtClimateForcast.Rows[0]["DavidsDescription"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("numscenarios", NumScenarios.ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("region", dtPaddocksDetails.Rows[0]["RegionType"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("stationname", dtPaddocksDetails.Rows[0]["MetStationName"].ToString(), ref xmlPaddock, xmlDocPaddock);
			AddStringNode("stationNumber", dtPaddocksDetails.Rows[0]["StationNumber"].ToString(), ref xmlPaddock, xmlDocPaddock);

			DataTable dtPaddocksSoilSample =
				DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, dtUsersDetails.Rows[0]["UserName"].ToString());
			if(dtPaddocksSoilSample.Rows.Count > 0)
				{
				string soilName = dtPaddocksDetails.Rows[0]["SoilName"].ToString();
				Soil PaddockSoil = DataAccessClass.GetSoil(soilName);
				string CropType =  dtPaddocksDetails.Rows[0]["CropType"].ToString();
				if (CropType.ToLower() == "barley" || CropType.ToLower() == "none")
					CropType = "wheat";

				SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSample.Rows[0]["Data"].ToString()));
				Sample.LinkedSoil = PaddockSoil;
				double dPAW = MathUtility.Sum(Sample.PAW(CropType));
				
				string paw = Math.Round(dPAW, 2).ToString("F2");
				AddStringNode("InitialConditionsPaw", paw, ref xmlPaddock, xmlDocPaddock);
				}
			
			AddDayMonthNodesToNode(ref xmlPaddock, xmlDocPaddock);
			return xmlPaddock;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private static XmlNode CreateScenarioReportXML(XmlDocument xmlDocPaddock, string ScenarioName,
			GridEX grdFertiliser, GridEX grdIrrigation)
		{	
			XmlNode xmlScenario = xmlDocPaddock.CreateNode(XmlNodeType.Element, "scenario", "");
			XmlNode xmlScenarioAttribute = xmlDocPaddock.CreateNode(XmlNodeType.Attribute, "name", "");
			xmlScenarioAttribute.Value = ScenarioName;
			xmlScenario.Attributes.SetNamedItem(xmlScenarioAttribute);

			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());

			string CropType = dtPaddocksDetails.Rows[0]["CropType"].ToString();
			if (CropType.ToLower() == "barley")
				CropType = "wheat";			

			AddStringNode("cultivar", dtPaddocksDetails.Rows[0]["CultivarType"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddStringNode("crop", CropType, ref xmlScenario, xmlDocPaddock);
			AddStringNode("rowconfiguration", dtPaddocksDetails.Rows[0]["RowConfigurationType"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddDateNode("sowdate", dtPaddocksDetails.Rows[0]["SowDate"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddNumericalNode("triazine", dtPaddocksDetails.Rows[0]["Triazine"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddNumericalNode("population", dtPaddocksDetails.Rows[0]["Population"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddNumericalNode("rootingdepth", dtPaddocksDetails.Rows[0]["RootingDepth"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddNumericalNode("ftn", dtPaddocksDetails.Rows[0]["TillerNumber"].ToString(), ref xmlScenario, xmlDocPaddock);
			AddNumericalNode("rowspacing", dtPaddocksDetails.Rows[0]["RowSpacing"].ToString(), ref xmlScenario, xmlDocPaddock);

			
			AddDayMonthNodesToNode(ref xmlScenario, xmlDocPaddock);
			AddFertiliserNodes(grdFertiliser, ref xmlScenario, xmlDocPaddock);
			AddIrrigationNodes(grdIrrigation, ref xmlScenario, xmlDocPaddock);

			
			return xmlScenario;
		}
		//-------------------------------------------------------------------------
		#endregion
		
		

		#region Soil File Functions
		//-------------------------------------------------------------------------
		//Create the soil file needed for all reports
		//-------------------------------------------------------------------------
		static public void CreateSoilFile(string szDirectoryLocation, ref StringCollection scAttachments)
			{

			DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
			string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());			

			//Gets the paddocks selected soil data in the form of an xml string and
			//converts that data into a APSIMData variable
			Soil PaddockSoil = DataAccessClass.GetSoil(dtPaddocksDetails.Rows[0]["SoilName"].ToString());

			//Gets the soil sample data from the first grid and copies it to our soil
			DataTable dtSoilSampleOne = DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, FunctionsClass.GetActiveUserName());
			string szSoilSampleOneXml = dtSoilSampleOne.Rows[0]["Data"].ToString();
			if(szSoilSampleOneXml != "")
				{
				SoilSample PaddockSoilSample = new SoilSample(new APSIMData(szSoilSampleOneXml));
				PaddockSoilSample.LinkedSoil = PaddockSoil;
				PaddockSoil.InitialWater.SetUsingLayered(PaddockSoilSample.SWMapedToSoil);
				BoundSWValuesInSample(PaddockSoil);
				PaddockSoil.InitialNitrogen.NO3 = PaddockSoilSample.NO3MapedToSoil;
				PaddockSoil.InitialNitrogen.NH4 = PaddockSoilSample.NH4MapedToSoil;
				}

			//Gets the soil sample data from the second grid and copies to our soil.
			DataTable dtSoilSampleTwo = DataAccessClass.GetPaddocksSoilSample("GridTwo", szPaddockName, FunctionsClass.GetActiveUserName());
			if(dtSoilSampleTwo.Rows.Count > 0)
				{
				string szSoilSampleTwoXml = dtSoilSampleTwo.Rows[0]["Data"].ToString();
				if(szSoilSampleTwoXml != "")
					{
					SoilSample PaddockSoilSample = new SoilSample(new APSIMData(szSoilSampleTwoXml));
					PaddockSoilSample.LinkedSoil = PaddockSoil;
					PaddockSoil.OC = PaddockSoilSample.OCMapedToSoil;
					PaddockSoil.PH = PaddockSoilSample.PHMapedToSoil;
					PaddockSoil.EC = PaddockSoilSample.ECMapedToSoil;
					int RootDepth = Convert.ToInt32(dtPaddocksDetails.Rows[0]["RootingDepth"]);
					string Crop = dtPaddocksDetails.Rows[0]["CropType"].ToString();
					if (Crop.ToLower() == "barley")
						Crop = "wheat";
					RootDepth *= 10;   // cm to mm
					bool bUseEC = Convert.ToBoolean(Convert.ToInt32(dtPaddocksDetails.Rows[0]["UseEC"].ToString()));
					if(bUseEC == true)
						PaddockSoil.ApplyECXFFunction(Crop, RootDepth);
					}
				}
			string FullSoilFileName = szDirectoryLocation + "\\grower.soil"; 
			PaddockSoil.ExportToPar(FullSoilFileName);
			scAttachments.Add(FullSoilFileName);
			}	


		static public void BoundSWValuesInSample(Soil S)
			{	
			double[] sw = S.InitialWater.SW;
			double[] airdry = S.Airdry;
			double[] sat = S.SAT;

			for (int i = 0; i != sw.Length; i++)
				{
				sw[i] = Math.Max(sw[i], airdry[i]);
				sw[i] = Math.Min(sw[i], sat[i]);
				}
			S.InitialWater.SetUsingLayered(sw);
			}

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

			bool bUseDefalutRainfall = Convert.ToBoolean(Convert.ToInt32(dtPaddocksDetails.Rows[0]["DefaultRainfall"].ToString()));
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
			//sbFileText.Append("start_patching_from = "+szPatchDate+"\n");
			if(!bUseDefalutRainfall)
				{
    			sbFileText.Append("patch_variables_long_term = maxt mint radn ()\n");
				sbFileText.Append("           date     patch_rain\n");
				sbFileText.Append("             ()             ()\n");
				}
			else
				{
    			sbFileText.Append("patch_variables_long_term = maxt mint radn rain ()\n");
				sbFileText.Append("           date\n");
				sbFileText.Append("             ()\n");
				}
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
				string record = "     "+dtDateToRecord.ToString("yyyy-MM-dd");
				if (!bUseDefalutRainfall)
					record += "              0";
				record += "\n";
				sbFileText.Append(record);
				dtDateToRecord = dtDateToRecord.AddDays(1);
			}
			if(bUseDefalutRainfall == false)
			{
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
			}
			swReportFile.Write(sbFileText.ToString());
			swReportFile.Close();
			//Store the location of this file in a datatable
			scAttachments.Add(szFileLocation);
				
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Agronomic and Climate Report Functions
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in all basic reports such as the climate
		//report and the agronomic report
		//---------------------------------------------------------------------------
		public static string PrepareBasicReportXML(string szReportName, string szReportType)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 1);
			XmlNode xmlScenario = CreateScenarioReportXML(xmlDocPaddock, "scenario1", null, null);
			
			xmlPaddock.AppendChild(xmlScenario);
			xmlRoot.AppendChild(xmlPaddock);	
			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
		}
		//---------------------------------------------------------------------------
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
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in the nitrogen comparison report
		//---------------------------------------------------------------------------
		public static string PrepareNitrogenComparisonXML(string szReportName, string szReportType,
			GridEX grdNitrogenOne, GridEX grdNitrogenTwo, GridEX grdNitrogenThree)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 3);
			XmlNode xmlScenarioOne = CreateScenarioReportXML(xmlDocPaddock, "scenario1", grdNitrogenOne, null);
			XmlNode xmlScenarioTwo = CreateScenarioReportXML(xmlDocPaddock, "scenario2", grdNitrogenTwo, null);
			XmlNode xmlScenarioThree = CreateScenarioReportXML(xmlDocPaddock, "scenario3", grdNitrogenThree, null);
			
			xmlPaddock.AppendChild(xmlScenarioOne);
			xmlPaddock.AppendChild(xmlScenarioTwo);
			xmlPaddock.AppendChild(xmlScenarioThree);
			xmlRoot.AppendChild(xmlPaddock);	
			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
		}
		//-------------------------------------------------------------------------
		#endregion
		

		
		#region Nitrogen Profit Report Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static DataTable CreateNitrogenProfitOtherValues(
			string szClassification, string szPrice, 
			string szProteinContent, string szProteinIncrement, string szFertiliserCost, 
			string szApplicationCost, DataTable dtScenarioOne, 
			DataTable dtScenarioTwo, DataTable dtScenarioThree)
		{	
			DataTable dtOtherValues = new DataTable();
			dtOtherValues.Columns.Add("Name");
			dtOtherValues.Columns.Add("Value");

			AddValuesToTable("classification", szClassification, ref dtOtherValues);
			AddValuesToTable("cropprice", szPrice, ref dtOtherValues);
			AddValuesToTable("minproteincontent", szProteinContent, ref dtOtherValues);
			AddValuesToTable("proteinincrementpay", szProteinIncrement, ref dtOtherValues);
			AddValuesToTable("nitrogencost", szFertiliserCost, ref dtOtherValues);
			AddValuesToTable("applicationcost", szApplicationCost, ref dtOtherValues);

			int iMaximumNumberOfNitrogenApplications = 5;
			//Gets all the data for the first scenario	
			int iIndex = 1;
			foreach(DataRow drScenarioOne in dtScenarioOne.Rows)
			{
				AddValuesToTable("scenario1fert"+iIndex.ToString()+"rate",  
					drScenarioOne["Rate"].ToString(), ref dtOtherValues);
				AddValuesToTable("scenario1fert"+iIndex.ToString()+"daymonth",  
					((DateTime)drScenarioOne["ApplicationDate"]).ToString("dd-MMM"), 
					ref dtOtherValues);
		
				iIndex++;
			}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
			{
				AddValuesToTable("scenario1fert"+iIndex.ToString()+"rate",  
					"0", ref dtOtherValues);
				AddValuesToTable("scenario1fert"+iIndex.ToString()+"daymonth",  
					"", ref dtOtherValues);
			}
			//Gets all the data for the second scenario
			iIndex = 1;
			foreach(DataRow drScenarioTwo in dtScenarioTwo.Rows)
			{
				AddValuesToTable("scenario2fert"+iIndex.ToString()+"rate",  
					drScenarioTwo["Rate"].ToString(), ref dtOtherValues);
				AddValuesToTable("scenario2fert"+iIndex.ToString()+"daymonth",  
					((DateTime)drScenarioTwo["ApplicationDate"]).ToString("dd-MMM"), 
					ref dtOtherValues);
				
				iIndex++;
			}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
			{
				AddValuesToTable("scenario2fert"+iIndex.ToString()+"rate",  
					"0", ref dtOtherValues);
				AddValuesToTable("scenario2fert"+iIndex.ToString()+"daymonth",  
					"", ref dtOtherValues);
			}
			//Gets all the data for the third scenario
			iIndex = 1;
			foreach(DataRow drScenarioThree in dtScenarioThree.Rows)
			{
				AddValuesToTable("scenario3fert"+iIndex.ToString()+"rate",  
					drScenarioThree["Rate"].ToString(), ref dtOtherValues);
				AddValuesToTable("scenario3fert"+iIndex.ToString()+"daymonth",  
					((DateTime)drScenarioThree["ApplicationDate"]).ToString("dd-MMM"), 
					ref dtOtherValues);
				
				iIndex++;
			}
			for(iIndex = iIndex; iIndex <= iMaximumNumberOfNitrogenApplications; iIndex++)
			{
				AddValuesToTable("scenario3fert"+iIndex.ToString()+"rate",  
					"0", ref dtOtherValues);
				AddValuesToTable("scenario3fert"+iIndex.ToString()+"daymonth",  
					"",ref  dtOtherValues);
			}
			return dtOtherValues;
		}	
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in the nitrogen profit report
		//---------------------------------------------------------------------------
		public static string PrepareNitrogenProfitXML(string szReportName, string szReportType,
			string szClassification, string szPrice, string szProteinContent, string szProteinIncrement, 
			string szFertiliserCost, string szApplicationCost, GridEX grdNitrogenOne, 
			GridEX grdNitrogenTwo, GridEX grdNitrogenThree)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 3);

			XmlNode xmlScenarioOne = CreateScenarioReportXML(xmlDocPaddock, "scenario1", grdNitrogenOne, null);
			AddNitrogenProfitScenarioNodes(szClassification, szPrice, szProteinContent, 
				szProteinIncrement, szFertiliserCost, szApplicationCost, ref xmlScenarioOne, xmlDocPaddock);

			XmlNode xmlScenarioTwo = CreateScenarioReportXML(xmlDocPaddock, "scenario2", grdNitrogenTwo, null);
			AddNitrogenProfitScenarioNodes(szClassification, szPrice, szProteinContent, 
				szProteinIncrement, szFertiliserCost, szApplicationCost, ref xmlScenarioTwo, xmlDocPaddock);

			XmlNode xmlScenarioThree = CreateScenarioReportXML(xmlDocPaddock, "scenario3", grdNitrogenThree, null);
			AddNitrogenProfitScenarioNodes(szClassification, szPrice, szProteinContent, 
				szProteinIncrement, szFertiliserCost, szApplicationCost, ref xmlScenarioThree, xmlDocPaddock);
			
			xmlPaddock.AppendChild(xmlScenarioOne);
			xmlPaddock.AppendChild(xmlScenarioTwo);
			xmlPaddock.AppendChild(xmlScenarioThree);

			xmlRoot.AppendChild(xmlPaddock);	
			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
		}
		//---------------------------------------------------------------------------
		//Add additional scenario information required by the nitrogen profit report
		//---------------------------------------------------------------------------
		private static void AddNitrogenProfitScenarioNodes(string szClassification, 
			string szPrice, string szProteinContent, string szProteinIncrement, 
			string szFertiliserCost, string szApplicationCost, ref XmlNode xmlScenario, 
			XmlDocument xmlDocPaddock)
		{
			AddStringNode("classification", szClassification, ref xmlScenario, xmlDocPaddock);
			AddStringNode("cropprice", szPrice, ref xmlScenario, xmlDocPaddock);
			AddStringNode("minproteincontent", szProteinContent, ref xmlScenario, xmlDocPaddock);
			AddStringNode("proteinincrementpay", szProteinIncrement, ref xmlScenario, xmlDocPaddock);
			AddStringNode("nitrogencost", szFertiliserCost, ref xmlScenario, xmlDocPaddock);
			AddStringNode("applicationcost", szApplicationCost, ref xmlScenario, xmlDocPaddock);
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
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in the sowing x variety report
		//---------------------------------------------------------------------------
		public static string PrepareSowingXVarietyXML(string szReportName, string szReportType,
			string szVarietyOne, string szSowingDateOne, string szVarietyTwo, string szSowingDateTwo,
			string szVarietyThree, string szSowingDateThree, string szCropType, string szRowConfigurationOne,
			int iPopulationOne, double dFertileTillerNumberOne, double dRowSpacingOne, string szRowConfigurationTwo,
			int iPopulationTwo, double dFertileTillerNumberTwo, double dRowSpacingTwo, string szRowConfigurationThree,
			int iPopulationThree, double dFertileTillerNumberThree, double dRowSpacingThree, GridEX grdNitrogen)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 3);

			XmlNode xmlScenarioOne = CreateScenarioReportXML(xmlDocPaddock, "scenario1", grdNitrogen, null);
			EditSowingXVarietyScenarioNodes(szVarietyOne, szSowingDateOne, szCropType, szRowConfigurationOne, 
				iPopulationOne, dFertileTillerNumberOne, dRowSpacingOne, ref xmlScenarioOne, xmlDocPaddock);

			XmlNode xmlScenarioTwo = CreateScenarioReportXML(xmlDocPaddock, "scenario2", grdNitrogen, null);
			EditSowingXVarietyScenarioNodes(szVarietyTwo, szSowingDateTwo, szCropType, szRowConfigurationTwo, 
				iPopulationTwo, dFertileTillerNumberTwo, dRowSpacingTwo, ref xmlScenarioTwo, xmlDocPaddock);

			XmlNode xmlScenarioThree = CreateScenarioReportXML(xmlDocPaddock, "scenario3", grdNitrogen, null);
			EditSowingXVarietyScenarioNodes(szVarietyThree, szSowingDateThree, szCropType, szRowConfigurationThree, 
				iPopulationThree, dFertileTillerNumberThree, dRowSpacingThree, ref xmlScenarioThree, xmlDocPaddock);

			xmlPaddock.AppendChild(xmlScenarioOne);
			xmlPaddock.AppendChild(xmlScenarioTwo);
			xmlPaddock.AppendChild(xmlScenarioThree);

			xmlRoot.AppendChild(xmlPaddock);	

			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
		}
		//---------------------------------------------------------------------------
		//Add additional scenario information required by the nitrogen profit report
		//---------------------------------------------------------------------------
		private static void EditSowingXVarietyScenarioNodes(string szVariety, 
			string szSowingDate, string szCropType, string szRowConfiguration,
			int iPopulation, double dFertileTillerNumber, double dRowSpacing, 
			ref XmlNode xmlScenario, XmlDocument xmlDocPaddock)
		{
			foreach(XmlNode xmlChildNode in xmlScenario.ChildNodes)
			{
				switch(xmlChildNode.Name)
				{
					case "cultivar":
						xmlChildNode.InnerText = szVariety;
						break;
					case "crop":
						xmlChildNode.InnerText = szCropType;
						break;
					case "sowdate":
						xmlChildNode.InnerText = szSowingDate;
						break;
					case "sowdaymonth":
						xmlChildNode.InnerText = DateTime.ParseExact(szSowingDate, "dd/MM/yyyy", null).ToString("dd-MMM");
						break;
					case "rowconfiguration":
						xmlChildNode.InnerText = szRowConfiguration;
						break;
					case "population":
						xmlChildNode.InnerText = iPopulation.ToString();
						break;
					case "ftn":
						xmlChildNode.InnerText = dFertileTillerNumber.ToString();
						break;
					case "rowspacing":
						xmlChildNode.InnerText = dRowSpacing.ToString();
						break;
				}
			}
		}
		//-------------------------------------------------------------------------
		#endregion


		
		#region Fallow Report Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static DataTable CreateFallowReportOtherValues(DataTable dtNitrogen, 
			string szVariety, string szSowingDate)
		{	
			DataTable dtOtherValues = new DataTable();
			dtOtherValues.Columns.Add("Name");
			dtOtherValues.Columns.Add("Value");
			DataRow drOtherValue;

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "variety";  
			drOtherValue["Value"] = szVariety;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdate";  
			drOtherValue["Value"] = szSowingDate;	
			dtOtherValues.Rows.Add(drOtherValue);

			drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = "sowingdaymonth";  
			drOtherValue["Value"] = (DateTime.ParseExact(szSowingDate, "yyyy-MM-dd", null)).ToString("dd-MMM");
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
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in the fallow report
		//---------------------------------------------------------------------------
		public static string PrepareFallowXML(string szReportName, string szReportType,
			string szVariety, string szSowingDate, string szCropType, string szRowConfiguration,
			int iPopulation, double dFertileTillerNumber, double dRowSpacing, GridEX grdNitrogen)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 1);

			XmlNode xmlScenario = CreateScenarioReportXML(xmlDocPaddock, "scenario1", grdNitrogen, null);
			EditFallowScenarioNodes(szVariety, szSowingDate, szCropType, szRowConfiguration, 
				iPopulation, dFertileTillerNumber, dRowSpacing, ref xmlScenario, xmlDocPaddock);
			AddDayMonthNodesToNode(ref xmlScenario, xmlDocPaddock);
			xmlPaddock.AppendChild(xmlScenario);

			xmlRoot.AppendChild(xmlPaddock);	
			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
		}
		//---------------------------------------------------------------------------
		//Edit existing scenario information for fallow report
		//---------------------------------------------------------------------------
		private static void EditFallowScenarioNodes(string szVariety, 
			string szSowingDate, string szCropType, string szRowConfiguration,
			int iPopulation, double dFertileTillerNumber, double dRowSpacing, 
			ref XmlNode xmlScenario, XmlDocument xmlDocPaddock)
		{
			foreach(XmlNode xmlChildNode in xmlScenario.ChildNodes)
			{
				switch(xmlChildNode.Name)
				{
					case "cultivar":
						xmlChildNode.InnerText = szVariety;
						break;
					case "crop":
						xmlChildNode.InnerText = szCropType;
						break;
					case "sowdate":
						xmlChildNode.InnerText = szSowingDate;
						break;
					case "sowdaymonth":
						xmlChildNode.InnerText = DateTime.ParseExact(szSowingDate, "dd/MM/yyyy", null).ToString("dd-MMM");
						break;
					case "rowconfiguration":
						xmlChildNode.InnerText = szRowConfiguration;
						break;
					case "population":
						xmlChildNode.InnerText = iPopulation.ToString();
						break;
					case "ftn":
						xmlChildNode.InnerText = dFertileTillerNumber.ToString();
						break;
					case "rowspacing":
						xmlChildNode.InnerText = dRowSpacing.ToString();
						break;
				}
			}
		}
		//-------------------------------------------------------------------------
		#endregion


		
		#region Irrigation Comparision Report Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static DataTable CreateIrrigationComparisonOtherValues(DataSet dsNitrogen,
			DataSet dsIrrigation)
		{	
			DataTable dtOtherValues = new DataTable();
			dtOtherValues.Columns.Add("Name");
			dtOtherValues.Columns.Add("Value");
			DataRow drOtherValue;

			int iApplicationIndex = 1;
			int iScenarioIndex = 1;

			//Gets all the data for the nitrogen scenarios	
			int iMaximumNumberOfNitrogenApplications = 5;
			foreach(DataTable dtScenario in dsNitrogen.Tables)
			{
				iApplicationIndex = 1;
				foreach(DataRow drScenarioApplication in dtScenario.Rows)
				{
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"fert"+iApplicationIndex.ToString()+"rate";  
					drOtherValue["Value"] = drScenarioApplication["Rate"].ToString();	
					dtOtherValues.Rows.Add(drOtherValue);
				
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"fert"+iApplicationIndex.ToString()+"daymonth"; 
					drOtherValue["Value"] = ((DateTime)drScenarioApplication["ApplicationDate"]).ToString("dd-MMM");
					dtOtherValues.Rows.Add(drOtherValue);
				
					iApplicationIndex++;
				}
				for(iApplicationIndex = iApplicationIndex; iApplicationIndex <= iMaximumNumberOfNitrogenApplications; iApplicationIndex++)
				{
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"fert"+iApplicationIndex.ToString()+"rate";  
					drOtherValue["Value"] = "0";	
					dtOtherValues.Rows.Add(drOtherValue);
				
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"fert"+iApplicationIndex.ToString()+"daymonth"; 
					drOtherValue["Value"] = "";
					dtOtherValues.Rows.Add(drOtherValue);
				}
				iScenarioIndex++;
			}

			iApplicationIndex = 1;
			iScenarioIndex = 1;

			//Gets all the data for the irrigation scenarios	
			int iMaximumNumberOfIrrigationApplications = 10;
			foreach(DataTable dtScenario in dsIrrigation.Tables)
			{
				iApplicationIndex = 1;
				foreach(DataRow drScenarioApplication in dtScenario.Rows)
				{
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"amount";  
					drOtherValue["Value"] = drScenarioApplication["amount"].ToString();	
					dtOtherValues.Rows.Add(drOtherValue);

					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"efficency";  
					drOtherValue["Value"] = drScenarioApplication["efficency"].ToString();	
					dtOtherValues.Rows.Add(drOtherValue);
				
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"daymonth"; 
					drOtherValue["Value"] = ((DateTime)drScenarioApplication["Date"]).ToString("dd-MMM");
					dtOtherValues.Rows.Add(drOtherValue);
				
					iApplicationIndex++;
				}
				for(iApplicationIndex = iApplicationIndex; iApplicationIndex <= iMaximumNumberOfIrrigationApplications; iApplicationIndex++)
				{
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"amount";  
					drOtherValue["Value"] = "0";	
					dtOtherValues.Rows.Add(drOtherValue);

					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"efficency";  
					drOtherValue["Value"] = "0";	
					dtOtherValues.Rows.Add(drOtherValue);
				
					drOtherValue = dtOtherValues.NewRow();
					drOtherValue["Name"] = "scenario"+iScenarioIndex+"irr"+iApplicationIndex.ToString()+"daymonth"; 
					drOtherValue["Value"] = "";
					dtOtherValues.Rows.Add(drOtherValue);
				}
				iScenarioIndex++;
			}
			return dtOtherValues;
		}	
		//---------------------------------------------------------------------------
		//Prepares the xml string to be used in the irrigation comparison report
		//---------------------------------------------------------------------------
		public static string PrepareIrrigationComparisonXML(string szReportName, string szReportType,
			GridEX grdNitrogenOne, GridEX grdIrrigationOne, GridEX grdNitrogenTwo, GridEX grdIrrigationTwo, 
			GridEX grdNitrogenThree, GridEX grdIrrigationThree)
		{
			string szReportXML = "";
			XmlDocument xmlDocPaddock = new XmlDocument();
			xmlDocPaddock.LoadXml("<Paddocks></Paddocks>"); 
			XmlElement xmlRoot = xmlDocPaddock.DocumentElement;

			XmlNode xmlPaddock = CreatePaddockXML(szReportName, szReportType, xmlDocPaddock, 3);
			XmlNode xmlScenarioOne = CreateScenarioReportXML(xmlDocPaddock, "scenario1", grdNitrogenOne, grdIrrigationOne);
			XmlNode xmlScenarioTwo = CreateScenarioReportXML(xmlDocPaddock, "scenario2", grdNitrogenTwo, grdIrrigationTwo);
			XmlNode xmlScenarioThree = CreateScenarioReportXML(xmlDocPaddock, "scenario3", grdNitrogenThree, grdIrrigationThree);
			
			xmlPaddock.AppendChild(xmlScenarioOne);
			xmlPaddock.AppendChild(xmlScenarioTwo);
			xmlPaddock.AppendChild(xmlScenarioThree);
			xmlRoot.AppendChild(xmlPaddock);	
			szReportXML = xmlDocPaddock.OuterXml;
			return szReportXML;
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
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private static void AddValuesToTable(string szName, string szValue, 
			ref DataTable dtOtherValues)
		{
			DataRow drOtherValue = dtOtherValues.NewRow();
			drOtherValue["Name"] = szName;  
			drOtherValue["Value"] = szValue;	
			dtOtherValues.Rows.Add(drOtherValue);
		}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddStringNode(string szNodeName, string szNodeValue,
			ref XmlNode xmlMainNode, XmlDocument xmlDoc)
		{
			XmlNode xmlNewNode = xmlDoc.CreateNode(XmlNodeType.Element, szNodeName, "");  
			xmlNewNode.InnerText = szNodeValue;	
			xmlMainNode.AppendChild(xmlNewNode);	
		}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddNumericalNode(string szNodeName, string szNodeValue,
			ref XmlNode xmlPaddock, XmlDocument xmlDocPaddock)
		{
			if(szNodeValue == "" || InputValidationClass.IsInputADecimal(szNodeValue) == false)
			{
				szNodeValue = "0";
			}
			AddStringNode(szNodeName, szNodeValue, ref xmlPaddock, xmlDocPaddock);
		}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddDateNode(string szNodeName, string szNodeValue,
			ref XmlNode xmlPaddock, XmlDocument xmlDocPaddock)
		{
			if(szNodeValue != "")
			{
				szNodeValue = (DateTime.ParseExact(szNodeValue, "yyyy-MM-dd", null)).ToString("dd/MM/yyyy");
			}
			AddStringNode(szNodeName, szNodeValue, ref xmlPaddock, xmlDocPaddock);
		}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddDayMonthNodesToNode(ref XmlNode xmlSingleNode, XmlDocument xmlDoc)
		{
			foreach(XmlNode xmlChildNode in xmlSingleNode.ChildNodes)
			{
				if(xmlChildNode.Name.IndexOf("date") != -1)
				{
					XmlNode xmlNewNode = xmlDoc.CreateNode(XmlNodeType.Element, xmlChildNode.Name.Replace("date", "daymonth"), "");  
					if (xmlChildNode.InnerText != "")
						xmlNewNode.InnerText = DateTime.ParseExact(xmlChildNode.InnerText, "dd/MM/yyyy", null).ToString("dd-MMM");	
					xmlSingleNode.AppendChild(xmlNewNode);	
				}
			}
		}
		//---------------------------------------------------------------------------
		private static string MergeXMLStrings(string szOldReportXML, string szNewReportXML)
		{
			string szReportXML;
			szNewReportXML = szNewReportXML.Replace("<Paddocks>", "");
			szNewReportXML = szNewReportXML.Replace("</Paddocks>", "");

			int iEndOfNodeIndex = szOldReportXML.IndexOf("</Paddocks>");
			szReportXML = szOldReportXML.Insert(iEndOfNodeIndex, szNewReportXML);
			return szReportXML;
		}

		#endregion



		#region Grid Functions
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddFertiliserNodes(GridEX grdFertilser, ref XmlNode xmlScenario, 
			XmlDocument xmlDoc)
		{	
			XmlNode xmlFertilise;
			
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtFertiliserApplications = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", szPaddockName, FunctionsClass.GetActiveUserName());
			foreach(DataRow drNitrogenApplication in dtFertiliserApplications.Rows)
			{
				xmlFertilise = xmlDoc.CreateNode(XmlNodeType.Element, "fertilise", ""); 
				AddDateNode("date", drNitrogenApplication["ApplicationDate"].ToString(), ref xmlFertilise, xmlDoc); 
				AddStringNode("rate", drNitrogenApplication["Rate"].ToString(), ref xmlFertilise, xmlDoc);
				AddDayMonthNodesToNode(ref xmlFertilise, xmlDoc);
				xmlScenario.AppendChild(xmlFertilise);
			}

			if(grdFertilser != null)
			{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdFertilser.RowCount; iIndex++)
				{
					grdRow = grdFertilser.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Rate"].Value != null && grdRow.Cells["ApplicationDate"].Value != null)
					{
						xmlFertilise = xmlDoc.CreateNode(XmlNodeType.Element, "fertilise", ""); 
						AddStringNode("date", grdRow.Cells["ApplicationDate"].Text, ref xmlFertilise, xmlDoc); 
						AddStringNode("rate", grdRow.Cells["Rate"].Text, ref xmlFertilise, xmlDoc);
						AddDayMonthNodesToNode(ref xmlFertilise, xmlDoc);
						xmlScenario.AppendChild(xmlFertilise);
					}
				}
			}
		}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private static void AddIrrigationNodes(GridEX grdIrrigation, ref XmlNode xmlScenario, 
			XmlDocument xmlDoc)
		{	
			XmlNode xmlIrrigation;
			
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			DataTable dtIrrigationApplications = DataAccessClass.GetPaddocksIrrigationApplications(szPaddockName, FunctionsClass.GetActiveUserName());
			foreach(DataRow drIrrigationApplication in dtIrrigationApplications.Rows)
			{
				xmlIrrigation = xmlDoc.CreateNode(XmlNodeType.Element, "irrigate", ""); 
				AddDateNode("date", drIrrigationApplication["ApplicationDate"].ToString(), ref xmlIrrigation, xmlDoc); 
				AddStringNode("rate", drIrrigationApplication["Amount"].ToString(), ref xmlIrrigation, xmlDoc);
				AddStringNode("efficency", drIrrigationApplication["Efficency"].ToString(), ref xmlIrrigation, xmlDoc);
				AddDayMonthNodesToNode(ref xmlIrrigation, xmlDoc);
				xmlScenario.AppendChild(xmlIrrigation);
			}

			if(grdIrrigation != null)
			{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdIrrigation.RowCount; iIndex++)
				{
					grdRow = grdIrrigation.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Amount"].Value != null && grdRow.Cells["Date"].Value != null && 
						grdRow.Cells["Efficency"].Value != null)
					{
						xmlIrrigation = xmlDoc.CreateNode(XmlNodeType.Element, "irrigate", ""); 
						AddStringNode("date", grdRow.Cells["Date"].Text, ref xmlIrrigation, xmlDoc); 
						AddStringNode("rate", grdRow.Cells["Amount"].Text, ref xmlIrrigation, xmlDoc);
						AddStringNode("efficency", grdRow.Cells["Efficency"].Text, ref xmlIrrigation, xmlDoc);
						AddDayMonthNodesToNode(ref xmlIrrigation, xmlDoc);
						xmlScenario.AppendChild(xmlIrrigation);
					}
				}
			}
		}
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
