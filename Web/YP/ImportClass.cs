using System;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Data;
using System.IO;
using System.Xml;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for ImportClass.
	/// </summary>
	public class ImportClass
		{
		public ImportClass()
			{}
		//-------------------------------------------------------------------------
		//Imports a file that contains a list of met stations
		//-------------------------------------------------------------------------
		public static void ImportMetStations(Page pgPageInfo, string szTableName, string szRegionID)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo);
			UploadImportedMetStations(hpfImportedFile, pgPageInfo, szTableName, szRegionID);
			}
		//-------------------------------------------------------------------------
		//Imports a file that contains a list of soils
		//-------------------------------------------------------------------------
		public static void ImportSoils(Page pgPageInfo, string szTableName, string szRegionID)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo);
			UploadImportedSoils(hpfImportedFile, pgPageInfo, szTableName, szRegionID);
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the a file has been selected for upload and
		//that the file isn't not empty.  If both checks are passed then the file
		//is uploaded
		//-------------------------------------------------------------------------
		private static HttpPostedFile CheckForUploadedFiles(Page pgPageInfo)
			{
			HttpPostedFile hpfImportedFile = null;
			HttpFileCollection hfcImportedFiles = HttpContext.Current.Request.Files;
			//If there is at least one file to upload then proceed to the next check
			if(HttpContext.Current.Request.Files.Count > 0)
				{
				//Get the first file
				hpfImportedFile = hfcImportedFiles[0];
				//If the file is empty then upload the file
				if(hpfImportedFile.ContentLength == 0)
					{
					FunctionsClass.DisplayMessage(pgPageInfo, "File is empty");
					hpfImportedFile = null;
					}
				}
			//If there is no file to upload then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(pgPageInfo, "Please select a file to upload");
				}
			return hpfImportedFile;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the met file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedMetStations(HttpPostedFile hpfImportedFile, Page pgPageInfo, string szTableName, string szRegionID)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/plain")
					{
					StreamReader strImportedFile = new StreamReader(hpfImportedFile.InputStream);
					DataTable dtImportedTable = FillMetStationsDataTable(strImportedFile, szRegionID);
					//Sets the data to the selected region
					DataAccessClass.InsertMulitpleRecords(dtImportedTable, szTableName);
					}
				else
					{
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(pgPageInfo, "Error Importing File");
				}
			}
		//-------------------------------------------------------------------------
		//Reads the met file and reads that data into a datatable which can be 
		//inserted into a database
		//-------------------------------------------------------------------------
		private static DataTable FillMetStationsDataTable(StreamReader strImportedFile, string szRegionID)
			{
			DataTable dtMetStations = DataAccessClass.GetCompleteMetStationsTable("0");
			DataRow drMetStation;
			string szMetStation;
			string[] szMetStationParts;
			int iNameOrdinal = 1;
			int iNumberOrdinal = 0;
			try
				{
				dtMetStations.Rows.Clear();
				while((szMetStation = strImportedFile.ReadLine()) != null)
					{
					drMetStation = dtMetStations.NewRow();
					szMetStationParts = szMetStation.Split(",".ToCharArray(), 2);
					drMetStation["StationNumber"] = szMetStationParts[iNumberOrdinal];
					drMetStation["Name"] = szMetStationParts[iNameOrdinal];
					drMetStation["RegionID"] = szRegionID;
					dtMetStations.Rows.Add(drMetStation);
					}
				}
			catch(Exception)
				{}
			return dtMetStations;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the soil file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedSoils(HttpPostedFile hpfImportedFile, Page pgPageInfo, string szTableName, string szRegionID)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/xml")
					{
					XmlParserContext context = new XmlParserContext(null, null, null, XmlSpace.None);
					XmlTextReader xtrSoilSample = new XmlTextReader(hpfImportedFile.InputStream, XmlNodeType.Document, context);
					DataTable dtImportedTable = FillSoilsDataTable(xtrSoilSample, szRegionID);
					//Sets the data to the selected region
					DataAccessClass.InsertMulitpleRecords(dtImportedTable, szTableName);
					}
				else
					{
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(pgPageInfo, "Error Importing File");
				}
			}
		//-------------------------------------------------------------------------
		//Reads the soil file and reads that data into a datatable which can be 
		//inserted into a database
		//-------------------------------------------------------------------------
		private static DataTable FillSoilsDataTable(XmlTextReader xtrSoilSample, string szRegionID)
			{
			DataTable dtSoils = DataAccessClass.GetCompleteSoilsTable("0");
			DataRow drSoil;
			try
				{
				dtSoils.Rows.Clear();
				
				XmlDocument xmlDoc = new XmlDocument();
				xmlDoc.Load(xtrSoilSample);
				XmlNode xlnRoot = xmlDoc.DocumentElement;
				XmlNode xlnCurrentNode = xlnRoot;
				string szSoilName;
				int iLengthOfSubString = 0;
				int iStartOfSubStringIndex = 0;

				for(int iIndex = 0; iIndex < xlnRoot.ChildNodes.Count; iIndex++)
					{
					drSoil = dtSoils.NewRow();
					xlnCurrentNode = xlnRoot.ChildNodes[iIndex];
					szSoilName = xlnCurrentNode.Attributes[0].InnerText;
					if(szSoilName.StartsWith("Grower soil:") == true)
						{
						iStartOfSubStringIndex = szSoilName.IndexOf(":", 0, szSoilName.Length)+1;
						iLengthOfSubString = (szSoilName.Length - iStartOfSubStringIndex);
						drSoil["SpecificUserID"] = DataAccessClass.
							GetIDOfGrower(szSoilName.Substring(iStartOfSubStringIndex, iLengthOfSubString)).ToString();
						}
					else
						{
						drSoil["SpecificUserID"] = "0";
						}
					drSoil["Name"] = szSoilName;
					drSoil["Data"] = xlnCurrentNode.OuterXml;
					drSoil["RegionID"] = szRegionID;
					dtSoils.Rows.Add(drSoil);
					}
				}
			catch(Exception)
				{}
			return dtSoils;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
