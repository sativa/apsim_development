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
		public static void ImportMetStations(Page pgPageInfo, string szRegionType, ref bool bErrors)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo, ref bErrors);
			UploadImportedMetStations(hpfImportedFile, pgPageInfo, szRegionType, ref bErrors);
			}
		//-------------------------------------------------------------------------
		//Imports a file that contains a list of soils
		//-------------------------------------------------------------------------
		public static void ImportSoils(Page pgPageInfo, string szRegionType, ref bool bErrors)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo, ref bErrors);
			UploadImportedSoils(hpfImportedFile, pgPageInfo, szRegionType, ref bErrors);
			}
		//-------------------------------------------------------------------------
		//Imports a file that contains a list of cultivars
		//-------------------------------------------------------------------------
		public static void ImportCultivars(Page pgPageInfo, string szCropType, ref bool bErrors)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo, ref bErrors);
			UploadImportedCultivars(hpfImportedFile, pgPageInfo, szCropType, ref bErrors);
			}
		//-------------------------------------------------------------------------
		//Imports a file that contains a report template
		//-------------------------------------------------------------------------
		public static void ImportReportTemplate(Page pgPageInfo, string szReportType, 
			string szTemplateType, string szCropType, ref bool bErrors)
			{
			HttpPostedFile hpfImportedFile = CheckForUploadedFiles(pgPageInfo, ref bErrors);
			UploadImportedReportTemplate(hpfImportedFile, pgPageInfo, szReportType, szTemplateType, szCropType, ref bErrors);
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the a file has been selected for upload and
		//that the file isn't not empty.  If both checks are passed then the file
		//is uploaded
		//-------------------------------------------------------------------------
		private static HttpPostedFile CheckForUploadedFiles(Page pgPageInfo, ref bool bErrors)
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
				bErrors = true;
				FunctionsClass.DisplayMessage(pgPageInfo, "Please select a file to upload");
				}
			return hpfImportedFile;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the met file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedMetStations(HttpPostedFile hpfImportedFile, Page pgPageInfo, string szRegionType, ref bool bErrors)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/plain")
					{
					bool bDuplicateDetected = false;
					StreamReader strImportedFile = new StreamReader(hpfImportedFile.InputStream);
					DataTable dtMetStations = FillMetStationsDataTable(strImportedFile, szRegionType, ref bErrors);
					//Sets the data to the selected region
					foreach(DataRow drMetStation in dtMetStations.Rows)
						{
						try
							{
							DataAccessClass.InsertMetStation(drMetStation["Region"].ToString(), drMetStation["Name"].ToString(), 
								Convert.ToInt32(drMetStation["StationNumber"].ToString()));
							}
						catch(Exception)
							{
							bDuplicateDetected = true;
							}
						}
					if(bDuplicateDetected == true)
						{
						bErrors = true;
						FunctionsClass.DisplayMessage(pgPageInfo,"At least one duplicate met station wasn't imported");
						}
					}
				else
					{
					bErrors = true;
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception E)
				{
				bErrors = true;
				FunctionsClass.DisplayMessage(pgPageInfo, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Reads the met file and reads that data into a datatable which can be 
		//inserted into a database
		//-------------------------------------------------------------------------
		private static DataTable FillMetStationsDataTable(StreamReader strImportedFile, string szRegionType, ref bool bErrors)
			{
			DataTable dtMetStations = new DataTable();
			dtMetStations.Columns.Add("Region");
			dtMetStations.Columns.Add("Name");
			dtMetStations.Columns.Add("StationNumber");
			DataRow drMetStation;
			string szMetStation;
			string[] szMetStationParts;
			int iNameOrdinal = 1;
			int iNumberOrdinal = 0;

			dtMetStations.Rows.Clear();
			while((szMetStation = strImportedFile.ReadLine()) != null)
				{
				drMetStation = dtMetStations.NewRow();
				szMetStationParts = szMetStation.Split(",".ToCharArray(), 2);
				if(szMetStationParts.Length == 2)
					{
					drMetStation["StationNumber"] = szMetStationParts[iNumberOrdinal];
					drMetStation["Name"] = szMetStationParts[iNameOrdinal];
					drMetStation["Region"] = szRegionType;
					dtMetStations.Rows.Add(drMetStation);
					}
				}
			return dtMetStations;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the soil file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedSoils(HttpPostedFile hpfImportedFile, Page pgPageInfo, string szRegionType, ref bool bErrors)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/xml")
					{
					bool bDuplicateDetected = false;
					XmlParserContext context = new XmlParserContext(null, null, null, XmlSpace.None);
					XmlTextReader xtrSoilSample = new XmlTextReader(hpfImportedFile.InputStream, XmlNodeType.Document, context);
					DataTable dtSoils = FillSoilsDataTable(xtrSoilSample, szRegionType, ref bErrors);
					//Sets the data to the selected region
					foreach(DataRow drSoil in dtSoils.Rows)
						{
						try
							{
							DataAccessClass.InsertSoil(drSoil["Region"].ToString(), drSoil["Name"].ToString(), 
								drSoil["Data"].ToString());
							}
						catch(Exception)
							{
							bDuplicateDetected = true;
							}
						}
					if(bDuplicateDetected == true)
						{
						bErrors = true;
						FunctionsClass.DisplayMessage(pgPageInfo,"At least one duplicate soil wasn't imported");
						}
					}
				else
					{
					bErrors = true;
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception E)
				{
				bErrors = true;
				FunctionsClass.DisplayMessage(pgPageInfo, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Reads the soil file and reads that data into a datatable which can be 
		//inserted into a database
		//-------------------------------------------------------------------------
		private static DataTable FillSoilsDataTable(XmlTextReader xtrSoilSample, string szRegionType, ref bool bErrors)
			{
			DataTable dtSoils = new DataTable();
			dtSoils.Columns.Add("Name");
			dtSoils.Columns.Add("Data");
			dtSoils.Columns.Add("Region");
			DataRow drSoil;
			dtSoils.Rows.Clear();
			
			XmlDocument xmlDoc = new XmlDocument();
			xmlDoc.Load(xtrSoilSample);
			XmlNode xlnRoot = xmlDoc.DocumentElement;
			XmlNode xlnCurrentNode = xlnRoot;

			for(int iIndex = 0; iIndex < xlnRoot.ChildNodes.Count; iIndex++)
				{
				drSoil = dtSoils.NewRow();
				xlnCurrentNode = xlnRoot.ChildNodes[iIndex];
				drSoil["Name"] = xlnCurrentNode.Attributes[0].InnerText;
				drSoil["Data"] = xlnCurrentNode.OuterXml;
				drSoil["Region"] = szRegionType;
				dtSoils.Rows.Add(drSoil);
				}
				
			return dtSoils;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the cultivar file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedCultivars(HttpPostedFile hpfImportedFile, Page pgPageInfo, string szCropType, ref bool bErrors)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/plain")
					{
					bool bDuplicateDetected = false;
					StreamReader strImportedFile = new StreamReader(hpfImportedFile.InputStream);
					DataTable dtCrops = FillCultivarsDataTable(strImportedFile, szCropType);
					//Sets the data to the selected region
					foreach(DataRow drCrop in dtCrops.Rows)
						{
						try
							{
							DataAccessClass.InsertCultivar(drCrop["CropType"].ToString(), drCrop["Type"].ToString());
							}
						catch(Exception)
							{
							bDuplicateDetected = true;
							}
						}
					if(bDuplicateDetected == true)
						{
						bErrors = true;
						FunctionsClass.DisplayMessage(pgPageInfo,"At least one duplicate cultivar wasn't imported");
						}
					}
				else
					{
					bErrors = true;
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception E)
				{
				bErrors = true;
				FunctionsClass.DisplayMessage(pgPageInfo, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Reads the met file and reads that data into a datatable which can be 
		//inserted into a database
		//-------------------------------------------------------------------------
		private static DataTable FillCultivarsDataTable(StreamReader strImportedFile, string szCropType)
		{
			DataTable dtCultivars = new DataTable();
			dtCultivars.Columns.Add("CropType");
			dtCultivars.Columns.Add("Type");
			DataRow drCultivars;
			string szCultivar;

			dtCultivars.Rows.Clear();
			while((szCultivar = strImportedFile.ReadLine()) != null)
				{
				if(szCultivar != "")
					{
					drCultivars = dtCultivars.NewRow();
					drCultivars["Type"] = szCultivar;
					drCultivars["CropType"] = szCropType;
					dtCultivars.Rows.Add(drCultivars);
					}
				}

			return dtCultivars;
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the cultivar file is in the correct format, and 
		//if it is then upload the file.
		//-------------------------------------------------------------------------
		private static void UploadImportedReportTemplate(HttpPostedFile hpfImportedFile, Page pgPageInfo, 
			string szReportType, string szTemplateType, string szCropType, ref bool bErrors)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/plain")
					{
					StreamReader strImportedFile = new StreamReader(hpfImportedFile.InputStream);
					string szTemplateText = strImportedFile.ReadToEnd();
					szTemplateText = SetUpTemplateTextForSaving(szTemplateText);
					//Saves the template to the database
					DataAccessClass.UpdateReportTypes(szTemplateText, szReportType, szTemplateType, szCropType);		
					}
				else
					{
					bErrors = true;
					FunctionsClass.DisplayMessage(pgPageInfo,"Invalid file type");
					}
				}
			catch(Exception E)
				{
				bErrors = true;
				FunctionsClass.DisplayMessage(pgPageInfo, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Set up the string for saving to the database by replacing the quote and
		//double quote characters with place holders
		//-------------------------------------------------------------------------
		private static string SetUpTemplateTextForSaving(string szTemplateText)
			{
			szTemplateText = szTemplateText.Replace("'", "#Quote#");
			szTemplateText = szTemplateText.Replace("\"", "#DQuote#");
			return szTemplateText;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
