using System;
using System.Data;
using System.Data.OleDb;
using System.IO;
using System.Web;

namespace UrbanWater
	{
	/// <summary>
	/// Summary description for DataAccessClass.
	/// </summary>
	public class DataAccessClass
		{
		public DataAccessClass()
			{}
		
		
		#region Functions to access the database
		//---------------------------------------------------------------------
		//Initialises the connection and Command objects that will be used to
		//access the database
		//---------------------------------------------------------------------
		static private void ConnectToDatabase(ref OleDbConnection dbConnection, ref OleDbCommand dbCommand)
			{
			try
				{
				string szCurrentLocation = HttpContext.Current.Server.MapPath("/UrbanWater/")+"Data";
				string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\UrbanWater.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
				dbConnection = new OleDbConnection(szConnectionString);
				dbConnection.Close();
				dbConnection.Open();
				dbCommand = dbConnection.CreateCommand();
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Initialises the connection object that will be used to
		//access the database
		//---------------------------------------------------------------------
		static private void ConnectToDatabase(ref OleDbConnection dbConnection)
			{
			try
				{
				string szCurrentLocation = HttpContext.Current.Server.MapPath("/UrbanWater/")+"Data";
				string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\UrbanWater.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
				dbConnection = new OleDbConnection(szConnectionString);
				dbConnection.Close();
				dbConnection.Open();
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Returns a single value from the database given the sql statement
		//and the field name to return
		//---------------------------------------------------------------------
		static private object ReturnSingleValueFromDB(string szFieldName, string szSQL)
			{
			OleDbConnection dbConnection = null;
			OleDbCommand dbCommand = null;
			ConnectToDatabase(ref dbConnection, ref dbCommand);
			object obResult = new object();
			try
				{
				dbCommand.CommandText = szSQL;
				OleDbDataReader dbDataReader = dbCommand.ExecuteReader();
				if(dbDataReader.Read())
					{
					int iOrdinal = dbDataReader.GetOrdinal(szFieldName);
					obResult = dbDataReader.GetValue(iOrdinal);
					}
				dbDataReader.Close();
				}
			catch(Exception)
				{}
				
			dbConnection.Close();
			return obResult;
			}
		//---------------------------------------------------------------------
		//Returns multiple values in the a DataTable format from the database 
		//given the sql statement
		//---------------------------------------------------------------------
		static private DataTable ReturnMultipleValuesFromDB(string szSQL)
			{
			OleDbConnection dbConnection = null;
			ConnectToDatabase(ref dbConnection);
			DataTable dtQueryResults = new DataTable();
			try
				{
				OleDbDataAdapter daQueryAdapter = new  OleDbDataAdapter(szSQL, dbConnection);
				daQueryAdapter.Fill(dtQueryResults);
				}
			catch(Exception)
				{}
			dbConnection.Close();
			return dtQueryResults;
			}
		//---------------------------------------------------------------------
		//Runs a passed in SQL string.
		//NOTE: SQL string must not be a query as this function will not return
		//any values
		//---------------------------------------------------------------------
		static public void RunSQLStatement(string szSQL)
			{
			OleDbConnection dbConnection = null;
			OleDbCommand dbCommand = null;
			ConnectToDatabase(ref dbConnection, ref dbCommand);
			try
				{
				dbCommand.CommandText = szSQL;
				dbCommand.ExecuteNonQuery();
				}
			catch(Exception)
				{}
			dbConnection.Close();
			}
		#endregion
		
		
		#region Page Information
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		static public DataTable GetAllPlantTypes()
			{
			DataTable dtPlantTypes;
			string szSQL = "SELECT ID, Name FROM PlantTypes";
			dtPlantTypes = ReturnMultipleValuesFromDB(szSQL);
			return dtPlantTypes;
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		static public DataTable GetAllLocations()
			{
			DataTable dtLocations;
			string szSQL = "SELECT ID, Name FROM Locations";
			dtLocations = ReturnMultipleValuesFromDB(szSQL);
			return dtLocations;
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		static public DataTable GetAllSoilTypes()
			{
			DataTable dtSoilTypes;
			string szSQL = "SELECT ID, Name FROM SoilTypes";
			dtSoilTypes = ReturnMultipleValuesFromDB(szSQL);
			return dtSoilTypes;
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public double GetPlantTypeRootingDepth(string szPlantTypeID)
		{
			double dRootingDepth = 0;
			try
				{
				string szSQL = "SELECT RootingDepth FROM PlantTypes "+
					"WHERE ID = "+szPlantTypeID;
				dRootingDepth = Convert.ToDouble(ReturnSingleValueFromDB("RootingDepth", szSQL).ToString());
				}
			catch(Exception)
				{}
			return dRootingDepth;
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public double GetPlantTypeCropFactor(string szPlantTypeID)
		{
			double dCropFactor = 0;
			try
			{
				string szSQL = "SELECT CropFactor FROM PlantTypes "+
					"WHERE ID = "+szPlantTypeID;
				dCropFactor = Convert.ToDouble(ReturnSingleValueFromDB("CropFactor", szSQL).ToString());
			}
			catch(Exception)
			{}
			return dCropFactor;
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public string GetPlantTypeName(string szPlantTypeID)
		{
			string szPlantTypeName = "";
			try
			{
				string szSQL = "SELECT Name FROM PlantTypes "+
					"WHERE ID = "+szPlantTypeID;
				szPlantTypeName = ReturnSingleValueFromDB("Name", szSQL).ToString();
			}
			catch(Exception)
			{}
			return szPlantTypeName;
		}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public double GetSoilTypePAWC(string szSoilTypeID)
			{
			double dPAWC = 0;
			try
				{
				string szSQL = "SELECT PAWC FROM SoilTypes "+
					"WHERE ID = "+szSoilTypeID;
				dPAWC = Convert.ToDouble(ReturnSingleValueFromDB("PAWC", szSQL).ToString());
				}
			catch(Exception)
				{}
			return dPAWC;
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public string GetSoilTypeName(string szSoilTypeID)
			{
			string szSoilTypeName = "";
			try
				{
				string szSQL = "SELECT Name FROM SoilTypes "+
					"WHERE ID = "+szSoilTypeID;
				szSoilTypeName = ReturnSingleValueFromDB("Name", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szSoilTypeName;
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public string GetLocationFileName(string szLocationID)
			{
			string szFilename = "";
			try
				{
				string szSQL = "SELECT FileName FROM Locations "+
					"WHERE ID = "+szLocationID;
				szFilename = ReturnSingleValueFromDB("FileName", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szFilename;
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public string GetLocationName(string szLocationID)
			{
			string szLocationName = "";
			try
				{
				string szSQL = "SELECT Name FROM Locations "+
					"WHERE ID = "+szLocationID;
				szLocationName = ReturnSingleValueFromDB("Name", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szLocationName;
			}	
		//-------------------------------------------------------------------------
		#endregion
		
		
		}//END OF CLASS
	}//END OF NAMESPACE
