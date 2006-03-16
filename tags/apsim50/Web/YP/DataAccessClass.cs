using System;
using System.Data;
using System.Data.SqlClient;
using System.Data.OleDb;
using System.Web;
using System.Collections.Specialized;
using CSGeneral;
using VBGeneral;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for DataAccessClass..
	/// </summary>
	public class DataAccessClass
		{	
		
		#region Functions to access the database
		//---------------------------------------------------------------------
		//Initialises the connection and Command objects that will be used to
		//access the database
		//---------------------------------------------------------------------
		static private void ConnectToDatabase(ref OleDbConnection dbConnection, ref OleDbCommand dbCommand)
			{
			string szCurrentLocation = HttpContext.Current.Server.MapPath("/YP/")+"Data";
			string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\yp2005.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
			dbConnection = new OleDbConnection(szConnectionString);
			dbConnection.Close();
			dbConnection.Open();
			dbCommand = dbConnection.CreateCommand();
			}
		//---------------------------------------------------------------------
		//Initialises the connection object that will be used to
		//access the database
		//---------------------------------------------------------------------
		static private void ConnectToDatabase(ref OleDbConnection dbConnection)
			{
			string szCurrentLocation = HttpContext.Current.Server.MapPath("/YP/")+"Data";
			string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\yp2005.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
			dbConnection = new OleDbConnection(szConnectionString);
			dbConnection.Close();
			dbConnection.Open();
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

			dbCommand.CommandText = szSQL;
			OleDbDataReader dbDataReader = dbCommand.ExecuteReader();
			if(dbDataReader.Read())
				{
				int iOrdinal = dbDataReader.GetOrdinal(szFieldName);
				obResult = dbDataReader.GetValue(iOrdinal);
				}
			dbDataReader.Close();

				
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

			OleDbDataAdapter daQueryAdapter = new  OleDbDataAdapter(szSQL, dbConnection);
			daQueryAdapter.Fill(dtQueryResults);

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

			dbCommand.CommandText = szSQL;
			dbCommand.ExecuteNonQuery();

			dbConnection.Close();
			}
		//---------------------------------------------------------------------	
		#endregion



		#region Functions to manipulate user information 
		//---------------------------------------------------------------------
		//Takes the username and password that was submitted at log in and
		//returns true if the login was successful
		//if the login details weren't correct it returns false
		//The comparision is case sensitive (collate Latin1_General_CS_AS)
		//---------------------------------------------------------------------
		static public bool AuthenticateUser(string szUserName, string szPassword)
			{
			bool bAuthenticated = false;

			string szSalt = GetSaltValueOfUser(szUserName);

			string szSQL = "SELECT COUNT (UserName) AS NumberOfRecords FROM Users "+
				"WHERE UserName = '"+szUserName+"' AND "+
				"(StrComp(UserName, '"+szUserName+"', 0)=False) AND "+
				"Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"' ";
			int iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
			if(iNumberOfRecords == 1)
				{
				bAuthenticated = true;
				}

			return bAuthenticated;
			}
		//---------------------------------------------------------------------
		//Takes a UserName and returns the access type of that user
		//---------------------------------------------------------------------
		static public string GetAccessTypeOfUser(string szUserName)
			{
			string szAccessType = "";

			string szSQL = "SELECT Type FROM AccessTypes "+
				"INNER JOIN Users ON AccessTypes.ID = Users.AccessTypeID "+
				"WHERE Users.UserName = '"+szUserName+"'";
			szAccessType = ReturnSingleValueFromDB("Type", szSQL).ToString();
		
			return szAccessType;
			}
		//---------------------------------------------------------------------
		//Takes a UserName and returns all the details of the user
		//---------------------------------------------------------------------
		static public DataTable GetDetailsOfUser(string szUserName)
			{
			DataTable dtResults;
			string szSQL = "SELECT * FROM Users "+
				"WHERE UserName = '"+szUserName+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Saves a new user to the database
		//---------------------------------------------------------------------
		static public void InsertUser(string szName, string szEmail, string szUserName,
			string szPassword, string szAccessType, StringCollection scConsultants, 
			StringCollection scUsersCrops)
			{
			string szSalt = FunctionsClass.CreateSalt();
			int iAccessTypeID = ReturnAccessTypeID(szAccessType);

			string szSQL = "INSERT INTO Users "+
				"(Name, Email, UserName, Salt, Pass, AccessTypeID) VALUES "+
				"('"+szName+"', '"+szEmail+"', '"+szUserName+"', '"+szSalt+"', '"+
				FunctionsClass.EncryptPassword(szPassword, szSalt)+"', "+iAccessTypeID.ToString()+")";
			RunSQLStatement(szSQL);

			if(scConsultants != null)
				{
				if(scConsultants.Count > 0)
					{
					for(int iIndex = 0; iIndex < scConsultants.Count; iIndex++)
						{
						InsertUserIntoConsultantUserMap(scConsultants[iIndex], szUserName);	
						}
					}
				}
			//Save the list of crops mapped to the user
			if(scUsersCrops != null)
				{
				if(scUsersCrops.Count > 0)
					{
					for(int iIndex = 0; iIndex < scUsersCrops.Count; iIndex++)
						{
						InsertUserIntoUsersCrops(scUsersCrops[iIndex], szUserName);	
						}
					}
				}
			}
		//---------------------------------------------------------------------
		//updates an existing user in the database
		//---------------------------------------------------------------------
		static public void UpdateUser(string szName, string szEmail, 
			string szNewUserName, string szPassword, string szUserName, 
			string szAccessType, StringCollection scConsultants, 
			StringCollection scUsersCrops)
			{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			System.Text.StringBuilder sbSQL = new System.Text.StringBuilder();
			sbSQL.Append("UPDATE Users SET ");
			if(szName != null && szName != "")
				{
				sbSQL.Append("Name = '"+szName+"', ");
				}
			if(szEmail != null && szEmail != "")
				{
				sbSQL.Append("Email = '"+szEmail+"', ");
				}
			if(szAccessType != null && szAccessType != "")
				{
				int iAccessTypeID = ReturnAccessTypeID(szAccessType);
				sbSQL.Append("AccessTypeID = "+iAccessTypeID.ToString()+", ");
				}
			if(szPassword != null && szPassword != "")
				{
				string szSalt = FunctionsClass.CreateSalt();
				sbSQL.Append("Salt = '"+szSalt+"', ");
				sbSQL.Append("Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"', ");
				}
			if(szNewUserName != null && szNewUserName != "")
			{
				sbSQL.Append("UserName = '"+szNewUserName+"', ");
				szUserName = szNewUserName;
			}
			sbSQL.Append("WHERE ID = "+iUserID.ToString());
			string szSQL = sbSQL.ToString();
			//Removes last , if it exists
			int iIndex = szSQL.LastIndexOf(",");
			if (iIndex > 0)
			{
				szSQL = szSQL.Remove(iIndex, 1);
			}
			RunSQLStatement(szSQL);

			//Save the list of consultants mapped to the user
			if(scConsultants != null)
				{
				if(scConsultants.Count > 0)
					{
					RemoveUserFromConsultantUserMap(szUserName);
					for(iIndex = 0; iIndex < scConsultants.Count; iIndex++)
						{
						InsertUserIntoConsultantUserMap(scConsultants[iIndex], szUserName);	
						}
					}
				}
			//Save the list of crops mapped to the user
			if(scUsersCrops != null)
				{
				if(scUsersCrops.Count > 0)
					{
					RemoveUserFromUsersCrops(szUserName);
					for(iIndex = 0; iIndex < scUsersCrops.Count; iIndex++)
						{
						InsertUserIntoUsersCrops(scUsersCrops[iIndex], szUserName);	
						}
					}
				}
			}
		//---------------------------------------------------------------------
		//Takes a UserName and deletes that user from the database
		//---------------------------------------------------------------------
		static public void DeleteUser(string szUserName)
			{
			int iUserID = ReturnUserIDFromUserName(szUserName);

			string szSQL = "DELETE FROM ConsultantUserMap "+
				"WHERE UserID = "+iUserID.ToString()+" "+
				"OR ConsultantID = "+iUserID.ToString();
			RunSQLStatement(szSQL);

			RemoveUserFromUsersCrops(szUserName);
			DeleteUsersPaddocks(szUserName);

			szSQL = "DELETE FROM Users "+
				"WHERE UserName = '"+szUserName+"'";
			RunSQLStatement(szSQL);

			}
		//---------------------------------------------------------------------
		//Returns all consultants from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllConsultants()
			{
			DataTable dtResults;
			string szSQL = "SELECT Users.Name, Users.UserName FROM Users "+
				"INNER JOIN AccessTypes ON Users.AccessTypeID = AccessTypes.ID "+
				"WHERE AccessTypes.Type = '"+FunctionsClass.szConsultant+"' OR "+
				"AccessTypes.Type = '"+FunctionsClass.szVisitorConsultant+"' OR "+
				"AccessTypes.Type = '"+FunctionsClass.szAdministrator+"' "+
				"ORDER BY Users.Name";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Get a list of all the users that belong to the the passed in Consultant's
		//user name, if no username is passed in then all users are returned
		//---------------------------------------------------------------------
		static public DataTable GetUsersMappedToConsultant(string szUserName)
			{
			DataTable dtResults;
			System.Text.StringBuilder sbSQL = new System.Text.StringBuilder();
			sbSQL.Append("SELECT Users_1.Name AS Name, Users_1.UserName AS UserName, ");
			sbSQL.Append("Users_1.ID AS ID, ConsultantUserMap.ConsultantID AS ParentID, ");
			sbSQL.Append("AccessTypes.Type AS AccessType ");
			sbSQL.Append("FROM AccessTypes INNER JOIN ");
			sbSQL.Append("(Users AS Users_1 INNER JOIN ");
			sbSQL.Append("(Users INNER JOIN ConsultantUserMap ON Users.ID = ConsultantUserMap.ConsultantID) ");
			sbSQL.Append("ON Users_1.ID = ConsultantUserMap.UserID) ");
			sbSQL.Append("ON AccessTypes.ID = Users_1.AccessTypeID ");
			if(szUserName != null && szUserName != "")
				{
				sbSQL.Append("WHERE Users.UserName='"+szUserName+"' ");
				}
			sbSQL.Append("ORDER BY Users_1.Name, Users.Name");
			dtResults = ReturnMultipleValuesFromDB(sbSQL.ToString());
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Get a list of all the users that do not belong to the the passed in Consultant's
		//user name, if no username is passed in then all users that do not
		//have consultants are returned
		//---------------------------------------------------------------------
		static public DataTable GetUsersNotMappedToConsultant(string szUserName)
			{
			DataTable dtResults;
			System.Text.StringBuilder sbSQL = new System.Text.StringBuilder();
			sbSQL.Append("SELECT Users.Name AS Name, Users.ID AS ID, ");
			sbSQL.Append("Users.UserName AS UserName, AccessTypes.Type AS AccessType ");
			sbSQL.Append("FROM Users INNER JOIN AccessTypes ");
			sbSQL.Append("ON Users.AccessTypeID = AccessTypes.ID ");
			sbSQL.Append("WHERE Users.ID NOT IN  (SELECT UserID FROM ConsultantUserMap) ");
			if(szUserName != null && szUserName != "")
				{
				sbSQL.Append("AND Users.UserName='"+szUserName+"'");
				}
			sbSQL.Append("ORDER BY Users.Name");
			dtResults = ReturnMultipleValuesFromDB(sbSQL.ToString());
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Gets all the consultants linked to the user
		//---------------------------------------------------------------------
		static public DataTable GetUsersConsultants(string szUserName)
			{
			DataTable dtResults;
			string szSQL = "SELECT Users.UserName, Users.Name "+
				"FROM Users AS Users_1 INNER JOIN "+
				"(Users INNER JOIN ConsultantUserMap ON Users.ID = ConsultantUserMap.ConsultantID) "+
				"ON Users_1.ID = ConsultantUserMap.UserID "+
				"WHERE Users_1.UserName = '"+szUserName+"'";

			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Gets all the crops linked to the user
		//---------------------------------------------------------------------
		static public DataTable GetUsersCrops(string szUserName)
			{
			DataTable dtResults;
			string szSQL = "SELECT CropTypes.Type "+
				"FROM Users INNER JOIN (CropTypes INNER JOIN UsersCrops ON CropTypes.ID = UsersCrops.CropTypeID) "+
				"ON Users.ID = UsersCrops.UserID "+
				"WHERE Users.UserName = '"+szUserName+"'";

			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Checks to see if the selected user name is in use or not
		//Returns true if the username is available
		//---------------------------------------------------------------------
		static public bool IsUserNameAvailable(string szUserName)
			{
			int iNumberOfRecords = 0;
			bool bAvailable = false;

			string szSQL = "SELECT COUNT(ID) AS NumberOfRecords FROM Users "+
			"WHERE UserName ='"+szUserName+"'";
			iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
			if(iNumberOfRecords == 0)
				{
				bAvailable = true;
				}

			return bAvailable;
			}
		//---------------------------------------------------------------------
		//Takes a user name and deletes all the paddocks linked to the user
		//---------------------------------------------------------------------
		static private void DeleteUsersPaddocks(string szUserName)
			{
			DataTable dtPaddocks = GetPaddocksOfUser(szUserName);
			foreach(DataRow drPaddock in dtPaddocks.Rows)
				{
				DeletePaddock(drPaddock["Name"].ToString(), szUserName);
				}
			}	
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void InsertUserIntoConsultantUserMap(string szConsultantUserName, string szUserName)
			{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			int iConsultantID = ReturnUserIDFromUserName(szConsultantUserName);

			string szSQL = "INSERT INTO ConsultantUserMap "+
				"(ConsultantID, UserID) VALUES "+
				"("+iConsultantID.ToString()+", "+iUserID.ToString()+")";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void RemoveUserFromConsultantUserMap(string szUserName)
		{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			string szSQL = "DELETE FROM ConsultantUserMap "+
				"WHERE UserID = "+iUserID.ToString();
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void InsertUserIntoUsersCrops(string szCropType, string szUserName)
		{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			int iCropTypeID = DataAccessClass.ReturnCropTypeID(szCropType);

			string szSQL = "INSERT INTO UsersCrops "+
				"(CropTypeID, UserID) VALUES "+
				"("+iCropTypeID.ToString()+", "+iUserID.ToString()+")";
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void RemoveUserFromUsersCrops(string szUserName)
		{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			string szSQL = "DELETE FROM UsersCrops "+
				"WHERE UserID = "+iUserID.ToString();
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//Takes a username and returns the salt value of that user
		//---------------------------------------------------------------------
		static private string GetSaltValueOfUser(string szUserName)
			{
			string szSalt = "";

			string szSQL = "SELECT Salt FROM Users "+
				"WHERE UserName = '"+szUserName+"'";
			szSalt = ReturnSingleValueFromDB("Salt", szSQL).ToString();

			return szSalt;
			}
		//---------------------------------------------------------------------
		//Takes a user's username and returns their UserID
		//---------------------------------------------------------------------
		static private int ReturnUserIDFromUserName(string szUserName)
			{
			int iUserID = 0;

			string szSQL = "SELECT ID FROM Users "+
				"WHERE UserName = '"+szUserName+"'";
			iUserID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iUserID;
			}
		//---------------------------------------------------------------------
		//Takes a users's name and returns their UserID
		//---------------------------------------------------------------------
		static private int ReturnUserIDFromName(string szName)
		{
			int iUserID = 0;

			string szSQL = "SELECT ID FROM Users "+
				"WHERE Name = '"+szName+"'";
			iUserID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iUserID;
		}
		//---------------------------------------------------------------------
		#endregion



		#region Functions to Manipulate Access Priviledges
		//---------------------------------------------------------------------
		//Takes an access type and returns the AccessTypeID of that access type 
		//---------------------------------------------------------------------
		static private int ReturnAccessTypeID(string szAccessType)
			{
			int iAccessTypeID = 0;
			try
				{
				string szSQL = "SELECT ID FROM AccessTypes "+
					"WHERE Type = '"+szAccessType+"'";
				iAccessTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iAccessTypeID;
			}
		//---------------------------------------------------------------------
		//Returns all Access Types from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllAccessTypes()
			{
			DataTable dtResults;
			string szSQL = "SELECT Type, ID FROM AccessTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate paddock information
		//---------------------------------------------------------------------
		//Takes a paddock name and a username and returns all the details of 
		//that paddock
		//---------------------------------------------------------------------
		static public DataTable GetDetailsOfPaddock(string szPaddockName, string szUserName)
			{
			DataTable dtResults;

			string szSQL = "SELECT Paddocks.SowDate, Paddocks.StartOfGrowingSeasonDate, MetStations.Name AS MetStationName, "+
				"MetStations.StationNumber, CultivarTypes.Type AS CultivarType, "+
				"RowConfigurationTypes.Type AS RowConfigurationType, Soils.Name AS SoilName, "+
				"Regions.Type AS RegionType, CropTypes.Type AS CropType, Paddocks.DefaultRainfall, "+
				"LinkedRainfallPaddock.Name AS LinkedRainfallPaddockName, Paddocks.Triazine, "+
				"Paddocks.RootingDepth, Paddocks.Population, Paddocks.TillerNumber, Paddocks.RowSpacing, "+
				"Paddocks.AutoFTN, Paddocks.UseEC "+
				"FROM Regions INNER JOIN (CropTypes INNER JOIN (Paddocks AS LinkedRainfallPaddock RIGHT JOIN "+
				"(RowConfigurationTypes INNER JOIN (Soils INNER JOIN (CultivarTypes INNER JOIN (Users INNER JOIN "+
				"(MetStations INNER JOIN Paddocks ON MetStations.ID = Paddocks.MetStationID) ON Users.ID = Paddocks.UserID) "+
				"ON CultivarTypes.ID = Paddocks.CultivarTypeID) ON Soils.ID = Paddocks.SoilID) "+
				"ON RowConfigurationTypes.ID = Paddocks.RowConfigurationTypeID) "+
				"ON LinkedRainfallPaddock.ID = Paddocks.LinkedTemporalPaddockID) "+
				"ON CropTypes.ID = CultivarTypes.CropTypeID) ON Regions.ID = MetStations.RegionID "+
				"WHERE Paddocks.Name = '"+szPaddockName+"' AND Users.UserName ='"+szUserName+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);

			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns a datatable with all the paddocks that are available for linking
		//---------------------------------------------------------------------
		static public DataTable GetAllPaddocksForTemporalLinking(string szPaddockName, string szUserName)
			{
			DataTable dtResults = new DataTable();
			dtResults.Columns.Add("Name");

			if(IsPaddockATemporalMaster(szPaddockName, szUserName) == false)
				{
				string szSQL = "SELECT Paddocks.Name FROM Paddocks "+
					"INNER JOIN Users ON Paddocks.UserID = Users.ID "+
					"WHERE Paddocks.Name <> '"+szPaddockName+"' "+
					"AND Users.UserName = '"+szUserName+"' "+
					"AND LinkedTemporalPaddockID = 0";
				dtResults = ReturnMultipleValuesFromDB(szSQL);
				}

			return dtResults;
			}
		//---------------------------------------------------------------------
		//Saves a new paddock to the database
		//---------------------------------------------------------------------
		static public void InsertPaddock(string szName, string szSowDate, 
			string szCultivarType, int iTriazine, string szRowConfigurationType, 
			int iPopulation, double dRowSpacing, string szUserName)
			{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			int iCultivarTypeID = ReturnCultivarTypeID(szCultivarType);
			int iRowConfigurationTypeID = 1;
			if(szRowConfigurationType != "")
			{
				iRowConfigurationTypeID = ReturnRowConfigurationTypeID(szRowConfigurationType);
			}

			string szSQL = "INSERT INTO Paddocks "+
				"(Name, SowDate, CultivarTypeID, Triazine, RowConfigurationTypeID, Population, RowSpacing, UserID) VALUES "+
				"('"+szName+"', '"+szSowDate+"', "+iCultivarTypeID.ToString()+", "+iTriazine.ToString()+", "+
				iRowConfigurationTypeID.ToString()+", "+iPopulation.ToString()+", "+dRowSpacing.ToString()+", "+iUserID.ToString()+")";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//updates an existing paddock cropping details in the database
		//---------------------------------------------------------------------
		static public void UpdatePaddock(string szSowDate, string szCultivarType, int iTriazine, 
			string szMetStaionName, string szSoilName, string szRowConfigurationType, 
			int iDefaultRainfall, string szLinkedTemporalPaddockName, string szStartOfGrowingSeason, 
			int iPopulation, int iMaxRootingDepth, double dTillerNumber, double dRowSpacing, 
			int iAutoFTN, int iUseEC, string szNewPaddockName, string szPaddockName, string szUserName)
			{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);

			System.Text.StringBuilder sbSQL = new System.Text.StringBuilder();
			sbSQL.Append("UPDATE Paddocks SET ");
			if(szCultivarType != null && szCultivarType != "")
				{
				int iCultivarTypeID = ReturnCultivarTypeID(szCultivarType);
				sbSQL.Append("CultivarTypeID = "+iCultivarTypeID.ToString()+", ");
				}
			if(iTriazine == 1 || iTriazine == 0)
				{
				sbSQL.Append("Triazine = "+iTriazine.ToString()+", ");
				}
			if(iPopulation > -1)
				{
				sbSQL.Append("Population = "+iPopulation.ToString()+", ");
				}
			if(iMaxRootingDepth > -1)
				{
				sbSQL.Append("RootingDepth = "+iMaxRootingDepth.ToString()+", ");
				}
			if(iDefaultRainfall > -1)
				{
				sbSQL.Append("DefaultRainfall = "+iDefaultRainfall.ToString()+", ");
				}
			if(dTillerNumber > -1)
			{
				sbSQL.Append("TillerNumber = "+dTillerNumber.ToString()+", ");
			}
			if(dRowSpacing > -1)
			{
				sbSQL.Append("RowSpacing = "+dRowSpacing.ToString()+", ");
			}
			if(iAutoFTN == 1 || iAutoFTN == 0)
			{
				sbSQL.Append("AutoFTN = "+iAutoFTN.ToString()+", ");
			}
			if(iUseEC == 1 || iUseEC == 0)
			{
				sbSQL.Append("UseEC = "+iUseEC.ToString()+", ");
			}
			if(szRowConfigurationType != null && szRowConfigurationType != "")
			{
				int iRowConfigurationTypeID = ReturnRowConfigurationTypeID(szRowConfigurationType);
				sbSQL.Append("RowConfigurationTypeID = "+iRowConfigurationTypeID.ToString()+", ");
			}
			if(szMetStaionName != null && szMetStaionName != "")
				{
				int iMetStationID = ReturnMetStationID(szMetStaionName);
				sbSQL.Append("MetStationID = "+iMetStationID.ToString()+", ");
				}
			if(szSoilName != null && szSoilName != "")
				{
				int iSoilID = ReturnSoilID(szSoilName);
				sbSQL.Append("SoilID = "+iSoilID.ToString()+", ");
				}
			if(szLinkedTemporalPaddockName != null && szLinkedTemporalPaddockName != "")
				{
				int iLinkedTemporalPaddockID = 0;
				if(szLinkedTemporalPaddockName != "NONE")
				{
					iLinkedTemporalPaddockID = ReturnPaddockID(szLinkedTemporalPaddockName, szUserName);
				}
				sbSQL.Append("LinkedTemporalPaddockID = "+iLinkedTemporalPaddockID.ToString()+", ");
				}
			if(szSowDate != null && szSowDate != "")
				{
				sbSQL.Append("SowDate = '"+szSowDate+"', ");
				}
			if(szStartOfGrowingSeason != null && szStartOfGrowingSeason != "")
			{
				sbSQL.Append("StartOfGrowingSeasonDate = '"+szStartOfGrowingSeason+"', ");
			}
			if(szNewPaddockName != null && szNewPaddockName != "")
			{
			sbSQL.Append("Name = '"+szNewPaddockName+"', ");
			}
			sbSQL.Append("WHERE ID = "+iPaddockID.ToString());
			string szSQL = sbSQL.ToString();
			//Removes last , if it exists
			int iIndex = szSQL.LastIndexOf(",");
			if (iIndex > 0)
			{
				szSQL = szSQL.Remove(iIndex, 1);
			}
			RunSQLStatement(szSQL);
			}
		//-------------------------------------------------------------------------
		//Resets the paddock information, by clearing the sow date and cultivar type
		//-------------------------------------------------------------------------
		static public void ResetPaddock(string szPaddockName, string szNewPaddockName, string szUserName)
			{
			int iUserID = ReturnUserIDFromUserName(szUserName);
			string szSQL = "UPDATE Paddocks SET "+
				"Name = '"+szNewPaddockName+"', "+
				"SowDate = '', "+
				"CultivarTypeID = 1 "+
				"WHERE Name = '"+szPaddockName+"' "+
				"AND UserID = "+iUserID.ToString();
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Takes a UserName and returns all the paddocks of that user
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksOfUser(string szUserName)
			{
			DataTable dtResults;
			string szSQL = "SELECT Paddocks.Name FROM Paddocks "+
				"INNER JOIN Users ON Paddocks.UserID = Users.ID "+
				"WHERE Users.UserName = '"+szUserName+"' "+
				"ORDER BY Paddocks.Name";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Checks to see if the selected paddock name is in use or not
		//Returns true if the paddock name is available
		//---------------------------------------------------------------------
		static public bool IsPaddockNameAvailable(string szPaddockName, string szUserName)
			{
			int iNumberOfRecords = 0;
			bool bAvailable = false;

			string szSQL = "SELECT COUNT(Paddocks.ID) AS NumberOfRecords FROM Paddocks "+
				"INNER JOIN Users ON Paddocks.UserID = Users.ID "+
				"WHERE Users.UserName ='"+szUserName+"' "+
				"AND Paddocks.Name = '"+szPaddockName+"'";
			iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
			if(iNumberOfRecords == 0)
			{
				bAvailable = true;
			}

			return bAvailable;
			}
		//---------------------------------------------------------------------
		//Takes a PaddockID and deletes that paddock and all dependant data 
		//from the database
		//---------------------------------------------------------------------
		static public void DeletePaddock(string szPaddockName, string szUserName)
			{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);

			string szSQL = "DELETE FROM Paddocks "+
				"WHERE ID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
			
			DeletePaddocksSoilSamples(iPaddockID);
			DeletePaddocksTemporalEvents(iPaddockID);
			DeletePaddocksFertiliserApplications(iPaddockID);
			DeletePaddocksIrrigationApplications(iPaddockID);
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private int ReturnPaddockID(string szPaddockName, string szUserName)
			{
			int iPaddockID = 0;

			string szSQL = "SELECT Paddocks.ID FROM Paddocks "+
				"INNER JOIN Users ON Paddocks.UserID = Users.ID "+
				"WHERE Users.UserName = '"+szUserName+"' "+
				"AND Paddocks.Name = '"+szPaddockName+"'";
			iPaddockID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iPaddockID;
			}
		//---------------------------------------------------------------------
		//Checks to see if the paddock is already linked to by another paddock.
		//This is to stop chain linking ie: padock1 -> padock2 -> paddock3
		//---------------------------------------------------------------------
		static private bool IsPaddockATemporalMaster(string szPaddockName, string szUserName)
			{
			bool bPaddockTemporalMaster = false;
			int iNumberOfLinkedTemporalPaddocks = 0;
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);

			string szSQL = "SELECT Count(ID) As NumberOfLinkedTemporalPaddocks FROM Paddocks "+
				"WHERE LinkedTemporalPaddockID = "+iPaddockID;
			iNumberOfLinkedTemporalPaddocks = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfLinkedTemporalPaddocks", szSQL).ToString());
			if(iNumberOfLinkedTemporalPaddocks > 0)
				{
				bPaddockTemporalMaster = true;
				}

			return bPaddockTemporalMaster;
			}
		//-------------------------------------------------------------------------

		
		#endregion



		#region Functions to manipulate Soil Information
		//-------------------------------------------------------------------------
		//Inserts a new soil into the database
		//-------------------------------------------------------------------------
		static public void InsertOrEditSoil(string szRegion, string szExistingSoilName, 
			string szSoilData, string szNewSoilName)
			{
			int iRegionID = DataAccessClass.ReturnRegionID(szRegion);

			//Convert the cropnames to lower
			APSIMData Data = new APSIMData(szSoilData);
			foreach (APSIMData crop in Data.get_Children("SoilCrop"))
				{
				crop.Name = crop.Name.ToLower();
				}

			Soil sImportSoil = new Soil(Data);
			string Errors = sImportSoil.CheckForErrors();
			if (Errors != "")
				throw new Exception(Errors);

			szSoilData = sImportSoil.Data.XML;

			int SoilID;
			try
				{
				SoilID = ReturnSoilID(szExistingSoilName);
				}
			catch (Exception)
				{
				SoilID = 0;
				}

			if (SoilID == 0)
				{
				string szSQL = "INSERT INTO Soils "+
					"(RegionID, Name, Data) VALUES "+
					"("+iRegionID.ToString()+", '"+szNewSoilName+"', '"+szSoilData+"')";
				RunSQLStatement(szSQL);
				}
			else
				{
				string szSQL = "UPDATE Soils SET Data = '" +	szSoilData + "', " +
					"Name = '"+szNewSoilName+"' " +
					"WHERE ID = " + SoilID.ToString();
				RunSQLStatement(szSQL);
				}


		
			}
		//-------------------------------------------------------------------------
		//Deletes the selected soil from the database
		//-------------------------------------------------------------------------
		static public void DeleteSoil(string szRegion, string szSoilName)
			{
			int iRegionID = DataAccessClass.ReturnRegionID(szRegion);
			string szSQL = "DELETE FROM Soils "+
				"WHERE RegionID = "+iRegionID.ToString()+" "+
				"AND Name = '"+szSoilName+"'";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Takes a RegionID and a UserID and returns all the Soils for that region
		//and that and that user
		//---------------------------------------------------------------------
		static public DataTable GetSoilsOfRegion(string szRegion)
			{
			DataTable dtResults;
			string szSQL = "SELECT Soils.Name FROM Soils "+
				"INNER JOIN Regions ON Soils.RegionID = Regions.ID "+
				"WHERE Regions.Type = '"+szRegion+"' "+
				"ORDER BY Soils.Name";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns the selected soils data (stored in xml format)
		//---------------------------------------------------------------------
		static public Soil GetSoil(string szSoilName)
			{
			string szSoilData = "";

			string szSQL = "SELECT Data FROM Soils "+
				"WHERE Name = '"+szSoilName+"'";
			szSoilData = ReturnSingleValueFromDB("Data", szSQL).ToString();

			return new Soil(new APSIMData(szSoilData));
			}
		//---------------------------------------------------------------------
		//Returns the selected soils data (stored in xml format)
		//---------------------------------------------------------------------
		static public string GetSoilData(string szSoilName)
		{
			string szSoilData = "";

			string szSQL = "SELECT Data FROM Soils "+
				"WHERE Name = '"+szSoilName+"'";
			szSoilData = ReturnSingleValueFromDB("Data", szSQL).ToString();

			return szSoilData;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksUsingSoil(string szRegion, string szSoilName)
		{
			string szSQL = "SELECT Users.Name, Paddocks.Name FROM Users, Paddocks, MetStations, Regions, Soils" +
				           " WHERE Paddocks.SoilID = Soils.ID" +
				           "   AND MetStations.ID = Paddocks.MetStationID" +
				           "   AND Regions.ID = MetStations.RegionID" +
				           "   AND Users.ID = Paddocks.UserID" +
				           "   AND Soils.Name = '"+szSoilName+"'" +
				           "   AND Regions.Type = '"+szRegion+"'";
			DataTable dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Returns the ID of the specified Soil
		//---------------------------------------------------------------------
		static private int ReturnSoilID(string szSoilName)
			{
			int iSoilID = 0;

			string szSQL = "SELECT ID FROM Soils "+
				"WHERE Name = '"+szSoilName+"'";
			iSoilID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iSoilID;
			}
		//---------------------------------------------------------------------
		// Check all soils 
		//---------------------------------------------------------------------
		static public void CheckAllSoils()
			{
			string Errors = "";
			DataTable Regions = GetAllRegions();
			foreach (DataRow Region in Regions.Rows)
				{
				string RegionName = Region["Type"].ToString();
				DataTable Soils = GetSoilsOfRegion(RegionName);
				foreach (DataRow Soil in Soils.Rows)
					{
					string SoilName = Soil["Name"].ToString();
					string SoilData = GetSoilData(SoilName);
					if (SoilData != "")
						{
						APSIMData Data = new APSIMData(SoilData);
						Soil ImportSoil = new Soil(Data);
						string msg = ImportSoil.CheckForErrors();
						if (msg != "")
							Errors += "Cannot convert soil: " + SoilName.Replace("&", " ") + ". Error: " + msg.Replace("\r\n", "");
						}
					}

				}
			if (Errors != "")
				throw new Exception(Errors);
			}

		// ----------------------------------------------
		// Check to see if the specified sample is valid
		// against its linked soil. Return true if sample
		// is ok.
		//-------------------------------------------------------------------------
		static public bool IsSoilSampleOk(SoilSample Sample)
			{
			double[] sw = Sample.SWMapedToSoil;
			double[] airdry = Sample.LinkedSoil.Airdry;
			double[] sat = Sample.LinkedSoil.SAT;

			for (int i = 0; i != sw.Length; i++)
				{
				if (sw[i] - airdry[i] < -0.015 || sw[i] - sat[i] > 0.015)
					return false;
				}
			return true;
			}

		#endregion



		#region Functions to manipulate Met Station Information
		//-------------------------------------------------------------------------
		//Inserts a new met station into the database
		//-------------------------------------------------------------------------
		static public void InsertMetStation(string szRegion, string szMetStationName, int iMetStationNumber)
			{
			int iRegionID = DataAccessClass.ReturnRegionID(szRegion);
			string szSQL = "INSERT INTO MetStations "+
				"(RegionID, Name, StationNumber) VALUES "+
				"("+iRegionID.ToString()+", '"+szMetStationName+"', "+iMetStationNumber.ToString()+")";
			RunSQLStatement(szSQL);
			}
		//-------------------------------------------------------------------------
		//Updates an existing met station in the database
		//-------------------------------------------------------------------------
		static public void UpdateMetStation(string szRegion, string szExistingMetStationName,
			int iMetStationNumber, string szNewMetStationName)
		{
			int iMetStationID = ReturnMetStationID(szExistingMetStationName);

			string szSQL = "UPDATE MetStations SET Name = '" +szNewMetStationName+ "', " +
				"StationNumber = "+iMetStationNumber+" " +
				"WHERE ID = " + iMetStationID.ToString();
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//Returns the selected met station
		//---------------------------------------------------------------------
		static public DataTable GetMetStation(string szMetStationName)
		{
			DataTable dtResults;

			string szSQL = "SELECT MetStations.* FROM MetStations "+
				"WHERE Name = '"+szMetStationName+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);

			return dtResults;
		}
		//---------------------------------------------------------------------
		//Takes a RegionID and returns all the MetStations for that region
		//---------------------------------------------------------------------
		static public DataTable GetMetStationsOfRegion(string szRegion)
			{
			DataTable dtResults;
			string szSQL = "SELECT MetStations.Name FROM MetStations "+
				"INNER JOIN Regions ON MetStations.RegionID = Regions.ID "+
				"WHERE Regions.Type = '"+szRegion+"' "+
				"ORDER BY MetStations.Name";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//-------------------------------------------------------------------------
		//Deletes the selected metstation from the database
		//-------------------------------------------------------------------------
		static public void DeleteMetStation(string szRegion, string szMetStationName)
			{
			int iRegionID = DataAccessClass.ReturnRegionID(szRegion);
			string szSQL = "DELETE FROM MetStations "+
				"WHERE RegionID = "+iRegionID.ToString()+" "+
				"AND Name = '"+szMetStationName+"'";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public bool IsMetStationInUse(string szRegion, string szMetStation)
		{
			int iNumberOfRecords = 0;
			bool bInUse = true;

			string szSQL = "SELECT COUNT(Paddocks.ID) AS NumberOfRecords FROM (Paddocks "+
				"INNER JOIN MetStations ON MetStations.ID = Paddocks.MetStationID) "+
				"INNER JOIN Regions ON Regions.ID = MetStations.RegionID "+
				"WHERE MetStations.Name = '"+szMetStation+"' AND Regions.Type = '"+szRegion+"'";
			iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
			if(iNumberOfRecords == 0)
			{
				bInUse = false;
			}
			return bInUse;
		}
		//---------------------------------------------------------------------
		//Returns the ID for the specified station name
		//---------------------------------------------------------------------
		static private int ReturnMetStationID(string szMetStationName)
			{
			int iMetStationID = 0;

			string szSQL = "SELECT ID FROM MetStations "+
				"WHERE Name = '"+szMetStationName+"'";
			iMetStationID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iMetStationID;
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Functions to manipulate Crop and Cultivar Information

		//-------------------------------------------------------------------------
		//Returns all the crops from the database
		//-------------------------------------------------------------------------
		static public DataTable GetAllCrops()
		{
			DataTable dtResults;
			string szSQL = "SELECT CropTypes.Type FROM CropTypes "+
				"ORDER BY CropTypes.Type";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//-------------------------------------------------------------------------
		//Takes a Crop Type and returns all cultivars of that crop
		//-------------------------------------------------------------------------
		static public DataTable GetAllCultivarsOfCrop(string szCropType)
		{
			DataTable dtResults;
			string szSQL = "SELECT CultivarTypes.Type FROM CultivarTypes "+
				"INNER JOIN CropTypes ON CultivarTypes.CropTypeID = CropTypes.ID "+
				"WHERE CropTypes.Type = '"+szCropType+"' "+
				"ORDER BY CultivarTypes.Type";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Inserts a new cultivar into the database
		//---------------------------------------------------------------------
		static public void InsertCultivar(string szCropType, string szCultivarType)
		{
			int iCropTypeID = DataAccessClass.ReturnCropTypeID(szCropType);
			string szSQL = "INSERT INTO CultivarTypes "+
				"(CropTypeID, Type) VALUES "+
				"("+iCropTypeID.ToString()+", '"+szCultivarType+"')";
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//Takes a Cultivar Type and deletes that Cultivar from the database
		//---------------------------------------------------------------------
		static public void DeleteCulivar(string szCultivarType)
		{
			string szSQL = "DELETE FROM CultivarTypes "+
				"WHERE Type = '"+szCultivarType+"'";
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns the ID for the specifed crop type
		//---------------------------------------------------------------------
		static private int ReturnCropTypeID(string szCropType)
		{
			int iCropTypeID = 0;

			string szSQL = "SELECT ID FROM CropTypes "+
				"WHERE Type = '"+szCropType+"'";
			iCropTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iCropTypeID;
		}
		//---------------------------------------------------------------------
		//Returns the ID of the specified Cultivar type
		//---------------------------------------------------------------------
		static private int ReturnCultivarTypeID(string szCultivarType)
		{
			int iCultivarTypeID = 0;

			string szSQL = "SELECT ID FROM CultivarTypes "+
				"WHERE Type = '"+szCultivarType+"'";
			iCultivarTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iCultivarTypeID;
		}
		//-------------------------------------------------------------------------
		#endregion



		#region Functions to manipulate fertiliser Information

		//---------------------------------------------------------------------
		//Inserts a new Fertiliser application into the database
		//---------------------------------------------------------------------
		static public void InsertFertiliserApplication(string szApplicationDate, string szApplicationRate,
			string szFertiliserType, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iFertiliserTypeID = DataAccessClass.ReturnFertiliserTypeID(szFertiliserType);
			string szSQL = "INSERT INTO FertiliserApplication "+
				"(ApplicationDate, Rate, FertiliserTypeID, PaddockID) VALUES "+
				"('"+szApplicationDate+"', "+szApplicationRate+", "+
				iFertiliserTypeID.ToString()+", "+iPaddockID.ToString()+")";
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns all the fertiliser applications of a selected fertiliser type 
		//for a selected paddock
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksFertiliserApplications(string szFertiliserType, 
			string szPaddockName, string szUserName)
		{
			DataTable dtResults;
			string szSQL = "SELECT FertiliserApplication.Rate, FertiliserApplication.ApplicationDate "+
				"FROM Users INNER JOIN (Paddocks INNER JOIN (FertiliserTypes INNER JOIN FertiliserApplication "+
				"ON FertiliserTypes.ID = FertiliserApplication.FertiliserTypeID) "+
				"ON Paddocks.ID = FertiliserApplication.PaddockID) ON Users.ID = Paddocks.UserID "+
				"WHERE FertiliserTypes.Type = '"+szFertiliserType+"' "+
				"AND Paddocks.Name ='"+szPaddockName+"' "+
				"AND Users.UserName='"+szUserName+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Deletes a fertiliser application from the database
		//---------------------------------------------------------------------
		static public void DeletePaddocksFertiliserApplications(string szFertiliserType, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iFertiliserTypeID = ReturnFertiliserTypeID(szFertiliserType);

			string szSQL = "DELETE FROM FertiliserApplication "+
				"WHERE PaddockID = "+iPaddockID.ToString()+" "+
				"AND FertiliserTypeID = "+iFertiliserTypeID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Deletes all the fertiliser application records for the selected paddock
		//---------------------------------------------------------------------
		static private void DeletePaddocksFertiliserApplications(int iPaddockID)
		{
			string szSQL = "DELETE FROM FertiliserApplication "+
				"WHERE PaddockID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns the ID of the specified fertiliser type
		//---------------------------------------------------------------------
		static private int ReturnFertiliserTypeID(string szFertiliserType)
		{
			int iFertiliserType = 0;

			string szSQL = "SELECT ID FROM FertiliserTypes "+
				"WHERE Type = '"+szFertiliserType+"'";
			iFertiliserType = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iFertiliserType;
		}
		//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate irrigation Information

		//---------------------------------------------------------------------
		//Inserts a new Fertiliser application into the database
		//---------------------------------------------------------------------
		static public void InsertIrrigationApplication(string szApplicationDate, string szApplicationAmount,
			string szEfficency, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			string szSQL = "INSERT INTO IrrigationApplication "+
				"(ApplicationDate, Amount, Efficency, PaddockID) VALUES "+
				"('"+szApplicationDate+"', "+szApplicationAmount+", "+
				szEfficency+", "+iPaddockID.ToString()+")";
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns all the irrigation applications of a selected irrigation type 
		//for a selected paddock
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksIrrigationApplications(string szPaddockName, 
			string szUserName)
		{
			DataTable dtResults;
			string szSQL = "SELECT IrrigationApplication.Amount, IrrigationApplication.ApplicationDate, "+
				"IrrigationApplication.Efficency FROM Users INNER JOIN "+
				"(Paddocks INNER JOIN  IrrigationApplication "+
				"ON Paddocks.ID = IrrigationApplication.PaddockID) ON Users.ID = Paddocks.UserID "+
				"WHERE Paddocks.Name ='"+szPaddockName+"' "+
				"AND Users.UserName='"+szUserName+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Deletes a fertiliser application from the database
		//---------------------------------------------------------------------
		static public void DeletePaddocksIrrigationApplications(string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);

			string szSQL = "DELETE FROM IrrigationApplication "+
				"WHERE PaddockID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Deletes all the fertiliser application records for the selected paddock
		//---------------------------------------------------------------------
		static private void DeletePaddocksIrrigationApplications(int iPaddockID)
		{
			string szSQL = "DELETE FROM IrrigationApplication "+
				"WHERE PaddockID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		#endregion



		#region Functions to Manipulate Soil Sample Information

		//---------------------------------------------------------------------
		//Updates or inserts the soil sample into the database
		//---------------------------------------------------------------------
		static public void SetSoilSample(string szSoilSampleDate, string szSoilSampleData,
			string szSoilSampleType, string szPaddockName, string szUserName)
		{
			int iNumberOfSoilSampleRecords = ReturnNumberOfSoilSamples(szSoilSampleType, 
				szPaddockName, szUserName);
			if(iNumberOfSoilSampleRecords == 0)
			{
				InsertSoilSample(szSoilSampleDate, szSoilSampleData, szSoilSampleType, 
					szPaddockName, szUserName);
			}
			else
			{
				UpdateSoilSample(szSoilSampleDate, szSoilSampleData, szSoilSampleType, 
					szPaddockName, szUserName);
			}
		}	
		//---------------------------------------------------------------------
		//Returns the date of the on which the soil sample took place for the 
		//selected paddock
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksSoilSample(string szSoilSampleType, 
			string szPaddockName, string szUserName)
		{
			DataTable dtResults;

			string szSQL = "SELECT SoilSamples.SampleDate, SoilSamples.Data FROM SoilSampleTypes "+
				"INNER JOIN ((Users INNER JOIN Paddocks ON Users.ID = Paddocks.UserID) "+
				"INNER JOIN SoilSamples ON Paddocks.ID = SoilSamples.PaddockID) "+
				"ON SoilSampleTypes.ID = SoilSamples.SoilSampleTypeID "+
				"WHERE Paddocks.Name = '"+szPaddockName+"' "+
				"AND Users.UserName = '"+szUserName+"' "+
				"AND SoilSampleTypes.Type='"+szSoilSampleType+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}	
		//---------------------------------------------------------------------
		//Inserts a new Soil sampe  into the database
		//---------------------------------------------------------------------
		static private void InsertSoilSample(string szSoilSampleDate, string szSoilSampleData,
			string szSoilSampleType, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iSoilSampleTypeID = ReturnSoilSampleTypeID(szSoilSampleType);

			string szSQL = "INSERT INTO SoilSamples "+
				"(SampleDate, Data, PaddockID, SoilSampleTypeID) VALUES "+
				"('"+szSoilSampleDate+"', '"+szSoilSampleData+"', "+
				iPaddockID.ToString()+", "+iSoilSampleTypeID.ToString()+")";
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Updates an existing soil sample in the database
		//---------------------------------------------------------------------
		static private void UpdateSoilSample(string szSoilSampleDate, string szSoilSampleData,
			string szSoilSampleType, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iSoilSampleTypeID = ReturnSoilSampleTypeID(szSoilSampleType);

			string szSQL = "UPDATE SoilSamples SET "+
				"SampleDate = '"+szSoilSampleDate+"', "+
				"Data = '"+szSoilSampleData+"' "+
				"WHERE PaddockID = "+iPaddockID.ToString()+" "+
				"AND SoilSampleTypeID = "+iSoilSampleTypeID.ToString();
			RunSQLStatement(szSQL);		
		}	
		//---------------------------------------------------------------------
		//Returns the number of soil sample records in the database for the
		//selected paddock
		//---------------------------------------------------------------------
		static private int ReturnNumberOfSoilSamples(string szSoilSampleType, 
			string szPaddockName, string szUserName)
		{
			int iNumberOfSoilSamples = 0;

			string szSQL = "SELECT COUNT(SoilSamples.ID) AS NumberOfSoilSamples FROM SoilSampleTypes "+
				"INNER JOIN ((Users INNER JOIN Paddocks ON Users.ID = Paddocks.UserID) "+
				"INNER JOIN SoilSamples ON Paddocks.ID = SoilSamples.PaddockID) "+
				"ON SoilSampleTypes.ID = SoilSamples.SoilSampleTypeID "+
				"WHERE Paddocks.Name = '"+szPaddockName+"' "+
				"AND Users.UserName = '"+szUserName+"' "+
				"AND SoilSampleTypes.Type='"+szSoilSampleType+"'";
			iNumberOfSoilSamples = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfSoilSamples", szSQL).ToString());

			return iNumberOfSoilSamples;
		}	
		//---------------------------------------------------------------------
		//Deletes all the soilsample records for the selected paddock
		//---------------------------------------------------------------------
		static private void DeletePaddocksSoilSamples(int iPaddockID)
		{
			string szSQL = "DELETE FROM SoilSamples "+
				"WHERE PaddockID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns the SoilSampleTypeID for the specified SoilSampleType
		//---------------------------------------------------------------------
		static private int ReturnSoilSampleTypeID(string szSoilSampleType)
		{
			int iSoilSampleTypeID = 0;

			string szSQL = "SELECT ID FROM SoilSampleTypes "+
				"WHERE Type = '"+szSoilSampleType+"'";
			iSoilSampleTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iSoilSampleTypeID;
		}	
		//---------------------------------------------------------------------
		#endregion



		#region Functions to Manipulate Temporal Events

		//---------------------------------------------------------------------
		//Returns all dates and values of a selected temporal type for the 
		//selected paddock in the selected year.
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksTemporalEvents(string szPaddockName, string szUserName, 
			string szTemporalEventType, string szStartDate, string szEndDate)
		{	
			DataTable dtResults;
			string szSQL = "SELECT TemporalEvents.EventDate, TemporalEvents.EventValue FROM TemporalEventTypes "+
				"INNER JOIN ((Users INNER JOIN Paddocks ON Users.ID = Paddocks.UserID) "+
				"INNER JOIN TemporalEvents ON Paddocks.ID = TemporalEvents.PaddockID) "+
				"ON TemporalEventTypes.ID = TemporalEvents.TemporalEventTypeID "+
				"WHERE Users.UserName = '"+szUserName+"' AND Paddocks.Name = '"+szPaddockName+"' AND TemporalEventTypes.Type = '"+szTemporalEventType+"' "+
				"AND CDate(EventDate) >= CDate(#"+szStartDate+"#) AND CDate(EventDate) <= CDate(#"+szEndDate+"#)";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Returns the sum of the total temporal event values for a certain period
		//---------------------------------------------------------------------
		static public double GetPaddocksTotalTemporalEventsValues(string szPaddockName, string szUserName, 
			string szTemporalEventType, string szStartDate, string szEndDate)
		{	
			double dTotalTemporalEventValues = 0;

			string szSQL = "SELECT SUM(EventValue) AS TotalValues FROM TemporalEventTypes "+
				"INNER JOIN ((Users INNER JOIN Paddocks ON Users.ID = Paddocks.UserID) "+
				"INNER JOIN TemporalEvents ON Paddocks.ID = TemporalEvents.PaddockID) "+
				"ON TemporalEventTypes.ID = TemporalEvents.TemporalEventTypeID "+
				"WHERE Users.UserName = '"+szUserName+"' AND Paddocks.Name = '"+szPaddockName+"' AND TemporalEventTypes.Type = '"+szTemporalEventType+"' "+
				"AND CDate(EventDate) >= CDate(#"+szStartDate+"#) AND CDate(EventDate) <= CDate(#"+szEndDate+"#)";
			string szResult = ReturnSingleValueFromDB("TotalValues", szSQL).ToString();
			if(szResult != "")
			{
				dTotalTemporalEventValues = Convert.ToDouble(szResult);
			}
	
			return dTotalTemporalEventValues;
		}
		//---------------------------------------------------------------------
		//Inserts a new Temporal event application into the database
		//---------------------------------------------------------------------
		static public void InsertTemporalEvent(string szTemporalEventDate, double dTemporalEventValue,
			string szTemporalEventType, string szPaddockName, string szUserName)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iTemporalEventTypeID = ReturnTemporalEventTypeID(szTemporalEventType);

			string szSQL = "INSERT INTO TemporalEvents "+
				"(EventDate, EventValue, TemporalEventTypeID, PaddockID) VALUES "+
				"('"+szTemporalEventDate+"', "+dTemporalEventValue.ToString()+", "+
				iTemporalEventTypeID.ToString()+", "+iPaddockID.ToString()+")";
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Deletes a fertiliser application from the database
		//---------------------------------------------------------------------
		static public void DeletePaddocksTemporalEvents(string szPaddockName, string szUserName, 
			string szTemporalEventType, string szStartDate, string szEndDate)
		{
			int iPaddockID = ReturnPaddockID(szPaddockName, szUserName);
			int iTemporalEventTypeID = ReturnTemporalEventTypeID(szTemporalEventType);

			string szSQL = "DELETE FROM TemporalEvents "+
				"WHERE PaddockID = "+iPaddockID.ToString()+" "+
				"AND TemporalEventTypeID = "+iTemporalEventTypeID.ToString()+" "+
				"AND CDate(EventDate) >= CDate(#"+szStartDate+"#) AND CDate(EventDate) <= CDate(#"+szEndDate+"#)";
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//Deletes all the temporal events for the selected paddock
		//---------------------------------------------------------------------
		static private void DeletePaddocksTemporalEvents(int iPaddockID)
		{
			string szSQL = "DELETE FROM TemporalEvents "+
				"WHERE PaddockID = "+iPaddockID.ToString();
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Returns the ID of the selected Temporal Data type
		//---------------------------------------------------------------------
		static private int ReturnTemporalEventTypeID(string szTemporalEventType)
		{
			int iTemporalEventTypeID = 0;

			string szSQL = "SELECT ID FROM TemporalEventTypes "+
				"WHERE Type = '"+szTemporalEventType+"'";
			iTemporalEventTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iTemporalEventTypeID;
		}		
		//---------------------------------------------------------------------

		#endregion



		#region Functions to Manipulate Row Configuration Information

		//---------------------------------------------------------------------
		//Returns all SubSoilConstraint Types
		//---------------------------------------------------------------------
		static public DataTable GetAllRowConfigurationTypes()
		{
			DataTable dtResults;
			string szSQL = "SELECT Type FROM RowConfigurationTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private int ReturnRowConfigurationTypeID(string szRowConfigurationType)
		{
			int iRowConfigurationTypeID = 0;

			string szSQL = "SELECT ID FROM RowConfigurationTypes "+
				"WHERE Type = '"+szRowConfigurationType+"'";
			iRowConfigurationTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iRowConfigurationTypeID;
		}
		//---------------------------------------------------------------------

		#endregion
		


		#region Functions to manipulate region information
		//---------------------------------------------------------------------
		//Returns all Regions from the Database
		//---------------------------------------------------------------------
		static public DataTable GetAllRegions()
			{
			DataTable dtResults;
			string szSQL = "SELECT Regions.Type FROM Regions "+
				"ORDER BY Regions.Type";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns the ID of the specified Region
		//---------------------------------------------------------------------
		static private int ReturnRegionID(string szRegion)
			{
			int iRegionID = 0;

			string szSQL = "SELECT ID FROM Regions "+
				"WHERE Type = '"+szRegion+"'";
			iRegionID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iRegionID;
			}
		//-------------------------------------------------------------------------	
		#endregion
		


		#region Functions to manipulate climate forecast information
		//---------------------------------------------------------------------
		//Returns the climate forecast record
		//---------------------------------------------------------------------
		static public DataTable GetClimateForecast()
			{
			DataTable dtResults;
			string szSQL = "SELECT ClimateForecast.SoiMonth, SOIPhases.Type AS SOIPhase, "+
				"ClimateForecast.DavidsYearOne, ClimateForecast.DavidsYearTwo, "+
				"ClimateForecast.DavidsYearThree, ClimateForecast.DavidsYearFour, "+
				"ClimateForecast.DavidsYearFive, ClimateForecast.SoiDescription, "+
				"ClimateForecast.DavidsDescription "+
				"FROM ClimateForecast INNER JOIN SOIPhases "+
				"ON ClimateForecast.SoiPhaseID = SOIPhases.ID";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all SOI Phases
		//---------------------------------------------------------------------
		static public DataTable GetAllSOIPhases()
		{
			DataTable dtResults;
			string szSQL = "SELECT Type FROM SOIPhases ";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetClimateForecast(string szSOIMonth, string szSOIPhase, 
			string szDavidsYearOne, string szDavidsYearTwo, string szDavidsYearThree, 
			string szDavidsYearFour, string szDavidsYearFive, string szSOIDescription, 
			string szDavidsDescription)
			{
			int iNumberOfClimateForecasts = ReturnNumberOfClimateForecastRecords();
			
			if(iNumberOfClimateForecasts == 0)
				{
				InsertClimateForecast(szSOIMonth, szSOIPhase, 
					szDavidsYearOne, szDavidsYearTwo, szDavidsYearThree, 
					szDavidsYearFour, szDavidsYearFive, szSOIDescription, 
					szDavidsDescription);
				}
			else
				{
				UpdateClimateForecast(szSOIMonth, szSOIPhase, 
					szDavidsYearOne, szDavidsYearTwo, szDavidsYearThree, 
					szDavidsYearFour, szDavidsYearFive, szSOIDescription, 
					szDavidsDescription);
				}
			}
		//---------------------------------------------------------------------
		//updates the climate forecast record in the database
		//---------------------------------------------------------------------
		static private void UpdateClimateForecast(string szSOIMonth, string szSOIPhase, 
			string szDavidsYearOne, string szDavidsYearTwo, string szDavidsYearThree, 
			string szDavidsYearFour, string szDavidsYearFive, string szSOIDescription, 
			string szDavidsDescription)
			{
			int iSOIPhaseID = ReturnSOIPhaseID(szSOIPhase);
			string szSQL = "UPDATE ClimateForecast "+
				"SET SoiMonth = "+szSOIMonth+", SoiPhaseID = "+iSOIPhaseID.ToString()+", "+
				"DavidsYearOne = '"+szDavidsYearOne+"', DavidsYearTwo = '"+szDavidsYearTwo+"', "+
				"DavidsYearThree = '"+szDavidsYearThree+"', DavidsYearFour = '"+szDavidsYearFour+"', "+
				"DavidsYearFive = '"+szDavidsYearFive+"', SoiDescription = '"+szSOIDescription+"', "+
				"DavidsDescription = '"+szDavidsDescription+"'";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Saves a new climate forecast
		//This will only be run the first time that a climate forecast is entered
		//---------------------------------------------------------------------
		static private void InsertClimateForecast(string szSOIMonth, string szSOIPhase, 
			string szDavidsYearOne, string szDavidsYearTwo, string szDavidsYearThree, 
			string szDavidsYearFour, string szDavidsYearFive, string szSOIDescription, 
			string szDavidsDescription)
			{
			int iSOIPhaseID = ReturnSOIPhaseID(szSOIPhase);
			string szSQL = "INSERT INTO ClimateForecast "+
				"(SoiMonth, SoiPhaseID, DavidsYearOne, DavidsYearTwo, DavidsYearThree, "+
				"DavidsYearFour, DavidsYearFive, SoiDescription, DavidsDescription) VALUES "+
				"("+szSOIMonth+", "+iSOIPhaseID.ToString()+", '"+szDavidsYearOne+"', '"
				+szDavidsYearTwo+"', '"+szDavidsYearThree+"', '"+szDavidsYearFour+"', '"
				+szDavidsYearFive+"', '"+szSOIDescription+"', '"+szDavidsDescription+"')";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Returns the number of climate forecast records in the database
		//---------------------------------------------------------------------
		static private int ReturnNumberOfClimateForecastRecords()
			{
			int iNumberOfRecords = 0;

			string szSQL = "SELECT COUNT(ID) AS NumberOfRecords FROM ClimateForecast ";
			iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());

			return iNumberOfRecords;
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private int ReturnSOIPhaseID(string szSOIPhase)
		{
			int iSOIPhaseID = 0;

			string szSQL = "SELECT ID FROM SOIPhases "+
			"WHERE Type = '"+szSOIPhase+"'";
			iSOIPhaseID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());

			return iSOIPhaseID;
		}
			//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate report information

		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTypesForCropType(string szCropType)
			{
			DataTable dtResults;
			string szSQL = "SELECT ReportTypes.Type FROM (ReportTypes "+
				"INNER JOIN ReportTemplateMap ON ReportTypes.ID = ReportTemplateMap.ReportTypeID) "+
				"INNER JOIN CropTypes ON "+
				"ReportTemplateMap.CropTypeID = CropTypes.ID "+
				"WHERE CropTypes.Type = '"+szCropType+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTypes()
		{
			DataTable dtResults;
			string szSQL = "SELECT ReportTypes.Type FROM ReportTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Returns all the report template types from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTemplates()
			{
			DataTable dtResults;
			string szSQL = "SELECT Name, Template FROM ReportTemplates";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a template name and returns the matching template
		//---------------------------------------------------------------------
		static public string GetReportTemplate(string szTemplateName)
			{
			string szReportTemplate = "";

			string szSQL = "SELECT Template FROM ReportTemplates "+
				"WHERE Name = '"+szTemplateName+"'";

			szReportTemplate = ReturnSingleValueFromDB("Template", szSQL).ToString();
			if(szReportTemplate == "System.Object")
				{
				szReportTemplate = "";
				}
			return szReportTemplate;
			}
		//---------------------------------------------------------------------
		//Takes a ReportType ID and the template for that report type and updates
		//the name template of the selected report type
		//---------------------------------------------------------------------
		static public void UpdateReportTemplate(string szReportTemplate, string szTemplateName)
			{
			string szSQL = "UPDATE ReportTemplates "+
				"SET Template = '"+szReportTemplate+"' "+
				"WHERE Name = '"+szTemplateName+"'";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Takes a ReportTypeID and deletes the ReportType from the database
		//---------------------------------------------------------------------
		static public void DeleteReportTemplate(string szTemplateName)
			{
			string szSQL = "DELETE FROM ReportTemplates "+
				"WHERE Name = '"+szTemplateName+"'";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Saves a new Report Template into the database
		//---------------------------------------------------------------------
		static public void InsertReportTemplate(string szReportTemplate, string szTemplateName)
			{
			string szSQL = "INSERT INTO ReportTemplates "+
				"(Name, Template) VALUES "+
				"('"+szTemplateName+"', '"+szReportTemplate+"')";
			RunSQLStatement(szSQL);
			}
		//---------------------------------------------------------------------
		//Saves a new Report Template into the database
		//---------------------------------------------------------------------
		static public void SetReportTemplateMap(string szCropType, string szReportType,
			string szConParTemplateName, string szAPSIMTemplateName)
		{
			if(ReturnNumberOfReportTemplateMapEntries(szCropType, szReportType) == 0)
			{
				InsertReportTemplateMap(szCropType, szReportType, szConParTemplateName, 
					szAPSIMTemplateName);
			}
			else
			{
				UpdateReportTemplateMap(szCropType, szReportType, szConParTemplateName, 
					szAPSIMTemplateName);
			}
		}
		//---------------------------------------------------------------------
		//Returns the number of climate forecast records in the database
		//---------------------------------------------------------------------
		static public string GetAPSIMReportTemplateName(string szCropType, string szReportType)
		{
			string szTemplateName;

			string szSQL = "SELECT ReportTemplates.Name  FROM ((ReportTemplates "+
				"INNER JOIN ReportTemplateMap ON ReportTemplates.ID = ReportTemplateMap.ReportTemplateID) "+
				"INNER JOIN CropTypes ON ReportTemplateMap.CropTypeID = CropTypes.ID) "+
				"INNER JOIN ReportTypes ON ReportTemplateMap.ReportTypeID = ReportTypes.ID "+
				"WHERE ReportTypes.Type = '"+szReportType+"' "+
				"AND CropTypes.Type = '"+szCropType+"'";

			szTemplateName = ReturnSingleValueFromDB("Name", szSQL).ToString();
			if(szTemplateName == "System.Object")
			{
				szTemplateName = "";
			}
			return szTemplateName;
		}
		//---------------------------------------------------------------------
		//Returns the number of climate forecast records in the database
		//---------------------------------------------------------------------
		static public string GetConParReportTemplateName(string szCropType, string szReportType)
		{
			string szTemplateName;

			string szSQL = "SELECT ReportTemplates.Name  FROM ((ReportTemplates "+
				"INNER JOIN ReportTemplateMap ON ReportTemplates.ID = ReportTemplateMap.ConParTemplateID) "+
				"INNER JOIN CropTypes ON ReportTemplateMap.CropTypeID = CropTypes.ID) "+
				"INNER JOIN ReportTypes ON ReportTemplateMap.ReportTypeID = ReportTypes.ID "+
				"WHERE ReportTypes.Type = '"+szReportType+"' "+
				"AND CropTypes.Type = '"+szCropType+"'";

			szTemplateName = ReturnSingleValueFromDB("Name", szSQL).ToString();
			if(szTemplateName == "System.Object")
			{
				szTemplateName = "";
			}
			return szTemplateName;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void UpdateReportTemplateMap(string szCropType, string szReportType,
			string szConParTemplateName, string szAPSIMTemplateName)
		{
			int iCropTypeID = ReturnCropTypeID(szCropType);
			int iReportTypeID = ReturnReportTypeID(szReportType);
			int iConParTemplateID = ReturnReportTemplateID(szConParTemplateName);
			int iAPSIMTemplateID = ReturnReportTemplateID(szAPSIMTemplateName);

			string szSQL = "UPDATE ReportTemplateMap "+
				"SET ConParTemplateID = "+iConParTemplateID.ToString()+", "+
				"ReportTemplateID = "+iAPSIMTemplateID.ToString()+" "+
				"WHERE CropTypeID = "+iCropTypeID.ToString()+" "+
				"AND ReportTypeID = "+iReportTypeID.ToString();
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void InsertReportTemplateMap(string szCropType, string szReportType,
			string szConParTemplateName, string szAPSIMTemplateName)
		{
			int iCropTypeID = ReturnCropTypeID(szCropType);
			int iReportTypeID = ReturnReportTypeID(szReportType);
			int iConParTemplateID = ReturnReportTemplateID(szConParTemplateName);
			int iAPSIMTemplateID = ReturnReportTemplateID(szAPSIMTemplateName);

			string szSQL = "INSERT INTO ReportTemplateMap "+
				"(CropTypeID, ReportTypeID, ConParTemplateID, ReportTemplateID) VALUES "+
				"("+iCropTypeID.ToString()+", "+iReportTypeID.ToString()+", "+
				iConParTemplateID.ToString()+", "+iAPSIMTemplateID.ToString()+")";
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void DeleteReportTemplate(string szCropType, string szReportType)
		{
			int iCropTypeID = ReturnCropTypeID(szCropType);
			int iReportTypeID = ReturnReportTypeID(szReportType);

			string szSQL = "DELETE FROM ReportTemplateMap "+
				"WHERE CropTypeID = "+iCropTypeID.ToString()+" "+
				"AND ReportTypeID = "+iReportTypeID.ToString();
			RunSQLStatement(szSQL);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private int ReturnReportTypeID(string szReportType)
		{
			int iReportTypeID = 0;
			string szSQL = "SELECT ID FROM ReportTypes "+
				"WHERE Type = '"+szReportType+"'";
			iReportTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
			return iReportTypeID;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private int ReturnReportTemplateID(string szTemplateName)
		{
			int iReportTemplateID = 0;
			string szSQL = "SELECT ID FROM ReportTemplates "+
				"WHERE Name = '"+szTemplateName+"'";
			iReportTemplateID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
			return iReportTemplateID;
		}
		//---------------------------------------------------------------------
		//Returns the number of climate forecast records in the database
		//---------------------------------------------------------------------
		static private int ReturnNumberOfReportTemplateMapEntries(string szCropType, string szReportType)
		{
			int iNumberOfRecords = 0;

			string szSQL = "SELECT COUNT(ReportTemplateMap.ID) AS NumberOfRecords FROM (ReportTemplateMap "+
				"INNER JOIN CropTypes ON ReportTemplateMap.CropTypeID = CropTypes.ID) "+
				"INNER JOIN ReportTypes ON ReportTemplateMap.ReportTypeID = ReportTypes.ID "+
				"WHERE ReportTypes.Type = '"+szReportType+"' "+
				"AND CropTypes.Type = '"+szCropType+"'";
			iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());

			return iNumberOfRecords;
		}

		//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate drop down information

		//---------------------------------------------------------------------
		//Generic function which takes a table name and retuns Type and ID 
		//fields for all records
		//---------------------------------------------------------------------
		static public DataTable GetAllTypesAndIDsFromTable(string szTableName)
			{
			DataTable dtResults;
			string szSQL = "SELECT Type, ID FROM "+szTableName;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all drop down types from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllDropDownTypes()
			{
			DataTable dtResults;
			string szSQL = "SELECT Type, TableName FROM DropDownTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a dropdown type and deletes it from the database
		//---------------------------------------------------------------------
		static public void DeleteDropDownType(string szDropDownType)
			{
			try
				{
				string szSQL = "DELETE FROM DropDownTypes "+
					"WHERE Type = '"+szDropDownType+"'";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Takes a dropdown type and a table name and inserts a new dropdown type
		//into the DropDownTypes table
		//---------------------------------------------------------------------
		static public void InsertDropDownType(string szDropDownType, string szTableName)
			{
			try
				{
				string szSQL = "INSERT INTO DropDownTypes "+
					"(Type, TableName) "+
					"VALUES ('"+szDropDownType+"', '"+szTableName+"')";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Takes a dropdown value and a tableName and inserts a new record into
		//the specified table
		//---------------------------------------------------------------------
		static public void InsertDropDownValue(string szDropDownValue, string szTableName)
			{
			try
				{
				string szSQL = "INSERT INTO "+szTableName+" "+
					"(Type) VALUES ('"+szDropDownValue+"')";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		#endregion



		#region Generic functions
		//---------------------------------------------------------------------
		//Takes an ID and a table name and deletes the record from the 
		//specified table
		//---------------------------------------------------------------------
		static public void DeleteRecordFromTable(string szID, string szTableName)
			{
			try
				{
				string szSQL = "DELETE FROM "+szTableName+" "+
					"WHERE ID = "+szID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		#endregion


		
		#region Old MS SQL functions to access the database
		/*
		//---------------------------------------------------------------------
		//Initialises the connection and Command objects that will be used to
		//access the database
		//---------------------------------------------------------------------
		static private void ConnectToDatabase(ref SqlConnection dbConnection, ref SqlCommand dbCommand)
		{
			try
			{
				System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				string szConnectionString = Convert.ToString(settings["MsSqlDBConnection"]);
				dbConnection = new SqlConnection(szConnectionString);
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
		static private void ConnectToDatabase(ref SqlConnection dbConnection)
		{
			try
			{
				System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				string szConnectionString = Convert.ToString(settings["MsSqlDBConnection"]);
				dbConnection = new SqlConnection(szConnectionString);
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
			SqlConnection dbConnection = null;
			SqlCommand dbCommand = null;
			ConnectToDatabase(ref dbConnection, ref dbCommand);
			object obResult = new object();
			try
			{
				dbCommand.CommandText = szSQL;
				SqlDataReader dbDataReader = dbCommand.ExecuteReader();
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
			SqlConnection dbConnection = null;
			ConnectToDatabase(ref dbConnection);
			DataTable dtQueryResults = new DataTable();
			try
			{
				SqlDataAdapter daQueryAdapter = new  SqlDataAdapter(szSQL, dbConnection);
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
			SqlConnection dbConnection = null;
			SqlCommand dbCommand = null;
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
		//---------------------------------------------------------------------
		//Takes a populated and named datatable and a table name and inserts
		//all the records in the datatable into the specified table.
		//NOTE: the TableName property of the DataTable must match the 
		//string szTableName.
		//---------------------------------------------------------------------
		static public void InsertMulitpleRecords(DataTable dtTableToInsert, string szTableName)
		{
			SqlConnection dbConnection = null;
			ConnectToDatabase(ref dbConnection);
			try
			{	
				string szSQL = "SELECT * FROM "+szTableName;
				SqlDataAdapter daInsertTable = new  SqlDataAdapter(szSQL, dbConnection);
				SqlCommandBuilder cmdInsertTable = new SqlCommandBuilder(daInsertTable);
				daInsertTable.InsertCommand = cmdInsertTable.GetInsertCommand();
				daInsertTable.UpdateCommand = cmdInsertTable.GetUpdateCommand();
				daInsertTable.Update(dtTableToInsert);
				daInsertTable = null;
			}
			catch(Exception)
			{}
			dbConnection.Close();
		}
		*/
		#endregion
		
		
		
		#region Old MS SQL Login function
		/*
			//---------------------------------------------------------------------
		//Takes the username and password that was submitted at log in and
		//returns the UserID for that user if the login was successful
		//if the login details weren't correct it returns 0
		//The comparision is case sensitive (collate Latin1_General_CS_AS)
		//---------------------------------------------------------------------
		static public int AuthenticateUser(string szUserName, string szPassword)
			{
			int iUserID = 0;
			string szSalt = GetSaltValueOfUser(szUserName);
			try
				{
				string szSQL = "SELECT ID FROM Users "+
					"WHERE UserName = '"+szUserName+"' collate Latin1_General_CS_AS AND "+
					"Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"' ";
				iUserID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{
				}
			return iUserID;
			}
			*/
			#endregion
		

	}//END OF CLASS
	}//END OF NAMESPACE
