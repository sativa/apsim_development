using System;
using System.Data;
using System.Data.SqlClient;
using System.Data.OleDb;
using System.Web;

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
			try
				{
				System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				//string szConnectionString = Convert.ToString(settings["AccessDBConnection"]);
				string szCurrentLocation = HttpContext.Current.Server.MapPath("/YP/")+"Data";
				string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\yp2005.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
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
				System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				//string szConnectionString = Convert.ToString(settings["AccessDBConnection"]);
				string szCurrentLocation = HttpContext.Current.Server.MapPath("/YP/")+"Data";
				string szConnectionString = "Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Registry Path=;Jet OLEDB:Database Locking Mode=0;Jet OLEDB:Database Password=;Data Source=\""+szCurrentLocation+"\\yp2005.mdb\";Password=;Jet OLEDB:Engine Type=5;Jet OLEDB:Global Bulk Transactions=1;Provider=\"Microsoft.Jet.OLEDB.4.0\";Jet OLEDB:System database=;Jet OLEDB:SFP=False;Extended Properties=;Mode=Share Deny None;Jet OLEDB:New Database Password=;Jet OLEDB:Create System Database=False;Jet OLEDB:Don't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica Repair=False;User ID=Admin;Jet OLEDB:Encrypt Database=False";
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
		//---------------------------------------------------------------------
		//Takes a populated and named datatable and a table name and inserts
		//all the records in the datatable into the specified table.
		//NOTE: the TableName property of the DataTable must match the 
		//string szTableName.
		//---------------------------------------------------------------------
		static public void InsertMulitpleRecords(DataTable dtTableToInsert, string szTableName)
			{
			OleDbConnection dbConnection = null;
			ConnectToDatabase(ref dbConnection);
			try
				{	
				string szSQL = "SELECT * FROM "+szTableName;
				OleDbDataAdapter daInsertTable = new  OleDbDataAdapter(szSQL, dbConnection);
				OleDbCommandBuilder cmdInsertTable = new OleDbCommandBuilder(daInsertTable);
				daInsertTable.InsertCommand = cmdInsertTable.GetInsertCommand();
				daInsertTable.UpdateCommand = cmdInsertTable.GetUpdateCommand();
				daInsertTable.Update(dtTableToInsert);
				daInsertTable = null;
				}
			catch(Exception)
				{}
			dbConnection.Close();
			}
		//---------------------------------------------------------------------	
		#endregion



		#region Functions to manipulate user information 
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
					"WHERE UserName = '"+szUserName+"' AND "+
					"(StrComp(UserName, '"+szUserName+"', 0)=False) AND "+
					"Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"' ";
				iUserID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{
				}
			return iUserID;
			}
		//---------------------------------------------------------------------
		//Takes a UserID and returns the access type of that user
		//---------------------------------------------------------------------
		static public string GetAccessTypeOfUser(string szUserID)
			{
			string szAccessType = "";
			try
				{
				string szSQL = "SELECT Type FROM AccessTypes "+
					"INNER JOIN Users ON AccessTypes.ID = Users.AccessTypeID "+
					"WHERE Users.ID = "+szUserID;
				szAccessType = ReturnSingleValueFromDB("Type", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szAccessType;
			}
		//---------------------------------------------------------------------
		//Takes a grower's name and returns the ID of that grower
		//---------------------------------------------------------------------
		static public int GetIDOfGrower(string szName)
		{
			int iID = 0;
			try
			{
				string szSQL = "SELECT ID FROM Users "+
					"WHERE Name = '"+szName+"'";
				iID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iID;
		}
		//---------------------------------------------------------------------
		//Takes a username and returns the salt value of that user
		//---------------------------------------------------------------------
		static public string GetSaltValueOfUser(string szUserName)
		{
			string szSalt = "";
			try
			{
				string szSQL = "SELECT Salt FROM Users "+
					"WHERE UserName = '"+szUserName+"'";
				szSalt = ReturnSingleValueFromDB("Salt", szSQL).ToString();
			}
			catch(Exception)
			{}
			return szSalt;
		}
		//---------------------------------------------------------------------
		//Takes a UserID and returns the full name of that user
		//---------------------------------------------------------------------
		static public string GetNameOfUser(string szUserID)
			{
			string szName = "";
			try
				{
				string szSQL = "SELECT Name FROM Users "+
					"WHERE ID = "+szUserID;
				szName = ReturnSingleValueFromDB("Name", szSQL).ToString();
				}
			catch(Exception)
			{}
			return szName;
			}
		//---------------------------------------------------------------------
		//Takes a UserID and returns the email address of that user
		//---------------------------------------------------------------------
		static public string GetEmailOfUser(string szUserID)
			{
			string szEmail = "";
			try
				{
				string szSQL = "SELECT Email FROM Users "+
					"WHERE ID = "+szUserID;
				szEmail = ReturnSingleValueFromDB("Email", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szEmail;
			}
		//---------------------------------------------------------------------
		//Take a UserID and returns the User name of that user
		//---------------------------------------------------------------------
		static public string GetUserNameOfUser(string szUserID)
			{
			string szEmail = "";
			try
				{
				string szSQL = "SELECT UserName FROM Users "+
					"WHERE ID = "+szUserID;
				szEmail = ReturnSingleValueFromDB("UserName", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szEmail;
			}
		//---------------------------------------------------------------------
		//Takes a UserID and returns all the paddocks of that user
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksOfUser(string szUserID)
			{
			DataTable dtResults;
			string szSQL = "SELECT Paddocks.Name, Paddocks.ID FROM Paddocks "+
				"WHERE Paddocks.UserID = "+szUserID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Updates the name of the user whos UserID matches the passed in UserID
		//---------------------------------------------------------------------
		static public void SetNameOfUser(string szNameOfUser, string szUserID)
			{
			try
				{
				string szSQL = "UPDATE Users "+
					"SET Name = '"+szNameOfUser+"' "+
					"WHERE ID = "+szUserID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Updates the email address of the user whos UserID matches the passed in UserID
		//---------------------------------------------------------------------
		static public void SetEmailOfUser(string szEmailAddress, string szUserID)
			{
			try
				{
				string szSQL = "UPDATE Users "+
					"SET Email = '"+szEmailAddress+"' "+
					"WHERE ID = "+szUserID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Updates the password of the user whos UserID matches the passed in UserID
		//---------------------------------------------------------------------
		static public void SetPasswordOfUser(string szPassword, string szUserID)
			{
			string szSalt = FunctionsClass.CreateSalt();
			try
				{
				string szSQL = "UPDATE Users "+
					"SET Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"', "+
					"Salt = '"+szSalt+"' "+
					"WHERE ID = "+szUserID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Saves a new grower to the database and assigns them to the consultant
		//that added them to the database
		//---------------------------------------------------------------------
		static public void InsertGrower(string szName, string szEmail, string szUserName,
			string szPassword, string szConsultantID)
			{
			int iGrowerID = 0;
			int iAccessType = ReturnAccessTypeID(FunctionsClass.szGrower);
			string szSalt = FunctionsClass.CreateSalt();
			try
				{
				string szSQL = "INSERT INTO Users "+
					"(Name, Email, UserName, Salt, Pass, AccessTypeID) VALUES "+
					"('"+szName+"', '"+szEmail+"', '"+szUserName+"', '"+szSalt+"', '"+
					FunctionsClass.EncryptPassword(szPassword, szSalt)+"', "+iAccessType.ToString()+")";
				RunSQLStatement(szSQL);

				iGrowerID = AuthenticateUser(szUserName, szPassword);

				szSQL = "INSERT INTO ConsultantUserMap "+
					"(ConsultantID, UserID) VALUES "+
					"("+szConsultantID+", "+iGrowerID.ToString()+")";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
		}
		//---------------------------------------------------------------------
		//Saves a new user to the database
		//---------------------------------------------------------------------
		static public void InsertUser(string szName, string szEmail, string szUserName,
			string szPassword, string szAccessTypeID)
			{
			string szSalt = FunctionsClass.CreateSalt();
			try
				{
				string szSQL = "INSERT INTO Users "+
					"(Name, Email, UserName, Salt, Pass, AccessTypeID) VALUES "+
					"('"+szName+"', '"+szEmail+"', '"+szUserName+"', '"+szSalt+"', '"+
					FunctionsClass.EncryptPassword(szPassword, szSalt)+"', "+szAccessTypeID+")";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//updates an existing grower in the database
		//---------------------------------------------------------------------
		static public void UpdateGrower(string szName, string szEmail, string szUserName,
			string szPassword, string szUserID)
			{
			string szSalt = FunctionsClass.CreateSalt();
			try
				{
				string szSQL = "UPDATE Users "+
					"SET Name = '"+szName+"', Email = '"+szEmail+"', Salt = '"+szSalt+"', "+
					"UserName = '"+szUserName+"', Pass = '"+FunctionsClass.EncryptPassword(szPassword, szSalt)+"' "+
					"WHERE ID = "+szUserID;
				RunSQLStatement(szSQL);

				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Takes a UserID and deletes that user from the database
		//---------------------------------------------------------------------
		static public void DeleteUser(string szUserID)
			{
			try
				{	
				string szSQL = "DELETE FROM Users "+
					"WHERE ID = "+szUserID;
				RunSQLStatement(szSQL);
				
				szSQL = "DELETE FROM ConsultantUserMap "+
					"WHERE UserID = "+szUserID+" "+
					"OR ConsultantID = "+szUserID;
				RunSQLStatement(szSQL);

				DeleteUsersPaddocks(szUserID);
				}
			catch(Exception)
			{}
		}
		//---------------------------------------------------------------------
		//Takes a UserID of a consultant and returns all the growers assigned
		//to that consultant
		//---------------------------------------------------------------------
		static public DataTable GetGrowersOfConsultant(string szUserID)
			{
			DataTable dtResults;
			string szSQL = "SELECT Users.Name, Users.ID FROM Users "+
				"LEFT JOIN ConsultantUserMap ON Users.ID = ConsultantUserMap.UserID "+
				"WHERE ConsultantUserMap.ConsultantID = "+szUserID+" "+
				"OR Users.ID = "+szUserID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all users from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllUsers()
			{
			DataTable dtResults;
			string szSQL = "SELECT Users.Name, Users.ID FROM Users";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all consultants from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllConsultants()
			{
			int iAccessTypeID = ReturnAccessTypeID(FunctionsClass.szConsultant);
			DataTable dtResults;
			string szSQL = "SELECT Users.Name, Users.ID FROM Users "+
				"WHERE Users.AccessTypeID = "+iAccessTypeID.ToString();
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes an access type and returns the AccessTypeID of that access type 
		//---------------------------------------------------------------------
		static public int ReturnAccessTypeID(string szAccessType)
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
		//Checks to see if the selected user name is in use or not
		//Returns true if the username is available
		//---------------------------------------------------------------------
		static public bool IsUserNameAvailable(string szUserName)
			{
			int iNumberOfRecords = 0;
			bool bAvailable = false;
			try
				{
				string szSQL = "SELECT COUNT(ID) AS NumberOfRecords FROM Users "+
				"WHERE UserName ='"+szUserName+"'";
				iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
				if(iNumberOfRecords == 0)
					{
					bAvailable = true;
					}
				}
			catch(Exception)
				{}
			return bAvailable;
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void DeleteUsersPaddocks(string szUserID)
			{
			try
				{
				DataTable dtPaddocks = GetPaddocksOfUser(szUserID);
				foreach(DataRow drPaddock in dtPaddocks.Rows)
					{
					DeletePaddock(drPaddock["ID"].ToString());
					}
				}
			catch
				{}
			}
		//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate paddock information
		//---------------------------------------------------------------------
		//Takes a PaddockID and returns the UserID of that paddock
		//---------------------------------------------------------------------
		static public int GetUserIDOfPaddock(string szPaddockID)
			{
			int iUserID = 0;
			try
				{
				string szSQL = "SELECT UserID FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				iUserID = Convert.ToInt32(ReturnSingleValueFromDB("UserID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iUserID;
			}
		//---------------------------------------------------------------------
		//Takes a PaddockID and returns the UserID of that paddock
		//---------------------------------------------------------------------
		static public string GetNameOfPaddock(string szPaddockID)
			{
			string szName = "";
			try
				{
				string szSQL = "SELECT Name FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				szName = ReturnSingleValueFromDB("Name", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szName;
			}
		//---------------------------------------------------------------------
		//Takes a paddock name and a UserID and returns the Paddock ID of 
		//that paddock
		//---------------------------------------------------------------------
		static public int GetIDOfPaddock(string szPaddockName, string szUserID)
			{
			int iPaddockID = 0;
			try
				{
				string szSQL = "SELECT ID FROM Paddocks "+
					"WHERE Name = '"+szPaddockName+"' AND UserID = "+szUserID;
				iPaddockID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iPaddockID;
			}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the date of sowing for that paddock
		//---------------------------------------------------------------------
		static public string GetSowDateOfPaddock(string szPaddockID)
			{
			string szSowDate = "";
			try
				{
				string szSQL = "SELECT SowDate FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				szSowDate = ReturnSingleValueFromDB("SowDate", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szSowDate;
			}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the cultivar type ID for that paddock
		//---------------------------------------------------------------------
		static public int GetCultivarTypeIDOfPaddock(string szPaddockID)
			{
			int iCultivarTypeID = 0;
			try
				{
				string szSQL = "SELECT CultivarTypeID FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				iCultivarTypeID = Convert.ToInt32(ReturnSingleValueFromDB("CultivarTypeID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iCultivarTypeID;
			}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the cultivar type for that paddock
		//---------------------------------------------------------------------
		static public string GetCultivarTypeOfPaddock(string szPaddockID)
			{
			string szCultivarType = "";
			try 
				{
				string szSQL = "SELECT Type FROM CultivarTypes "+
					"INNER JOIN Paddocks ON CultivarTypes.ID = Paddocks.CultivarTypeID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				szCultivarType = Convert.ToString(ReturnSingleValueFromDB("Type", szSQL).ToString());
				}
			catch(Exception)
				{}
			return szCultivarType;
			}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the CropTypeID for that paddock
		//---------------------------------------------------------------------
		static public int GetCropTypeIDOfPaddock(string szPaddockID)
			{
			int iCropTypeID = 0;
			try
				{
				string szSQL = "SELECT CultivarTypes.CropTypeID FROM CultivarTypes "+
					"INNER JOIN Paddocks ON CultivarTypes.ID = Paddocks.CultivarTypeID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				iCropTypeID = Convert.ToInt32(ReturnSingleValueFromDB("CropTypeID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iCropTypeID;
			}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the CropType for that paddock
		//---------------------------------------------------------------------
		static public string GetCropTypeOfPaddock(string szPaddockID)
			{
			string szCropType = "";
			try
				{
				string szSQL = "SELECT CropTypes.Type FROM (CropTypes "+
					"INNER JOIN CultivarTypes ON CropTypes.ID = CultivarTypes.CropTypeID) "+
					"INNER JOIN Paddocks ON CultivarTypes.ID = Paddocks.CultivarTypeID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				szCropType = ReturnSingleValueFromDB("Type", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szCropType;
			}
		//-------------------------------------------------------------------------
		//Gets the linked temporal paddock ID for the selected paddock
		//Basically this is just the Paddock ID that holds this paddocks rainfall data
		//-------------------------------------------------------------------------
		static public int GetLinkedTemporalPaddockIDOfPaddock(string szPaddockID)
			{
			int iLinkedTemporalPaddockID = 0;
			try
				{
				string szSQL = "SELECT LinkedTemporalPaddockID FROM Paddocks "+
				"WHERE ID = "+szPaddockID;
				iLinkedTemporalPaddockID = Convert.ToInt32(ReturnSingleValueFromDB("LinkedTemporalPaddockID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iLinkedTemporalPaddockID;	
			}
		//---------------------------------------------------------------------
		//Returns a datatable with all the paddocks that are available for linking
		//---------------------------------------------------------------------
		static public DataTable GetAllPaddocksForTemporalLinking(string szPaddockID, string szUserID)
		{
			DataTable dtResults;
			string szSQL = "SELECT Name, ID FROM Paddocks "+
				"WHERE ID <> "+szPaddockID+" "+
				"AND UserID = "+szUserID+" "+
				"AND LinkedTemporalPaddockID = 0";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the RegionID for that paddock
		//---------------------------------------------------------------------
		static public int GetRegionIDOfPaddock(string szPaddockID)
		{
			int iRegionID = 0;
			try
			{
				string szSQL = "SELECT MetStations.RegionID FROM MetStations "+
					"INNER JOIN Paddocks ON MetStations.ID = Paddocks.MetStationID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				iRegionID = Convert.ToInt32(ReturnSingleValueFromDB("RegionID", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iRegionID;
		}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the SoilID  for that paddock
		//---------------------------------------------------------------------
		static public int GetSoilIDOfPaddock(string szPaddockID)
		{
			int iSoilID = 0;
			try
			{
				string szSQL = "SELECT SoilID FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				iSoilID = Convert.ToInt32(ReturnSingleValueFromDB("SoilID", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iSoilID;
		}
		//---------------------------------------------------------------------
		//Takes a PaddockID and returns the MetStationID for that paddock
		//---------------------------------------------------------------------
		static public int GetMetStationIDOfPaddock(string szPaddockID)
		{
			int iMetStationID = 0;
			try
			{
				string szSQL = "SELECT MetStationID FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				iMetStationID = Convert.ToInt32(ReturnSingleValueFromDB("MetStationID", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iMetStationID;
		}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the MetStation Name for that paddock
		//---------------------------------------------------------------------
		static public string GetMetStationNameOfPaddock(string szPaddockID)
		{
			string szMetStationsName = "";
			try 
			{
				string szSQL = "SELECT MetStations.Name FROM MetStations "+
					"INNER JOIN Paddocks ON MetStations.ID = Paddocks.MetStationID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				szMetStationsName = Convert.ToString(ReturnSingleValueFromDB("Name", szSQL).ToString());
			}
			catch(Exception)
			{}
			return szMetStationsName;
		}
		//---------------------------------------------------------------------
		//Takes a paddockID and returns the MetStation Number for that paddock
		//---------------------------------------------------------------------
		static public int GetMetStationNumberOfPaddock(string szPaddockID)
		{
			int iMetStationNumber = 0;
			try 
			{
				string szSQL = "SELECT StationNumber FROM MetStations "+
					"INNER JOIN Paddocks ON MetStations.ID = Paddocks.MetStationID "+
					"WHERE Paddocks.ID = "+szPaddockID;
				iMetStationNumber = Convert.ToInt32(ReturnSingleValueFromDB("StationNumber", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iMetStationNumber;
		}
		//---------------------------------------------------------------------
		//Takes a PaddockID and returns the SubSoilConstraintTypeID for that
		//paddock
		//---------------------------------------------------------------------
		static public int GetSubSoilConstraintTypeIDOfPaddock(string szPaddockID)
		{
			int iSubSoilConstraintTypeID = 0;
			try
			{
				string szSQL = "SELECT SubSoilConstraintTypeID FROM Paddocks "+
					"WHERE ID = "+szPaddockID;
				iSubSoilConstraintTypeID = Convert.ToInt32(ReturnSingleValueFromDB("SubSoilConstraintTypeID", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iSubSoilConstraintTypeID;
		}
		//---------------------------------------------------------------------
		//Saves a new paddock to the database
		//---------------------------------------------------------------------
		static public void InsertPaddock(string szName, string szSowDate, 
			string szCultivarTypeID, string szUserID)
		{
			try
			{
				string szSQL = "INSERT INTO Paddocks "+
					"(Name, SowDate, CultivarTypeID, UserID) VALUES "+
					"('"+szName+"', '"+szSowDate+"', "
					+szCultivarTypeID.ToString()+", "+szUserID+")";
				RunSQLStatement(szSQL);
			}
			catch(Exception)
			{}
		}
		//---------------------------------------------------------------------
		//updates an existing paddock cropping details in the database
		//---------------------------------------------------------------------
		static public void UpdatePaddockCrop(string szSowDate, string szCultivarTypeID,
			string szPaddockID)
		{
			try
			{
				string szSQL = "UPDATE Paddocks "+
					"SET SowDate = '"+szSowDate+"', CultivarTypeID = "+szCultivarTypeID+" "+
					"WHERE ID = "+szPaddockID;
				RunSQLStatement(szSQL);

			}
			catch(Exception)
			{}
		}
		//---------------------------------------------------------------------
		//Takes a PaddockID and deletes that paddock and all dependant data 
		//from the database
		//---------------------------------------------------------------------
		static public void DeletePaddock(string szPaddockID)
		{
			string szSQL = "DELETE FROM Paddocks "+
				"WHERE ID = "+szPaddockID;
			RunSQLStatement(szSQL);
			
			DeletePaddocksSoilSamples(szPaddockID);
			DeletePaddocksTemporalEvents(szPaddockID);
			DeletePaddocksFertiliserAppliations(szPaddockID);
		}
		//---------------------------------------------------------------------
		//updates an existing paddock settings in the database
		//---------------------------------------------------------------------
		static public void UpdatePaddockSettings(string szMetStaionID, string szSoilID,
			string szSubSoilConstraintTypeID, string szLinkedTemporalPaddockID, string szPaddockID)
		{
			try
			{
				string szSQL = "UPDATE Paddocks "+
					"SET MetStationID = "+szMetStaionID+", SoilID = "+szSoilID+", "+
					"SubSoilConstraintTypeID = "+szSubSoilConstraintTypeID+", "+
					"LinkedTemporalPaddockID = "+szLinkedTemporalPaddockID+" "+
					"WHERE ID = "+szPaddockID;
				RunSQLStatement(szSQL);
			}
			catch(Exception)
			{}
		}
		//---------------------------------------------------------------------
		//Returns all the crops from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllCrops()
			{
			DataTable dtResults;
			string szSQL = "SELECT CropTypes.Type, CropTypes.ID FROM CropTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a CropID and returns all cultivars of that crop
		//---------------------------------------------------------------------
		static public DataTable GetAllCultivarsOfCrop(string szCropID)
			{
			DataTable dtResults;
			string szSQL = "SELECT CultivarTypes.Type, CultivarTypes.ID FROM CultivarTypes "+
				"WHERE CultivarTypes.CropTypeID = "+szCropID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Return sall SubSoilConstraint Types
		//---------------------------------------------------------------------
		static public DataTable GetAllSubSoilConstraintTypes()
			{
			DataTable dtResults;
			string szSQL = "SELECT Type, ID FROM SubSoilConstraintTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a CultivarID and deletes that Cultivar from the database
		//---------------------------------------------------------------------
		static public void DeleteCulivar(string szCultivarID)
			{
			try
				{
				string szSQL = "DELETE FROM CultivarTypes "+
					"WHERE ID = "+szCultivarID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Updates an existing Fertiliser application in the database
		//---------------------------------------------------------------------
		static public void UpdateFertliserApplication(string szApplicationDate, string szApplicationRate,
			string szFertiliserTypeID, string szFertiliserApplicationID)
			{
			try
				{
				string szSQL = "UPDATE FertiliserApplication SET "+
					"ApplicationDate = '"+szApplicationDate+"', "+
					"Rate = "+szApplicationRate+", "+
					"FertiliserTypeID = "+szFertiliserTypeID+" "+
					"WHERE ID = "+szFertiliserApplicationID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Inserts a new Fertiliser application into the database
		//---------------------------------------------------------------------
		static public void InsertFertliserApplication(string szApplicationDate, string szApplicationRate,
			string szFertiliserTypeID, string szPaddockID)
			{
			try
				{
				string szSQL = "INSERT INTO FertiliserApplication "+
					"(ApplicationDate, Rate, FertiliserTypeID, PaddockID) VALUES "+
					"('"+szApplicationDate+"', "+szApplicationRate+", "+
					szFertiliserTypeID+", "+szPaddockID+")";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Returns all the fertiliser applications of a selected fertiliser type 
		//for a selected paddock
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksFertiliserApplications(string szPaddockID, string szFertiliserTypeID)
			{
			DataTable dtResults;
			string szSQL = "SELECT ID, Rate, ApplicationDate FROM FertiliserApplication "+
				"WHERE PaddockID = "+szPaddockID+" AND FertiliserTypeID = "+szFertiliserTypeID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Deletes a fertiliser application from the database
		//---------------------------------------------------------------------
		static public void DeleteFertiliserApplication(string szFertiliserApplicationID)
			{
			try
				{
				string szSQL = "DELETE FROM FertiliserApplication "+
					"WHERE ID = "+szFertiliserApplicationID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Returns the ID of a selected Fertiliser type
		//---------------------------------------------------------------------
		static public int GetFertiliserTypeIDOfFertiliserType(string szFertiliserType)
			{
			int iFertiliserTypeID = 0;
			try
				{
				string szSQL = "SELECT ID FROM FertiliserTypes "+
					"WHERE Type = '"+szFertiliserType+"'";
				iFertiliserTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iFertiliserTypeID;
			}
		//---------------------------------------------------------------------
		//Returns all dates and values of a selected temporal type for the 
		//selected paddock.
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksTemporalEvents(string szPaddockID, string szTemporalEventTypeID)
			{	
			DataTable dtResults;
			string szSQL = "SELECT EventDate, EventValue FROM TemporalEvents "+
				"WHERE PaddockID = "+szPaddockID+" AND TemporalEventTypeID = "+szTemporalEventTypeID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all dates and values of a selected temporal type for the 
		//selected paddock in the selected year.
		//---------------------------------------------------------------------
		static public DataTable GetPaddocksTemporalEventsInYear(string szPaddockID, string szTemporalEventTypeID, string szYear)
			{	
			string szEndYear = (Convert.ToInt32(szYear)+1).ToString();
			DataTable dtResults;
			string szSQL = "SELECT EventDate, EventValue FROM TemporalEvents "+
				"WHERE PaddockID = "+szPaddockID+" AND TemporalEventTypeID = "+szTemporalEventTypeID+" "+
				"AND CDate(EventDate) >= #1/1/"+szYear+"# AND CDate(EventDate) < #1/1/"+szEndYear+"#";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public double GetPaddocksTotalTemporalEventsValues(string szPaddockID, string szTemporalEventTypeID, 
			string szStartDate, string szEndDate)
			{	
			double dTotalTemporalEventValues = 0;
			try
				{
				string szSQL = "SELECT SUM(EventValue) AS TotalValues FROM TemporalEvents "+
					"WHERE PaddockID = "+szPaddockID+" AND TemporalEventTypeID = "+szTemporalEventTypeID+" "+
					"AND CDate(EventDate) >= #"+szStartDate+"# AND CDate(EventDate) <= #"+szEndDate+"#";
				string szResult = ReturnSingleValueFromDB("TotalValues", szSQL).ToString();
				if(szResult != "")
					{
					dTotalTemporalEventValues = Convert.ToDouble(szResult);
					}
				}
			catch(Exception)
				{}
			return dTotalTemporalEventValues;
			}
		//---------------------------------------------------------------------
		//Returns the ID of the selected Temporal Data type
		//---------------------------------------------------------------------
		static public int GetTemporalEventTypeIDOfTemporalEventType(string szTemporalEventType)
			{
			int iTemporalEventTypeID = 0;
			try
				{
				string szSQL = "SELECT ID FROM TemporalEventTypes "+
					"WHERE Type = '"+szTemporalEventType+"'";
				iTemporalEventTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iTemporalEventTypeID;
			}		
		//---------------------------------------------------------------------
		//Inserts a new Temporal event application into the database
		//---------------------------------------------------------------------
		static public void InsertTemporalEvent(string szTemporalEventDate, string szTemporalEventValue,
			string szTemporalEventTypeID, string szPaddockID)
			{
			try
				{
				string szSQL = "INSERT INTO TemporalEvents "+
					"(EventDate, EventValue, TemporalEventTypeID, PaddockID) VALUES "+
					"('"+szTemporalEventDate+"', "+szTemporalEventValue+", "+
					szTemporalEventTypeID+", "+szPaddockID+")";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Deletes a fertiliser application from the database
		//---------------------------------------------------------------------
		static public void DeleteAllPaddocksRainfallEventsInYear(string szPaddockID, string szTemporalEventTypeID, string szYear)
			{
			string szEndYear = (Convert.ToInt32(szYear)+1).ToString();
			try
				{
				string szSQL = "DELETE FROM TemporalEvents "+
					"WHERE PaddockID = "+szPaddockID+" "+
					"AND TemporalEventTypeID = "+szTemporalEventTypeID+" "+
					"AND CDate(EventDate) >= #1/1/"+szYear+"# AND CDate(EventDate) < #1/1/"+szEndYear+"#";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Inserts a new Soil sampe  into the database
		//---------------------------------------------------------------------
		static public void InsertSoilSample(string szSoilSampleDate, string szSoilSampleData,
			string szPaddockID, string szSoilSampleTypeID)
			{
			try
				{
				string szSQL = "INSERT INTO SoilSamples "+
					"(SampleDate, Data, PaddockID, SoilSampleTypeID) VALUES "+
					"('"+szSoilSampleDate+"', '"+szSoilSampleData+"', "+
					szPaddockID+", "+szSoilSampleTypeID+")";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Updates an existing soil sample in the database
		//---------------------------------------------------------------------
		static public void UpdateSoilSample(string szSoilSampleDate, string szSoilSampleData,
			string szPaddockID, string szSoilSampleTypeID)
			{
			try
				{
				string szSQL = "UPDATE SoilSamples SET "+
					"SampleDate = '"+szSoilSampleDate+"', "+
					"Data = '"+szSoilSampleData+"' "+
					"WHERE PaddockID = "+szPaddockID+" "+
					"AND SoilSampleTypeID = "+szSoilSampleTypeID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Returns the number of soil sample records in the database for the
		//selected paddock
		//---------------------------------------------------------------------
		static public int ReturnNumberOfSoilSamples(string szPaddockID, string szSoilSampleType)
		{
			int iNumberOfSoilSamples = 0;
			try
			{
				string szSQL = "SELECT COUNT(SoilSamples.ID) AS NumberOfSoilSamples FROM SoilSamples "+
					"INNER JOIN SoilSampleTypes ON SoilSamples.SoilSampleTypeID = SoilSampleTypes.ID "+
					"WHERE SoilSamples.PaddockID = "+szPaddockID+" "+
					"AND SoilSampleTypes.Type = '"+szSoilSampleType+"'";
				iNumberOfSoilSamples = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfSoilSamples", szSQL).ToString());
			}
			catch(Exception)
			{}
			return iNumberOfSoilSamples;
		}	
		//---------------------------------------------------------------------
		//Returns the date of the on which the soil sample took place for the 
		//selected paddock
		//---------------------------------------------------------------------
		static public string GetPaddocksSoilSampleDate(string szPaddockID, string szSoilSampleType)
			{
			string szSoilDate = "";
			try
				{
				string szSQL = "SELECT SoilSamples.SampleDate FROM SoilSamples "+
					"INNER JOIN SoilSampleTypes ON SoilSamples.SoilSampleTypeID = SoilSampleTypes.ID "+
					"WHERE SoilSamples.PaddockID = "+szPaddockID+" "+
					"AND SoilSampleTypes.Type = '"+szSoilSampleType+"'";
				szSoilDate = ReturnSingleValueFromDB("SampleDate", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szSoilDate;
			}	
		//---------------------------------------------------------------------
		//Returns the soil sample xml data for the selected paddock
		//---------------------------------------------------------------------
		static public string GetPaddocksSoilSampleData(string szPaddockID, string szSoilSampleType)
			{
			string szSoilData = "";
			try
				{
				string szSQL = "SELECT Data FROM SoilSamples "+
					"INNER JOIN SoilSampleTypes ON SoilSamples.SoilSampleTypeID = SoilSampleTypes.ID "+
					"WHERE SoilSamples.PaddockID = "+szPaddockID+" "+
					"AND SoilSampleTypes.Type = '"+szSoilSampleType+"'";
				szSoilData = ReturnSingleValueFromDB("Data", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szSoilData;
			}	
		//---------------------------------------------------------------------
		//Returns the SoilSampleTypeID for the specified SoilSampleType
		//---------------------------------------------------------------------
		static public int GetSoilSampleTypeID(string szSoilSampleType)
			{
			int iSoilSampleTypeID = 0;
			try
				{
				string szSQL = "SELECT ID FROM SoilSampleTypes "+
					"WHERE Type = '"+szSoilSampleType+"'";
				iSoilSampleTypeID = Convert.ToInt32(ReturnSingleValueFromDB("ID", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iSoilSampleTypeID;
			}	
		//---------------------------------------------------------------------
		//Deletes all the soilsample records for the selected paddock
		//---------------------------------------------------------------------
		static public void DeletePaddocksSoilSamples(string szPaddockID)
			{
			string szSQL = "DELETE * FROM SoilSamples "+
				"WHERE PaddockID = "+szPaddockID;
			RunSQLStatement(szSQL);
			}	
		//---------------------------------------------------------------------
		//Deletes all the fertiliser application records for the selected paddock
		//---------------------------------------------------------------------
		static public void DeletePaddocksFertiliserAppliations(string szPaddockID)
		{
			string szSQL = "DELETE * FROM FertiliserApplication "+
				"WHERE PaddockID = "+szPaddockID;
			RunSQLStatement(szSQL);
		}	
		//---------------------------------------------------------------------
		//Deletes all the temporal events for the selected paddock
		//---------------------------------------------------------------------
		static public void DeletePaddocksTemporalEvents(string szPaddockID)
		{
			string szSQL = "DELETE * FROM TemporalEvents "+
				"WHERE PaddockID = "+szPaddockID;
			RunSQLStatement(szSQL);
		}	
		//-------------------------------------------------------------------------

		#endregion



		#region Functions to manipulate region information
		//---------------------------------------------------------------------
		//Returns all Regions from the Database
		//---------------------------------------------------------------------
		static public DataTable GetAllRegions()
			{
			DataTable dtResults;
			string szSQL = "SELECT Regions.Type, Regions.ID FROM Regions";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Generic function that takes a Region ID and a table Name and returns
		//all records from that table which belong to the specified region.
		//---------------------------------------------------------------------
		static public DataTable GetAllNamesFromRegion(string szRegionID, string szTableName)
			{
			DataTable dtResults;
			string szSQL = "SELECT Name, ID FROM "+szTableName+" "+
				"WHERE RegionID ="+szRegionID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a RegionID and returns all the MetStations for that region
		//---------------------------------------------------------------------
		static public DataTable GetMetStationsOfRegion(string szRegionID)
			{
			DataTable dtResults;
			string szSQL = "SELECT MetStations.Name, MetStations.ID FROM MetStations "+
				"WHERE MetStations.RegionID = "+szRegionID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns the whole complete met station list for the selected region
		//---------------------------------------------------------------------
		static public DataTable GetCompleteMetStationsTable(string szRegionID)
		{
			DataTable dtResults;
			string szSQL = "SELECT * FROM MetStations "+
				"WHERE MetStations.RegionID = "+szRegionID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//Takes a RegionID and a UserID and returns all the Soils for that region
		//and that and that user
		//---------------------------------------------------------------------
		static public DataTable GetSoilsOfRegion(string szRegionID, string szUserID)
			{
			DataTable dtResults;
			string szSQL = "SELECT Soils.Name, Soils.ID FROM Soils "+
				"WHERE (Soils.RegionID = "+szRegionID+" AND Soils.SpecificUserID = 0) "+
				"OR Soils.SpecificUserID = "+szUserID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all soil data from a selected region
		//---------------------------------------------------------------------
		static public DataTable GetCompleteSoilsTable(string szRegionID)
			{
			DataTable dtResults;
			string szSQL = "SELECT * FROM Soils "+
				"WHERE Soils.RegionID = "+szRegionID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns the selected soils data (stored in xml format)
		//---------------------------------------------------------------------
		static public string GetSoilData(string szSoilID)
			{
			string szSoilData = "";
			try
			{
				string szSQL = "SELECT Data FROM Soils "+
					"WHERE ID = "+szSoilID;
				szSoilData = ReturnSingleValueFromDB("Data", szSQL).ToString();
			}
			catch(Exception)
			{}
			return szSoilData;
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
			string szSQL = "SELECT * FROM ClimateForecast ";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all SOI Phases
		//---------------------------------------------------------------------
		static public DataTable GetAllSOIPhases()
		{
			DataTable dtResults;
			string szSQL = "SELECT ID, Type FROM SOIPhases ";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
		}
		//---------------------------------------------------------------------
		//updates the climate forecast record in the database
		//---------------------------------------------------------------------
		static public void UpdateClimateForecast(string szSOIMonth, string szSOIPhase, 
			string szDavidsYearOne, string szDavidsYearTwo, string szDavidsYearThree, 
			string szDavidsYearFour, string szDavidsYearFive, string szSOIDescription, 
			string szDavidsDescription)
			{
			try
				{
				string szSQL = "UPDATE ClimateForecast "+
					"SET SoiMonth = "+szSOIMonth+", SoiPhase = "+szSOIPhase+", "+
					"DavidsYearOne = '"+szDavidsYearOne+"', DavidsYearTwo = '"+szDavidsYearTwo+"', "+
					"DavidsYearThree = '"+szDavidsYearThree+"', DavidsYearFour = '"+szDavidsYearFour+"', "+
					"DavidsYearFive = '"+szDavidsYearFive+"', SoiDescription = '"+szSOIDescription+"', "+
					"DavidsDescription = '"+szDavidsDescription+"'";
				RunSQLStatement(szSQL);

				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Saves a new climate forecast
		//This will only be run the first time that a climate forecast is entered
		//---------------------------------------------------------------------
		static public void InsertClimateForecast(string szSOIMonth, string szSOIPhase, 
			string szDavidsYearOne, string szDavidsYearTwo, string szDavidsYearThree, 
			string szDavidsYearFour, string szDavidsYearFive, string szSOIDescription, 
			string szDavidsDescription)
			{
			try
				{
				string szSQL = "INSERT INTO ClimateForecast "+
					"(SoiMonth, SoiPhase, DavidsYearOne, DavidsYearTwo, DavidsYearThree, "+
					"DavidsYearFour, DavidsYearFive, SoiDescription, DavidsDescription) VALUES "+
					"("+szSOIMonth+", "+szSOIPhase+", '"+szDavidsYearOne+"', '"
					+szDavidsYearTwo+"', '"+szDavidsYearThree+"', '"+szDavidsYearFour+"', '"
					+szDavidsYearFive+"', '"+szSOIDescription+"', '"+szDavidsDescription+"')";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Returns the number of climate forecast records in the database
		//---------------------------------------------------------------------
		static public int GetNumberOfClimateForecastRecords()
			{
			int iNumberOfRecords = 0;
			try
				{
				string szSQL = "SELECT COUNT(ID) AS NumberOfRecords FROM ClimateForecast ";
				iNumberOfRecords = Convert.ToInt32(ReturnSingleValueFromDB("NumberOfRecords", szSQL).ToString());
				}
			catch(Exception)
				{}
			return iNumberOfRecords;
			}
		//---------------------------------------------------------------------
		//Gets the SOI Phase of the selected SOIPhase ID
		//---------------------------------------------------------------------
		static public string GetSOIPhase(string szSOIPhaseID)
			{
			string szSOIPhase = "";
			try
				{
				string szSQL = "SELECT Type FROM SOIPhases "+
				"WHERE ID = "+szSOIPhaseID;
				szSOIPhase = ReturnSingleValueFromDB("Type", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szSOIPhase;
			}
			//---------------------------------------------------------------------
		#endregion



		#region Functions to manipulate report information

		//---------------------------------------------------------------------
		//Takes a ReportTemplateTypeID and returns all the report types associated
		//with that template type
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTypesOfTemplateType(string szTemplateTypeID)
			{
			DataTable dtResults;
			string szSQL = "SELECT ReportTypes.Type, ReportTypes.ID FROM ReportTypes "+
				"WHERE ReportTypes.ReportTemplateTypeID = "+szTemplateTypeID;
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a ReportTemplateTypeID and returns all the report types associated
		//with that template type
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTypes(string szTemplateType)
			{
			DataTable dtResults;
			string szSQL = "SELECT ReportTypes.Type, ReportTypes.ID FROM ReportTypes "+
				"INNER JOIN ReportTemplateTypes "+
				"ON ReportTypes.ReportTemplateTypeID = ReportTemplateTypes.ID "+
				"WHERE ReportTemplateTypes.Type = '"+szTemplateType+"'";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Returns all the report template types from the database
		//---------------------------------------------------------------------
		static public DataTable GetAllReportTemplateTypes()
			{
			DataTable dtResults;
			string szSQL = "SELECT Type, ID FROM ReportTemplateTypes";
			dtResults = ReturnMultipleValuesFromDB(szSQL);
			return dtResults;
			}
		//---------------------------------------------------------------------
		//Takes a ReportTypeID and returns the template of that report type
		//---------------------------------------------------------------------
		static public string GetReportTypeTemplate(string szReportTypeID)
			{
			string szReportTypeTemplate = "";
			try
				{
				string szSQL = "SELECT Template FROM ReportTypes "+
					"WHERE ID = "+szReportTypeID;
				szReportTypeTemplate = ReturnSingleValueFromDB("Template", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szReportTypeTemplate;
			}
		//---------------------------------------------------------------------
		//Returns the other report template for the selected report type
		//For example, if you pass in the ID for the the Apsimreport type of 
		//the Climate report it will return the template of the conpar type
		//of the Climate report
		//---------------------------------------------------------------------
		static public string GetReportTypeTemplateSecondary(string szPrimaryReportTypeID)
		{
			string szReportTypeTemplate = "";
			string szReportType = "";
			try
				{
				string szSQL = "SELECT Type FROM ReportTypes "+
					"WHERE ID = "+szPrimaryReportTypeID;
				szReportType = ReturnSingleValueFromDB("Type", szSQL).ToString();
				
				szSQL = "SELECT Template FROM ReportTypes "+
					"WHERE Type = '"+szReportType+"' AND "+
					"ID <> "+szPrimaryReportTypeID;
				szReportTypeTemplate = ReturnSingleValueFromDB("Template", szSQL).ToString();
				}
			catch(Exception)
				{}
			return szReportTypeTemplate;
			}
		//---------------------------------------------------------------------
		//Takes a ReportType ID and the template for that report type and updates
		//the name template of the selected report type
		//---------------------------------------------------------------------
		static public void UpdateReportTypes(string szReportTypeID, string szTemplate)
			{
			try
				{
				string szSQL = "UPDATE ReportTypes "+
					"SET Template = '"+szTemplate+"' "+
					"WHERE ID = "+szReportTypeID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Takes a ReportTypeID and deletes the ReportType from the database
		//---------------------------------------------------------------------
		static public void DeleteReportType(string szReportTypeID)
			{
			try
				{
				string szSQL = "DELETE FROM ReportTypes "+
					"WHERE ID = "+szReportTypeID;
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
			}
		//---------------------------------------------------------------------
		//Saves a new Report Type into the database
		//---------------------------------------------------------------------
		static public void InsertReportType(string szTemplateTypeID, string szReportType)
			{
			try
				{
				string szSQL = "INSERT INTO ReportTypes "+
					"(Type, ReportTemplateTypeID, Template) VALUES "+
					"('"+szReportType+"', "+szTemplateTypeID+", '')";
				RunSQLStatement(szSQL);
				}
			catch(Exception)
				{}
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
		//Take a table name and deletes the all the records from the 
		//specified table
		//---------------------------------------------------------------------
		static public void DeleteAllRecordsFromTable(string szTableName)
			{
			try
				{
				string szSQL = "DELETE FROM "+szTableName;
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
