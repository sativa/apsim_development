using System;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Data;
using VBGeneral;
using CSGeneral;

namespace YieldProphet
{
	/// <summary>
	/// Summary description for FunctionsClass.
	/// </summary>
	public class FunctionsClass
	{

		public const string szAdministrator = "administrator";
		public const string szGrower = "grower";
		public const string szConsultant = "consultant";
		public const string szVisitorConsultant = "consultant visitor";
		public const string szVisitor = "visitor";
		

		public FunctionsClass()
			{}


		#region Functions controlling access priviledges
		//-------------------------------------------------------------------------
		//Checks to see if the user is a visitor
		//-------------------------------------------------------------------------
		public static bool IsVisitor(string szUserName)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bVisitor = false;
			if(szAccessType == szVisitor)
				{
				bVisitor = true;
				}
			return bVisitor;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user has visitor priviledges or greater
		//-------------------------------------------------------------------------
		public static bool IsVisitorOrHigher(string szUserName)
		{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bVisitor = false;
			if(szAccessType == szAdministrator || szAccessType == szGrower ||
				szAccessType == szConsultant || szAccessType == szVisitorConsultant || 
				szAccessType == szVisitor)
			{
				bVisitor = true;
			}
			return bVisitor;
		}
		//-------------------------------------------------------------------------
		//Checks to see if the user has grower priviledges or greater
		//-------------------------------------------------------------------------
		public static bool IsGrowerOrHigher(string szUserName)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bGrower = false;
			if(szAccessType == szAdministrator || szAccessType == szGrower ||
				szAccessType == szConsultant)
				{
				bGrower = true;
				}
			return bGrower;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user has consultant priviledges or greater
		//-------------------------------------------------------------------------
		public static bool IsConsultantOrHigher(string szUserName)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bConsultant = false;
			if(szAccessType == szAdministrator || szAccessType == szConsultant)
				{
				bConsultant = true;
				}
			return bConsultant;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user has visitor constultant greater
		//-------------------------------------------------------------------------
		public static bool IsVisitorConsultant(string szUserName)
		{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bVisitorConsultant = false;
			if(szAccessType == szVisitorConsultant)
			{
				bVisitorConsultant = true;
			}
			return bVisitorConsultant;
		}
		//-------------------------------------------------------------------------
		//Checks to see if the user has consultant priviledges
		//-------------------------------------------------------------------------
		public static bool IsConsultant(string szUserName)
		{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bConsultant = false;
			if(szAccessType == szConsultant)
			{
				bConsultant = true;
			}
			return bConsultant;
		}
		//-------------------------------------------------------------------------
		//Checks to see if the user is an administrator
		//-------------------------------------------------------------------------
		public static bool IsAdministrator(string szUserName)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserName);
			bool bAdministrator = false;
			if(szAccessType == szAdministrator)
				{
				bAdministrator = true;
				}
			return bAdministrator;
			}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't an administrator
		//This is used to stop users from accessing pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForAdministratorLevelPriviledges()
		{
			if(IsAdministrator(HttpContext.Current.Session["UserName"].ToString()) == false)
			{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
			}
		}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't a consultant
		//This is used to stop users from accessing pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForConsultantLevelPriviledges()
			{
			if(IsConsultantOrHigher(HttpContext.Current.Session["UserName"].ToString()) == false)
				{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't a grower
		//This is used to stop users from accessings pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForGrowerLevelPriviledges()
			{
			if(IsGrowerOrHigher(HttpContext.Current.Session["UserName"].ToString()) == false)
				{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't a visitor consultant, consultant or Adminstrator
		//This is used to stop users from accessing pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForVisitorConsultantLevelPriviledges()
		{
			if(IsConsultantOrHigher(HttpContext.Current.Session["UserName"].ToString()) == false &&
				IsVisitorConsultant(HttpContext.Current.Session["UserName"].ToString()) == false)
			{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
			}
		}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't a visitor
		//This is used to stop users from accessing pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForVisitorLevelPriviledges()
		{
			if(IsVisitorOrHigher(HttpContext.Current.Session["UserName"].ToString()) == false)
			{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
			}
		}
		#endregion


		#region Generic functions
		//-------------------------------------------------------------------------
		//Checks that a session exists
		//-------------------------------------------------------------------------
		public static void CheckSession()
			{
			if(HttpContext.Current.Session== null || HttpContext.Current.Session["UserName"].ToString() == "0")
				{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
				}
			}
		//-------------------------------------------------------------------------
		//Encrypts the password for storage in the database
		//-------------------------------------------------------------------------
		public static string EncryptPassword(string szPassword, string szSalt)
			{
			szPassword = 
				System.Web.Security.FormsAuthentication.HashPasswordForStoringInConfigFile
				(szPassword+szSalt, "SHA1");
			return szPassword;
			}
		//-------------------------------------------------------------------------
		//Creates the salt value that is used to strengthen an encryption
		//A Salt value is a value that is appened to a string before the string is
		//encrypted.  This makes it harder to crack an encypted string.
		//-------------------------------------------------------------------------
		public static string CreateSalt()
			{
			int iSaltSize = 10;
			// Generate a cryptographic random number using the cryptographic
			// service provider
			System.Security.Cryptography.RNGCryptoServiceProvider rngSalt = 
				new System.Security.Cryptography.RNGCryptoServiceProvider();
			byte[] baSaltValue = new byte[iSaltSize];
			rngSalt.GetBytes(baSaltValue);
			// Return a Base64 string representation of the random number
			return Convert.ToBase64String(baSaltValue);
			}
		//-------------------------------------------------------------------------
		//Displays a java script alert on the page.
		//-------------------------------------------------------------------------
		public static void DisplayMessage(Page pgPageInfo, string szMessageText)
			{
			pgPageInfo.RegisterStartupScript("startupScript", "<script language=JavaScript>alert('"+szMessageText+"');</script>");
			}
		//-------------------------------------------------------------------------
		//Displays a java script alert on the page and transfers them to another page.
		//-------------------------------------------------------------------------
		public static void DisplayMessage(Page pgPageInfo, string szMessageText, string szURL)
		{
			pgPageInfo.RegisterStartupScript("startupScript", "<script language=JavaScript>alert('"+szMessageText+"');document.location.href='"+szURL+"';</script>");
		}
		//-------------------------------------------------------------------------
		//Gives a specified control focus.
		//-------------------------------------------------------------------------
		public static void SetControlFocus(string szControlName, Page pgCurrent)
			{
			pgCurrent.Controls.Add(new LiteralControl("<Script Language=javascript>"));
			pgCurrent.Controls.Add(new LiteralControl("document.forms[0].elements['"+szControlName+"'].focus()"));
			pgCurrent.Controls.Add(new LiteralControl("</Script>"));
			}
		//-------------------------------------------------------------------------
		//Determines which UserID to use, either the SelectedUserID or the 
		//UserID.  This depends on whether it is the user viewing their own reports
		//or it a user view their grower's reports
		//-------------------------------------------------------------------------	
		public static string GetActiveUserName()
			{
			string szUserName = "";
			if(HttpContext.Current.Session["SelectedUserName"].ToString() != "")
			{
				szUserName = HttpContext.Current.Session["SelectedUserName"].ToString();
			}
			else
			{
				szUserName = HttpContext.Current.Session["UserName"].ToString();
			}
			return szUserName;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the browser is IE
		//-------------------------------------------------------------------------
		public static bool IsBrowserIE(Page pgCurrent)
			{
			bool bIE = false;
			HttpBrowserCapabilities bc;
			bc = pgCurrent.Request.Browser;
			if(bc.Browser == "IE")
				{
				bIE = true;
				}
			return bIE;
			}

		//-------------------------------------------------------------------------
		//Runs a check to ensure that the initial water and nitrogen conditions are
		//set up for the paddocks soil sample (Depth, Water and NO3)
		//-------------------------------------------------------------------------
		static public bool IsSampleValid(string szPaddockName, string szUserName)
			{
			DataTable dtPaddocksSoilSameple =
				DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName,szUserName);
			DataTable dtSoilSampleFromDB = new DataTable();
			if(dtPaddocksSoilSameple.Rows.Count > 0)
				{
				SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSameple.Rows[0]["Data"].ToString()));
				if (Sample.SWUnit == SoilSample.SWUnits.Gravimetric)
					return (Sample.Thickness.Length > 0 && Sample.SWGrav.Length > 0 && Sample.NO3.Length > 0);
				else
					return (Sample.Thickness != null && Sample.SW != null && Sample.NO3 != null);
				}
			else
				return false;
			}

		//-------------------------------------------------------------------------
		//Runs a check to ensure that the initial water and nitrogen conditions are
		//set up for the paddocks soil sample (Depth, Water and NO3)
		//-------------------------------------------------------------------------
		static public bool IsSampleSWValid(string szPaddockName, string szUserName)
			{
			DataTable dtPaddocksSoilSample =
				DataAccessClass.GetPaddocksSoilSample("GridOne", szPaddockName, szUserName);
			DataTable dtPaddockDetails = 
				DataAccessClass.GetDetailsOfPaddock(szPaddockName, szUserName);
			if(dtPaddocksSoilSample.Rows.Count > 0)
				{
				SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSample.Rows[0]["Data"].ToString()));
				Sample.LinkedSoil = DataAccessClass.GetSoil(dtPaddockDetails.Rows[0]["SoilName"].ToString());
				return DataAccessClass.IsSoilSampleOk(Sample);
				}
			else
				return false;
			}

		static public string RemoveLastCharacter(string szStringToCheck, char cCharacterToCheckFor)
		{
			//Removes last charcater if it exists
			int iIndex = szStringToCheck.LastIndexOf(cCharacterToCheckFor.ToString());
			if (iIndex > 0)
			{
				szStringToCheck = szStringToCheck.Remove(iIndex, 1);
			}
			return szStringToCheck;
		}

		//-------------------------------------------------------------------------		
		#endregion	
		

		#region Janus Grid functions

		// ------------------------------------------------------
		// Populate the specified grid column with double values.
		// ------------------------------------------------------
		static public void SetColumnAsDoubles(Janus.Web.GridEX.GridEX Grid,
												int ColumnIndex,
												double[] Values,
												string Format)
			{
			// Add values to column
			for (int Row = 0; Row != Values.Length; Row++)
				Grid.GetRow(Row).Cells[ColumnIndex].Value = Values[Row].ToString(Format);
			}


		// ------------------------------------------------------
		// Populate the specified grid column with string values.
		// ------------------------------------------------------
		static public void SetColumnAsStrings(Janus.Web.GridEX.GridEX Grid,
												int ColumnIndex,
												string[] Values)
			{
			// Add values to column
			for (int Row = 0; Row != Values.Length; Row++)
				Grid.GetRow(Row).Cells[ColumnIndex].Value = Values[Row];
			}


		// ----------------------------------------------------------
		// Get a column of double values from the specified grid column
		// ----------------------------------------------------------
		static public double[] GetColumnAsDoubles(Janus.Web.GridEX.GridEX Grid, 
													int ColumnIndex,
													int NumValues)
			{
            double[] Values = new double[NumValues];

			for (int Row = 0; Row != Grid.RowCount && Row != NumValues; Row++)
				{
				if (Grid.GetRow(Row).Cells[ColumnIndex].Text == "")
					Values[Row] = MathUtility.MissingValue;
				else
					Values[Row] = Convert.ToDouble(Grid.GetRow(Row).Cells[ColumnIndex].Text);
				}
			return Values;
			}


		// ----------------------------------------------------------
		// Get a column of string values from the specified grid column
		// ----------------------------------------------------------
		static public string[] GetColumnAsStrings(Janus.Web.GridEX.GridEX Grid, 
													int ColumnIndex,
													int NumValues)
			{
            string[] Values = new string[NumValues];

			for (int Row = 0; Row != Grid.RowCount && Row != NumValues; Row++)
				Values[Row] = Grid.GetRow(Row).Cells[ColumnIndex].Text;
			return Values;
			}


		// ---------------------------------------------------------------------
		// Get number of non blank values in column of the specified grid
		// ---------------------------------------------------------------------
		static public int GetNumberOfNonBlankRows(ref Janus.Web.GridEX.GridEX Grid, int ColumnIndex)
			{
			for (int Row = Grid.RowCount-1; Row >= 0; Row--)
				{
				Janus.Web.GridEX.GridEXRow grdRow = Grid.GetRow(Row);
				if (grdRow.Cells[ColumnIndex].Text != "")
					return Row + 1;
				}
			return 0;
			}


		#endregion

		#region Tiller calculation functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static double ReturnTillerNumber(string szSkipType, string szRegion, DateTime dtSowingDate, int iPopulation)
			{
			double dTillerNumber = 0;

			int iDensFact1 = 0;
			int iDensFact2 = 0;
			int iDensFact3 = 0;
			double dDensity = (iPopulation/10000);

			if(dDensity < 5.0)
				iDensFact1 =1;
			else if(dDensity >= 5.0 && dDensity < 10.0)
				iDensFact2 = 1;
			else if(dDensity >=10.0 && dDensity < 15.0)
				iDensFact3 = 1;

			switch(szSkipType)
				{
				case "Solid":
					switch(szRegion)
						{
						case "Queensland":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.2*dDensity + 2.5) + iDensFact2*(-0.2*dDensity + 2.5) + iDensFact3*(-0.1*dDensity + 1.5));
							else
								dTillerNumber = (iDensFact1*(-0.2*dDensity + 2.0) + iDensFact2*(-0.15*dDensity + 1.75) + iDensFact3*(-0.05*dDensity + 0.75));
							break;
						case "Northern NSW":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.2*dDensity + 3.0) + iDensFact2*(-0.3*dDensity + 3.5) + iDensFact3*(-0.1*dDensity + 1.5));
							else if(dtSowingDate.DayOfYear >= 319 && dtSowingDate.DayOfYear < 349)
								dTillerNumber = (iDensFact1*(-0.2*dDensity + 2.5) + iDensFact2*(-0.25*dDensity + 2.75) + iDensFact3*(-0.05*dDensity + 0.75));
							else
								dTillerNumber = (iDensFact1*(-0.2*dDensity + 2.0) + iDensFact2*(-0.2*dDensity + 2.0) + iDensFact3*(0*dDensity + 0.0));
							break;
						default:
							break;
						}
					break;

				case "Single skip":
					switch(szRegion)
						{
						case "Queensland":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.3*dDensity + 2.5) + iDensFact2*(-0.15*dDensity + 1.75) + iDensFact3*(-0.5*dDensity + 0.75));
							else
								dTillerNumber = (iDensFact1*(-0.25*dDensity + 1.875) + iDensFact2*(-0.1*dDensity + 1.125) + iDensFact3*(-0.025*dDensity + 0.375));
							break;
						case "Northern NSW":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.4*dDensity + 3.25) + iDensFact2*(-0.2*dDensity + 2.25) + iDensFact3*(-0.05*dDensity + 0.75));
							else if(dtSowingDate.DayOfYear >= 319 && dtSowingDate.DayOfYear < 349)
								dTillerNumber = (iDensFact1*(-0.35*dDensity + 2.625) + iDensFact2*(-0.15*dDensity + 1.625) + iDensFact3*(-0.025*dDensity + 0.375));
							else
								dTillerNumber = (iDensFact1*(-0.3*dDensity + 2.0) + iDensFact2*(-0.1*dDensity + 1.0) + iDensFact3*(0*dDensity + 0.0));
							break;
						default:
							break;
						}
					break;

				case "Double skip":
					switch(szRegion)
						{
						case "Queensland":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.4*dDensity + 2.5) + iDensFact2*(-0.1*dDensity + 1.0) + iDensFact3*(0.0*dDensity + 0));
							else
								dTillerNumber = (iDensFact1*(-0.3*dDensity + 1.75) + iDensFact2*(-0.05*dDensity + 0.5) + iDensFact3*(0.0*dDensity + 0.0));
							break;
						case "Northern NSW":
							if(dtSowingDate.DayOfYear < 319)
								dTillerNumber = (iDensFact1*(-0.6*dDensity + 3.5) + iDensFact2*(-0.1*dDensity + 1.0) + iDensFact3*(0.0*dDensity + 0.0));
							else if(dtSowingDate.DayOfYear >= 319 && dtSowingDate.DayOfYear < 349)
								dTillerNumber = (iDensFact1*(-0.5*dDensity + 2.75) + iDensFact2*(-0.05*dDensity + 0.5) + iDensFact3*(0.0*dDensity + 0.0));
							else
								dTillerNumber = (iDensFact1*(-0.4*dDensity + 2.0) + iDensFact2*(0.0*dDensity + 0.0) + iDensFact3*(0*dDensity + 0.0));
							break;
						default:
							break;
						}
					break;

				default:
					break;
				}

			return dTillerNumber;
			}
		//-------------------------------------------------------------------------
		#endregion


		}//END OF CLASS
	}//END OF NAMESPACE
