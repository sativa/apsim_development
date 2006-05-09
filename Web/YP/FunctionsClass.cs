using System;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web;
using System.Data;
using System.Xml;
using VBGeneral;
using CSGeneral;

namespace YP2006
{
	/// <summary>
	/// Summary description for FunctionsClass.
	/// </summary>
	public class FunctionsClass
	{


		public const string szAdministrator = "administrator";
		public const string szGrower = "grower";
		public const string szConsultant = "consultant";
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public FunctionsClass()
		{

		}


		#region Functions to find a user on a Janus Grid

		//-------------------------------------------------------------------------
		//Find a user on the user grid.  This is achieved by using the
		//name entered into the edit box and searching through the grid.
		//-------------------------------------------------------------------------
		static public void FindUserOnGrid(string szCurrentSearchName, ref string szPreviousSearchName,
			int iNumberOfRowsToSearch, ref int iPreviousRowIndex, Janus.Web.GridEX.GridEX grdUsers)
		{
			if(szCurrentSearchName != null && szCurrentSearchName != "")
			{
				if(szPreviousSearchName != szCurrentSearchName)
				{
					iPreviousRowIndex = 0;
				}
				int iRowIndex = ReturnSelectedUsersRowIndex(szCurrentSearchName, "Name",
					iNumberOfRowsToSearch, ref iPreviousRowIndex, grdUsers);
				SelectUserOnGrid(grdUsers, iRowIndex, iNumberOfRowsToSearch);

				szPreviousSearchName = szCurrentSearchName;
			}
			else
				throw new Exception("No name supplied");
		}
		//-------------------------------------------------------------------------
		//Find the row index where the specified name is found in the specified column
		//-------------------------------------------------------------------------
		static public int ReturnSelectedUsersRowIndex(string szUserNameToFind, string szColumnToSearch,
			int iNumberOfRowsToSearch, ref int iPreviousRowIndex, Janus.Web.GridEX.GridEX grdUsers)
		{
			int iRowIndex = 0;
			if(szUserNameToFind != "" && szUserNameToFind != null)
			{
				szUserNameToFind = szUserNameToFind.ToLower();
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = iPreviousRowIndex; iIndex < iNumberOfRowsToSearch; iIndex++)
				{
					grdRow = grdUsers.GetRow(iIndex);
					if(grdRow.Cells[szColumnToSearch].Text.ToLower().IndexOf(szUserNameToFind) >= 0)
					{
						iRowIndex = grdRow.Position;
						iPreviousRowIndex = iRowIndex+1;
						break;
					}
				}
				if(iPreviousRowIndex > 1 && iRowIndex == 0)
				{
					iPreviousRowIndex = 0;
					throw new Exception("No more matching results");
				}
			}
			return iRowIndex;
		}
		//-------------------------------------------------------------------------
		//Select the row on the grid with the corresponding rowindex
		//-------------------------------------------------------------------------
		static public void SelectUserOnGrid(Janus.Web.GridEX.GridEX grdUsers,
			int iRowIndex, int iNumberOfRowsToSearch)
		{
			//Due to a problem with not being able to count the number of rows from the
			//grid itself, we count the number of rows when we get them from the databse
			//and we store them on the page
			grdUsers.SelectedItems.Clear();

			//Search through every row until we find the row that we need
			Janus.Web.GridEX.GridEXRow grdRow;
			for(int iIndex = 0; iIndex < iNumberOfRowsToSearch; iIndex++)
			{
				grdRow = grdUsers.GetRow(iIndex);
				//Check to see if the row we are looking for is grouped under a consultant
				//if they are then expand this row to show the row we are looking for
				if((grdRow.Children + grdRow.Position) >= iRowIndex)
				{
					grdRow.Expanded = true;
				}
				//If it is the row we are looking for, select it and stop the search
				if(grdRow.Position == iRowIndex)
				{
					grdUsers.SelectedItems.Add(iRowIndex);
					break;
				}
			}
		}
		//-------------------------------------------------------------------------
		//Checks to see if the user is returning to the page, if they are, then find
		//the grower that they had last selected.  This is achieved by using the
		//SelectedUserName session variable and searching through the grid.
		//-------------------------------------------------------------------------
		static public void PreSetSelectedUser(ref bool bSetPosition, string szCurrentSearchName,
			int iNumberOfRowsToSearch, Janus.Web.GridEX.GridEX grdUsers)
		{
			//Checks to see if a user is returning
			if(bSetPosition == true)
			{
				bSetPosition = false;
				int iPreviousRowIndex = 0;
				//Find the row number of the selected user
				int iRowIndex = ReturnSelectedUsersRowIndex(szCurrentSearchName, "UserName",
					iNumberOfRowsToSearch, ref iPreviousRowIndex, grdUsers);
				SelectUserOnGrid(grdUsers, iRowIndex, iNumberOfRowsToSearch);
			}
		}
		//-------------------------------------------------------------------------
		#endregion




		#region Access Permissions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsGrower()
		{
			bool bIsGrower =  false;
			if(HttpContext.Current.Session["AccessType"].ToString() == szGrower)
			{
				bIsGrower = true;
			}
			return bIsGrower;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsGrowerOrHigher()
		{
			bool bIsConsultantOrHigher =  false;
			if(IsAdministrator() == true || IsConsultant() == true || IsGrower() == true)
			{
				bIsConsultantOrHigher = true;
			}
			return bIsConsultantOrHigher;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsConsultant()
		{
			bool bIsConsultant =  false;
			if(HttpContext.Current.Session["AccessType"].ToString() == szConsultant)
			{
				bIsConsultant = true;
			}
			return bIsConsultant;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsConsultantOrHigher()
		{
			bool bIsConsultantOrHigher =  false;
			if(IsAdministrator() == true || IsConsultant() == true)
			{
				bIsConsultantOrHigher = true;
			}
			return bIsConsultantOrHigher;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsAdministrator()
		{
			bool bIsAdministrator =  false;
			if(HttpContext.Current.Session["AccessType"].ToString() == szAdministrator)
			{
				bIsAdministrator = true;
			}
			return bIsAdministrator;
		}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who isn't an administrator
		//This is used to stop users from accessing pages they aren't allowed to
		//-------------------------------------------------------------------------
		public static void CheckForAdministratorLevelPriviledges()
		{
			if(IsAdministrator() == false)
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
			if(IsConsultantOrHigher() == false)
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
			if(IsGrowerOrHigher() == false)
			{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
			}
		}
		//-------------------------------------------------------------------------
		//Cancels the session of a user who is a read only user
		//-------------------------------------------------------------------------
		public static void CheckForWritePriviledges()
		{
			if(IsReadOnly() == true)
			{
				HttpContext.Current.Server.Transfer("wfSessionTimeOut.aspx");
			}
		}
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
		//Checks that either the user nor the consultant viewing that users data
		//has only read only access.
		//-------------------------------------------------------------------------
		public static bool IsReadOnly()
		{
			bool bIsReadOnly = true;

			if(HttpContext.Current.Session["SelectedUserName"].ToString() != "" &&
				HttpContext.Current.Session["SelectedUserName"].ToString() != null)
				bIsReadOnly =
					DataAccessClass.IsConsultantReadOnly(HttpContext.Current.Session["SelectedUserName"].ToString(),
					HttpContext.Current.Session["UserName"].ToString());
			else
				bIsReadOnly = DataAccessClass.IsUserReadOnly(HttpContext.Current.Session["UserName"].ToString());
			return bIsReadOnly;
		}

		public static string ReturnReadOnlyMessage()
		{
			string szMessage = "This account is not set up to make changes to the current grower – contact the administrator if you wish to alter this";
			return szMessage;
		}
		#endregion




		#region Report Generation Page Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void StorePaddockSelectionConsultant(System.Web.UI.StateBag ViewState)
		{
			wfReportsGenerateConsultant ReportsGenerateConsultant = (wfReportsGenerateConsultant) HttpContext.Current.Handler;
			ViewState["Paddocks"] = ReportsGenerateConsultant.ReturnsPaddockSelection();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void StorePaddockSelectionGrower(System.Web.UI.StateBag ViewState)
		{
			wfReportsGenerate ReportsGenerate = (wfReportsGenerate) HttpContext.Current.Handler;
			ViewState["Paddocks"] = ReportsGenerate.ReturnsPaddockSelection();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void StoreFavouriteSelectionGrower(System.Web.UI.StateBag ViewState)
		{
			wfReportsFavourites ReportsFavourites = (wfReportsFavourites) HttpContext.Current.Handler;
			ViewState["Paddocks"] = ReportsFavourites.ReturnsPaddockSelection();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void StoreFavouriteSelectionConsultant(System.Web.UI.StateBag ViewState)
		{
			wfReportsFavouritesConsultant ReportsFavouritesConsultant = (wfReportsFavouritesConsultant) HttpContext.Current.Handler;
			ViewState["Paddocks"] = ReportsFavouritesConsultant.ReturnsPaddockSelection();

		}
		//-------------------------------------------------------------------------
		//Intitialise the grids to contain 5 blank rows
		//-------------------------------------------------------------------------
		public static void InitialiseApplicationGrid(DataSet dsApplications, int iMaxNumberOfRows,
			Control ctCurrentPage)
		{
			DataRow drAppliction;
			for(int iTableCount = 0; iTableCount < dsApplications.Tables.Count; iTableCount++)
			{
				for(int iIndex = dsApplications.Tables[iTableCount].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
				{
					drAppliction = dsApplications.Tables[iTableCount].NewRow();
					drAppliction["ID"] = 0;
					dsApplications.Tables[iTableCount].Rows.Add(drAppliction);
				}
			}
			ctCurrentPage.DataBind();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void AddNitrogenNodeToNitrogenDataSet(int iTableNumber,
			XmlNode xmlUpperChild, DataSet dsNitrogen)
		{
			DataRow drNitrogen;
			drNitrogen = dsNitrogen.Tables[iTableNumber].NewRow();
			drNitrogen["ID"] = 0;

			foreach(XmlNode xmlChildNode in xmlUpperChild.ChildNodes)
			{
				if(xmlChildNode.Name == "date")
				{
					drNitrogen["ApplicationDate"] = xmlChildNode.InnerText;
				}
				if(xmlChildNode.Name == "rate")
				{
					drNitrogen["Rate"] = xmlChildNode.InnerText;
				}
			}
			dsNitrogen.Tables[iTableNumber].Rows.Add(drNitrogen);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void AddIrrigationNodeToIrrigationDataSet(int iTableNumber,
			XmlNode xmlUpperChild, DataSet dsIrrigation)
		{
			DataRow drIrrigation;
			drIrrigation = dsIrrigation.Tables[iTableNumber].NewRow();
			drIrrigation["ID"] = 0;

			foreach(XmlNode xmlChildNode in xmlUpperChild.ChildNodes)
			{
				if(xmlChildNode.Name == "date")
				{
					drIrrigation["Date"] = xmlChildNode.InnerText;
				}
				else if(xmlChildNode.Name == "rate")
				{
					drIrrigation["Amount"] = xmlChildNode.InnerText;
				}
				else if(xmlChildNode.Name == "efficency")
				{
					drIrrigation["Efficency"] = xmlChildNode.InnerText;
				}
			}
			dsIrrigation.Tables[iTableNumber].Rows.Add(drIrrigation);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void SetToEditFavouriteMode(CheckBox chkFavourite, Button btnGenerate)
		{
			chkFavourite.Checked = true;
			chkFavourite.Enabled = false;
			btnGenerate.Text = "Save";
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void SetReportNavigationButtons(LinkButton btnReportsView,
			LinkButton btnReportsGenerate, LinkButton btnReportsManageFavourites)
		{
			if(IsConsultantOrHigher() == true  &&
				HttpContext.Current.Session["SelectedUserName"].ToString() == "")
			{
				btnReportsView.CommandName = "wfReportsViewConsultant.aspx";
				btnReportsGenerate.CommandName = "wfReportsGenerateConsultant.aspx";
				btnReportsManageFavourites.CommandName = "wfReportsFavouritesConsultant.aspx";
			}
			else
			{
				btnReportsView.CommandName = "wfReportsView.aspx";
				btnReportsGenerate.CommandName = "wfReportsGenerate.aspx";
				btnReportsManageFavourites.CommandName = "wfReportsFavourites.aspx";
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsSendEmail(Button btnGenerate)
		{
			bool bSendEmail = true;
			if(btnGenerate.Text == "Save")
				bSendEmail = false;
			return bSendEmail;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void TransferAfterCompletion(Button btnGenerate)
		{
			if(IsSendEmail(btnGenerate))
			{
				HttpContext.Current.Server.Transfer("wfReportsSuccessful.aspx");
			}
			else
			{
				if(FunctionsClass.IsConsultantOrHigher() &&
					HttpContext.Current.Session["SelectedUserName"].ToString() == "")
				{
					HttpContext.Current.Server.Transfer("wfReportsFavouritesConsultant.aspx");
				}
				else
				{
					HttpContext.Current.Server.Transfer("wfReportsFavourites.aspx");
				}
			}
		}
		//-------------------------------------------------------------------------
		#endregion






		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public bool IsPaddockSown(string szPaddockName, string szUserName)
		{
			bool bPaddockSown = false;
			DataTable dtPaddock =
				DataAccessClass.GetDetailsOfPaddock(szPaddockName,szUserName);
			if(dtPaddock.Rows.Count > 0)
			{
				if(dtPaddock.Rows[0]["SowDate"].ToString() != "")
					bPaddockSown = true;
			}
			return bPaddockSown;
		}

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void SetDisplayBanner(System.Web.UI.WebControls.Image imgDisplayBanner)
		{
			if(HttpContext.Current.Session["BannerImage"].ToString() == "NONE")
			{
				imgDisplayBanner.Visible = false;
				imgDisplayBanner.ImageUrl = "";
			}
			else
			{
				imgDisplayBanner.ImageUrl = "BannerImages\\" + HttpContext.Current.Session["BannerImage"].ToString();
			}
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
		//
		//-------------------------------------------------------------------------
		static private string ReturnGrowersFullName()
		{
			string szGrowersName = "";
			if(HttpContext.Current.Session["SelectedUserName"].ToString() != "")
			{
				szGrowersName = DataAccessClass.GetNameOfUser(HttpContext.Current.Session["SelectedUserName"].ToString());
			}
			else
			{
				szGrowersName = DataAccessClass.GetNameOfUser(HttpContext.Current.Session["UserName"].ToString());
			}
			return szGrowersName;
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



		#region Heading Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void SetHeadingString(Label lblHeading)
		{
			string szGrowersName = ReturnGrowersFullName();
			if(szGrowersName != null && szGrowersName != "")
			{
				if(szGrowersName[szGrowersName.Length-1] != 's')
				{
					lblHeading.Text = lblHeading.Text.Replace("GrowerPlaceHolder", szGrowersName);
				}
				else
				{
					lblHeading.Text = lblHeading.Text.Replace("GrowerPlaceHolder's", szGrowersName+"'");
				}
			}
			string szUsersName = DataAccessClass.GetNameOfUser(HttpContext.Current.Session["UserName"].ToString());
			if(szUsersName != null && szUsersName != "")
			{
				if(szUsersName[szUsersName.Length-1] != 's')
				{
					lblHeading.Text = lblHeading.Text.Replace("UserPlaceHolder", szUsersName);
				}
				else
				{
					lblHeading.Text = lblHeading.Text.Replace("UserPlaceHolder's", szUsersName+"'");
				}
			}

			if(IsConsultantOrHigher() == true)
			{
				string szConsultantsName = DataAccessClass.GetNameOfUser(HttpContext.Current.Session["UserName"].ToString());
				if(szConsultantsName[szConsultantsName.Length-1] != 's')
				{
					lblHeading.Text = lblHeading.Text.Replace("ConsultantPlaceHolder", szConsultantsName);
				}
				else
				{
					lblHeading.Text = lblHeading.Text.Replace("ConsultantPlaceHolder's", szConsultantsName+"'");
				}
			}
			string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
			if(szPaddockName != null && szPaddockName != "")
			{
				lblHeading.Text = lblHeading.Text.Replace("PaddockPlaceHolder", szPaddockName);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static void SetNavigationMenu(System.Web.UI.WebControls.LinkButton btnGrowersPaddocks,
			System.Web.UI.WebControls.LinkButton btnGrowersReports,
			System.Web.UI.WebControls.LinkButton btnManageItems,
			System.Web.UI.WebControls.LinkButton btnManageReports)
		{
			if(IsConsultant() == true)
			{
				btnGrowersPaddocks.Visible = true;
				btnGrowersReports.Visible = true;
				btnManageItems.Text = "My Growers";
				btnManageItems.CommandName = "wfManageGrowers.aspx";
				btnManageReports.CommandName = "wfReportsMenuConsultant.aspx";
				SetNavigationButtonText(btnGrowersReports);
				SetNavigationButtonText(btnGrowersPaddocks);
			}
			else if(IsAdministrator() == true)
			{
				btnGrowersPaddocks.Visible = true;
				btnGrowersReports.Visible = true;
				btnManageItems.Text = "My Growers";
				btnManageItems.CommandName = "wfManageGrowers.aspx";
				btnManageReports.CommandName = "wfReportsMenuConsultant.aspx";
				SetNavigationButtonText(btnGrowersReports);
				SetNavigationButtonText(btnGrowersPaddocks);
			}
			else
			{
				btnGrowersPaddocks.Visible = false;
				btnGrowersReports.Visible = false;
				btnManageItems.Text = "My Paddocks";
				btnManageItems.CommandName = "wfPaddocksMenu.aspx";
				btnManageReports.CommandName = "wfReportsMenu.aspx";
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static void SetNavigationMenu(System.Web.UI.WebControls.LinkButton btnManageItems,
			System.Web.UI.WebControls.LinkButton btnManageReports)
		{
			if(IsConsultant() == true)
			{
				btnManageItems.Text = "My Growers";
				btnManageItems.CommandName = "wfManageGrowers.aspx";
				btnManageReports.CommandName = "wfReportsMenuConsultant.aspx";
			}
			else if(IsAdministrator() == true)
			{
				btnManageItems.Text = "My Growers";
				btnManageItems.CommandName = "wfManageGrowers.aspx";
				btnManageReports.CommandName = "wfReportsMenuConsultant.aspx";
			}
			else
			{
				btnManageItems.Text = "My Paddocks";
				btnManageItems.CommandName = "wfPaddocksMenu.aspx";
				btnManageReports.CommandName = "wfReportsMenu.aspx";
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		static public void SetNavigationButtonText(LinkButton btnNavigation)
		{
			if(HttpContext.Current.Session["SelectedUserName"].ToString() != "")
			{
				btnNavigation.Visible = true;
				string szGrowersName = ReturnGrowersFullName();
				if(szGrowersName != null && szGrowersName != "")
				{
					if(szGrowersName[szGrowersName.Length-1] != 's')
					{
						btnNavigation.Text = btnNavigation.Text.Replace("GrowerPlaceHolder", szGrowersName);
					}
					else
					{
						btnNavigation.Text = btnNavigation.Text.Replace("GrowerPlaceHolder's", szGrowersName+"'");
					}
				}
			}
			else
			{
				btnNavigation.Visible = false;
			}
		}
		//-------------------------------------------------------------------------
		#endregion



		#region Tiller calculation functions

		//-------------------------------------------------------------------------
		//Returns the value of the Population text box after it is checked to insure
		//that the value is a valid integer
		//-------------------------------------------------------------------------
		public static int ReturnPopulationValue(TextBox edtPopulation)
		{
			int iPopulation = 0;
			if(edtPopulation.Text != "")
			{
				if(InputValidationClass.IsInputAPositiveInteger(edtPopulation.Text))
				{
					iPopulation = Convert.ToInt32(edtPopulation.Text);
				}
			}
			return iPopulation;
		}
		//-------------------------------------------------------------------------
		//Calculates and displays the fertile Tiller number
		//-------------------------------------------------------------------------
		static public void SetFertileTillerNumber(string szUserName, string szPaddockName,
			TextBox edtTiller, int iPopulation,
			string szRowConfiguration, DateTime dtSowingDate)
		{

			DataTable dtPaddockDetails =
				DataAccessClass.GetDetailsOfPaddock(szPaddockName, szUserName);

			if(dtPaddockDetails.Rows[0]["RegionType"].ToString() != null &&
				dtPaddockDetails.Rows[0]["RegionType"].ToString() != "" &&
				dtPaddockDetails.Rows[0]["RegionType"].ToString() != "None")
			{
				edtTiller.Text = ReturnTillerNumber(szRowConfiguration, dtPaddockDetails.Rows[0]["RegionType"].ToString(),
					dtSowingDate, iPopulation).ToString();
			}
			else
			{
				throw new Exception("Please ensure that a valid region is selected and a valid population is entered");
			}

		}
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


	}//END OF CLASS
}//END OF NAMESPACE
