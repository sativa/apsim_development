using System;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Data;

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
		public const string szVisitor = "visitor";

		public FunctionsClass()
			{}


		#region Functions controlling access priviledges
		//-------------------------------------------------------------------------
		//Checks to see if the user is a visitor
		//-------------------------------------------------------------------------
		public static bool IsVisitor(string szUserID)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserID);
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
		public static bool IsVisitorOrHigher(string szUserID)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserID);
			bool bVisitor = false;
			if(szAccessType == szAdministrator || szAccessType == szGrower ||
				szAccessType == szConsultant || szAccessType == szVisitor)
				{
				bVisitor = true;
				}
			return bVisitor;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user has grower priviledges or greater
		//-------------------------------------------------------------------------
		public static bool IsGrowerOrHigher(string szUserID)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserID);
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
		public static bool IsConsultantOrHigher(string szUserID)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserID);
			bool bConsultant = false;
			if(szAccessType == szAdministrator || szAccessType == szConsultant)
				{
				bConsultant = true;
				}
			return bConsultant;
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user is an administrator
		//-------------------------------------------------------------------------
		public static bool IsAdministrator(string szUserID)
			{
			string szAccessType = DataAccessClass.GetAccessTypeOfUser(szUserID);
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
			if(IsAdministrator(HttpContext.Current.Session["UserID"].ToString()) == false)
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
			if(IsConsultantOrHigher(HttpContext.Current.Session["UserID"].ToString()) == false)
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
			if(IsGrowerOrHigher(HttpContext.Current.Session["UserID"].ToString()) == false)
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
			if(IsVisitorOrHigher(HttpContext.Current.Session["UserID"].ToString()) == false)
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
			if(HttpContext.Current.Session== null || HttpContext.Current.Session["UserID"].ToString() == "0")
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
		//Gives a specified control focus.
		//-------------------------------------------------------------------------
		public static void SetControlFocus(string szControlName, Page pgCurrent)
			{
			pgCurrent.Controls.Add(new LiteralControl("<Script Language=javascript>"));
			pgCurrent.Controls.Add(new LiteralControl("document.forms[0].elements['"+szControlName+"'].focus()"));
			pgCurrent.Controls.Add(new LiteralControl("</Script>"));
			}
		//-------------------------------------------------------------------------		
		#endregion	
		
		}//END OF CLASS
	}//END OF NAMESPACE
