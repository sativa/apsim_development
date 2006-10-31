using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;

namespace YP2006
	{
	/// <summary>
	/// Summary description for wfYieldProphet.
	/// </summary>
	public class wfLogin : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.Label Label7;
		protected System.Web.UI.WebControls.Label Label6;
		protected System.Web.UI.WebControls.Label Label5;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.HyperLink HyperLink1;
		protected System.Web.UI.WebControls.HyperLink HyperLink2;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Label lblVisitorText;
		protected System.Web.UI.WebControls.Label lblWelcomeText2;
		protected System.Web.UI.WebControls.Button btnLogin;
		protected System.Web.UI.WebControls.Label lblSubscribers;
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.Label lblUserName;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.CheckBox chkRemember;
		protected System.Web.UI.WebControls.LinkButton btnJoin;
		protected System.Web.UI.WebControls.LinkButton btnFeedback;
		protected System.Web.UI.WebControls.HyperLink hylCsiro;
		protected System.Web.UI.WebControls.HyperLink hylBCG;
		protected System.Web.UI.WebControls.HyperLink hylAPSRU;
		protected System.Web.UI.WebControls.HyperLink hlyDCITA;
		protected System.Web.UI.WebControls.Label lblTest;
		protected System.Web.UI.WebControls.RadioButtonList RadioButtonList1;



		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//
			// CODEGEN: This call is required by the ASP.NET Web Form Designer.
			//
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.btnJoin.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFeedback.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnLogin.Click += new System.EventHandler(this.btnLogin_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Checks the login details of the user and if a match is found then 
		//the users details are stored in the session and the user is sent 
		//to the frames page (YieldProphet.htm)
		//-------------------------------------------------------------------------
		private void LogUserIn()
		{
			try
			{
				if(DataAccessClass.AuthenticateUser(InputValidationClass.ValidateString(edtUserName.Text),
					InputValidationClass.ValidateString(edtPassword.Text)) == true)
				{
					DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(InputValidationClass.ValidateString(edtUserName.Text));
					if(dtUserDetails.Rows.Count > 0)
					{
						Session["UserName"] = dtUserDetails.Rows[0]["UserName"].ToString();
						Session["AccessType"] = DataAccessClass.GetAccessTypeOfUser(Session["UserName"].ToString());
						Session["BannerImage"] = dtUserDetails.Rows[0]["BannerImageFileName"].ToString();
						string szFirstPage = dtUserDetails.Rows[0]["StartPageName"].ToString();
						if(ReportClass.DoesUsersReportDirectoryExisit(Session["UserName"].ToString(), DateTime.Today.Year) == false)
						{
							ReportClass.CreateUsersReportDirectory(Session["UserName"].ToString(), DateTime.Today.Year); 
						}
						if(chkRemember.Checked == true)
						{	
							System.Web.HttpCookie YPCookie = new HttpCookie("YP", dtUserDetails.Rows[0]["UserName"].ToString()+"||"+
								dtUserDetails.Rows[0]["Pass"].ToString());
							YPCookie.Expires = DateTime.Today.AddYears(1);
							Response.Cookies.Add(YPCookie);
						}
						Server.Transfer(szFirstPage);
					}
					else
						throw new Exception("Error getting user's details");
				}
				else
				{
					ClearFormInformation();
					throw new Exception("Incorrect Login Details.  All login details are case sensitive");
				}	
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------
		//Clears the text from the two textboxes that contain the user's
		//login details
		//---------------------------------------------------------------------
		private void ClearFormInformation()
		{
			edtPassword.Text = "";
			edtUserName.Text = "";
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void LoginFromCookie()
		{
			string szCookieString = "";
			HttpCookie YPCookie = Request.Cookies["YP"];
			if (YPCookie != null && YPCookie.Value != null)
			{
				try
				{
					szCookieString = YPCookie.Value.ToString();
					string[] szLoginDetails = szCookieString.Split("||".ToCharArray());
					if(szLoginDetails.Length == 3)
					{
						if(DataAccessClass.AuthenticateUserFromCookie(szLoginDetails[0], szLoginDetails[2]) == true)
						{
							DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(InputValidationClass.ValidateString(szLoginDetails[0]));
							if(dtUserDetails.Rows.Count > 0)
							{
								Session["UserName"] = dtUserDetails.Rows[0]["UserName"].ToString();
								Session["AccessType"] = DataAccessClass.GetAccessTypeOfUser(Session["UserName"].ToString());
								Session["BannerImage"] = dtUserDetails.Rows[0]["BannerImageFileName"].ToString();
								string szFirstPage = dtUserDetails.Rows[0]["StartPageName"].ToString();
								if(ReportClass.DoesUsersReportDirectoryExisit(Session["UserName"].ToString(), DateTime.Today.Year) == false)
								{
									ReportClass.CreateUsersReportDirectory(Session["UserName"].ToString(), DateTime.Today.Year); 
								}
								Server.Transfer(szFirstPage);
							}
							else
								throw new Exception("Error getting user's details");
						}
					}
				}
				catch(Exception E)
				{
					FunctionsClass.DisplayMessage(Page, E.Message);
				}

			}
		}
		//---------------------------------------------------------------------
		#endregion



		#region Form Events
		//---------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the page is 
		//initialised
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
			{
				Session["UserName"] = "";
				Session["SelectedUserName"] = "";
				Session["SelectedPaddockName"] = "";
				Session["AccessType"] = "";
				Session["BannerImage"] = "";
				FunctionsClass.SetControlFocus("edtUserName", this);
				LoginFromCookie();
			}
			}
		//---------------------------------------------------------------------
		//When the user presses the login button their login details are 
		//sent to the server for authentication
		//---------------------------------------------------------------------
		private void btnLogin_Click(object sender, System.EventArgs e)
			{
			LogUserIn();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//---------------------------------------------------------------------
		//User has clicked the feedback info button
		//---------------------------------------------------------------------
		private void btnFeedback_Click(object sender, System.EventArgs e)
		{
		Server.Transfer("wfSurvey.aspx");
		}
		//---------------------------------------------------------------------
		#endregion


		//---------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE