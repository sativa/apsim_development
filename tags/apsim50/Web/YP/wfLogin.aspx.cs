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

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for wfYieldProphet.
	/// </summary>
	public class wfLogin : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Button btnLogin;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblDescription;
		protected System.Web.UI.WebControls.LinkButton btnFeedback;
		protected System.Web.UI.WebControls.LinkButton btnJoin;
		protected System.Web.UI.WebControls.HyperLink hylContactUs;
		protected System.Web.UI.WebControls.Label lblVisitorDescription;
		protected System.Web.UI.WebControls.Label lblNoticeOne;
		protected System.Web.UI.WebControls.Label lblSubscribers;
		protected System.Web.UI.WebControls.Label lblVisitors;
		protected System.Web.UI.WebControls.Label lblNoticeTwo;
		protected System.Web.UI.WebControls.HyperLink hylApsim;
		protected System.Web.UI.WebControls.Image imgBCG;
		protected System.Web.UI.WebControls.Image imgASPRU;
		protected System.Web.UI.WebControls.Image imgDCITA;
		protected System.Web.UI.WebControls.Image imgTents;
		protected System.Web.UI.WebControls.HyperLink hylFurtherInfo;
		protected System.Web.UI.WebControls.Label lblUserName;



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
			this.btnLogin.Click += new System.EventHandler(this.btnLogin_Click);
			this.btnFeedback.Click += new System.EventHandler(this.btnFeedback_Click);
			this.btnJoin.Click += new System.EventHandler(this.btnJoin_Click);
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
				Session["UserName"] = "";
				Session["SelectedUserName"] = "";
				Session["SelectedPaddockName"] = "";
				Session["SelectedReportName"] = "";
				if(DataAccessClass.AuthenticateUser(InputValidationClass.ValidateString(edtUserName.Text),
					InputValidationClass.ValidateString(edtPassword.Text)) == true)
					{
					DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(InputValidationClass.ValidateString(edtUserName.Text));
					Session["UserName"] = dtUserDetails.Rows[0]["UserName"];
					if(ReportClass.DoesUsersReportDirectoryExisit(Session["UserName"].ToString(), DateTime.Today.Year) == false)
						{
						ReportClass.CreateUsersReportDirectory(Session["UserName"].ToString(), DateTime.Today.Year); 
						}
					Response.Redirect("YieldProphet.htm");	
					}
				else
					{
					FunctionsClass.DisplayMessage(Page,"Incorrect Login Details.  All login details are case sensitive");
					ClearFormInformation();
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
		#endregion



		#region Form Events
		//---------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the page is 
		//initialised
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{
				Session["UserName"] = "";
				ClearFormInformation();
				FunctionsClass.SetControlFocus("edtUserName", this);
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
		//---------------------------------------------------------------------
		//User has clicked the join info button
		//---------------------------------------------------------------------
		private void btnJoin_Click(object sender, System.EventArgs e)
		{
		Server.Transfer("wfRegForm1.aspx");
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