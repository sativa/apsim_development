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
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.Label lblUserName;
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.Button btnLogin;
		protected System.Web.UI.WebControls.Label lblWelcome;
		protected System.Web.UI.WebControls.Label lblVisiting;
		protected System.Web.UI.WebControls.Label lblVisitingDetail;
		protected System.Web.UI.WebControls.Label lblWarningTwo;
		protected System.Web.UI.WebControls.Label lblWarningOne;
		protected System.Web.UI.WebControls.HyperLink hylWarning;
		protected System.Web.UI.WebControls.Label lblFurtherDetails;
		protected System.Web.UI.WebControls.HyperLink hylEmail;
		protected System.Web.UI.WebControls.Image imgSide;

		//---------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the page is 
		//initialised
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{
				imgSide.Height = Unit.Percentage(100);
				Session["UserID"] = "0";
				ClearFormInformation();
				FunctionsClass.SetControlFocus("edtUserName", this);
				}
			}

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
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		//---------------------------------------------------------------------
		//Checks the login details of the user and if a match is found then 
		//the users details are stored in the session and the user is sent 
		//to the frames page (YieldProphet.htm)
		//---------------------------------------------------------------------
		private void btnLogin_Click(object sender, System.EventArgs e)
			{
			int iUserID = DataAccessClass.AuthenticateUser(InputValidationClass.ValidateString(edtUserName.Text),
				InputValidationClass.ValidateString(edtPassword.Text));
			ClearFormInformation();
			if(iUserID != 0)
				{
				Session["UserID"] = iUserID.ToString();
				if(ReportClass.DoesUsersReportDirectoryExisit(iUserID.ToString(), DateTime.Today.Year.ToString()) == false)
					{
					ReportClass.CreateUsersReportDirectory(iUserID, DateTime.Today.Year.ToString()); 
					}
				Response.Redirect("YieldProphet.htm");	
				}
			else
				{
				FunctionsClass.DisplayMessage(Page,"Incorrect Login Details.  All login details are case sensitive");
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
		}//END OF CLASS
	}//END OF NAMESPACE