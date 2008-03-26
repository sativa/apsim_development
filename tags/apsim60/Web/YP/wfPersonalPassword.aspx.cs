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
	/// Summary description for wfPersonalPassword.
	/// </summary>
	public class wfPersonalPassword : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.TextBox edtPasswordOne;
		protected System.Web.UI.WebControls.TextBox edtPasswordTwo;
		protected System.Web.UI.WebControls.Label lblPasswordTwo;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton imgHelpPasswordReenter;
		protected System.Web.UI.WebControls.ImageButton imgHelpPassword;
		protected System.Web.UI.WebControls.ImageButton btnHelpPasswordPage;
		protected System.Web.UI.WebControls.Label lblPasswordOne;


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
			this.btnPersonalDetailsConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		
		#region Form Functions
		//-------------------------------------------------------------------------
		//Updates the user's password, but firstly two checks are run, the first 
		//is to ensure that the user has entered a password, and the second is to
		//ensure that both the passwords entered by the user are the same.  If both
		//checks are passed, then the new password is updated into the database
		//-------------------------------------------------------------------------
		private void SavePassword()
			{
			try
				{
				//If the password isn't empty run the next check
				if(edtPasswordOne.Text != "")
					{
					//If both passwords are the same, save the new password into the database
					if(edtPasswordOne.Text == edtPasswordTwo.Text)
						{
						DataAccessClass.UpdateUser("", "", "", InputValidationClass.ValidateString(edtPasswordOne.Text),
							Session["UserName"].ToString(), "", null, null, -1, null, null);
						Server.Transfer("wfPersonalDetails.aspx");
						}
					//If both passwords are not the same, then display an error to the user
					else
						throw new Exception("Passwords do not match");
					}
				//If the password is empty, then display an error to the user
				else
					throw new Exception("Please enter a password");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		#endregion


		#region Form Events
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.CheckForWritePriviledges();

				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);

				HelpClass.SetHelpForPersonalPasswordPage(imgHelpPassword, 
					imgHelpPasswordReenter, btnHelpPasswordPage);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
		Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SavePassword();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfPersonalDetails.aspx");
		}
		//-------------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE
