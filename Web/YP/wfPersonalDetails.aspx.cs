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
	/// Summary description for wfPersonalDetails.
	/// </summary>
	public class wfPersonalDetails : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Button btnPassword;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.DropDownList cboStartPages;
		protected System.Web.UI.WebControls.Label lblStartPage;
		protected System.Web.UI.WebControls.ImageButton imgHelpEmail;
		protected System.Web.UI.WebControls.ImageButton imgHelpName;
		protected System.Web.UI.WebControls.ImageButton imgHelpStartPage;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpPersonalDetailsPage;
		protected System.Web.UI.WebControls.Panel pnlNavigatoinMenu;
	


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
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnPassword.Click += new System.EventHandler(this.btnPassword_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillStartPagesCombo()
		{
			try
			{
				DataTable dtStartPages = DataAccessClass.GetAllValidStartPagesForAccessType(Session["AccessType"].ToString());
				cboStartPages.DataSource = dtStartPages;
				cboStartPages.DataTextField = "DisplayName";
				cboStartPages.DataValueField = "Name";
				cboStartPages.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}

		}
		//-------------------------------------------------------------------------
		//Updates the user's details in the database
		//-------------------------------------------------------------------------
		private void SaveUserDetails()
		{
			try
			{
				if(FunctionsClass.IsReadOnly() == false)
				{
					//If both text boxes contain text then the details are updated in the
					//database
					if(edtName.Text != "" && edtEmail.Text != "" && cboStartPages.SelectedValue != "")
					{
						//As the user name is used in the file name of any reports generated
						//a check is run to ensure it doesn't have any characters in it
						//that will stop a file from being created.
						if(InputValidationClass.IsInputAValidFileLocationString(edtName.Text) == true)
						{
							DataAccessClass.UpdateUser(InputValidationClass.ValidateString(edtName.Text), 
								InputValidationClass.ValidateString(edtEmail.Text), "", "", 
								Session["UserName"].ToString(), "", null, cboStartPages.SelectedValue, -1, null, null);
							Server.Transfer("wfPersonalDetails.aspx");
						}
						else
							throw new Exception(InputValidationClass.ReturnInvalidLocationMessage("Your name"));
					}
					else
						throw new Exception("Please enter text in all fields");
				}
				else
					throw new Exception(FunctionsClass.ReturnReadOnlyMessage());
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Fills the form with the user's details from the database
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			try
			{
				FillStartPagesCombo();
				Session["SelectedUserName"] = "";
				DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(Session["UserName"].ToString());
				edtName.Text = dtUserDetails.Rows[0]["Name"].ToString();
				edtEmail.Text = dtUserDetails.Rows[0]["Email"].ToString();
				cboStartPages.SelectedValue = dtUserDetails.Rows[0]["StartPageName"].ToString();
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

				FillForm();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
				HelpClass.SetHelpForPersonalDetailsPage(imgHelpName, imgHelpEmail, imgHelpStartPage, 
					btnHelpPersonalDetailsPage);
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
		private void btnPassword_Click(object sender, System.EventArgs e)
		{
			if(FunctionsClass.IsReadOnly() == false)
			{
				Server.Transfer("wfPersonalPassword.aspx");
			}
			else
				FunctionsClass.DisplayMessage(Page, FunctionsClass.ReturnReadOnlyMessage());
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SaveUserDetails();
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
