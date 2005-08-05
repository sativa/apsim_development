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
	/// Summary description for wfEditPassword.
	/// </summary>
	public class wfEditPassword : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblPasswordTwo;
		protected System.Web.UI.WebControls.Label lblPasswordOne;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.TextBox edtPasswordTwo;
		protected System.Web.UI.WebControls.TextBox edtPasswordOne;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;


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
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
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
			//If the password isn't empty run the next check
			if(edtPasswordOne.Text != "")
				{
				//If both passwords are the same, save the new password into the database
				if(edtPasswordOne.Text == edtPasswordTwo.Text)
					{
					try
						{
						DataAccessClass.UpdateUser("", "", InputValidationClass.ValidateString(edtPasswordOne.Text),
							FunctionsClass.GetActiveUserName(), "", null, null);
						if(Session["SelectedUserName"].ToString() == FunctionsClass.GetActiveUserName())
							{
							Server.Transfer("wfEditUser.aspx");
							}
						else
							{
							Server.Transfer("wfEditUserDetails.aspx");
							}
						}
					catch(Exception E)
						{
						FunctionsClass.DisplayMessage(Page, E.Message);
						}
					}
				//If both passwords are not the same, then display an error to the user
				else
					{
					FunctionsClass.DisplayMessage(Page, "Passwords do not match");
					}
				}
			//If the password is empty, then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter a password");
				}
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.SetControlFocus("edtPasswordOne", this);
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the the save button the password is updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SavePassword();
			}
		//-------------------------------------------------------------------------
		//When the user presses the the save image the password is updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePassword();
			}
		//-------------------------------------------------------------------------
		//When the cancel button is pressed by the user, send the user back to the 
		//user details page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditUserDetails.aspx");
			}
		//-------------------------------------------------------------------------
		//When the cancel image is pressed by the user, send the user back to the 
		//user details page.
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditUserDetails.aspx");
			}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
