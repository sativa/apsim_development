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
	/// Summary description for wfEditUserDetails.
	/// </summary>
	public class wfEditUserDetails : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.Button btnPassword;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnCancel;


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
			this.btnPassword.Click += new System.EventHandler(this.btnPassword_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
		
		

		#region Form Functions
		//-------------------------------------------------------------------------
		//Updates the user's details in the database
		//-------------------------------------------------------------------------
		private void SaveUserDetails()
			{
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				//If both text boxes contain text then the details are updated in the
				//database
				if(edtName.Text != "" && edtEmail.Text != "")
					{
					//As the user name is used in the file name of any reports generated
					//a check is run to ensure it doesn't have any characters in it
					//that will stop a file from being created.
					if(InputValidationClass.IsInputAValidFileLocationString(edtName.Text) == true)
						{
						try
							{
							DataAccessClass.UpdateUser(InputValidationClass.ValidateString(edtName.Text), 
								InputValidationClass.ValidateString(edtEmail.Text), "", 
								Session["UserName"].ToString(), "", null);
							}
						catch(Exception E)
							{
							FunctionsClass.DisplayMessage(Page, E.Message);
							}
						}
					else
						{
						FunctionsClass.DisplayMessage(Page, "Your name contains invalid characters. Please remove any of the following characters \\\\ / : * ? \" < > |");
						}
					}
				//If either of the text boxes doen't contain text then an error message
				//is displayed to the user
				else
					{
					FunctionsClass.DisplayMessage(Page, "Please enter text in all fields");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
				}
			}
		//-------------------------------------------------------------------------
		//Fills the form with the user's details from the database
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			try
				{
				Session["SelectedUserName"] = "";
				DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(Session["UserName"].ToString());
				edtName.Text = dtUserDetails.Rows[0]["Name"].ToString();
				edtEmail.Text = dtUserDetails.Rows[0]["Email"].ToString();
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
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForVisitorLevelPriviledges();
				FillForm();
				FunctionsClass.SetControlFocus("edtName", this);	
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the password button, they are transfered to the 
		//Password page.
		//-------------------------------------------------------------------------
		private void btnPassword_Click(object sender, System.EventArgs e)
			{
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				Server.Transfer("wfEditPassword.aspx");
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button, the users details are updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveUserDetails();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save image, the users details are updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveUserDetails();
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button the form is refreshed to remove
		//all changes
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			FillForm();
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image the form is refreshed to remove
		//all changes
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			FillForm();
			}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
