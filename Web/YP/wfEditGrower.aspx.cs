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
	/// Summary description for wfEditGrower.
	/// </summary>
	public class wfEditGrower : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Panel pnlTop;



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
		//Gets all the details of the selected grower and fills the form with these
		//details.
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			try
				{
				DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(Session["SelectedUserName"].ToString());
				edtName.Text = dtUserDetails.Rows[0]["Name"].ToString();
				edtEmail.Text = dtUserDetails.Rows[0]["Email"].ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Updates the existing grower's details in the database.
		//-------------------------------------------------------------------------
		private void SaveExistingGrower()
			{
			if(edtName.Text != "" && edtEmail.Text != "" && edtPassword.Text != "")
				{
				try
					{
					DataAccessClass.UpdateGrower(InputValidationClass.ValidateString(edtName.Text), 
						InputValidationClass.ValidateString(edtEmail.Text), 
						InputValidationClass.ValidateString(edtPassword.Text), 
						FunctionsClass.GetActiveUserName());
					Session["SelectedUserName"] = "";
					Server.Transfer("wfViewGrowers.aspx");
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter all details");
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
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();	
				FunctionsClass.CheckForConsultantLevelPriviledges();
				FunctionsClass.SetControlFocus("edtName", this);
				FillForm();
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the Save button, the grower's information is updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveExistingGrower();	
			}
		//-------------------------------------------------------------------------
		//When the user presses the Save image, the grower's information is updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveExistingGrower();	
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel button, they are sent back the view 
		//grower's page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewGrowers.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel image, they are sent back the view 
		//grower's page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewGrowers.aspx");
			}	
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------	
		}//END CLASS
	}//END OF NAMESPACE
