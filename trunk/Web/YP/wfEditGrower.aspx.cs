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
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.Label lblUserName;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Panel pnlTop;

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

		//-------------------------------------------------------------------------
		//Updates the existing grower's details in the database.
		//-------------------------------------------------------------------------
		private void SaveExistingGrower()
			{
			if(edtName.Text != "" && edtEmail.Text != "" && 
				edtUserName.Text != "" && edtPassword.Text != "")
				{
				DataAccessClass.UpdateGrower(InputValidationClass.ValidateString(edtName.Text), 
					InputValidationClass.ValidateString(edtEmail.Text), 
					InputValidationClass.ValidateString(edtUserName.Text),
					InputValidationClass.ValidateString(edtPassword.Text), 
					Session["SelectedUserID"].ToString());
				Session["SelectedUserID"] = "0";
				Server.Transfer("wfViewGrowers.aspx");
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter all details");
				}
			}
		//-------------------------------------------------------------------------
		//Gets all the details of the selected grower and fills the form with these
		//details.
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			edtName.Text = DataAccessClass.GetNameOfUser(Session["SelectedUserID"].ToString());
			edtEmail.Text = DataAccessClass.GetEmailOfUser(Session["SelectedUserID"].ToString());
			edtUserName.Text = DataAccessClass.GetUserNameOfUser(Session["SelectedUserID"].ToString());
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
		}//END CLASS
	}//END OF NAMESPACE
