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
using System.Collections.Specialized;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for wfEditGrower.
	/// </summary>
	public class wfEditUser : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.ListBox lstConsultants;
		protected System.Web.UI.WebControls.DropDownList cboAccessType;
		protected System.Web.UI.WebControls.Label lblConsultantTwo;
		protected System.Web.UI.WebControls.Label lblConsultant;
		protected System.Web.UI.WebControls.Label lblAccess;
		protected System.Web.UI.WebControls.Button btnPassword;
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
			this.cboAccessType.SelectedIndexChanged += new System.EventHandler(this.cboAccessType_SelectedIndexChanged);
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
				FillAccessTypeCombo();
				DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(Session["SelectedUserName"].ToString());
				edtName.Text = dtUserDetails.Rows[0]["Name"].ToString();
				edtEmail.Text = dtUserDetails.Rows[0]["Email"].ToString();
				string szAccessType = DataAccessClass.GetAccessTypeOfUser(FunctionsClass.GetActiveUserName());
				if(szAccessType != "")
					cboAccessType.SelectedValue = szAccessType;
				if(szAccessType == FunctionsClass.szGrower || szAccessType == FunctionsClass.szVisitor)
					{
					DisplayConsultantListBox();
					SelectUsersConsultants();
					}
				else
					{
					HideConsultantListBox();
					}
				
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//---------------------------------------------------------------------------
		//Fills the AccessType combo box with all the access types from the database
		//---------------------------------------------------------------------------
		private void FillAccessTypeCombo()
		{
			try
			{
				DataTable dtAccessTypes = DataAccessClass.GetAllAccessTypes();
				cboAccessType.DataSource = dtAccessTypes;
				cboAccessType.DataTextField = "Type";
				cboAccessType.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------------
		//Fills the consultant list box with all the consultants from the database
		//---------------------------------------------------------------------------
		private void FillConsultantsListBox()
			{
			try
				{
				DataTable dtConsultants = DataAccessClass.GetAllConsultants();
				lstConsultants.DataSource = dtConsultants;
				lstConsultants.DataTextField = "Name";
				lstConsultants.DataValueField = "UserName";
				lstConsultants.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//---------------------------------------------------------------------------
		//Makes the consultant list box and label visible to the user
		//---------------------------------------------------------------------------
		private void DisplayConsultantListBox()
		{
			FillConsultantsListBox();
			lblConsultant.Visible = true;
			lblConsultantTwo.Visible = true;
			lstConsultants.Visible = true;	
		}
		//---------------------------------------------------------------------------
		//Hides the consultant list box and label from the user
		//---------------------------------------------------------------------------
		private void HideConsultantListBox()
		{
			lblConsultant.Visible = false;
			lblConsultantTwo.Visible = false;
			lstConsultants.Visible = false;
		}
		//-------------------------------------------------------------------------
		//Updates the existing grower's details in the database.
		//-------------------------------------------------------------------------
		private void SaveExistingGrower()
			{
			if(edtName.Text != "" && edtEmail.Text != "")
				{
				try
					{
					DataAccessClass.UpdateUser(InputValidationClass.ValidateString(edtName.Text), 
						InputValidationClass.ValidateString(edtEmail.Text), "", FunctionsClass.GetActiveUserName(),
						ReturnConsultantCollection());
					Session["SelectedUserName"] = "";
					Server.Transfer("wfManageUsers.aspx");
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
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private StringCollection ReturnConsultantCollection()
			{
			StringCollection scConsultants = new StringCollection();
			if(FunctionsClass.IsAdministrator(Session["UserName"].ToString()) == true)
				{
				foreach(ListItem lsiConsultants in lstConsultants.Items)
					{
					if(lsiConsultants.Selected == true)
						{
						scConsultants.Add(lsiConsultants.Value);
						}
					}
				}
			else if(FunctionsClass.IsConsultant(Session["UserName"].ToString()) == true)
				{
				scConsultants.Add(Session["UserName"].ToString());
				}
			return scConsultants;
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SelectUsersConsultants()
			{
			DataTable dtUsersConsultants = DataAccessClass.GetUsersConsultants(FunctionsClass.GetActiveUserName());
			foreach(DataRow drConsultant in dtUsersConsultants.Rows)
				{
				foreach(ListItem lsiConsultant in lstConsultants.Items)
					{
					if(lsiConsultant.Value == drConsultant["UserName"].ToString())
						{
						lsiConsultant.Selected = true;
						}
					}	
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
				if(FunctionsClass.IsConsultant(Session["UserName"].ToString()) == true)
					{
					if(cboAccessType.Items.Count > 0)
						{
						cboAccessType.SelectedValue = FunctionsClass.szGrower;
						}
					cboAccessType.Enabled = false;
					}
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
			Server.Transfer("wfManageUsers.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel image, they are sent back the view 
		//grower's page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfManageUsers.aspx");
			}
		//---------------------------------------------------------------------------
		//When the user changes the access type of the user they wish to add, a check
		//is run to determine if that access type is that of a grower.  If it is, then
		//The consultant combo box is made visible
		//---------------------------------------------------------------------------
		private void cboAccessType_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			if(cboAccessType.SelectedItem.Text == FunctionsClass.szGrower ||
				cboAccessType.SelectedItem.Text == FunctionsClass.szVisitor)
				{
				DisplayConsultantListBox();
				}
			else
				{
				HideConsultantListBox();
				}
			}	
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------	
		}//END CLASS
	}//END OF NAMESPACE
