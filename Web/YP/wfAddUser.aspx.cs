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
	/// Summary description for wfAddConsultant.
	/// </summary>
	public class wfAddUser : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.Label lblUserName;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.DropDownList cboAccessType;
		protected System.Web.UI.WebControls.Label lblAccess;
		protected System.Web.UI.WebControls.ListBox lstConsultants;
		protected System.Web.UI.WebControls.Label lblConsultantTwo;
		protected System.Web.UI.WebControls.Label lblConsultant;
		

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
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.cboAccessType.SelectedIndexChanged += new System.EventHandler(this.cboAccessType_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
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
			lblConsultant.Visible = true;
			lblConsultantTwo.Visible = true;
			lstConsultants.Visible = true;
			FillConsultantsListBox();
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
		//---------------------------------------------------------------------------
		//Saves a new user, but firstly a check is run to see what kind of
		//user is being added, if a grower is being added then the grower is saved to
		//the database and linked to the selected consultant.  If it is any other user
		//they are saved directly to the database.
		//---------------------------------------------------------------------------
		private void SaveUser()
			{
			try
				{
				//Checks to ensure all fields have data entered in them
				if(edtName.Text != "" && edtEmail.Text != "" &&
					edtUserName.Text != "" && edtPassword.Text != "")
					{
					//As the user name is used in the file name of any reports generated
					//a check is run to ensure it doesn't have any characters in it
					//that will stop a file from being created.
					if(InputValidationClass.IsInputAValidFileLocationString(edtName.Text) == true)
						{
						//Checks to ensure the user name isn't already in use
						if(DataAccessClass.IsUserNameAvailable(InputValidationClass.ValidateString(edtUserName.Text)) == true)
							{
							if(cboAccessType.SelectedValue != "")
								{
								DataAccessClass.InsertUser(InputValidationClass.ValidateString(edtName.Text), 
									InputValidationClass.ValidateString(edtEmail.Text), 
									InputValidationClass.ValidateString(edtUserName.Text),
									InputValidationClass.ValidateString(edtPassword.Text), 
									cboAccessType.SelectedItem.Text, ReturnConsultantCollection());
								}
							else
								throw new Exception("Please select an access type");	
							ReportClass.CreateUsersReportDirectory(InputValidationClass.ValidateString(edtUserName.Text));
							Server.Transfer("wfManageUsers.aspx");
							}
						else
							throw new Exception("Username is already being used");
						}
					else
						throw new Exception("Report Description contains invalid characters. Please remove any of the following characters \\\\ / : * \" ? \\' # < > |");
					}
				else
					throw new Exception("Please enter all details");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
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
		//---------------------------------------------------------------------------
		#endregion



		#region Form Events
		//---------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//---------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForConsultantLevelPriviledges();
				FunctionsClass.SetControlFocus("edtName", this);
				FillAccessTypeCombo();
				if(FunctionsClass.IsConsultant(Session["UserName"].ToString()) == true)
					{
					if(cboAccessType.Items.Count > 0)
						{
						cboAccessType.SelectedValue = FunctionsClass.szGrower;
						}
					cboAccessType.Enabled = false;
					}
				HideConsultantListBox();
				}
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
		//---------------------------------------------------------------------------
		//When the user presses the save button, a new user is saved to the database
		//---------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveUser();
			}
		//---------------------------------------------------------------------------
		//When the user presses the save image, a new user is saved to the database
		//---------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveUser();
			}
		//---------------------------------------------------------------------------
		//Restores the form to its original state
		//---------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfManageUsers.aspx");
			}
		//---------------------------------------------------------------------------
		//Restores the form to its original state
		//---------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfManageUsers.aspx");
			}
		//---------------------------------------------------------------------------
		#endregion


		//---------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
