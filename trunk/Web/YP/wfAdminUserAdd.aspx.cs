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


namespace YP2006
	{
	/// <summary>
	/// Summary description for wfAdminUserAdd.
	/// </summary>
	public class wfAdminUserAdd : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblPassword;
		protected System.Web.UI.WebControls.TextBox edtPassword;
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.Label lblUserName;
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.DropDownList cboAccessType;
		protected System.Web.UI.WebControls.Label lblAccess;
		protected System.Web.UI.WebControls.Label lblConsultantTwo;
		protected System.Web.UI.WebControls.ListBox lstUsersCrops;
		protected System.Web.UI.WebControls.Label lblUsersCropsTwo;
		protected System.Web.UI.WebControls.Label lblUsersCrops;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected Janus.Web.GridEX.GridEX grdConsultants;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.CheckBox chkReadOnly;
		protected System.Web.UI.WebControls.DropDownList cboBannerImage;
		protected System.Web.UI.WebControls.Label lblBannerImage;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
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
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.cboAccessType.SelectedIndexChanged += new System.EventHandler(this.cboAccessType_SelectedIndexChanged);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.grdConsultants.PreRender += new System.EventHandler(this.grdConsultants_PreRender);
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
		private void FillConsultantsGrid()
			{
			try
				{
				DataTable dtConsultants = DataAccessClass.GetAllConsultants();
				dtConsultants.Columns.Add("ReadOnly", System.Type.GetType("System.Boolean"));
				dtConsultants.Columns.Add("Email", System.Type.GetType("System.Boolean"));
				foreach(DataRow drConsultant in dtConsultants.Rows)
				{
					drConsultant["ReadOnly"] = false;
					drConsultant["Email"] = false;
				}
				grdConsultants.DataSource = dtConsultants;
				grdConsultants.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//---------------------------------------------------------------------------
		//Fills the crops list box with all the crops from the database
		//---------------------------------------------------------------------------
		private void FillCropsListBox()
		{
			try
			{
				DataTable dtCrops = DataAccessClass.GetAllCrops();
				lstUsersCrops.DataSource = dtCrops;
				lstUsersCrops.DataTextField = "Type";
				lstUsersCrops.DataValueField = "Type";
				lstUsersCrops.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------------
		//Fills the Banner Image combo box with all the banner images from the database
		//---------------------------------------------------------------------------
		private void FillBannerImageCombo()
		{
			try
			{
				DataTable dtBannerImages = DataAccessClass.GetAllBannerImages();
				cboBannerImage.DataSource = dtBannerImages;
				cboBannerImage.DataTextField = "FileName";
				cboBannerImage.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------------
		//Makes the consultant list box and label visible to the user
		//---------------------------------------------------------------------------
		private void DisplayConsultantGrid()
			{
			lblConsultant.Visible = true;
			lblConsultantTwo.Visible = true;
			grdConsultants.Visible = true;
			FillConsultantsGrid();
			}
		//---------------------------------------------------------------------------
		//Hides the consultant list box and label from the user
		//---------------------------------------------------------------------------
		private void HideConsultantGrid()
			{
			lblConsultant.Visible = false;
			lblConsultantTwo.Visible = false;
			grdConsultants.Visible = false;
			}
		//---------------------------------------------------------------------------
		//Saves a new user, but firstly a check is run to see what kind of
		//user is being added, if a grower is being added then the grower is saved to
		//the database and linked to the selected consultant.  If it is any other user
		//they are saved directly to the database.
		//---------------------------------------------------------------------------
		private bool SaveUser()
			{
			bool bSuccessful = false;
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
									cboAccessType.SelectedItem.Text, cboBannerImage.SelectedValue, 
									"wfMainMenu.aspx", Convert.ToInt32(chkReadOnly.Checked), 
									ReturnConsultantDataTable(), ReturnUsersCropCollection());
								ReportClass.CreateUsersReportDirectory(InputValidationClass.ValidateString(InputValidationClass.ValidateString(edtUserName.Text)));
								bSuccessful = true;
								}
							else
								throw new Exception("Please select an access type");	
							}
						else
							throw new Exception("Username is already being used");
						}
					else
						throw new Exception(InputValidationClass.ReturnInvalidLocationMessage("Grower\'s name"));
					}
				else
					throw new Exception("Please enter all details");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			return bSuccessful;
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private DataTable ReturnConsultantDataTable()
			{
			DataTable dtConsultants = new DataTable("Consultants");
			dtConsultants.Columns.Add("UserName", System.Type.GetType("System.String"));
			dtConsultants.Columns.Add("ReadOnly", System.Type.GetType("System.Boolean"));
			dtConsultants.Columns.Add("Email", System.Type.GetType("System.Boolean"));
			int iRowPosition = 0;
			DataRow drSelectedConsultant;
			for(int iIndex = 0; iIndex < grdConsultants.SelectedItems.Count; iIndex++)
			{
				iRowPosition = grdConsultants.SelectedItems[iIndex].Position;
				drSelectedConsultant = dtConsultants.NewRow();
				drSelectedConsultant["UserName"] = grdConsultants.GetRow(iRowPosition).Cells["UserName"].Text;
				drSelectedConsultant["ReadOnly"] = Convert.ToBoolean(grdConsultants.GetRow(iRowPosition).Cells["ReadOnly"].Value);
				drSelectedConsultant["Email"] = Convert.ToBoolean(grdConsultants.GetRow(iRowPosition).Cells["ReadOnly"].Value);
				dtConsultants.Rows.Add(drSelectedConsultant);
			}
			return dtConsultants;
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private StringCollection ReturnUsersCropCollection()
		{
			StringCollection scUsersCrops = new StringCollection();
			foreach(ListItem lsiUsersCrop in lstUsersCrops.Items)
			{
				if(lsiUsersCrop.Selected == true)
				{
					scUsersCrops.Add(lsiUsersCrop.Value);
				}
			}
			return scUsersCrops;
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
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FunctionsClass.SetControlFocus("edtName", this);
				FillAccessTypeCombo();
				FillBannerImageCombo();
				FillCropsListBox();
				HideConsultantGrid();
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//---------------------------------------------------------------------------
		//When the user changes the access type of the user they wish to add, a check
		//is run to determine if that access type is that of a grower.  If it is, then
		//The consultant combo box is made visible
		//---------------------------------------------------------------------------
		private void cboAccessType_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			if(cboAccessType.SelectedItem.Text == FunctionsClass.szGrower)
				{
				DisplayConsultantGrid();
				}
			else
				{
				HideConsultantGrid();
				}
			}
		//---------------------------------------------------------------------------
		//When the user presses the save button, a new user is saved to the database
		//---------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			if(SaveUser() == true)
				Server.Transfer("wfManageGrowers.aspx");
			}
		//---------------------------------------------------------------------------
		//Restores the form to its original state
		//---------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAdminUserAdd.aspx");
			}

		private void grdConsultants_PreRender(object sender, System.EventArgs e)
		{
			grdConsultants.SelectedItems.Clear();
		}
		//---------------------------------------------------------------------------
		#endregion


		//---------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
