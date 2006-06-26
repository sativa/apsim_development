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
	/// Summary description for wfAdminUserEdit.
	/// </summary>
	public class wfAdminUserEdit : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblEmail;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected System.Web.UI.WebControls.TextBox edtName;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.DropDownList cboAccessType;
		protected System.Web.UI.WebControls.Label lblConsultantTwo;
		protected System.Web.UI.WebControls.Label lblConsultant;
		protected System.Web.UI.WebControls.Label lblAccess;
		protected System.Web.UI.WebControls.Button btnPassword;
		protected System.Web.UI.WebControls.Label lblUsersCrops;
		protected System.Web.UI.WebControls.Label lblUsersCropsTwo;
		protected System.Web.UI.WebControls.ListBox lstUsersCrops;
		protected System.Web.UI.WebControls.TextBox edtUserName;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected Janus.Web.GridEX.GridEX grdConsultants;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.CheckBox chkReadOnly;
		protected System.Web.UI.WebControls.Label lblBannerImage;
		protected System.Web.UI.WebControls.DropDownList cboBannerImage;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Label lblUserName;



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
			this.btnManageGrowers.Click += new System.EventHandler(this.NavigationButtonClick);
			this.cboAccessType.SelectedIndexChanged += new System.EventHandler(this.cboAccessType_SelectedIndexChanged);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnPassword.Click += new System.EventHandler(this.btnPassword_Click);
			this.grdConsultants.PreRender += new System.EventHandler(this.grdConsultants_PreRender);
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
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
				FillCropsListBox();
				FillBannerImageCombo();
				DataTable dtUserDetails = DataAccessClass.GetDetailsOfUser(Session["SelectedUserName"].ToString());
				edtName.Text = dtUserDetails.Rows[0]["Name"].ToString();
				edtUserName.Text = Session["SelectedUserName"].ToString();
				edtEmail.Text = dtUserDetails.Rows[0]["Email"].ToString();
				chkReadOnly.Checked = Convert.ToBoolean(dtUserDetails.Rows[0]["ReadOnly"]);
				cboBannerImage.SelectedValue = dtUserDetails.Rows[0]["BannerImageFileName"].ToString();
				SelectUsersCrops();
				string szAccessType = DataAccessClass.GetAccessTypeOfUser(FunctionsClass.GetActiveUserName());
				if(szAccessType != "")
					cboAccessType.SelectedValue = szAccessType;
				if(szAccessType == FunctionsClass.szGrower)
				{
					DisplayConsultantGrid();
				}
				else
				{
					HideConsultantGrid();
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
				ViewState["NumberOfConsultants"] = dtConsultants.Rows.Count;
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
		//Makes the consultant list box and label visible to the user
		//---------------------------------------------------------------------------
		private void DisplayConsultantGrid()
		{
			lblConsultant.Visible = true;
			lblConsultantTwo.Visible = true;
			grdConsultants.Visible = true;
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
		//-------------------------------------------------------------------------
		//Updates the existing grower's details in the database.
		//-------------------------------------------------------------------------
		private void SaveExistingGrower()
			{
			if(edtName.Text != "" && edtEmail.Text != "" && edtUserName.Text != "")
				{
				try
					{
					if(edtUserName.Text == Session["SelectedUserName"].ToString() || DataAccessClass.IsUserNameAvailable(InputValidationClass.ValidateString(edtUserName.Text))== true)
						{
						DataAccessClass.UpdateUser(InputValidationClass.ValidateString(edtName.Text), 
							InputValidationClass.ValidateString(edtEmail.Text), 
							InputValidationClass.ValidateString(edtUserName.Text), 
							"", FunctionsClass.GetActiveUserName(),
							cboAccessType.SelectedValue, cboBannerImage.SelectedValue, 
							null, Convert.ToInt32(chkReadOnly.Checked),
							ReturnConsultantDataTable(), ReturnUsersCropCollection());
						CheckIfEditingThemSelves();
						ChangeReportDirectoryIfNeeded();
						Session["SelectedUserName"] = InputValidationClass.ValidateString(edtUserName.Text);
						Server.Transfer("wfManageGrowers.aspx");
						}
					else
						throw new Exception("Username is already in use");
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
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SelectUsersConsultants()
			{
			try
				{
				grdConsultants.SelectedItems.Clear();
				DataTable dtUsersConsultants = DataAccessClass.GetUsersConsultants(FunctionsClass.GetActiveUserName());
				int iRowNumber = 0;
				int iNumberOfConsultants = Convert.ToInt32(ViewState["NumberOfConsultants"]);
				int iStartRow = 0;
				foreach(DataRow drConsultant in dtUsersConsultants.Rows)
					{
					iRowNumber = FunctionsClass.ReturnSelectedUsersRowIndex(drConsultant["UserName"].ToString(),
						"UserName", iNumberOfConsultants, ref iStartRow, grdConsultants);
					iStartRow = 0;
					grdConsultants.SelectedItems.Add(iRowNumber);
					grdConsultants.GetRow(iRowNumber).Cells["Email"].Value = Convert.ToBoolean(drConsultant["Email"]);
					grdConsultants.GetRow(iRowNumber).Cells["ReadOnly"].Value = Convert.ToBoolean(drConsultant["ReadOnly"]);
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void CheckIfEditingThemSelves()
		{
			if(Session["SelectedUserName"].ToString() == Session["UserName"].ToString())
			{
				Session["UserName"] = InputValidationClass.ValidateString(edtUserName.Text);
				Session["BannerImage"] = cboBannerImage.SelectedValue;
				Session["AccessType"] = cboAccessType.SelectedValue;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void ChangeReportDirectoryIfNeeded()
		{
			string szNewUserName = InputValidationClass.ValidateString(edtUserName.Text);
			string szUserName = FunctionsClass.GetActiveUserName();
			if(szUserName != szNewUserName)
			{
				ReportClass.RenameUsersReportDirectory(szUserName, szNewUserName);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SelectUsersCrops()
		{
			DataTable dtUsersCrops = DataAccessClass.GetUsersCrops(FunctionsClass.GetActiveUserName());
			foreach(DataRow drUsersCrop in dtUsersCrops.Rows)
			{
				foreach(ListItem lsiUsersCrop in lstUsersCrops.Items)
				{
					if(lsiUsersCrop.Value == drUsersCrop["Type"].ToString())
					{
						lsiUsersCrop.Selected = true;
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
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				ViewState["NumberOfConsultants"] = 0;
				FunctionsClass.SetControlFocus("edtName", this);	
				FunctionsClass.SetHeadingString(lblHeading);
				FillForm();
				FillConsultantsGrid();
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
		//When the user presses the Save button, the grower's information is updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveExistingGrower();	
			}
		//-------------------------------------------------------------------------
		//When the user presses the Cancel button, they are sent back the view 
		//grower's page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAdminUserEdit.aspx");
			}
		//---------------------------------------------------------------------------
		//When the user changes the access type of the user they wish to add, a check
		//is run to determine if that access type is that of a grower.  If it is, then
		//The consultant combo box is made visible
		//---------------------------------------------------------------------------
		private void cboAccessType_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			if(cboAccessType.SelectedValue == FunctionsClass.szGrower)
				{
				DisplayConsultantGrid();
				}
			else
				{
				HideConsultantGrid();
				}
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private void btnPassword_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAdminUserEditPassword.aspx");
			}
		//---------------------------------------------------------------------------
		//
		//---------------------------------------------------------------------------
		private void grdConsultants_PreRender(object sender, System.EventArgs e)
		{
			SelectUsersConsultants();
		}	
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------	
		}//END CLASS
	}//END OF NAMESPACE
