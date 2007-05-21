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
	/// Summary description for wfManageGrowers.
	/// </summary>
	public class wfManageGrowers : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnEditReports;
		protected System.Web.UI.WebControls.Button btnEditPaddocks;
		protected System.Web.UI.WebControls.Button btnFind;
		protected System.Web.UI.WebControls.TextBox edtFind;
		protected System.Web.UI.WebControls.Label lblFind;
		protected System.Web.UI.WebControls.Panel pnlAdminOptions;
		protected System.Web.UI.WebControls.Button btnAddUser;
		protected System.Web.UI.WebControls.Button btnEditUser;
		protected System.Web.UI.WebControls.Button btnDeleteUser;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnEditPaddocksTwo;
		protected System.Web.UI.WebControls.Button btnEditReportsTwo;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.ImageButton btnHelpFind;
		protected System.Web.UI.WebControls.ImageButton btnHelpManageGrowerPage;
		protected Janus.Web.GridEX.GridEX grdUsers;
	

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
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.grdUsers.PreRender += new System.EventHandler(this.grdUsers_PreRender);
			this.grdUsers.DataBinding += new System.EventHandler(this.grdUsers_DataBinding);
			this.btnFind.Click += new System.EventHandler(this.btnFind_Click);
			this.btnEditPaddocks.Click += new System.EventHandler(this.btnEditPaddocks_Click);
			this.btnEditReports.Click += new System.EventHandler(this.btnEditReports_Click);
			this.btnAddUser.Click += new System.EventHandler(this.btnAddUser_Click);
			this.btnEditUser.Click += new System.EventHandler(this.btnEditUser_Click);
			this.btnDeleteUser.Click += new System.EventHandler(this.btnDeleteUser_Click);
			this.btnEditPaddocksTwo.Click += new System.EventHandler(this.btnEditPaddocks_Click);
			this.btnEditReportsTwo.Click += new System.EventHandler(this.btnEditReports_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions

		//-------------------------------------------------------------------------
		//Gets all the users for the specified user and then sets these to the grid
		//-------------------------------------------------------------------------
		private void FillGrowersGrid()
		{
			//Sets up the form
			string szConsultantName = Session["UserName"].ToString();
			if(FunctionsClass.IsAdministrator() == true)
			{
				szConsultantName = "";
			}
			//Returns all the users assigned to the passed through username.  NOTE: for adminsitrators
			//it passess through "" which returns all users.
			DataTable dtAssignedUsers = DataAccessClass.GetUsersMappedToConsultant(szConsultantName);
			DataTable dtOtherUsers = DataAccessClass.GetUsersNotMappedToConsultant(szConsultantName);
			//If there are duplicate values, the tree view doesn't work correctly so in one table
			//we alter the ID values.  NOTE: this means that we can not rely on ID values of users, 
			//this shouldn't be a problem as we use UserName instead to find users.
			int iStartValue = 10000;
			foreach(DataRow drTempUser in dtAssignedUsers.Rows)
			{
				drTempUser["ID"] = iStartValue;
				iStartValue++;
			}
			DataRow drAssignedUser;
			//Copies across the other users datatable into the assigned users datatable
			foreach(DataRow drOtherUser in dtOtherUsers.Rows)
			{
				drAssignedUser = dtAssignedUsers.NewRow();
				drAssignedUser["ID"] = drOtherUser["ID"].ToString();
				drAssignedUser["Name"] = drOtherUser["Name"].ToString();
				drAssignedUser["UserName"] = drOtherUser["UserName"].ToString();
				drAssignedUser["ParentID"] = 0;
				dtAssignedUsers.Rows.Add(drAssignedUser);
			}
			//Sets the number of rows to a view state variable
			//this is used for the searching functions on this page.
			ViewState["NumberOfRows"] = dtAssignedUsers.Rows.Count.ToString();
			this.grdUsers.DataSource =  dtAssignedUsers;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetSelectedUser()
		{
			if(grdUsers.SelectedItems.Count > 0)
			{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
			}
			else
				FunctionsClass.DisplayMessage(Page, "You are not allowed to delete your self.");
		}
		//-------------------------------------------------------------------------
		//Delete the selected user from the database.
		//-------------------------------------------------------------------------
		private void DeleteUser()
		{
			try
			{
				//If the grower the user is attempting to delete is not the current user
				//then delete the user
				if(Session["SelectedUserName"].ToString() != Session["UserName"].ToString())
				{
					DataAccessClass.DeleteUser(Session["SelectedUserName"].ToString());		
					ReportClass.DeleteUsersReportDirectory(Session["SelectedUserName"].ToString());
					Session["SelectedUserName"] = "";
					Server.Transfer("wfManageGrowers.aspx");
				}
				else
					throw new Exception("You are not allowed to delete your self.");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetAdminPanel()
		{
			if(FunctionsClass.IsAdministrator() == true)
				pnlAdminOptions.Visible = true;
			else
				pnlAdminOptions.Visible = false;
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
				FunctionsClass.CheckForConsultantLevelPriviledges();
				HelpClass.SetHelpForManageGrowersPage(btnHelpFind, btnHelpManageGrowerPage);

				ViewState["NumberOfRows"] = 0;
				ViewState["PreviousSearchName"] = "";
				ViewState["PreviousSearchRowIndex"] = 0;
				ViewState["SetPosition"] = false;

				grdUsers.DataBind();
				FunctionsClass.SetHeadingString(lblHeading);
				SetAdminPanel();
				FunctionsClass.SetDisplayBanner(imgBanner);
				
				if(Session["SelectedUserName"].ToString() != "" && Session["SelectedUserName"].ToString() != null)
				{
					ViewState["SetPosition"] = true;
				}			

				btnDeleteUser.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected user) \");");
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
		private void grdUsers_DataBinding(object sender, System.EventArgs e)
		{
			FillGrowersGrid();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnFind_Click(object sender, System.EventArgs e)
		{
			string szPreviousSearchName = ViewState["PreviousSearchName"].ToString();
			int iPreviousSearchRowIndex = Convert.ToInt32(ViewState["PreviousSearchRowIndex"]);

			try
			{
				FunctionsClass.FindUserOnGrid(edtFind.Text, ref szPreviousSearchName, 
					Convert.ToInt32(ViewState["NumberOfRows"]), ref iPreviousSearchRowIndex, 
					grdUsers);
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}

			ViewState["PreviousSearchRowIndex"] = iPreviousSearchRowIndex;
			ViewState["PreviousSearchName"] = szPreviousSearchName;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdUsers_PreRender(object sender, System.EventArgs e)
		{
			if(Session["SelectedUserName"].ToString() != "")
			{
				bool bSetPosition = Convert.ToBoolean(ViewState["SetPosition"]);

				try
				{
					FunctionsClass.PreSetSelectedUser(ref bSetPosition, 
						Session["SelectedUserName"].ToString(), 
						Convert.ToInt32(ViewState["NumberOfRows"]), grdUsers);
				}
				catch(Exception E)
				{
					FunctionsClass.DisplayMessage(Page, E.Message);
				}
				ViewState["SetPosition"] = bSetPosition;
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddUser_Click(object sender, System.EventArgs e)
		{
			Session["SelectedUserName"] = "";
			Session["SelectedPaddockName"] = "";
			Server.Transfer("wfAdminUserAdd.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditUser_Click(object sender, System.EventArgs e)
		{
			SetSelectedUser();
			Server.Transfer("wfAdminUserEdit.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeleteUser_Click(object sender, System.EventArgs e)
		{
			SetSelectedUser();
			DeleteUser();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditPaddocks_Click(object sender, System.EventArgs e)
		{
			SetSelectedUser();
			Server.Transfer("wfPaddocksMenu.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditReports_Click(object sender, System.EventArgs e)
		{
			SetSelectedUser();
			Server.Transfer("wfReportsMenu.aspx");	
		}
		//-------------------------------------------------------------------------
		#endregion



	}//END OF CLASS
}//END OF NAMESPACE
