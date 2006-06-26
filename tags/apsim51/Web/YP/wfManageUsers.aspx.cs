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
	/// Summary description for wfManageUsers.
	/// </summary>
	public class wfManageUsers : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.LinkButton btnAddUser;
		protected System.Web.UI.WebControls.ImageButton btnAddUserImg;
		protected System.Web.UI.WebControls.ImageButton btnEditUserImg;
		protected System.Web.UI.WebControls.LinkButton btnEditUser;
		protected System.Web.UI.WebControls.ImageButton btnViewReportsImg;
		protected System.Web.UI.WebControls.LinkButton btnViewReports;
		protected System.Web.UI.WebControls.ImageButton btnDeleteUserImg;
		protected System.Web.UI.WebControls.LinkButton btnDeleteUser;
		protected System.Web.UI.WebControls.Label lblUsers;
		protected System.Web.UI.WebControls.Label lblPaddocks;
		protected System.Web.UI.WebControls.ImageButton btnAddPaddockImg;
		protected System.Web.UI.WebControls.LinkButton btnAddPaddock;
		protected System.Web.UI.WebControls.ImageButton btnEditPaddockImg;
		protected System.Web.UI.WebControls.LinkButton btnEditPaddock;
		protected System.Web.UI.WebControls.ImageButton btnDeletePaddockImg;
		protected Janus.Web.GridEX.GridEX grdUsers;
		protected Janus.Web.GridEX.GridEX grdPaddocks;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.CheckBox chkSetPosition;
		protected System.Web.UI.WebControls.TextBox edtFind;
		protected System.Web.UI.WebControls.Label lblFind;
		protected System.Web.UI.WebControls.Button btnFind;
		protected System.Web.UI.WebControls.CheckBox chkRefreshUsersGrid;
		protected System.Web.UI.WebControls.LinkButton btnDeletePaddock;
		


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
			this.grdUsers.SelectionChanged += new System.EventHandler(this.grdUsers_SelectionChanged);
			this.grdUsers.PreRender += new System.EventHandler(this.grdUsers_PreRender);
			this.grdUsers.DataBinding += new System.EventHandler(this.grdUsers_DataBinding);
			this.grdPaddocks.DataBinding += new System.EventHandler(this.grdPaddocks_DataBinding);
			this.btnAddUser.Click += new System.EventHandler(this.btnAddUser_Click);
			this.btnAddUserImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnAddUserImg_Click);
			this.btnEditUserImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnEditUserImg_Click);
			this.btnEditUser.Click += new System.EventHandler(this.btnEditUser_Click);
			this.btnViewReportsImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnViewReportsImg_Click);
			this.btnViewReports.Click += new System.EventHandler(this.btnViewReports_Click);
			this.btnDeleteUserImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteUserImg_Click);
			this.btnDeleteUser.Click += new System.EventHandler(this.btnDeleteUser_Click);
			this.btnAddPaddockImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnAddPaddockImg_Click);
			this.btnAddPaddock.Click += new System.EventHandler(this.btnAddPaddock_Click);
			this.btnEditPaddockImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnEditPaddockImg_Click);
			this.btnEditPaddock.Click += new System.EventHandler(this.btnEditPaddock_Click);
			this.btnDeletePaddockImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeletePaddockImg_Click);
			this.btnDeletePaddock.Click += new System.EventHandler(this.btnDeletePaddock_Click);
			this.btnFind.Click += new System.EventHandler(this.btnFind_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Sets up the page, depending on their access type. Depending
		//on which kind of user they are returns their username.  NOTE: for adminsitrators
		//this will return "" for all other access types it will return their username;
		//-------------------------------------------------------------------------
		private string SetupForm()
			{
			string szConsultantName = "";
			//If the user is an administrator, allow them to see everything
			if(FunctionsClass.IsAdministrator(Session["UserName"].ToString()) == true)
			{
				grdUsers.Tables[0].Columns["AccessType"].Visible = true;
				grdUsers.Tables[0].Columns["Name"].Width = 250;
				btnAddUser.Enabled = true;
				btnAddUser.Enabled = true;
				btnAddUserImg.Enabled = true;
				btnEditUser.Enabled = true;
				btnEditUserImg.Enabled = true;
				btnDeleteUser.Enabled = true;
				btnDeleteUserImg.Enabled = true;
			}
			//If the user is a consultant, allow them to only edit/add/delete a paddock
			else if(FunctionsClass.IsConsultantOrHigher(Session["UserName"].ToString()) == true)
			{
				grdUsers.Tables[0].Columns["AccessType"].Visible = false;
				grdUsers.Tables[0].Columns["Name"].Width = 380;
				btnAddUser.Enabled = false;
				btnAddUser.Enabled = false;
				btnAddUserImg.Enabled = false;
				btnEditUser.Enabled = false;
				btnEditUserImg.Enabled = false;
				btnDeleteUser.Enabled = false;
				btnDeleteUserImg.Enabled = false;
				szConsultantName = Session["UserName"].ToString();
			}
			//If the user is a visitor consultant, allow them to only edit(view) a paddock
			else if(FunctionsClass.IsVisitorConsultant(Session["UserName"].ToString()) == true)
			{
				grdUsers.Tables[0].Columns["AccessType"].Visible = false;
				grdUsers.Tables[0].Columns["Name"].Width = 380;
				btnAddUser.Enabled = false;
				btnAddUser.Enabled = false;
				btnAddUserImg.Enabled = false;
				btnEditUser.Enabled = false;
				btnEditUserImg.Enabled = false;
				btnDeleteUser.Enabled = false;
				btnDeleteUserImg.Enabled = false;
				btnAddPaddock.Enabled = false;
				btnAddPaddockImg.Enabled = false;
				btnEditPaddock.Enabled = true;
				btnEditPaddockImg.Enabled = true;
				btnDeletePaddock.Enabled = false;
				btnDeletePaddockImg.Enabled = false;
				szConsultantName = Session["UserName"].ToString();
			}
			return szConsultantName;
			}
		//-------------------------------------------------------------------------
		//Transfer the user to the edit user page
		//-------------------------------------------------------------------------
		private void EditUser()
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				Server.Transfer("wfEditUser.aspx");
				}	
			}
		//-------------------------------------------------------------------------
		//Delete the selected user from the database.
		//-------------------------------------------------------------------------
		private void DeleteUser()
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				//If the grower the user is attempting to delete is not the current user
				//then delete the user
				if(Session["SelectedUserName"].ToString() != Session["UserName"].ToString())
					{
					try
						{
						DataAccessClass.DeleteUser(Session["SelectedUserName"].ToString());		
						ReportClass.DeleteUsersReportDirectory(Session["SelectedUserName"].ToString());
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
					FunctionsClass.DisplayMessage(Page, "You are not allowed to delete your self.");
					}
				}
			//If no grower is selected, then an error message is displayed to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Grower Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Transfer the user to the view report page
		//-------------------------------------------------------------------------
		private void ViewReports()
			{
			//If a grower is selected, the user is transfered to the add paddock page
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				Server.Transfer("wfViewReports.aspx");
				}
			//If no grower is selected, then an error message is sent to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Grower Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Transfer the user to the add paddock page
		//-------------------------------------------------------------------------
		private void AddPaddock()
		{
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				Server.Transfer("wfAddPaddock.aspx");
				}
			//If no grower is selected, then an error message is sent to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Grower Selected");
				}
		}
		//-------------------------------------------------------------------------
		//Transfer the user to the edit paddock page
		//-------------------------------------------------------------------------
		private void EditPaddock()
			{
			if(grdUsers.SelectedItems.Count > 0 && grdPaddocks.SelectedItems.Count > 0)
				{
				int iIndex = 0;
				//Sets the SelectedUserName session variable to identify the user later
				iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				//Sets the SelectePaddockID session variable to identify the paddock later
				iIndex = grdPaddocks.SelectedItems[0].Position;
				Session["SelectedPaddockName"] = grdPaddocks.GetRow(iIndex).Cells["Name"].Text;
				
				Server.Transfer("wfEditPaddock.aspx");
				}
				//If no paddock is selected then an error message is displayed to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Paddock Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Delete the selected paddock
		//-------------------------------------------------------------------------
		private void DeletePaddock()
			{
			//If a paddock is selected, then that paddock is removed from the database
			if(grdUsers.SelectedItems.Count > 0 && grdPaddocks.SelectedItems.Count > 0)
				{
				int iIndex = 0;
				//Sets the SelectedUserName session variable to identify the user later
				iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				//Sets the SelectePaddockID session variable to identify the paddock later
				iIndex = grdPaddocks.SelectedItems[0].Position;
				Session["SelectedPaddockName"] = grdPaddocks.GetRow(iIndex).Cells["Name"].Text;
				try
					{
					DataAccessClass.DeletePaddock(Session["SelectedPaddockName"].ToString(), Session["SelectedUserName"].ToString());
					Session["SelectedPaddockName"] = "";
					Server.Transfer("wfManageUsers.aspx");
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
				//If no paddock is selected then an error message is displayed to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Paddock Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Checks to see if the user is returning to the page, if they are, then find
		//the grower that they had last selected.  This is achieved by using the
		//SelectedUserName session variable and searching through the grid. 
		//-------------------------------------------------------------------------
		private void PreSetSelectedUser()
		{
			//Checks to see if a user is returning
			if(chkSetPosition.Checked == true)
			{
				chkSetPosition.Checked = false;
				//Find the row number of the selected user
				int iRowIndex = ReturnSelectedUsersRowIndex(Session["SelectedUserName"].ToString(), "UserName");
				SelectUserOnGrid(iRowIndex);
			}
		}
		//-------------------------------------------------------------------------
		//Find a user on the user grid.  This is achieved by using the
		//name entered into the edit box and searching through the grid. 
		//-------------------------------------------------------------------------
		private void FindUser()
		{
			if(edtFind.Text != null && edtFind.Text != "")
			{
				if(ViewState["PreviousSearchName"].ToString() != edtFind.Text)
				{
					ViewState["PreviousSearchRowIndex"] = 0;
				}
				int iRowIndex = ReturnSelectedUsersRowIndex(edtFind.Text, "Name");
				SelectUserOnGrid(iRowIndex);
				ViewState["PreviousSearchName"] = edtFind.Text;
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "No name supplied");
			}
		}
		//-------------------------------------------------------------------------
		//Find the row index where the specified name is found in the specified column
		//-------------------------------------------------------------------------
		private int ReturnSelectedUsersRowIndex(string szUserNameToFind, string szColumnToSearch)
		{
			int iRowIndex = 0;
			
			int iNumberOfRows = Convert.ToInt32(chkSetPosition.Text);
			try
			{
				if(szUserNameToFind != "" && szUserNameToFind != null)
				{
					szUserNameToFind = szUserNameToFind.ToLower();
					Janus.Web.GridEX.GridEXRow grdRow;
					for(int iIndex = Convert.ToInt32(ViewState["PreviousSearchRowIndex"].ToString()); iIndex < iNumberOfRows; iIndex++)
					{
						grdRow = grdUsers.GetRow(iIndex);
						if(grdRow.Cells[szColumnToSearch].Text.ToLower().IndexOf(szUserNameToFind) >= 0)
						{
							iRowIndex = grdRow.Position;
							ViewState["PreviousSearchRowIndex"] = iRowIndex+1;
							break;
						}
					}
					if(Convert.ToInt32(ViewState["PreviousSearchRowIndex"].ToString()) > 0 && iRowIndex == 0)
					{
						ViewState["PreviousSearchRowIndex"] = 0;
						throw new Exception("No more matching results");		
					}
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
			return iRowIndex;
		}
		//-------------------------------------------------------------------------
		//Select the row on the grid with the corresponding rowindex 
		//-------------------------------------------------------------------------
		private void SelectUserOnGrid(int iRowIndex)
		{
			//Due to a problem with not being able to count the number of rows from the
			//grid itself, we count the number of rows when we get them from the databse
			//and we store them on the page
			int iNumberOfRows = Convert.ToInt32(chkSetPosition.Text);
			grdUsers.SelectedItems.Clear();
			try
			{
				//Search through every row until we find the row that we need
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < iNumberOfRows; iIndex++)
				{
					grdRow = grdUsers.GetRow(iIndex);
					//Check to see if the row we are looking for is grouped under a consultant
					//if they are then expand this row to show the row we are looking for
					if((grdRow.Children + grdRow.Position) >= iRowIndex)
					{
						grdRow.Expanded = true;
					}
					//If it is the row we are looking for, select it and stop the search
					if(grdRow.Position == iRowIndex)
					{
						grdUsers.SelectedItems.Add(iRowIndex);
						break;
					}
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
			grdPaddocks.DataBind();
		}	
		//-------------------------------------------------------------------------


		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//When the page loads, check the level of the user and setup the page
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForVisitorConsultantLevelPriviledges();
				FunctionsClass.SetControlFocus("btnFind", Page);
				//Sets up the page, by firing the events for the grids, starts with the users grid.
				this.DataBind(); 
				if(Session["SelectedUserName"].ToString() != "" && Session["SelectedUserName"].ToString() != null)
					{
					chkSetPosition.Checked = true;
					}
				ViewState["PreviousSearchName"] = "";
				ViewState["PreviousSearchRowIndex"] = 0;
				chkRefreshUsersGrid.Checked = false;
				}

			//Adds an attribute to the four delete buttons that causes a 
			//confirmation warning to appear when the user presses the buttons
			btnDeleteUser.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected grower \");");
			btnDeleteUserImg.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected grower \");");
			btnDeletePaddock.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected paddock \");");	
			btnDeletePaddockImg.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected paddock \");");
			
			}
		//-------------------------------------------------------------------------
		//Gets all the paddocks for the specified user and then sets these
		//to the grid
		//-------------------------------------------------------------------------
		private void grdPaddocks_DataBinding(object sender, System.EventArgs e)
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				Session["SelectedUserName"] = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				DataTable dtPaddocks = DataAccessClass.GetPaddocksOfUser(Session["SelectedUserName"].ToString());
				this.grdPaddocks.DataSource = dtPaddocks;
				}
			}
		//-------------------------------------------------------------------------
		//When the selected user changes on the user grid changes, refresh the paddock grid
		//-------------------------------------------------------------------------
		private void grdUsers_SelectionChanged(object sender, System.EventArgs e)
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				//Causes the grdPaddocks_DataBinding event
				grdPaddocks.DataBind();
				}
			}
		//-------------------------------------------------------------------------
		//Gets all the users for the specified user and then sets these to the grid
		//-------------------------------------------------------------------------
		private void grdUsers_DataBinding(object sender, System.EventArgs e)
			{
			if(chkRefreshUsersGrid.Checked == true)
			{
				//Sets up the form
				string szConsultantName = SetupForm();
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
					drAssignedUser["AccessType"] = drOtherUser["AccessType"].ToString();
					drAssignedUser["UserName"] = drOtherUser["UserName"].ToString();
					drAssignedUser["ParentID"] = 0;
					dtAssignedUsers.Rows.Add(drAssignedUser);
				}
				//Sets the number of rows onto the hidden component to store
				//this is used for the searching functions on this page.
				chkSetPosition.Text = dtAssignedUsers.Rows.Count.ToString();
				this.grdUsers.DataSource =  dtAssignedUsers;
			}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddUser_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAddUser.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddUserImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfAddUser.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditUser_Click(object sender, System.EventArgs e)
			{
			EditUser();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditUserImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			EditUser();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeleteUser_Click(object sender, System.EventArgs e)
			{
			DeleteUser();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeleteUserImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteUser();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnViewReports_Click(object sender, System.EventArgs e)
			{
			ViewReports();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnViewReportsImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			ViewReports();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddPaddock_Click(object sender, System.EventArgs e)
			{
			AddPaddock();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnAddPaddockImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			AddPaddock();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditPaddock_Click(object sender, System.EventArgs e)
			{
			EditPaddock(); 
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditPaddockImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			EditPaddock();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeletePaddock_Click(object sender, System.EventArgs e)
			{
			DeletePaddock();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDeletePaddockImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeletePaddock();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdUsers_PreRender(object sender, System.EventArgs e)
		{
			if(Session["SelectedUserName"].ToString() != "")
			{
				PreSetSelectedUser();
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnFind_Click(object sender, System.EventArgs e)
		{
			FindUser();
		}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
