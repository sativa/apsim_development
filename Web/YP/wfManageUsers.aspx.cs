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
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private string SetupForm()
			{
			string szConsultantName = "";
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
			return szConsultantName;
			}
		//-------------------------------------------------------------------------
		//
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
		//
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
		//
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
		//
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
		//
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
		//
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
					 Session["SelectedUserName"] = "";
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
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForConsultantLevelPriviledges();
				Session["SelectedUserName"] = "";
				Session["SelectedPaddockName"] = "";
				this.DataBind(); 
				}
			//Adds an attribute to the four delete buttons that causes a 
			//confirmation warning to appear when the user presses the buttons
			btnDeleteUser.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected grower \");");
			btnDeleteUserImg.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected grower \");");
			btnDeletePaddock.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected paddock \");");	
			btnDeletePaddockImg.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected paddock \");");
			}
		//-------------------------------------------------------------------------
		//
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
		//
		//-------------------------------------------------------------------------
		private void grdUsers_SelectionChanged(object sender, System.EventArgs e)
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				grdPaddocks.DataBind();
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdUsers_DataBinding(object sender, System.EventArgs e)
			{
			string szConsultantName = SetupForm();
			DataTable dtAssignedUsers = DataAccessClass.GetUsersMappedToConsultant(szConsultantName);
			DataTable dtOtherUsers = DataAccessClass.GetUsersNotMappedToConsultant(szConsultantName);
			int iStartValue = 10000;
			foreach(DataRow drTempUser in dtAssignedUsers.Rows)
				{
				drTempUser["ID"] = iStartValue;
				iStartValue++;
				}
			DataRow drAssignedUser;
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
			this.grdUsers.DataSource =  dtAssignedUsers;
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void RadioButtonList1_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			this.DataBind(); 
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
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
