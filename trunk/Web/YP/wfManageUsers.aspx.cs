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
		protected System.Web.UI.WebControls.RadioButtonList RadioButtonList1;
		protected System.Web.UI.WebControls.Label Label1;
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
			this.RadioButtonList1.SelectedIndexChanged += new System.EventHandler(this.RadioButtonList1_SelectedIndexChanged);
			this.btnAddUser.Click += new System.EventHandler(this.btnAddUser_Click);
			this.btnEditUser.Click += new System.EventHandler(this.btnEditUser_Click);
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
			//REMOVE THIS LINE AFTER TESTING IS COMPLETE
			Session["UserName"] = RadioButtonList1.SelectedValue;
			//END OF REMOVE SEGMENT
			if(FunctionsClass.IsAdministrator(Session["UserName"].ToString()) == true)
				{
				grdUsers.Tables[0].Columns["AccessType"].Visible = true;
				}
			else if(FunctionsClass.IsConsultantOrHigher(Session["UserName"].ToString()) == true)
				{
				grdUsers.Tables[0].Columns["AccessType"].Visible = false;
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
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if(!IsPostBack)
				{
				this.DataBind(); 
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdPaddocks_DataBinding(object sender, System.EventArgs e)
			{
			if(grdUsers.SelectedItems.Count > 0)
				{
				int iIndex = grdUsers.SelectedItems[0].Position;
				string szSelectedUserName = grdUsers.GetRow(iIndex).Cells["UserName"].Text;
				DataTable dtPaddocks = DataAccessClass.GetPaddocksOfUser(szSelectedUserName);
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

		private void btnAddUser_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAddUser.aspx");
		}

		private void btnEditUser_Click(object sender, System.EventArgs e)
		{
			EditUser();
		}

		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
