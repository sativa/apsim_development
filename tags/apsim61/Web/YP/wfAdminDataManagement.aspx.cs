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
	/// Summary description for wfAdminDataManagement.
	/// </summary>
	public class wfAdminDataManagement : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.CheckBox chkSelectAll;
		protected System.Web.UI.WebControls.TextBox edtFind;
		protected System.Web.UI.WebControls.Label lblFind;
		protected System.Web.UI.WebControls.Label lblGrowers;
		protected System.Web.UI.WebControls.Button btnFind;
		protected Janus.Web.GridEX.GridEX grdUsers;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.CheckBoxList chklstPaddockInformation;
		protected System.Web.UI.WebControls.Button btnExport;
		protected System.Web.UI.WebControls.Button btnCopy;
		protected System.Web.UI.WebControls.Button btnPaste;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Label lblPaddockComponents;
	

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
			this.grdUsers.DataBinding += new System.EventHandler(this.grdUsers_DataBinding);
			this.btnFind.Click += new System.EventHandler(this.btnFind_Click);
			this.chkSelectAll.CheckedChanged += new System.EventHandler(this.chkSelectAll_CheckedChanged);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnAdmin.Click += new System.EventHandler(this.NavigationButtonClick);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public DataTable ReturnsPaddockSelection()
		{
			DataTable dtPaddocks = new DataTable("Paddocks");
			dtPaddocks.Columns.Add("PaddockName");
			dtPaddocks.Columns.Add("UserName");
			try
			{
				int iRowPosition = 0;
				DataRow drSelectedPaddock;
				for(int iIndex = 0; iIndex < grdUsers.SelectedItems.Count; iIndex++)
				{
					iRowPosition = grdUsers.SelectedItems[iIndex].Position;
					drSelectedPaddock = dtPaddocks.NewRow();
					drSelectedPaddock["UserName"] = grdUsers.GetRow(iRowPosition).Cells["UserName"].Text;
					drSelectedPaddock["PaddockName"] = grdUsers.GetRow(iRowPosition).Cells["PaddockName"].Text;
					if(drSelectedPaddock["PaddockName"].ToString() != "")
					{
						dtPaddocks.Rows.Add(drSelectedPaddock);
					}
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
			return dtPaddocks;
		}
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
			//Returns all the users assigned to the passed through username.  NOTE: for adminsitrators
			//it passess through "" which returns all users.
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

			dtAssignedUsers.Columns.Add("PaddockName");
			DataTable dtCopyOfAssignedUsers = dtAssignedUsers.Copy();

			DataTable dtPaddock;
			foreach(DataRow drCopyOfAssignedUser in dtCopyOfAssignedUsers.Rows)
			{
				dtPaddock = DataAccessClass.GetPaddocksOfUser(drCopyOfAssignedUser["UserName"].ToString());
				foreach(DataRow drPaddock in dtPaddock.Rows)
				{
					drAssignedUser = dtAssignedUsers.NewRow();
					drAssignedUser["ID"] = iStartValue;
					drAssignedUser["Name"] = "";
					drAssignedUser["UserName"] = drCopyOfAssignedUser["UserName"].ToString();
					drAssignedUser["PaddockName"] = drPaddock["Name"].ToString();
					drAssignedUser["ParentID"] = drCopyOfAssignedUser["ID"].ToString();
					dtAssignedUsers.Rows.Add(drAssignedUser);
					iStartValue++;
				}
			}
			ViewState["NumberOfRows"] = dtAssignedUsers.Rows.Count.ToString();
			
			this.grdUsers.DataSource =  dtAssignedUsers;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public void SelectAllPaddocks()
		{
			grdUsers.SelectedItems.Clear();
			int iNumberOfRows = Convert.ToInt32(ViewState["NumberOfRows"]);
			for(int iIndex = 0; iIndex < iNumberOfRows; iIndex++)
			{
				if(grdUsers.GetRow(iIndex).Cells["PaddockName"].Text != "")
				{
					grdUsers.SelectedItems.Add(iIndex);
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
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				ViewState["NumberOfRows"] = 0;
				ViewState["PreviousSearchName"] = "";
				ViewState["PreviousSearchRowIndex"] = 0;

				grdUsers.DataBind();
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
		private void chkSelectAll_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkSelectAll.Checked == true)
				SelectAllPaddocks();
			else
				grdUsers.SelectedItems.Clear();
		}
		//-------------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE
