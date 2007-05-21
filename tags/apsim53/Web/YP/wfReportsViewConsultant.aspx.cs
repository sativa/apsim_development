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
	/// Summary description for wfReportsViewConsultant.
	/// </summary>
	public class wfReportsViewConsultant : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.DropDownList cboYear;
		protected System.Web.UI.WebControls.Label lblYear;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnManageGrowers;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnShow;
		protected System.Web.UI.WebControls.Button btnDelete;
		protected System.Web.UI.WebControls.Button btnFind;
		protected System.Web.UI.WebControls.Label lblFind;
		protected System.Web.UI.WebControls.TextBox edtFind;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnDeleteTwo;
		protected System.Web.UI.WebControls.Button btnShowTwo;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.ImageButton btnHelpYear;
		protected System.Web.UI.WebControls.ImageButton btnHelpGrid;
		protected System.Web.UI.WebControls.ImageButton btnHelpFind;
		protected System.Web.UI.WebControls.ImageButton btnHelpConstulantReportViewPage;
		protected Janus.Web.GridEX.GridEX grdReports;
	


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
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.grdReports.DataBinding += new System.EventHandler(this.grdReports_DataBinding);
			this.cboYear.SelectedIndexChanged += new System.EventHandler(this.cboYear_SelectedIndexChanged);
			this.btnFind.Click += new System.EventHandler(this.btnFind_Click);
			this.btnShow.Click += new System.EventHandler(this.btnShow_Click);
			this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
			this.btnShowTwo.Click += new System.EventHandler(this.btnShow_Click);
			this.btnDeleteTwo.Click += new System.EventHandler(this.btnDelete_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions
		//-------------------------------------------------------------------------
		//Gets the current year and sets the Year Combo box to display this year
		//-------------------------------------------------------------------------
		private void SetYearComboBoxToCurrentYear()
		{
			string szYear = DateTime.Today.Year.ToString();
			for(int iIndex = 0; iIndex < cboYear.Items.Count; iIndex++)
			{
				if(szYear == cboYear.Items[iIndex].Text)
				{
					cboYear.SelectedIndex = iIndex;
					break;
				}
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public DataTable ReturnsReportSelection()
		{
			DataTable dtReports = new DataTable("Reports");
			dtReports.Columns.Add("ReportName");
			dtReports.Columns.Add("UserName");
			dtReports.Columns.Add("ReportYear");

			int iRowPosition = 0;
			DataRow drSelectedReport;
			for(int iIndex = 0; iIndex < grdReports.SelectedItems.Count; iIndex++)
			{
				iRowPosition = grdReports.SelectedItems[iIndex].Position;
				drSelectedReport = dtReports.NewRow();
				drSelectedReport["UserName"] = grdReports.GetRow(iRowPosition).Cells["UserName"].Text;
				drSelectedReport["ReportName"] = grdReports.GetRow(iRowPosition).Cells["ReportName"].Text;
				drSelectedReport["ReportYear"] = cboYear.SelectedValue;
				if(drSelectedReport["ReportName"].ToString() != "")
				{
					dtReports.Rows.Add(drSelectedReport);
				}
			}
			return dtReports;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void TransferToReportViewingPage()
		{
			//As users can select names, that have no reports.  We first
			//run the return paddock selection to ensure that only paddocks
			//are selected not users.  This is why we cant check straight
			//from the grid
			DataTable dtSelectedReports = ReturnsReportSelection();
			if(dtSelectedReports.Rows.Count > 0)
			{
				Server.Transfer("wfDisplayReport.aspx");
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "Please select a report first");
			}
		}
		//-------------------------------------------------------------------------
		//Gets all the users for the specified user and then sets these to the grid
		//-------------------------------------------------------------------------
		private void FillReportsGrid()
		{
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

			dtAssignedUsers.Columns.Add("ReportName");
			
			DataTable dtCopyOfAssignedUsers = dtAssignedUsers.Copy();


			DataTable dtReports;
			foreach(DataRow drCopyOfAssignedUser in dtCopyOfAssignedUsers.Rows)
			{
				dtReports = ReportClass.GetReportsOfUser(drCopyOfAssignedUser["UserName"].ToString(), 
					Convert.ToInt32(cboYear.SelectedItem.Text));
				foreach(DataRow drReport in dtReports.Rows)
				{
					drAssignedUser = dtAssignedUsers.NewRow();
					drAssignedUser["ID"] = iStartValue;
					drAssignedUser["Name"] = "";
					drAssignedUser["UserName"] = drCopyOfAssignedUser["UserName"].ToString();
					drAssignedUser["ReportName"] = drReport["Name"].ToString();
					drAssignedUser["ParentID"] = drCopyOfAssignedUser["ID"].ToString();
					dtAssignedUsers.Rows.Add(drAssignedUser);
					iStartValue++;
				}
			}
			ViewState["NumberOfRows"] = dtAssignedUsers.Rows.Count.ToString();
			
			this.grdReports.DataSource =  dtAssignedUsers;
		}
		//-------------------------------------------------------------------------
		//Deletes the selected reports from the database and the file system.
		//-------------------------------------------------------------------------	
		private void DeleteReport()
		{
			try
			{
				if(FunctionsClass.IsReadOnly() == false)
				{
					//If a report is selected then remove it from the database
					DataTable dtSelectedReports = ReturnsReportSelection();
					if(dtSelectedReports.Rows.Count > 0)
					{
						foreach(DataRow drSelectedReport in dtSelectedReports.Rows)
						{

							ReportClass.DeleteReport(drSelectedReport["UserName"].ToString(), 
								drSelectedReport["ReportName"].ToString(), Convert.ToInt32(cboYear.SelectedItem.Text));
						}//END FOREACH LOOP
						Server.Transfer("wfReportsViewConsultant.aspx");
					}
					//If no report is selected then display an error message to the user
					else
						throw new Exception("No Report Selected");
				}
				else
					throw new Exception(FunctionsClass.ReturnReadOnlyMessage());
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
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
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForConsultantLevelPriviledges();
				HelpClass.SetHelpForConsultantReportViewPage(btnHelpYear, btnHelpGrid, 
					btnHelpFind, btnHelpConstulantReportViewPage);

				ViewState["NumberOfRows"] = 0;
				ViewState["PreviousSearchName"] = "";
				ViewState["PreviousSearchRowIndex"] = 0;

				SetYearComboBoxToCurrentYear();
				grdReports.DataBind();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetDisplayBanner(imgBanner);

				btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected report(s) \");");
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
		private void cboYear_SelectedIndexChanged(object sender, System.EventArgs e)
		{
		grdReports.DataBind();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void grdReports_DataBinding(object sender, System.EventArgs e)
		{
		FillReportsGrid();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnShow_Click(object sender, System.EventArgs e)
		{
		TransferToReportViewingPage();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnDelete_Click(object sender, System.EventArgs e)
		{
		DeleteReport();
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
					grdReports);
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}

			ViewState["PreviousSearchRowIndex"] = iPreviousSearchRowIndex;
			ViewState["PreviousSearchName"] = szPreviousSearchName;
		}
		//-------------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE