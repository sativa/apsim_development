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
using Janus.Web.GridEX;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfReportsGenerateConsultant.
	/// </summary>
	public class wfReportsGenerateConsultant : System.Web.UI.Page
	{
		protected Janus.Web.GridEX.GridEX grdUsers;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.Button btnNext;
		protected System.Web.UI.WebControls.DropDownList cboReportTypes;
		protected System.Web.UI.WebControls.Label Label2;
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
		protected System.Web.UI.WebControls.Button btnFind;
		protected System.Web.UI.WebControls.Label lblFind;
		protected System.Web.UI.WebControls.TextBox edtFind;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.CheckBox chkSelectAll;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Label lblReportType;
		protected System.Web.UI.WebControls.ImageButton btnHelpFind;
		protected System.Web.UI.WebControls.ImageButton btnHelpGrid;
		protected System.Web.UI.WebControls.ImageButton btnHelpConsultantReportGeneratePage;
		protected System.Web.UI.WebControls.ImageButton btnHelpReportType;
		protected System.Web.UI.WebControls.ImageButton btnHelpSelectAll;
		protected System.Web.UI.WebControls.Label lblGrowers;
	


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
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.grdUsers.DataBinding += new System.EventHandler(this.grdUsers_DataBinding);
			this.btnFind.Click += new System.EventHandler(this.btnFind_Click);
			this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
			this.chkSelectAll.CheckedChanged += new System.EventHandler(this.chkSelectAll_CheckedChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillReportTypesCombo()
		{
			DataTable dtReports = DataAccessClass.GetAllReportTypes();
			cboReportTypes.DataSource = dtReports;
			cboReportTypes.DataTextField = "Type";
			cboReportTypes.DataValueField = "Type";
			cboReportTypes.DataBind();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public DataTable ReturnsPaddockSelection()
		{
			DataTable dtPaddocks = new DataTable("Paddocks");
			dtPaddocks.Columns.Add("PaddockName");
			dtPaddocks.Columns.Add("UserName");
			dtPaddocks.Columns.Add("CropType");
			dtPaddocks.Columns.Add("ReportType");
			try
			{
				int iRowPosition = 0;
				DataRow drSelectedPaddock;
				if(cboReportTypes.SelectedValue != "")
				{
					for(int iIndex = 0; iIndex < grdUsers.SelectedItems.Count; iIndex++)
					{
						iRowPosition = grdUsers.SelectedItems[iIndex].Position;
						drSelectedPaddock = dtPaddocks.NewRow();
						drSelectedPaddock["ReportType"] = cboReportTypes.SelectedValue;
						drSelectedPaddock["UserName"] = grdUsers.GetRow(iRowPosition).Cells["UserName"].Text;
						drSelectedPaddock["PaddockName"] = grdUsers.GetRow(iRowPosition).Cells["PaddockName"].Text;
						drSelectedPaddock["CropType"] = grdUsers.GetRow(iRowPosition).Cells["CropType"].Text;
						if(drSelectedPaddock["PaddockName"].ToString() != "")
						{
							dtPaddocks.Rows.Add(drSelectedPaddock);
						}
					}
				}
				else
					throw new Exception("Please select a report type");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}

			return dtPaddocks;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private bool IsPaddockSelectionValid(DataTable dtSelectedPaddocks, ref string szErrorMessage)
		{
			bool bIsPaddockSelectionValid = true;
			if(ReportClass.IsPreSowingReport(cboReportTypes.SelectedValue) == false)
			{
				szErrorMessage = "The following paddocks have the wrong crop for this report type: ";
			
				foreach(DataRow drSelectedPaddock in dtSelectedPaddocks.Rows)
				{
					if(DataAccessClass.IsReportCropComboValid(drSelectedPaddock["CropType"].ToString(),
						cboReportTypes.SelectedValue) == false)	
					{
						szErrorMessage = szErrorMessage + drSelectedPaddock["PaddockName"].ToString()+", ";
						bIsPaddockSelectionValid = false;
					}
				}
				int iLastIndex = szErrorMessage.LastIndexOf(",");
				if(iLastIndex > 0)
					szErrorMessage = szErrorMessage.Remove(iLastIndex, 1);
			}
			return bIsPaddockSelectionValid;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void TransferToReportGenerationPages()
		{
			if(FunctionsClass.IsReadOnly() == false)
			{
				string szErrorMessage = "";
				//As users can select names, that have no paddocks.  We first
				//run the return paddock selection to ensure that only paddocks
				//are selected not users.  This is why we cant check straight
				//from the grid
				DataTable dtSelectedPaddocks = ReturnsPaddockSelection();
				if(dtSelectedPaddocks.Rows.Count > 0)
				{
					if(IsPaddockSelectionValid(dtSelectedPaddocks, ref szErrorMessage))
					{
						switch(cboReportTypes.SelectedValue)
						{
							case "Fallow report":
								Server.Transfer("wfGenerateFallowReport.aspx");
								break;
							case "Irrigation comparison report":
								Server.Transfer("wfGenerateIrrigationComparisonReport.aspx");
								break;
							case "Agronomic report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Climate report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Irrigation scheduling report":
								Server.Transfer("wfGenerateReport.aspx");
								break;
							case "Nitrogen profit report":
								Server.Transfer("wfGenerateNitrogenProfitReport.aspx");
								break;
							case "Nitrogen comparison report":
								Server.Transfer("wfGenerateNitrogenComparisonReport.aspx");
								break;
							case "Pre-Sowing nitrogen comparison report":
								Server.Transfer("wfGeneratePreSowingNitrogenComparisonReport.aspx");
								break;
							case "Sowing X variety report":
								Server.Transfer("wfGenerateSowingXVarietyReport.aspx");
								break;
							case "Pre-sowing agronomic report":
								Server.Transfer("wfGeneratePreSowingReport.aspx");
								break;
							case "Pre-sowing climate report":
								Server.Transfer("wfGeneratePreSowingReport.aspx");
								break;
							case "Pre-season report":
								Server.Transfer("wfGeneratePreSeasonReport.aspx");
								break;
						}
					}
					else
						FunctionsClass.DisplayMessage(Page, szErrorMessage);
				}
				else
					FunctionsClass.DisplayMessage(Page, "Please select a paddock first");
			}
			else
				FunctionsClass.DisplayMessage(Page, FunctionsClass.ReturnReadOnlyMessage());
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

			dtAssignedUsers.Columns.Add("CropType");
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
					drAssignedUser["CropType"] = drPaddock["CropType"].ToString();
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
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForConsultantLevelPriviledges();
				HelpClass.SetHelpForConsultantReportGeneratePage(btnHelpGrid, btnHelpReportType, 
					btnHelpFind, btnHelpSelectAll, btnHelpConsultantReportGeneratePage);

				ViewState["NumberOfRows"] = 0;
				ViewState["PreviousSearchName"] = "";
				ViewState["PreviousSearchRowIndex"] = 0;

				grdUsers.DataBind();
				FillReportTypesCombo();

				FunctionsClass.SetHeadingString(lblHeading);
				lblHeading.Text = lblHeading.Text.Replace("'s", "");
				lblHeading.Text = lblHeading.Text.Replace("'", "");
				FunctionsClass.SetDisplayBanner(imgBanner);
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
		private void btnNext_Click(object sender, System.EventArgs e)
		{
			TransferToReportGenerationPages();
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
