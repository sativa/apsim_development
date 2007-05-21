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
	/// Summary description for wfReportsView.
	/// </summary>
	public class wfReportsView : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ListBox lstReports;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetailsConsultant;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnDelete;
		protected System.Web.UI.WebControls.Button btnShow;
		protected System.Web.UI.WebControls.DropDownList cboYear;
		protected System.Web.UI.WebControls.Label lblYear;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnDeleteTwo;
		protected System.Web.UI.WebControls.Button btnShowTwo;
		protected System.Web.UI.WebControls.LinkButton btnMainMenuConsultant;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton btnHelpReportViewPage;
		protected System.Web.UI.WebControls.ImageButton btnHelpYear;
		protected System.Web.UI.WebControls.ImageButton btnHelpGrid;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
	

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
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenuConsultant.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.cboYear.SelectedIndexChanged += new System.EventHandler(this.cboYear_SelectedIndexChanged);
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
		//Fills the report list with all the reports belonging to the current user
		//-------------------------------------------------------------------------
		private void FillReportList()
		{
			try
			{
				DataTable dtReports = ReportClass.GetReportsOfUser(FunctionsClass.GetActiveUserName(), 
					Convert.ToInt32(cboYear.SelectedItem.Text));
				lstReports.DataSource = dtReports;
				lstReports.DataTextField = "Name";
				lstReports.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
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
			DataRow drSelectedReport;
			if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
			{
				foreach(ListItem liReport in lstReports.Items)
				{
					if(liReport.Selected == true)
					{
						drSelectedReport = dtReports.NewRow();
						drSelectedReport["UserName"] = FunctionsClass.GetActiveUserName();
						drSelectedReport["ReportName"] =liReport.Text;
						drSelectedReport["ReportYear"] = cboYear.SelectedValue;
						dtReports.Rows.Add(drSelectedReport);
					}//END IF SELECTED
				}//END FOREACH LOOP

			}
			return dtReports;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void TransferToReportViewingPage()
		{
			
			if(lstReports.SelectedValue != "")
			{
				Server.Transfer("wfDisplayReport.aspx");
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "Please select a report first");
			}
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
						Server.Transfer("wfReportsView.aspx");
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
				FunctionsClass.CheckForGrowerLevelPriviledges();
				HelpClass.SetHelpForReportViewPage(btnHelpYear, btnHelpGrid, btnHelpReportViewPage);

				SetYearComboBoxToCurrentYear();
				FillReportList();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
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
		private void cboYear_SelectedIndexChanged(object sender, System.EventArgs e)
		{
		FillReportList();
		}
		//-------------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE
