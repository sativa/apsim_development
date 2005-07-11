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
	/// Summary description for wfViewReports.
	/// </summary>
	public class wfViewReports : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.LinkButton btnRenameReport;
		protected System.Web.UI.WebControls.ImageButton btnShowReportImg;
		protected System.Web.UI.WebControls.LinkButton btnShowReport;
		protected System.Web.UI.WebControls.ImageButton btnDeleteReportImg;
		protected System.Web.UI.WebControls.LinkButton btnDeleteReport;
		protected System.Web.UI.WebControls.ListBox lstReports;
		protected System.Web.UI.WebControls.ImageButton btnRenameImg;
		protected System.Web.UI.WebControls.DropDownList cboYear;
		protected System.Web.UI.WebControls.Label lblYear;
		protected System.Web.UI.WebControls.Panel pnlTop;


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
			this.btnDeleteReport.Click += new System.EventHandler(this.btnDeleteReport_Click);
			this.cboYear.SelectedIndexChanged += new System.EventHandler(this.cboYear_SelectedIndexChanged);
			this.btnDeleteReportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteReportImg_Click);
			this.btnShowReport.Click += new System.EventHandler(this.btnShowReport_Click);
			this.btnShowReportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnShowReportImg_Click);
			this.btnRenameReport.Click += new System.EventHandler(this.btnRenameReport_Click);
			this.btnRenameImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnRenameImg_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Sets the Year combo box to todays year
		//-------------------------------------------------------------------------
		private void SetYearComboBox(int iYear)
			{
			string szYear = iYear.ToString();
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
		//Stores the reportID in a session variable then transfers the user to the
		//Display report page to view the report
		//-------------------------------------------------------------------------
		private void DisplayReport()
			{
			//If a report is selected then transfer the user to the Display report page
			if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
				{

				Server.Transfer("wfDisplayReport.aspx");
				}
			//If no report is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Report Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Deletes the selected report from the database and the file system.
		//-------------------------------------------------------------------------	
		private void DeleteReport()
			{
			try
				{
				//If a report is selected then remove it from the database
				if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
					{
					if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
						{
						ReportClass.DeleteReport(FunctionsClass.GetActiveUserName(), 
							lstReports.SelectedItem.Text, Convert.ToInt32(cboYear.SelectedItem.Text));
						Server.Transfer("wfViewReports.aspx");
						}
					else
						throw new Exception("Functionality not available to visitors");
					}
				//If no report is selected then display an error message to the user
				else
					throw new Exception("No Report Selected");
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Sends the user to the EditReport page
		//-------------------------------------------------------------------------
		private void RenameReport()
			{
			//If a report is selected then transfer the user to the edit report page
			if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
				{
				if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
					{
					Server.Transfer("wfEditReport.aspx");
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
					}
				}
			//If no report is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Report Selected");
				}
			}
		//-------------------------------------------------------------------------
		//A public function to return the selected year to another page
		//-------------------------------------------------------------------------
		public int ReturnReportYear()
			{
			int iReportYear = 0;
			try
				{
				iReportYear = Convert.ToInt32(cboYear.SelectedItem.Text);
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			return iReportYear;
			}
		//-------------------------------------------------------------------------
		//A public function to return the selected report name to another page
		//-------------------------------------------------------------------------
		public string ReturnReportName()
			{
			string szReportName = "";
			if(lstReports.SelectedItem.Text != null && lstReports.SelectedItem.Text != "")
				{
				szReportName = lstReports.SelectedItem.Text;
				}
			return szReportName;
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
				FunctionsClass.CheckForVisitorLevelPriviledges();
				SetYearComboBox(DateTime.Today.Year);
				FillReportList();
				//If there are no reports then show last year's report list
				if(lstReports.Items.Count < 1)
					{
					SetYearComboBox(DateTime.Today.Year-1);
					FillReportList();
					}
				}
			//Adds an attribute to the delete report button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDeleteReport.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected report \");");
			}
		//-------------------------------------------------------------------------
		//When the user presses the rename report button, they are transfered to the
		// display report page
		//-------------------------------------------------------------------------
		private void btnShowReport_Click(object sender, System.EventArgs e)
			{
			DisplayReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the rename report image, they are transfered to the
		// display report page
		//-------------------------------------------------------------------------
		private void btnShowReportImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DisplayReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete report button, the selected report is
		//removed from the database
		//-------------------------------------------------------------------------
		private void btnDeleteReport_Click(object sender, System.EventArgs e)
		{
			DeleteReport();
		}
		//-------------------------------------------------------------------------
		//When the user presses the delete report image, the selected report is
		//removed from the database
		//-------------------------------------------------------------------------
		private void btnDeleteReportImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the rename report button, they are transfered to the
		// edit report page
		//-------------------------------------------------------------------------
		private void btnRenameReport_Click(object sender, System.EventArgs e)
			{
			RenameReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the rename report image, they are transfered to the
		// edit report page
		//-------------------------------------------------------------------------
		private void btnRenameImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			RenameReport();
			}
		//-------------------------------------------------------------------------
		//When the user changes the report year, the report list is updated
		//-------------------------------------------------------------------------
		private void cboYear_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillReportList();
			}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
