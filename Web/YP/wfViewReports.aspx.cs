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
				SetYearComboBoxToCurrentYear();
				FillReportList();
			}
			//Adds an attribute to the delete report button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDeleteReport.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected report \");");
		}

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
			this.btnDeleteReportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteReportImg_Click);
			this.btnShowReport.Click += new System.EventHandler(this.btnShowReport_Click);
			this.btnShowReportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnShowReportImg_Click);
			this.btnRenameReport.Click += new System.EventHandler(this.btnRenameReport_Click);
			this.btnRenameImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnRenameImg_Click);
			this.cboYear.SelectedIndexChanged += new System.EventHandler(this.cboYear_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		//-------------------------------------------------------------------------
		//Sets the Year combo box to todays year
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
			string szUserID = SetUserID();
			DataTable dtReports = ReportClass.GetReportsOfUser(szUserID, cboYear.SelectedItem.Text);
			lstReports.DataSource = dtReports;
			lstReports.DataTextField = "Name";
			lstReports.DataBind();
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
				Session["SelectedReportName"] = lstReports.SelectedItem.Text;
				Session["SelectedReportYear"] = cboYear.SelectedItem.Text;
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
			//If a report is selected then remove it from the database
			if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
				{
				if(FunctionsClass.IsGrowerOrHigher(Session["UserID"].ToString()) == true)
					{
					string szUserID = SetUserID();
					ReportClass.DeleteReport(szUserID, lstReports.SelectedItem.Text, cboYear.SelectedItem.Text);
					Server.Transfer("wfViewReports.aspx");
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
		//Sends the user to the EditReport page
		//-------------------------------------------------------------------------
		private void RenameReport()
			{
			//If a report is selected then transfer the user to the edit report page
			if(lstReports.SelectedValue != null && lstReports.SelectedValue != "")
				{
				if(FunctionsClass.IsGrowerOrHigher(Session["UserID"].ToString()) == true)
					{
					Session["SelectedReportYear"] = cboYear.SelectedItem.Text;
					Session["SelectedReportName"] = lstReports.SelectedItem.Text;
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
		//Determines which UserID to use, either the SelectedUserID or the 
		//UserID.  This depends on whether it is the user viewing their own reports
		//or it a user view their grower's reports
		//-------------------------------------------------------------------------	
		private string SetUserID()
		{
			string szUserID = "";
			if(Session["SelectedUserID"].ToString() != "" && Session["SelectedUserID"].ToString() != "0")
			{
				szUserID = Session["SelectedUserID"].ToString();
			}
			else
			{
				szUserID = Session["UserID"].ToString();
			}
			return szUserID;
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
		}//END OF CLASS
	}//END OF NAMESPACE
