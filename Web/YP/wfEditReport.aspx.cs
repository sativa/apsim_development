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
	/// Summary description for wfEditReport.
	/// </summary>
	public class wfEditReport : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.TextBox edtReportName;
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
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Stores the report name and report year selections from the previous page in view state
		//variables.
		//-------------------------------------------------------------------------
		private void StoreReportDetails()
		{
			try
			{
				wfViewReports ViewReports = (wfViewReports) Context.Handler;
				ViewState["ReportName"] = ViewReports.ReturnReportName();
				ViewState["ReportYear"] = ViewReports.ReturnReportYear();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Displays the existing report name.
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			if(ViewState["ReportName"].ToString() != "")
				{
				edtReportName.Text = ViewState["ReportName"].ToString();
				}
			}
		//-------------------------------------------------------------------------
		//The report is updated but firstly a check is made to ensure that
		//the a report name has been entered, if it has then the the report
		//is updated with the new name
		//-------------------------------------------------------------------------
		private void SaveReport()
			{
			//If the report name isn't empty, then the report name is updated in
			//the database
			if(edtReportName.Text != "")
				{
				try
					{
					//As the user name is used in the file name of any reports generated
					//a check is run to ensure it doesn't have any characters in it
					//that will stop a file from being created.
					if(InputValidationClass.IsInputAValidFileLocationString(edtReportName.Text) == true)
						{
						if(ReportClass.RenameReport(ViewState["ReportName"].ToString(), 
							InputValidationClass.ValidateString(edtReportName.Text), FunctionsClass.GetActiveUserName(), 
							Convert.ToInt32(ViewState["ReportYear"].ToString())) == false)
							{
							FunctionsClass.DisplayMessage(Page, "Report name already exists");
							}
						else
							{
							Server.Transfer("wfViewReports.aspx");
							}
						}
					else
						{
						FunctionsClass.DisplayMessage(Page, "New report name contains invalid characters. Please remove any of the following characters \\\\ / : * \" ? \\' # < > |");
						}
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If the report name is empty, then an error message is displayed to the user
			else
				{	
				FunctionsClass.DisplayMessage(Page, "Please enter a name for the report");
				}
			}
		//-------------------------------------------------------------------------
		#endregion


	
		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.SetControlFocus("edtReportName", this);
				StoreReportDetails();
				FillForm();
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the report name is updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save image the report name is updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button, they are transfered to the 
		//View reports page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewReports.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image, they are transfered to the 
		//View reports page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewReports.aspx");
			}
		//-------------------------------------------------------------------------
		#endregion



		//-------------------------------------------------------------------------
		}//END CLASS
	}//END OF NAMESPACE
