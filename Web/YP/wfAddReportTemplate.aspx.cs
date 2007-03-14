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
	/// Summary description for wfAddReportTemplate.
	/// </summary>
	public class wfAddReportTemplate : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Label lblReportType;
		protected System.Web.UI.WebControls.TextBox edtTemplateName;
		protected System.Web.UI.WebControls.Label lblCropType;
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
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Saves a new report type, but firstly a check is run to ensure that
		//a report type has been entered by the user, if this check is passed
		//then the report type is saved to the database.
		//-------------------------------------------------------------------------
		private void SaveReportTemplate()
			{
			try
				{
				if(edtTemplateName.Text != "")
					{
					DataAccessClass.InsertReportTemplate("", InputValidationClass.ValidateString(edtTemplateName.Text));
					Server.Transfer("wfEditReportTemplate.aspx");
					}
				else
					throw new Exception("Please enter a template name");
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
		//If the page hasn't been viewed by the user then the users
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FunctionsClass.SetControlFocus("edtTemplateName", this);
				}
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the reporttype is saved
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveReportTemplate();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the reporttype is saved
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveReportTemplate();
			}
		//-------------------------------------------------------------------------
		//Sends the user back to the ReportTemplate page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditReportTemplate.aspx");
			}
		//-------------------------------------------------------------------------
		//Sends the user back to the ReportTemplate page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditReportTemplate.aspx");
			}
		//---------------------------------------------------------------------------
		#endregion


		//---------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE