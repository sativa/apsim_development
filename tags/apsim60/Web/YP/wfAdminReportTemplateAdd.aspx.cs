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
	/// Summary description for wfAdminReportTemplateAdd.
	/// </summary>
	public class wfAdminReportTemplateAdd : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Label lblTemplateName;
		protected System.Web.UI.WebControls.TextBox edtTemplateName;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
	
	

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
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnAdmin.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
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
					Server.Transfer("wfAdminReportTemplateEdit.aspx");
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
		//Sends the user back to the ReportTemplate page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAdminReportTemplateEdit.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//---------------------------------------------------------------------------
		#endregion


		//---------------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
