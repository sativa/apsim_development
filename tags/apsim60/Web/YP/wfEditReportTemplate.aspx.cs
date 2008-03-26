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
	/// Summary description for wfEditReportTemplate.
	/// </summary>
	public class wfEditReportTemplate : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnImportImg;
		protected System.Web.UI.WebControls.LinkButton btnImport;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
		protected System.Web.UI.WebControls.TextBox edtDisplayTemplate;
		protected System.Web.UI.WebControls.LinkButton btnDeleteReportTemplate;
		protected System.Web.UI.WebControls.LinkButton btnAddReportTemplate;
		protected System.Web.UI.WebControls.ImageButton btnAddReportTemplateImg;
		protected System.Web.UI.WebControls.ImageButton btnDeleteReportTemplateImg;
		protected System.Web.UI.WebControls.DropDownList cboReportTemplates;
		protected System.Web.UI.WebControls.Label lbReportlTemplate;



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
			this.btnDeleteReportTemplate.Click += new System.EventHandler(this.btnDeleteReportTemplate_Click);
			this.btnAddReportTemplate.Click += new System.EventHandler(this.btnAddReportTemplate_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnAddReportTemplateImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnAddReportTemplateImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.btnDeleteReportTemplateImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteReportTemplateImg_Click);
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.btnImportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnImportImg_Click);
			this.cboReportTemplates.SelectedIndexChanged += new System.EventHandler(this.cboReportTemplates_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Fills the from with all the required information from the database.
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			FillReportTemplatesCombo();
			FillDisplayTemplateTextBox();
			
			}
		//-------------------------------------------------------------------------
		//Fills the report types combobox with all the report types that match
		//the selected crop type.
		//-------------------------------------------------------------------------
		private void FillReportTemplatesCombo()
			{
			try
				{
				DataTable dtReportTemplates = 
					DataAccessClass.GetAllReportTemplates();
				cboReportTemplates.DataSource = dtReportTemplates;
				cboReportTemplates.DataTextField = "Name";
				cboReportTemplates.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the disply template text box with the template type selected by
		//the user
		//-------------------------------------------------------------------------
		private void FillDisplayTemplateTextBox()
			{
			try
				{
				//If a report type has been selected, then download the report type
				//template from the database.		
					string szTemplateText = 
						DataAccessClass.GetReportTemplate(cboReportTemplates.SelectedValue);
					szTemplateText = SetUpTemplateTextForDisplay(szTemplateText);
					edtDisplayTemplate.Text = szTemplateText;		
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Clean up the string from the database by replacing the quote and double
		//quote place holders with the actual quote and double quote characters
		//-------------------------------------------------------------------------
		private string SetUpTemplateTextForDisplay(string szTemplateText)
			{
			szTemplateText = szTemplateText.Replace("\r", "");
			szTemplateText = szTemplateText.Replace("#Quote#", "'");
			szTemplateText = szTemplateText.Replace("#DQuote#", "\"");
			return szTemplateText;
			}
		//-------------------------------------------------------------------------
		//Set up the string for saving to the database by replacing the quote and
		//double quote characters with place holders
		//-------------------------------------------------------------------------
		private string SetUpTemplateTextForSaving(string szTemplateText)
		{
			szTemplateText = szTemplateText.Replace("'", "#Quote#");
			szTemplateText = szTemplateText.Replace("\"", "#DQuote#");
			return szTemplateText;
		}
		//-------------------------------------------------------------------------
		//The report is updated, but firstly a check is run to ensure that
		//there is information to save, if there is then the template is saved to the
		//database.
		//-------------------------------------------------------------------------	
		private void SaveReport()
			{
			//If the template text box isn't empty and a report type is selected
			//then update the template in the database
			if(edtDisplayTemplate.Text != "" && cboReportTemplates.SelectedValue != "")
				{
				string szTemplate = SetUpTemplateTextForSaving(edtDisplayTemplate.Text);
				DataAccessClass.UpdateReportTemplate(szTemplate, cboReportTemplates.SelectedValue);
				}
			//If the there is not text in the template text box or a report type isn't 
			//selected then display an error message to the user.
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please enter some text for the template and ensure that a valid report template is selected");
				}
			}
		//-------------------------------------------------------------------------
		//Deletes the selected report type
		//-------------------------------------------------------------------------
		private void DeleteReportType()
			{
			//If a report type is selected, then delete that report type from the
			//database
			if(cboReportTemplates.SelectedValue != "")
				{
				DataAccessClass.DeleteReportTemplate(cboReportTemplates.SelectedValue);
				//Refresh the page to show changes
				Server.Transfer("wfEditReportTemplate.aspx");
				}
				//If no report type is selected, display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No report type selected");
				}
			}
		//-------------------------------------------------------------------------
		//Imports the selected file
		//-------------------------------------------------------------------------
		private void ImportFile()
			{
			if(cboReportTemplates.SelectedValue != "")
				{
				bool bErrors = false;
				ImportClass.ImportReportTemplate(Page, cboReportTemplates.SelectedValue, 
					ref bErrors);
				Server.Transfer("wfEditReportTemplate.aspx");
				}
			//If no report type is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No report type selected");
				}
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
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FillForm();
				}
			//Adds an attribute to the delete  report type button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDeleteReportTemplate.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the report template \");");
			btnDeleteReportTemplateImg.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the report template \");");
		}	
		//-------------------------------------------------------------------------
		//When the user presses the save button the report is updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save image the report is updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveReport();
			}
		//-------------------------------------------------------------------------
		//When the user presses the import button, the selected file is imported
		//-------------------------------------------------------------------------
		private void btnImport_Click(object sender, System.EventArgs e)
			{
			ImportFile();
			}
		//-------------------------------------------------------------------------
		//When the user presses the import image, the selected file is imported
		//-------------------------------------------------------------------------
		private void btnImportImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			ImportFile();
			}
		//-------------------------------------------------------------------------
		//When the user presses the add report type button, they are sent to the 
		//Add report type page
		//-------------------------------------------------------------------------
		private void btnAddReportTemplate_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfAddReportTemplate.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the add report type image, they are sent to the 
		//Add report type page
		//-------------------------------------------------------------------------
		private void btnAddReportTemplateImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfAddReportTemplate.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete report type button, the selected report 
		//type is deleted from the database
		//-------------------------------------------------------------------------
		private void btnDeleteReportTemplate_Click(object sender, System.EventArgs e)
			{
			DeleteReportType();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete report type button, the selected report 
		//type is deleted from the database
		//-------------------------------------------------------------------------
		private void btnDeleteReportTemplateImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteReportType();
			}
		//-------------------------------------------------------------------------
		//When the user changes the selected report type, the template text box is
		//refreshed with the corresponing template from the database
		//-------------------------------------------------------------------------
		private void cboReportTemplates_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillDisplayTemplateTextBox();
			}
		//-------------------------------------------------------------------------	
		#endregion


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
