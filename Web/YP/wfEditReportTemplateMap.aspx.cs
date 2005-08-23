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
	/// Summary description for wfEditReportTemplateMap.
	/// </summary>
	public class wfEditReportTemplateMap : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.DropDownList cboCropTypes;
		protected System.Web.UI.WebControls.DropDownList cboReportTypes;
		protected System.Web.UI.WebControls.DropDownList cboConParTemplates;
		protected System.Web.UI.WebControls.DropDownList cboAPSIMTemplates;
		protected System.Web.UI.WebControls.Label lblCropType;
		protected System.Web.UI.WebControls.Label lblConParTemplate;
		protected System.Web.UI.WebControls.Label lblAPSIMReportTemplate;
		protected System.Web.UI.WebControls.Label lblReportTypes;
		protected System.Web.UI.WebControls.ImageButton btnEditTemplateImg;
		protected System.Web.UI.WebControls.LinkButton btnEditTemplate;
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
			this.cboCropTypes.SelectedIndexChanged += new System.EventHandler(this.cboCropTypes_SelectedIndexChanged);
			this.cboReportTypes.SelectedIndexChanged += new System.EventHandler(this.cboReportTypes_SelectedIndexChanged);
			this.btnEditTemplateImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnEditTemplateImg_Click);
			this.btnEditTemplate.Click += new System.EventHandler(this.btnEditTemplate_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			FillCropCombo();
			FillReportTypeCombo();
			FillTemplateTypeCombos();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillCropCombo()
		{
			try
			{
				DataTable dtCrops = DataAccessClass.GetAllCrops();
				cboCropTypes.DataSource = dtCrops;
				cboCropTypes.DataTextField = "Type";
				cboCropTypes.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillReportTypeCombo()
		{
			try
			{
				DataTable dtReportTypes = DataAccessClass.GetAllReportTypes();
				cboReportTypes.DataSource = dtReportTypes;
				cboReportTypes.DataTextField = "Type";
				cboReportTypes.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillTemplateTypeCombos()
		{
			try
			{
				DataTable dtReportTemplates = DataAccessClass.GetAllReportTemplates();
				cboConParTemplates.DataSource = dtReportTemplates;
				cboConParTemplates.DataTextField = "Name";
				cboConParTemplates.DataBind();
				cboAPSIMTemplates.DataSource = dtReportTemplates;
				cboAPSIMTemplates.DataTextField = "Name";
				cboAPSIMTemplates.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SaveReportTemplateMap()
			{
			try
				{
				if(cboCropTypes.SelectedValue != "" && cboReportTypes.SelectedValue != "" &&
					cboConParTemplates.SelectedValue != "" && cboAPSIMTemplates.SelectedValue != "")
					{
					DataAccessClass.SetReportTemplateMap(cboCropTypes.SelectedValue, cboReportTypes.SelectedValue, 
						cboConParTemplates.SelectedValue, cboAPSIMTemplates.SelectedValue);
					}
				else
					{
					throw new Exception("Please ensure all fields have valid data");
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SetDefaultTemplateSelections()
			{
			try
				{
				if(cboCropTypes.SelectedValue != "" && cboReportTypes.SelectedValue != "")
					{

					string szConParTemplate = DataAccessClass.GetConParReportTemplateName(cboCropTypes.SelectedValue, 
						cboReportTypes.SelectedValue);
					if(szConParTemplate != "")
						{
						cboConParTemplates.SelectedValue = szConParTemplate;
						}
						
					string szAPSIMTemplate = DataAccessClass.GetAPSIMReportTemplateName(cboCropTypes.SelectedValue, 
						cboReportTypes.SelectedValue);
					if(szAPSIMTemplate != "")
						{
						cboAPSIMTemplates.SelectedValue = szAPSIMTemplate;
						}			
					}
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
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FillForm();
				SetDefaultTemplateSelections();
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SaveReportTemplateMap();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveReportTemplateMap();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("EditReportTemplateMap.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditTemplate_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditReportTemplate.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnEditTemplateImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfEditReportTemplate.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("EditReportTemplateMap.aspx");
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void cboCropTypes_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			SetDefaultTemplateSelections();
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void cboReportTypes_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			SetDefaultTemplateSelections();
			}
		//-------------------------------------------------------------------------
		#endregion

		//-------------------------------------------------------------------------
		}//END CLASS
	}//END REGION
