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
using System.Xml;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfAdminSoilsEdit.
	/// </summary>
	public class wfAdminSoilsEdit : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.TextBox edtSoilData;
		protected System.Web.UI.WebControls.Label lblSoilData;
		protected System.Web.UI.WebControls.TextBox edtSoilName;
		protected System.Web.UI.WebControls.Label lblSoilName;
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
		//
		//-------------------------------------------------------------------------
		private void StoreSoilSelection()
		{
			try
			{
				wfAdminSoilsView ViewSoils = (wfAdminSoilsView) Context.Handler;
				ViewState["PreviousSoilName"] = ViewSoils.ReturnSelectedSoil();
				ViewState["RegionName"] = ViewSoils.ReturnSelectedRegion();
				edtSoilName.Text = ViewState["PreviousSoilName"].ToString();
			}
			catch(Exception)
			{}
		}	
		//-------------------------------------------------------------------------
		//Fills the soil name and the soil data edit boxes
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			StoreSoilSelection();
			try
			{
				if(edtSoilName.Text != "")
				{
					string szSoilData = DataAccessClass.GetSoilData(edtSoilName.Text);
					edtSoilData.Text = szSoilData;
				}
				else
					throw new Exception("Can not find specified soil");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Saves the changes to the soil name and then transfers the user back to the 
		//view regions page.
		//-------------------------------------------------------------------------
		private void SaveSoil()
		{
			try
			{
				if(edtSoilName.Text != "" && edtSoilData.Text != "")
				{
					System.Xml.XmlDocument xmlSoilData = new XmlDocument();
					xmlSoilData.InnerXml = edtSoilData.Text;

					DataAccessClass.InsertOrEditSoil(ViewState["RegionName"].ToString(),
						ViewState["PreviousSoilName"].ToString(),
						InputValidationClass.ValidateString(edtSoilData.Text),
						InputValidationClass.ValidateString(edtSoilName.Text));
					Server.Transfer("wfAdminSoilsView.aspx");
				}
				else
					throw new Exception("Soil name or soil data field is blank");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, InputValidationClass.StripSingleQuote(E.Message));
			}
		}
		//-------------------------------------------------------------------------
		//Returns the selected region, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedRegion()
		{
			return ViewState["RegionName"].ToString();
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
				ViewState["PreviousSoilName"] = "";
				ViewState["RegionName"] = "";
				FillForm();
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the save button, the changes are saved and they are
		//transfered back to the view regions page.
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SaveSoil();
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button, they are transfered back to the 
		//view regions page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfAdminSoilsView.aspx");
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
