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

namespace YieldProphet
{
	/// <summary>
	/// Summary description for wfEditSoil.
	/// </summary>
	public class wfEditSoil : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.TextBox edtSoilName;
		protected System.Web.UI.WebControls.Label lblSoilName;
		protected System.Web.UI.WebControls.TextBox edtSoilData;
		protected System.Web.UI.WebControls.Label lblSoilData;
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
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
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
				wfViewRegions RegionsPage = (wfViewRegions) Context.Handler;
				ViewState["PreviousSoilName"] = RegionsPage.ReturnSelectedValue();
				ViewState["RegionName"] = RegionsPage.ReturnSelectedRegion();
				edtSoilName.Text = RegionsPage.ReturnSelectedValue();
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
				{
					throw new Exception("Can not find specified soil");
				}
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
					Server.Transfer("wfViewRegions.aspx");
					}
				else
					{
					throw new Exception("Soil name or soil data field is blank");
					}
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
		//When the user presses the save image, the changes are saved and they are
		//transfered back to the view regions page.
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SaveSoil();
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button, they are transfered back to the 
		//view regions page.
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfViewRegions.aspx");
			}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image, they are transfered back to the 
		//view regions page.
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			Server.Transfer("wfViewRegions.aspx");
			}
		//-------------------------------------------------------------------------
		#endregion

		}//END OF CLASS
	}//END OF NAMESPACE
