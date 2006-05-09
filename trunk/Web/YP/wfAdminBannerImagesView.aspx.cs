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
	/// Summary description for wfAdminBannerImagesView.
	/// </summary>
	public class wfAdminBannerImagesView : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Button btnImport;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
		protected System.Web.UI.WebControls.Label lblBannerImages;
		protected System.Web.UI.WebControls.ListBox lstBannerImages;
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
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Imports the file selected by the user
		//-------------------------------------------------------------------------
		private void ImportFile()
		{
			try
			{
			bool bErrors = false;
			ImportClass.ImportBannerImage(Page, ref bErrors);
				if(bErrors == false)
				{
					Server.Transfer("wfAdminBannerImagesView.aspx");
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
		private void FillBannerImageList()
		{
			try
			{
				lstBannerImages.Items.Clear();
				DataTable dtBannerImages = new DataTable();
				dtBannerImages = DataAccessClass.GetAllBannerImages();
				lstBannerImages.DataSource = dtBannerImages;
				lstBannerImages.DataTextField = "FileName";
				lstBannerImages.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		#endregion


		#region Form Events

		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForAdministratorLevelPriviledges();
				FillBannerImageList();
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the import button we look for a file
		//to upload
		//-------------------------------------------------------------------------
		private void btnImport_Click(object sender, System.EventArgs e)
		{
			ImportFile();
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
	}
}
