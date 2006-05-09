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
	/// Summary description for wfAdminCrops.
	/// </summary>
	public class wfAdminCrops : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.ListBox lstCultivars;
		protected System.Web.UI.WebControls.Label lblCultivars;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.Label lblCrops;
		protected System.Web.UI.WebControls.Button btnImport;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
		protected System.Web.UI.WebControls.Button btnDeleteCultivar;
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
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.btnDeleteCultivar.Click += new System.EventHandler(this.btnDeleteCultivar_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
	

	

		#region Form Functions
		//-------------------------------------------------------------------------
		//Fills the form with information from the database
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			FillCropsCombo();
			FillCultivarList();
		}
		//-------------------------------------------------------------------------
		//Fills the crops combo with all the crop types from the database
		//-------------------------------------------------------------------------
		private void FillCropsCombo()
		{
			try
			{
				DataTable dtRegions = DataAccessClass.GetAllCrops();
				cboCrops.DataSource = dtRegions;
				cboCrops.DataTextField = "Type";
				cboCrops.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Fills the cultivar combo with all cultivars that are linked to the 
		//selected crop
		//-------------------------------------------------------------------------
		private void FillCultivarList()
		{
			try
			{
				//If a crop is selected then fill the cultivar combo box
				if(cboCrops.SelectedItem.Text != "" && cboCrops.SelectedItem.Text != "")
				{
				
					DataTable dtCultivars = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedItem.Text);
					lstCultivars.DataSource = dtCultivars;
					lstCultivars.DataTextField = "Type";
					lstCultivars.DataBind();
				}
				//If no crop is selected then display an error to the user
				else
					throw new Exception("No crop Selected");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Deletes the selected cultivar from the database
		//-------------------------------------------------------------------------
		private void DeleteCultivar()
		{
			try
			{
				//If a cultivar is selected then remove it from the database and
				//refresh the page to show the changes
				if(lstCultivars.SelectedValue != null && lstCultivars.SelectedValue != "")
				{
					DataAccessClass.DeleteCulivar(lstCultivars.SelectedValue);
					Server.Transfer("wfViewCrops.aspx");
				}
				//If no cultivar is selected then display an error to the user
				else
					throw new Exception("No Cultivar Selected");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Imports the file selected by the user
		//-------------------------------------------------------------------------
		private void ImportFile()
		{
			try
			{
				//If a crop type is selected check for any uploaded files
				if(cboCrops.SelectedItem.Text != "")
				{
					bool bErrors = false;
					ImportClass.ImportCultivars(Page, cboCrops.SelectedItem.Text, ref bErrors);
					Server.Transfer("wfViewCrops.aspx");

				
				}
				//If no crop is selected then display an error message to the user
				else
					throw new Exception("No crop selected");
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
			btnDeleteCultivar.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected cultivar \");");
		}
		//-------------------------------------------------------------------------
		//When the user presses the delete button, the selected cultivar is removed
		//-------------------------------------------------------------------------
		private void btnDeleteCultivar_Click(object sender, System.EventArgs e)
		{
			DeleteCultivar();
		}
		//-------------------------------------------------------------------------
		//When the user selects presses the import button we look for a file
		//to upload
		//-------------------------------------------------------------------------
		private void btnImport_Click(object sender, System.EventArgs e)
		{
			ImportFile();
		}
		//-------------------------------------------------------------------------
		//When the user selects a different crop type the cultivar list is updated
		//-------------------------------------------------------------------------
		private void cboCrops_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillCultivarList();
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


		//-------------------------------------------------------------------------
	}//END CLASS
}//END NAMESPACE
