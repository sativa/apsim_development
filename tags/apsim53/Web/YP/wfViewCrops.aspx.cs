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
	/// Summary description for wfViewCrops.
	/// </summary>
	public class wfViewCrops : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.Label lblCultivars;
		protected System.Web.UI.WebControls.ListBox lstCultivars;
		protected System.Web.UI.WebControls.DropDownList cboCrops;
		protected System.Web.UI.WebControls.ImageButton btnImportImg;
		protected System.Web.UI.WebControls.LinkButton btnImport;
		protected System.Web.UI.WebControls.ImageButton btnDeleteImg;
		protected System.Web.UI.WebControls.LinkButton btnDelete;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
		protected System.Web.UI.WebControls.Label lblCrops;


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
			this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
			this.btnDeleteImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteImg_Click);
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.btnImportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnImportImg_Click);
			this.cboCrops.SelectedIndexChanged += new System.EventHandler(this.cboCrops_SelectedIndexChanged);
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
			//If a crop is selected then fill the cultivar combo box
			if(cboCrops.SelectedItem.Text != "" && cboCrops.SelectedItem.Text != "")
				{
				try
					{
					DataTable dtCultivars = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedItem.Text);
					lstCultivars.DataSource = dtCultivars;
					lstCultivars.DataTextField = "Type";
					lstCultivars.DataBind();
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no crop is selected then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No crop Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Deletes the selected cultivar from the database
		//-------------------------------------------------------------------------
		private void DeleteCultivar()
			{
			//If a cultivar is selected then remove it from the database and
			//refresh the page to show the changes
			if(lstCultivars.SelectedItem.Text != null && lstCultivars.SelectedItem.Text != "")
				{
				try
					{
					DataAccessClass.DeleteCulivar(lstCultivars.SelectedItem.Text);
					Server.Transfer("wfViewCrops.aspx");
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no cultivar is selected then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No Cultivar Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Imports the file selected by the user
		//-------------------------------------------------------------------------
		private void ImportFile()
			{
			//If a crop type is selected check for any uploaded files
			if(cboCrops.SelectedItem.Text != "")
				{
				try
					{
					bool bErrors = false;
					ImportClass.ImportCultivars(Page, cboCrops.SelectedItem.Text, ref bErrors);
					Server.Transfer("wfViewCrops.aspx");

					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no crop is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No crop selected");
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
			btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected value \");");
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete button, the selected cultivar is removed
		//-------------------------------------------------------------------------
		private void btnDelete_Click(object sender, System.EventArgs e)
			{
			DeleteCultivar();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete button, the selected cultivar is removed
		//-------------------------------------------------------------------------
		private void btnDeleteImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
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
		//When the user selects presses the import button we look for a file
		//to upload
		//-------------------------------------------------------------------------
		private void btnImportImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
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
		#endregion


		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
