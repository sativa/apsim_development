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
	/// Summary description for wfViewRegions.
	/// </summary>
	public class wfViewRegions : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ListBox lstValues;
		protected System.Web.UI.WebControls.Label lblValues;
		protected System.Web.UI.WebControls.DropDownList cboRegions;
		protected System.Web.UI.WebControls.Label lblRegions;
		protected System.Web.UI.WebControls.LinkButton btnImport;
		protected System.Web.UI.WebControls.LinkButton btnDelete;
		protected System.Web.UI.WebControls.Panel pnlTop;
		protected System.Web.UI.WebControls.ImageButton btnImportImg;
		protected System.Web.UI.WebControls.ImageButton btnDeleteImg;
		protected System.Web.UI.WebControls.DropDownList cboTypes;
		protected System.Web.UI.WebControls.Label lblType;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
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
			//Adds an attribute to the delete button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected value \");");
			}

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
			this.cboRegions.SelectedIndexChanged += new System.EventHandler(this.cboRegions_SelectedIndexChanged);
			this.cboTypes.SelectedIndexChanged += new System.EventHandler(this.cboTypes_SelectedIndexChanged);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		//-------------------------------------------------------------------------
		//Fills the regions drop down and then fills the value listbox with
		//the corresponding items
		//-------------------------------------------------------------------------
		private void FillForm()
			{
			FillRegionsCombo();
			FillValueList();
			}
		//-------------------------------------------------------------------------
		//Fills the regions combo with all the regions from the database
		//-------------------------------------------------------------------------
		private void FillRegionsCombo()
			{
			try
				{
				DataTable dtRegions = DataAccessClass.GetAllRegions();
				cboRegions.DataSource = dtRegions;
				cboRegions.DataTextField = "Type";
				cboRegions.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the Value list with all values of a certain type in a regions.
		//EG if the region is victoria and type is met stations the value list will
		//be filled with all the metstations in victoria 
		//-------------------------------------------------------------------------
		private void FillValueList()
			{
			//If a region is selected and a type selected then the value is list is filled
			if(cboRegions.SelectedItem.Text != "" && cboTypes.SelectedItem.Text != "")
				{
				try
					{
					DataTable dtValues = new DataTable();
					dtValues.Columns.Add("Name");
					if(cboTypes.SelectedItem.Text == "Met Stations")
						{
						dtValues = DataAccessClass.GetMetStationsOfRegion(cboRegions.SelectedItem.Text);
						}
					else if(cboTypes.SelectedItem.Text == "Soils")
						{
						dtValues = DataAccessClass.GetSoilsOfRegion(cboRegions.SelectedItem.Text);
						}

					lstValues.DataSource = dtValues;
					lstValues.DataTextField = "Name";
					lstValues.DataBind();
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			}
		//-------------------------------------------------------------------------
		//Deletes the selected value from the database
		//-------------------------------------------------------------------------
		private void DeleteValueFromRegion()
			{
			//If a value is selected it is removed from the database
			if(lstValues.SelectedItem.Text != null && lstValues.SelectedItem.Text != "")
				{
				try
					{
					if(cboTypes.SelectedItem.Text == "Met Stations")
						{
						DataAccessClass.DeleteMetStation(cboRegions.SelectedItem.Text, 
							lstValues.SelectedItem.Text);
						Server.Transfer("wfViewRegions.aspx");
						}
					else if(cboTypes.SelectedItem.Text == "Soils")
						{
						DataAccessClass.DeleteSoil(cboRegions.SelectedItem.Text, 
							lstValues.SelectedItem.Text);
						Server.Transfer("wfViewRegions.aspx");
						}
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no value is selected then an error message is displayed to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No value Selected");
				}
			}
		//-------------------------------------------------------------------------
		//Imports the file selected by the user
		//-------------------------------------------------------------------------
		private void ImportFile()
			{
			if(cboTypes.SelectedItem.Text == "Met Stations")
				{
				ImportClass.ImportMetStations(Page, cboRegions.SelectedItem.Text);
				Server.Transfer("wfViewRegions.aspx");
				}
			else if(cboTypes.SelectedItem.Text == "Soils")
				{
				ImportClass.ImportSoils(Page, cboRegions.SelectedItem.Text);
				Server.Transfer("wfViewRegions.aspx");
				}
			//If no type is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No type selected");
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
		//When the user presses the import image we look for a file
		//to upload
		//-------------------------------------------------------------------------
		private void btnImportImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			ImportFile();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete button, the selected region is deleted
		//-------------------------------------------------------------------------
		private void btnDelete_Click(object sender, System.EventArgs e)
			{
			DeleteValueFromRegion();
			}
		//-------------------------------------------------------------------------
		//When the user presses the delete image, the selected region is deleted
		//-------------------------------------------------------------------------
		private void btnDeleteImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			DeleteValueFromRegion();
			}
		//-------------------------------------------------------------------------
		//When the user selects a different region the value list is updated
		//-------------------------------------------------------------------------
		private void cboRegions_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillValueList();
			}
		//-------------------------------------------------------------------------
		//When the user selects a different type the value list is updated
		//-------------------------------------------------------------------------
		private void cboTypes_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			lblValues.Text = cboTypes.SelectedItem.Text +":";
			FillValueList();
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
