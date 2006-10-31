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
	/// Summary description for wfAdminSoilsView.
	/// </summary>
	public class wfAdminSoilsView : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnAdmin;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlAdministration;
		protected System.Web.UI.WebControls.Label lblRegions;
		protected System.Web.UI.WebControls.DropDownList cboRegions;
		protected System.Web.UI.WebControls.ListBox lstSoils;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.Label lblSoils;
		protected System.Web.UI.WebControls.Button btnCheckAllSoils;
		protected System.Web.UI.WebControls.Button btnDelete;
		protected System.Web.UI.WebControls.Button btnImport;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;
		protected System.Web.UI.WebControls.Button btnEdit;
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
			this.btnCheckAllSoils.Click += new System.EventHandler(this.btnCheckAllSoils_Click);
			this.cboRegions.SelectedIndexChanged += new System.EventHandler(this.cboRegions_SelectedIndexChanged);
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
			this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
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
				string szPreviousPage = Context.Handler.ToString();
				if(szPreviousPage == "ASP.wfAdminSoilsView_aspx")
				{
					wfAdminSoilsView PreviousPage = (wfAdminSoilsView) Context.Handler;
					cboRegions.SelectedValue = PreviousPage.ReturnSelectedRegion();
					FillSoilList();
				}
				else if(szPreviousPage == "ASP.wfAdminSoilsEdit_aspx")
				{
					wfAdminSoilsEdit PreviousPage = (wfAdminSoilsEdit) Context.Handler;
					cboRegions.SelectedValue = PreviousPage.ReturnSelectedRegion();
					FillSoilList();
				}
				
			}
			catch(Exception)
			{}
		}	
		//-------------------------------------------------------------------------
		//Fills the regions drop down and then fills the value listbox with
		//the corresponding items
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			FillRegionsCombo();
			FillSoilList();
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
		private void FillSoilList()
		{
			//If a region is selected and a type selected then the value is list is filled
			if(cboRegions.SelectedValue != null && cboRegions.SelectedValue != "")
			{
				try
				{
					lstSoils.Items.Clear();
					DataTable dtSoils = new DataTable();
					dtSoils = DataAccessClass.GetSoilsOfRegion(cboRegions.SelectedValue);
					lstSoils.DataSource = dtSoils;
					lstSoils.DataTextField = "Name";
					lstSoils.DataBind();
				}
				catch(Exception E)
				{
					FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		}
		//-------------------------------------------------------------------------
		//Deletes the selected soils from the database
		//-------------------------------------------------------------------------
		private void DeleteSoilsFromRegion()
		{
			//If a value is selected it is removed from the database
			try
			{
				//Ensure that a value is selected
				if(lstSoils.SelectedValue != null && lstSoils.SelectedValue != "")
				{
					foreach(ListItem liSoil in lstSoils.Items)
					{
						if(liSoil.Selected == true)
						{
							DataTable paddocksUsingSoil = DataAccessClass.GetPaddocksUsingSoil(cboRegions.SelectedValue, 
								liSoil.Text);
							if(paddocksUsingSoil.Rows.Count == 0)
							{
								DataAccessClass.DeleteSoil(cboRegions.SelectedValue, 
									liSoil.Text);
							}
							else
							{
								string msg = "Soil:"+liSoil.Text+" can not be deleted, as the following paddocks are using it:";
								foreach(DataRow paddock in paddocksUsingSoil.Rows)
								{
									msg = msg + "   User " + paddock["Users.Name"] + "(" + paddock["Paddocks.Name"] + "); ";
								}
								FillSoilList();
								throw new Exception(msg);
							}
						}//END IF SELECTED			
					}//END FOREACH LOOP
					Server.Transfer("wfAdminSoilsView.aspx");
				}
					//If no value is selected then an error message is displayed to the user
				else
					throw new Exception("No soils selected");
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
				if(cboRegions.SelectedValue != "None")
				{
					bool bErrors = false;
					ImportClass.ImportSoils(Page, cboRegions.SelectedValue, ref bErrors);
					Server.Transfer("wfAdminSoilsView.aspx");
				}
				else
					throw new Exception("No region selected");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Returns the selected region, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		private void EditSoil()
		{
			if(lstSoils.SelectedValue != "")
			{
				Server.Transfer("wfAdminSoilsEdit.aspx");
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "No soil selected");
			}
		}
		//-------------------------------------------------------------------------
		//Returns the selected region, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedRegion()
		{
			return cboRegions.SelectedValue;
		} 
		//-------------------------------------------------------------------------
		//Returns the selected value, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedSoil()
		{
			return lstSoils.SelectedValue;
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
				StoreSoilSelection();
			}
			//Adds an attribute to the delete button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected soils(s) \");");
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
		//When the user presses the delete button, the selected region is deleted
		//-------------------------------------------------------------------------
		private void btnDelete_Click(object sender, System.EventArgs e)
		{
			DeleteSoilsFromRegion();
		}
		//-------------------------------------------------------------------------
		//When the user presses the edit button, they are transfered to the edit
		//page
		//-------------------------------------------------------------------------
		private void btnEdit_Click(object sender, System.EventArgs e)
		{
			EditSoil();
		}
		//-------------------------------------------------------------------------
		//When the user selects a different region the value list is updated
		//-------------------------------------------------------------------------
		private void cboRegions_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			FillSoilList();
		}
		//-------------------------------------------------------------------------
		//When the user selects a different region the value list is updated
		//-------------------------------------------------------------------------
		private void btnCheckAllSoils_Click(object sender, System.EventArgs e)
		{
			try
			{
				DataAccessClass.CheckAllSoils();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}

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
	}//END OF CLASS
}//END OF NAMESPACE
