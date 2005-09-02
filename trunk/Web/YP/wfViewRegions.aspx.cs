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
		protected System.Web.UI.WebControls.ImageButton btnEditImg;
		protected System.Web.UI.WebControls.LinkButton btnEdit;
		protected System.Web.UI.WebControls.Label lblMultipleSelect;
		protected System.Web.UI.WebControls.LinkButton LinkButton1;
		protected System.Web.UI.HtmlControls.HtmlInputFile flImport;


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
			this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
			this.btnEditImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnEditImg_Click);
			this.btnDeleteImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnDeleteImg_Click);
			this.btnImport.Click += new System.EventHandler(this.btnImport_Click);
			this.btnImportImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnImportImg_Click);
			this.cboRegions.SelectedIndexChanged += new System.EventHandler(this.cboRegions_SelectedIndexChanged);
			this.cboTypes.SelectedIndexChanged += new System.EventHandler(this.cboTypes_SelectedIndexChanged);
			this.LinkButton1.Click += new System.EventHandler(this.LinkButton1_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StoreReportSelection()
		{
			try
			{
				string szPreviousPage = Context.Handler.ToString();
				if(szPreviousPage == "ASP.wfViewRegions_aspx")
				{
					wfViewRegions PreviousPage = (wfViewRegions) Context.Handler;
					cboRegions.SelectedValue = PreviousPage.ReturnSelectedRegion();
					cboTypes.SelectedValue = PreviousPage.ReturnSelectedType();
					FillValueList();
				}
				else if(szPreviousPage == "ASP.wfEditMetStation_aspx")
				{
					wfEditMetStation PreviousPage = (wfEditMetStation) Context.Handler;
					cboRegions.SelectedValue = PreviousPage.ReturnSelectedRegion();
					cboTypes.SelectedValue = "Met Stations";
					FillValueList();
				}
				else if(szPreviousPage == "ASP.wfEditSoil_aspx")
				{
					wfEditSoil PreviousPage = (wfEditSoil) Context.Handler;
					cboRegions.SelectedValue = PreviousPage.ReturnSelectedRegion();
					cboTypes.SelectedValue = "Soils";
					FillValueList();
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
			if(cboRegions.SelectedValue != "" && cboTypes.SelectedValue != "")
				{
				try
					{
					lstValues.Items.Clear();
					DataTable dtValues = new DataTable();
					dtValues.Columns.Add("Name");
					if(cboTypes.SelectedValue == "Met Stations")
						{
						dtValues = DataAccessClass.GetMetStationsOfRegion(cboRegions.SelectedValue);
						}
					else if(cboTypes.SelectedValue == "Soils")
						{
						dtValues = DataAccessClass.GetSoilsOfRegion(cboRegions.SelectedValue);
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
			try
				{
				//Ensure that a value is selected
				if(lstValues.SelectedValue != null && lstValues.SelectedValue != "")
					{
					//If value is set to Met Stations
					if(cboTypes.SelectedItem.Text == "Met Stations")
						{
						foreach(ListItem liValue in lstValues.Items)
							{
							if(liValue.Selected == true)
								{
								if(DataAccessClass.IsMetStationInUse(cboRegions.SelectedValue, 
									liValue.Text) ==  false)
									{
									DataAccessClass.DeleteMetStation(cboRegions.SelectedValue, 
										liValue.Text);
									}
								else
									{
									FillValueList();
									throw new Exception("Met Statition:"+liValue.Text+" can not be deleted, as a paddock is using it");
									}
								}//END IF SELECTED
							}//END FOREACH LOOP
						Server.Transfer("wfViewRegions.aspx");
						}
					else if(cboTypes.SelectedItem.Text == "Soils")
						{
						foreach(ListItem liValue in lstValues.Items)
							{
							if(liValue.Selected == true)
								{
								DataTable paddocksUsingSoil = DataAccessClass.GetPaddocksUsingSoil(cboRegions.SelectedValue, 
									liValue.Text);
								if(paddocksUsingSoil.Rows.Count == 0)
									{
									DataAccessClass.DeleteSoil(cboRegions.SelectedValue, 
										liValue.Text);
									}
								else
									{
									string msg = "Soil:"+liValue.Text+" can not be deleted, as the following paddocks are using it:";
									foreach(DataRow paddock in paddocksUsingSoil.Rows)
										{
										msg = msg + "   User " + paddock["Users.Name"] + "(" + paddock["Paddocks.Name"] + "); ";
										}
									FillValueList();
									throw new Exception(msg);
									}
								}//END IF SELECTED			
							}//END FOREACH LOOP
						Server.Transfer("wfViewRegions.aspx");
						}
					else
						{
						throw new Exception("No type selected");
						}
					}
				//If no value is selected then an error message is displayed to the user
				else
					{
					throw new Exception("No value selected");
					}
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
					if(cboTypes.SelectedValue == "Met Stations")
						{
						bool bErrors = false;
						ImportClass.ImportMetStations(Page, cboRegions.SelectedValue, ref bErrors);
						Server.Transfer("wfViewRegions.aspx");
						}
					else if(cboTypes.SelectedItem.Text == "Soils")
						{
						bool bErrors = false;
						ImportClass.ImportSoils(Page, cboRegions.SelectedValue, ref bErrors);
						Server.Transfer("wfViewRegions.aspx");
						}
					//If no type is selected then display an error message to the user
					else
						{
						FunctionsClass.DisplayMessage(Page, "No type selected");
						}
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "No region selected");
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Returns the selected region, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		private void EditValue()
		{
			if(lstValues.SelectedValue != "")
			{
				if(cboTypes.SelectedValue == "Met Stations")
				{
					Server.Transfer("wfEditMetStation.aspx");
				}
				else if(cboTypes.SelectedItem.Text == "Soils")
				{
					Server.Transfer("wfEditSoil.aspx");
				}
					//If no type is selected then display an error message to the user
				else
				{
					FunctionsClass.DisplayMessage(Page, "No type selected");
				}
			}
			else
			{
				FunctionsClass.DisplayMessage(Page, "No value selected");
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
		//Returns the selected type, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedType()
		{
			return cboTypes.SelectedValue;
		} 
		//-------------------------------------------------------------------------
		//Returns the selected value, can be called from other pages on the page load event
		//-------------------------------------------------------------------------
		public string ReturnSelectedValue()
		{
			return lstValues.SelectedValue;
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
				StoreReportSelection();
				}
			//Adds an attribute to the delete button that causes a 
			//confirmation warning to appear when the user presses the button
			btnDelete.Attributes.Add("onclick", "return confirm (\"Are you sure you wish to delete the selected value(s) \");");
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
		//When the user presses the edit button, they are transfered to the edit
		//page
		//-------------------------------------------------------------------------
		private void btnEdit_Click(object sender, System.EventArgs e)
		{
		EditValue();
		}
		//-------------------------------------------------------------------------
		//When the user presses the edit image, they are transfered to the edit
		//page
		//-------------------------------------------------------------------------
		private void btnEditImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
		{
		EditValue();
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

		private void LinkButton1_Click(object sender, System.EventArgs e)
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
		#endregion



		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
