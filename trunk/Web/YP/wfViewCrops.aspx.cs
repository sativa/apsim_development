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
			DataTable dtRegions = DataAccessClass.GetAllCrops();
			cboCrops.DataSource = dtRegions;
			cboCrops.DataTextField = "Type";
			cboCrops.DataValueField = "ID";
			cboCrops.DataBind();
		}
		//-------------------------------------------------------------------------
		//Fills the cultivar combo with all cultivars that are linked to the 
		//selected crop
		//-------------------------------------------------------------------------
		private void FillCultivarList()
			{
			//If a crop is selected then fill the cultivar combo box
			if(cboCrops.SelectedValue != "" && cboCrops.SelectedValue != "")
				{
				DataTable dtCultivars = DataAccessClass.GetAllCultivarsOfCrop(cboCrops.SelectedValue);
				lstCultivars.DataSource = dtCultivars;
				lstCultivars.DataTextField = "Type";
				lstCultivars.DataValueField = "ID";
				lstCultivars.DataBind();
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
			if(lstCultivars.SelectedValue != null && lstCultivars.SelectedValue != "")
				{
				DataAccessClass.DeleteCulivar(lstCultivars.SelectedValue);
				Server.Transfer("wfViewCrops.aspx");
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
			if(cboCrops.SelectedValue != "")
				{
				CheckForUploadedFiles();
				}
			//If no crop is selected then display an error message to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "No crop selected");
				}
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the a file has been selected for upload and
		//that the file isn't not empty.  If both checks are passed then the file
		//is uploaded
		//-------------------------------------------------------------------------
		private void CheckForUploadedFiles()
		{
			HttpFileCollection hfcImportedFiles = Request.Files;
			//If there is at least one file to upload then proceed to the next check
			if(Request.Files.Count > 0)
				{
				//Get the first file
				HttpPostedFile hpfImportedFile = hfcImportedFiles[0];
				//If the file isn't empty then upload the file
				if(hpfImportedFile.ContentLength > 0)
					{
					UploadFile(hpfImportedFile);
					}
				//If the file is empty then display an error to the user
				else
					{
					FunctionsClass.DisplayMessage(Page, "File is empty");
					}
				}
			//If there is no file to upload then display an error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please select a file to upload");
				}
			hfcImportedFiles = null;
		}
		//-------------------------------------------------------------------------
		//The selected file is stored as a byte array which is then written to 
		//a memory stream and converted to xml which is then saved in the database
		//-------------------------------------------------------------------------
		private void UploadFile(HttpPostedFile hpfImportedFile)
			{
			try
				{
				//Checks to make sure that the file is an xml file
				string szContentType = hpfImportedFile.ContentType;
				if(szContentType == "text/xml")
					{
					//Reads the file into an byte array
					int iFileLength = hpfImportedFile.ContentLength;
					byte[] byFileData = new byte[iFileLength];
					hpfImportedFile.InputStream.Read(byFileData, 0, iFileLength);
					//Writes the byte array into a memory stream
					System.IO.MemoryStream msImportFile = new System.IO.MemoryStream();
					msImportFile.Write(byFileData, 0, iFileLength);
					msImportFile.Seek(0, System.IO.SeekOrigin.Begin);
					//Write the memory stream to a dataset
					DataSet dsImportedTable = new DataSet();
					dsImportedTable.ReadXml(msImportFile);
					//Sets the data to the selected crop
					DataTable dtImportedTable = SetDataToSelectedCrop(dsImportedTable.Tables[0]);

					DataAccessClass.InsertMulitpleRecords(dtImportedTable, "CultivarTypes");
					//Refreshes the data on the screen
					FillCultivarList();
					}
				else
					{
					FunctionsClass.DisplayMessage(Page, "Invalid file type");
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error Importing File");
				}
			}
		//-------------------------------------------------------------------------
		//Sets the imported data to be linked to the correct crop before being
		//stored in the database
		//-------------------------------------------------------------------------
		private DataTable SetDataToSelectedCrop(DataTable dtImportedTable)
			{
			dtImportedTable.TableName = "CropTypes";
			string szSelectedCropID = cboCrops.SelectedValue;
			foreach(DataRow drCurrent in dtImportedTable.Rows)
				{
				drCurrent["CropTypeID"] = szSelectedCropID;
				}
			return dtImportedTable;
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
		}//END CLASS
	}//END NAMESPACE
