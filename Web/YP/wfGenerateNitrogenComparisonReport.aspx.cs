using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Xml;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfGenerateNitrogenComparisonReport.
	/// </summary>
	public class wfGenerateNitrogenComparisonReport : System.Web.UI.Page
	{
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected System.Data.DataSet dsSowDate;
		protected System.Data.DataTable dtSowDate;
		protected System.Data.DataColumn dcSowDate;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.Label lblScenarioOneApplications;
		protected Janus.Web.GridEX.GridEX grdScenarioOne;
		protected Janus.Web.GridEX.GridEX grdScenarioTwo;
		protected System.Web.UI.WebControls.Label lblScenarioTwoApplications;
		protected System.Web.UI.WebControls.Label lblScenarioThreeApplications;
		protected Janus.Web.GridEX.GridEX grdScenarioThree;
		protected System.Web.UI.WebControls.Button btnGenerate;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.CheckBox chkFavourite;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Data.DataTable dtNitrogenOne;
		protected System.Data.DataColumn dcIDOne;
		protected System.Data.DataColumn dcApplicationDateOne;
		protected System.Data.DataColumn dcRateOne;
		protected System.Data.DataTable dtNitrogenTwo;
		protected System.Data.DataColumn dcIDTwo;
		protected System.Data.DataColumn dcApplicationDateTwo;
		protected System.Data.DataColumn dcRateTwo;
		protected System.Data.DataTable dtNitrogenThree;
		protected System.Data.DataColumn dcIDThree;
		protected System.Data.DataColumn dcApplicationDateThree;
		protected System.Data.DataColumn dcRateThree;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;

		private static string szReportType = "Nitrogen comparison report";
		

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Threading.Thread.CurrentThread.CurrentCulture = System.Globalization.CultureInfo.CreateSpecificCulture("en-AU");
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsSowDate = new System.Data.DataSet();
			this.dtSowDate = new System.Data.DataTable();
			this.dcSowDate = new System.Data.DataColumn();
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogenOne = new System.Data.DataTable();
			this.dcIDOne = new System.Data.DataColumn();
			this.dcApplicationDateOne = new System.Data.DataColumn();
			this.dcRateOne = new System.Data.DataColumn();
			this.dtNitrogenTwo = new System.Data.DataTable();
			this.dcIDTwo = new System.Data.DataColumn();
			this.dcApplicationDateTwo = new System.Data.DataColumn();
			this.dcRateTwo = new System.Data.DataColumn();
			this.dtNitrogenThree = new System.Data.DataTable();
			this.dcIDThree = new System.Data.DataColumn();
			this.dcApplicationDateThree = new System.Data.DataColumn();
			this.dcRateThree = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenThree)).BeginInit();
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.grdScenarioOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdScenarioTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdScenarioThree.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			// 
			// dsSowDate
			// 
			this.dsSowDate.DataSetName = "NewDataSet";
			this.dsSowDate.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsSowDate.Tables.AddRange(new System.Data.DataTable[] {
																		   this.dtSowDate});
			// 
			// dtSowDate
			// 
			this.dtSowDate.Columns.AddRange(new System.Data.DataColumn[] {
																			 this.dcSowDate});
			this.dtSowDate.TableName = "SowDate";
			// 
			// dcSowDate
			// 
			this.dcSowDate.ColumnName = "SowDate";
			this.dcSowDate.DataType = typeof(System.DateTime);
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsNitrogen.Tables.AddRange(new System.Data.DataTable[] {
																			this.dtNitrogenOne,
																			this.dtNitrogenTwo,
																			this.dtNitrogenThree});
			// 
			// dtNitrogenOne
			// 
			this.dtNitrogenOne.Columns.AddRange(new System.Data.DataColumn[] {
																				 this.dcIDOne,
																				 this.dcApplicationDateOne,
																				 this.dcRateOne});
			this.dtNitrogenOne.TableName = "NitrogenOne";
			// 
			// dcIDOne
			// 
			this.dcIDOne.ColumnName = "ID";
			// 
			// dcApplicationDateOne
			// 
			this.dcApplicationDateOne.Caption = "ApplicationDate";
			this.dcApplicationDateOne.ColumnName = "ApplicationDate";
			this.dcApplicationDateOne.DataType = typeof(System.DateTime);
			// 
			// dcRateOne
			// 
			this.dcRateOne.ColumnName = "Rate";
			// 
			// dtNitrogenTwo
			// 
			this.dtNitrogenTwo.Columns.AddRange(new System.Data.DataColumn[] {
																				 this.dcIDTwo,
																				 this.dcApplicationDateTwo,
																				 this.dcRateTwo});
			this.dtNitrogenTwo.TableName = "NitrogenTwo";
			// 
			// dcIDTwo
			// 
			this.dcIDTwo.ColumnName = "ID";
			// 
			// dcApplicationDateTwo
			// 
			this.dcApplicationDateTwo.ColumnName = "ApplicationDate";
			this.dcApplicationDateTwo.DataType = typeof(System.DateTime);
			// 
			// dcRateTwo
			// 
			this.dcRateTwo.ColumnName = "Rate";
			// 
			// dtNitrogenThree
			// 
			this.dtNitrogenThree.Columns.AddRange(new System.Data.DataColumn[] {
																				   this.dcIDThree,
																				   this.dcApplicationDateThree,
																				   this.dcRateThree});
			this.dtNitrogenThree.TableName = "NitrogenThree";
			// 
			// dcIDThree
			// 
			this.dcIDThree.ColumnName = "ID";
			// 
			// dcApplicationDateThree
			// 
			this.dcApplicationDateThree.ColumnName = "ApplicationDate";
			this.dcApplicationDateThree.DataType = typeof(System.DateTime);
			// 
			// dcRateThree
			// 
			this.dcRateThree.ColumnName = "Rate";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSowDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogenThree)).EndInit();

		}
		#endregion
		
		

		#region Form Functions
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void StorePaddockSelection()
		{
			try
			{
				string szPreviousPage = Context.Handler.ToString();
				ViewState["PreviousPage"] = szPreviousPage;

				switch(szPreviousPage)
				{
					case "ASP.wfReportsGenerateConsultant_aspx":
						FunctionsClass.StorePaddockSelectionConsultant(ViewState);
						InitialiseEmptyForm();
						break;

					case "ASP.wfReportsGenerate_aspx":
						FunctionsClass.StorePaddockSelectionGrower(ViewState);
						InitialiseEmptyForm();
						break;

					case "ASP.wfReportsFavouritesConsultant_aspx":
						FunctionsClass.StoreFavouriteSelectionConsultant(ViewState);
						FillFromForFavouriteMode();
						break;

					case "ASP.wfReportsFavourites_aspx":
						FunctionsClass.StoreFavouriteSelectionGrower(ViewState);
						FillFromForFavouriteMode();
						break;

					default:
						break;
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
		private void InitialiseEmptyForm()
		{
			edtReportName.Text = "[PaddockName] "+ szReportType;
			FunctionsClass.InitialiseApplicationGrid(dsNitrogen, 5, this);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFromForFavouriteMode()
		{
			edtReportName.Text = ((DataTable)ViewState["Paddocks"]).Rows[0]["ReportName"].ToString();
			FillFormFromFavouriteXML();
			FunctionsClass.InitialiseApplicationGrid(dsNitrogen, 5, this);
			FunctionsClass.SetToEditFavouriteMode(chkFavourite, btnGenerate);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFormFromFavouriteXML()
		{
			DataTable dtNitrogenApps = DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString());

			int iNitrogenLimit = dtNitrogenApps.Rows.Count;

			string szFavouriteXML = DataAccessClass.GetFavouriteReport(FunctionsClass.GetActiveUserName(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["UserName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["PaddockName"].ToString(), 
				((DataTable)ViewState["Paddocks"]).Rows[0]["ReportName"].ToString(),
				((DataTable)ViewState["Paddocks"]).Rows[0]["ReportType"].ToString());


			System.Xml.XmlDocument me = new XmlDocument();
			// Create the XmlReader object.
			XmlTextReader reader = new XmlTextReader(new System.IO.StringReader(szFavouriteXML));
			me.Load(reader);
			XmlNode xmlScenario = me.DocumentElement;
			xmlScenario = xmlScenario.FirstChild;
			int iScenarioCount = 0;
			int iNitrogenCount = 0;

			foreach(XmlNode xmlUpperChild in xmlScenario.ChildNodes)
			{
				if(xmlUpperChild.Name == "scenario")
				{
					iScenarioCount++;
					iNitrogenCount = 0;
				}

				foreach(XmlNode xmlChildNode in xmlUpperChild.ChildNodes)
				{	
					switch(xmlChildNode.Name)
					{
						case "fertilise":
							iNitrogenCount++;
							if(iNitrogenCount > iNitrogenLimit)
								FunctionsClass.AddNitrogenNodeToNitrogenDataSet(iScenarioCount-1, 
									xmlChildNode, dsNitrogen);
							break;
					}
				}
			}
		}
		//-------------------------------------------------------------------------
		//A report is generated and sent to the apsim run machine
		//-------------------------------------------------------------------------
		private void GenerateReport()
		{
			try
			{
				string szReportName = edtReportName.Text;
				szReportName = szReportName.Trim();
				//Check that their is a report name
				if(szReportName != "")
				{
					//Check that the name of the report won't cause a problem when it is stored in the file system
					if(InputValidationClass.IsInputAValidFileLocationString(szReportName) == true)
					{
						DataTable dtPaddocks = (DataTable)ViewState["Paddocks"];
						string szNewReportName = "";
						for(int iIndex = 0; iIndex < dtPaddocks.Rows.Count; iIndex++)
						{
							szNewReportName = szReportName.Replace("[PaddockName]", dtPaddocks.Rows[iIndex]["PaddockName"].ToString());
							InputValidationClass.ReplaceInvalidFileLocationCharacters(ref szNewReportName);
							//Generate a data table that stores the values particular to the nitrogen report
							string szReportXML = 
								ReportClass.PrepareNitrogenComparisonXML(szNewReportName, szReportType,
								grdScenarioOne, grdScenarioTwo, grdScenarioThree,
								dtPaddocks.Rows[iIndex]["UserName"].ToString(), dtPaddocks.Rows[iIndex]["PaddockName"].ToString());
							if(chkFavourite.Checked == true)
							{
								DataAccessClass.SetFavouriteReport(FunctionsClass.GetActiveUserName(), dtPaddocks.Rows[iIndex]["UserName"].ToString(),
									dtPaddocks.Rows[iIndex]["PaddockName"].ToString(), DateTime.Today.ToString("yyyy-MM-dd"), 
									szReportType, szNewReportName, szReportXML);
							}
							if(FunctionsClass.IsSendEmail(btnGenerate))
							{
								
								//Generate the files needed to generate a report and then email these files to the ApsimRun machine
								if(EmailClass.SendReportEmail(szNewReportName, dtPaddocks.Rows[iIndex]["CropType"].ToString(), 
									szReportType, szReportXML,  dtPaddocks.Rows[iIndex]["UserName"].ToString(),
									dtPaddocks.Rows[iIndex]["PaddockName"].ToString()) == true)
								{
								}
								else
									throw new Exception("Error requesting report");			
							}
						}
						FunctionsClass.TransferAfterCompletion(btnGenerate);
					}
					else
						throw new Exception(InputValidationClass.ReturnInvalidLocationMessage("Report description"));
				}
				else
					throw new Exception("Please enter a report name");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Cancel()
		{
			string szPreviousPage = ViewState["PreviousPage"].ToString();

			switch(szPreviousPage)
			{
				case "ASP.wfReportsFavouritesConsultant_aspx":
					Server.Transfer("wfReportsFavouritesConsultant.aspx");
					break;

				case "ASP.wfReportsFavourites_aspx":
					Server.Transfer("wfReportsFavourites.aspx");
					break;

				case "ASP.wfReportsGenerateConsultant_aspx":
					Server.Transfer("wfReportsGenerateConsultant.aspx");
					break;

				case "ASP.wfReportsGenerate_aspx":
					Server.Transfer("wfReportsGenerate.aspx");
					break;

				default:
					break;
			}
		}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//Sets the page up and stores the values need to generate the report
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				FunctionsClass.CheckForWritePriviledges();

				StorePaddockSelection();
				
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetReportNavigationButtons(btnReportsView, btnNewReports, btnFavouriteReports);
				FunctionsClass.SetControlFocus("edtReportName", this);
				FunctionsClass.SetDisplayBanner(imgBanner);
				}
			}
		//-------------------------------------------------------------------------
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
		//-------------------------------------------------------------------------
		private void grd_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
			{
			if(e.Column.Key == "Rate")
				{
				if(e.Value != null)
					{
					if(InputValidationClass.IsInputAPositiveDecimal(e.Value.ToString()) == false)
						{
						e.Value = "0";
						}
					}
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
		//
		//-------------------------------------------------------------------------
		private void btnGenerate_Click(object sender, System.EventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Cancel();
		}
		//-------------------------------------------------------------------------
		#endregion


		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
