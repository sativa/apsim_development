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
	/// Summary description for wfGenerateReport.
	/// </summary>
	public class wfGenerateReport : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnSave;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Web.UI.WebControls.LinkButton btnFavouriteReports;
		protected System.Web.UI.WebControls.LinkButton btnNewReports;
		protected System.Web.UI.WebControls.LinkButton btnReportsView;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.CheckBox chkFavourite;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnGenerate;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		

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
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnReportsView.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnNewReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnFavouriteReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGenerate.Click += new System.EventHandler(this.btnGenerate_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.Load += new System.EventHandler(this.Page_Load);

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
			if(((DataTable)ViewState["Paddocks"]).Rows.Count > 0)
			{
				edtReportName.Text = "[PaddockName] "+ ((DataTable)ViewState["Paddocks"]).Rows[0]["ReportType"].ToString();
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillFromForFavouriteMode()
		{
			edtReportName.Text = ((DataTable)ViewState["Paddocks"]).Rows[0]["ReportName"].ToString();
			FunctionsClass.SetToEditFavouriteMode(chkFavourite, btnGenerate);
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
				//Check that there is a report name
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
							string szReportXML = ReportClass.PrepareBasicReportXML(szNewReportName, dtPaddocks.Rows[0]["ReportType"].ToString(), dtPaddocks.Rows[iIndex]["UserName"].ToString(), 
										dtPaddocks.Rows[iIndex]["PaddockName"].ToString());

							if(chkFavourite.Checked == true)
								{
								DataAccessClass.SetFavouriteReport(FunctionsClass.GetActiveUserName(), dtPaddocks.Rows[iIndex]["UserName"].ToString(),
									dtPaddocks.Rows[iIndex]["PaddockName"].ToString(), DateTime.Today.ToString("yyyy-MM-dd"), 
									dtPaddocks.Rows[0]["ReportType"].ToString(), szNewReportName, szReportXML);
								}
							if(FunctionsClass.IsSendEmail(btnGenerate))
								{
								if(EmailClass.SendReportEmail(szNewReportName, dtPaddocks.Rows[iIndex]["CropType"].ToString(), 
									dtPaddocks.Rows[0]["ReportType"].ToString(), szReportXML,  dtPaddocks.Rows[iIndex]["UserName"].ToString(),
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
			catch (Exception E)
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

				FunctionsClass.SetControlFocus("edtReportName", this);
				StorePaddockSelection();
				
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetReportNavigationButtons(btnReportsView, btnNewReports, btnFavouriteReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
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
