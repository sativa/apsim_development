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
using System.Text;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfRegistrationSupportedConsultantGrowers.
	/// </summary>
	public class wfRegistrationSupportedConsultantGrowers : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Button btnShow;
		protected Janus.Web.GridEX.GridEX grdPaddocks;
		protected System.Web.UI.WebControls.TextBox edtEmail;
		protected Janus.Web.GridEX.EditControls.IntegerUpDown edtNumberOfPaddocks;
		protected System.Web.UI.WebControls.TextBox edtSecondName;
		protected System.Web.UI.WebControls.TextBox edtFirstName;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Label Label14;
		protected System.Web.UI.WebControls.Label Label12;
		protected System.Web.UI.WebControls.Label Label7;
		protected System.Web.UI.WebControls.Label Label10;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.Panel pnlConsultant;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnFinish;
		protected System.Web.UI.WebControls.TextBox edtFax;
		protected System.Web.UI.WebControls.TextBox edtMobile;
		protected System.Web.UI.WebControls.TextBox edtPhone;
		protected System.Web.UI.WebControls.TextBox edtPostCode;
		protected System.Web.UI.WebControls.Label Label16;
		protected System.Web.UI.WebControls.TextBox edtTown;
		protected System.Web.UI.WebControls.Label Label15;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressThree;
		protected System.Web.UI.WebControls.Label Label13;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressTwo;
		protected System.Web.UI.WebControls.Label Label11;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressOne;
		protected System.Web.UI.WebControls.Label Label5;
		protected System.Web.UI.WebControls.HyperLink hylRegistrationMeu;
		protected System.Web.UI.WebControls.HyperLink hylHome;
		protected System.Web.UI.WebControls.Label lblNumberOfPaddocks;
		protected System.Web.UI.WebControls.Label lblHelp;
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
			this.btnShow.Click += new System.EventHandler(this.btnShow_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnFinish.Click += new System.EventHandler(this.btnFinish_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion



		#region Form Functions
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SetNumberOfPaddocks()
		{
			DataTable dtGrowers = new DataTable("Growers");
			dtGrowers.Columns.Add("PaddockName");
			dtGrowers.Columns.Add("SoilType");
			dtGrowers.Columns.Add("NearestTown");
			dtGrowers.Columns.Add("Distance");
			//DataRow drGrower;
			for(int iIndex = 0; iIndex < edtNumberOfPaddocks.Value; iIndex++)
			{
				dtGrowers.Rows.Add(dtGrowers.NewRow());
			}
			grdPaddocks.DataSource = dtGrowers;
			grdPaddocks.DataBind();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SendRegistration()
		{
			try
			{
				string szBody = ViewState["EmailBody"].ToString();

				if(RegistrationClass.SendRegistrationEmail(szBody) != true)
				{
					throw new Exception("registration failed to send");
				}
				Server.Transfer("wfRegistrationFinished.aspx");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void StoreDetails()
		{
			string szNewBody = RegistrationClass.ReturnEmailBody(edtFirstName.Text, edtSecondName.Text, null, edtPostalAddressOne.Text,
				edtPostalAddressTwo.Text, edtPostalAddressThree.Text, edtTown.Text, edtPostCode.Text, edtPhone.Text,
				edtMobile.Text, edtFax.Text, edtEmail.Text, null, null, null, grdPaddocks, null);
			StringBuilder sbBody = new StringBuilder(ViewState["EmailBody"].ToString()+"\n");
			sbBody.Append(szNewBody);

			ViewState["EmailBody"] = sbBody.ToString();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SetNumberOfGrowers()
		{
			try
			{
				string szPreviousPage = Context.Handler.ToString();

				if("ASP.wfRegistrationSupportedConsultant_aspx" == szPreviousPage)
				{
					wfRegistrationSupportedConsultant PremiumConsultant = (wfRegistrationSupportedConsultant) HttpContext.Current.Handler;
					ViewState["TotalNumberOfGrowers"] = PremiumConsultant.ReturnNumberOfGrowers();
					ViewState["EmailBody"] = PremiumConsultant.ReturnEmailBody();
					ViewState["CurrentGrowerNumber"] = 1;
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void ResetForm()
		{
			edtFirstName.Text = "";
			edtSecondName.Text = "";
			edtPostalAddressOne.Text = "";
			edtPostalAddressTwo.Text = "";
			edtPostalAddressThree.Text = "";
			edtTown.Text = "";
			edtPostCode.Text = "";
			edtPhone.Text = "";
            edtMobile.Text = "";
			edtFax.Text = "";
			edtEmail.Text = "";

			edtNumberOfPaddocks.Value = 6;
			SetNumberOfPaddocks();
			int iCurrent = Convert.ToInt32(ViewState["CurrentGrowerNumber"].ToString());
			iCurrent++;
			ViewState["CurrentGrowerNumber"] = iCurrent;
			SetHeading();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SetHeading()
		{
			lblHeading.Text = "Yield Prophet 2006 - Grower "+ViewState["CurrentGrowerNumber"].ToString()+
			" of "+ViewState["TotalNumberOfGrowers"].ToString()+" Details";

			lblNumberOfPaddocks.Text = "How many paddocks would you like to subscribe to Grower "+
				ViewState["CurrentGrowerNumber"].ToString()+
				" of "+ViewState["TotalNumberOfGrowers"].ToString()+
				" (one paddock is included in subscription fee, extra paddocks are $110 each):";

			lblHelp.Text = "Register the details of your Grower "+
				ViewState["CurrentGrowerNumber"].ToString()+
				" of "+ViewState["TotalNumberOfGrowers"].ToString()+" and then click 'Next'";

			if(Convert.ToInt32(ViewState["CurrentGrowerNumber"].ToString()) >= 
				Convert.ToInt32(ViewState["TotalNumberOfGrowers"].ToString()))
			{
				btnFinish.Text = "Finish";
				lblHelp.Text = lblHelp.Text.Replace("Next", "Finish");
			}
		}
		#endregion



		#region Form Events
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				edtNumberOfPaddocks.Value = 1;
				SetNumberOfPaddocks();
				SetNumberOfGrowers();
				SetHeading();
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void btnShow_Click(object sender, System.EventArgs e)
		{
			SetNumberOfPaddocks();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfRegistrationMenu.aspx");
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void btnFinish_Click(object sender, System.EventArgs e)
		{
			if(Convert.ToInt32(ViewState["CurrentGrowerNumber"].ToString()) >= 
				Convert.ToInt32(ViewState["TotalNumberOfGrowers"].ToString()))
			{
				StoreDetails();
				SendRegistration();
			}
			else
			{
				StoreDetails();
				ResetForm();
			}
		}


		#endregion




	}
}
