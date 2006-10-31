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
	/// Summary description for wfRegistrationTrial.
	/// </summary>
	public class wfRegistrationTrial : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.CheckBox chkNo;
		protected System.Web.UI.WebControls.CheckBox chkYes;
		protected System.Web.UI.WebControls.Label Label9;
		protected System.Web.UI.WebControls.TextBox edtConsultantsEmail;
		protected System.Web.UI.WebControls.TextBox edtConsultantsName;
		protected System.Web.UI.WebControls.Label Label6;
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.Label Label8;
		protected Janus.Web.GridEX.GridEX grdPaddocks;
		protected System.Web.UI.WebControls.TextBox edtEmail;
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
		protected System.Web.UI.WebControls.TextBox edtConsultantsPhone;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressOne;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressTwo;
		protected System.Web.UI.WebControls.TextBox edtPostalAddressThree;
		protected System.Web.UI.WebControls.TextBox edtPostCode;
		protected System.Web.UI.WebControls.Label Label16;
		protected System.Web.UI.WebControls.Label Label15;
		protected System.Web.UI.WebControls.TextBox edtTown;
		protected System.Web.UI.WebControls.Label Label13;
		protected System.Web.UI.WebControls.Label Label11;
		protected System.Web.UI.WebControls.Label Label5;
		protected System.Web.UI.WebControls.HyperLink hylRegistrationMeu;
		protected System.Web.UI.WebControls.HyperLink hylHome;
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
			this.chkYes.CheckedChanged += new System.EventHandler(this.chkYes_CheckedChanged);
			this.chkNo.CheckedChanged += new System.EventHandler(this.chkNo_CheckedChanged);
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
			for(int iIndex = 0; iIndex < 1; iIndex++)
			{
				dtGrowers.Rows.Add(dtGrowers.NewRow());
			}
			grdPaddocks.DataSource = dtGrowers;
			grdPaddocks.DataBind();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SetConsultantStatus()
		{
			edtConsultantsPhone.Enabled = chkYes.Checked;
			edtConsultantsName.Enabled = chkYes.Checked;
			edtConsultantsEmail.Enabled = chkYes.Checked;
			if(chkYes.Checked == false)
			{
				edtConsultantsPhone.Text = "";
				edtConsultantsName.Text = "";
				edtConsultantsEmail.Text = "";
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void SendRegistration()
		{
			try
			{
				string szBody = RegistrationClass.ReturnEmailBody(edtFirstName.Text, edtSecondName.Text, null, edtPostalAddressOne.Text,
					edtPostalAddressTwo.Text, edtPostalAddressThree.Text, edtTown.Text, edtPostCode.Text, edtPhone.Text,
					edtMobile.Text, edtFax.Text, edtEmail.Text, edtConsultantsName.Text, edtConsultantsPhone.Text, 
					edtConsultantsEmail.Text, grdPaddocks, "Trial");

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
		#endregion



		#region Form Events
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				SetNumberOfPaddocks();
			}
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void chkNo_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkNo.Checked == true)
				chkYes.Checked = false;
			else
				chkYes.Checked = true;	
			SetConsultantStatus();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		private void chkYes_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkYes.Checked == true)
				chkNo.Checked = false;
			else
				chkNo.Checked = true;
			SetConsultantStatus();
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
			SendRegistration();
		}


		#endregion




	//---------------------------------------------------------------------
	}//END OF CLASS
}//END OF NAMESPACE
