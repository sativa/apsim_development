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
	/// Summary description for wfReportGenerated.
	/// </summary>
	public class wfReportGenerated : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.LinkButton btnBack;
		protected System.Web.UI.WebControls.Label lblNotice;
		protected System.Timers.Timer tmrRedirect;
		protected System.Web.UI.WebControls.LinkButton btnGrowers;
		protected System.Web.UI.WebControls.Label lblDivider;
		protected System.Web.UI.WebControls.Panel pnlTop;
	
		private void Page_Load(object sender, System.EventArgs e)
			{
			FunctionsClass.CheckSession();
			FunctionsClass.CheckForGrowerLevelPriviledges();
			if(FunctionsClass.IsConsultantOrHigher(Session["UserName"].ToString()) || 
				FunctionsClass.IsVisitorConsultant(Session["UserName"].ToString()) == true)
				{
				btnGrowers.Enabled = true;
				btnGrowers.Visible = true;
				}
			else
				{
				btnGrowers.Enabled = false;
				btnGrowers.Visible = false;
				}

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
			this.btnBack.Click += new System.EventHandler(this.btnBack_Click);
			this.btnGrowers.Click += new System.EventHandler(this.btnGrowers_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion

		private void btnBack_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfEditPaddock.aspx");
			}

		private void btnGrowers_Click(object sender, System.EventArgs e)
			{
			Server.Transfer("wfManageUsers.aspx");
			}
		}
	}
