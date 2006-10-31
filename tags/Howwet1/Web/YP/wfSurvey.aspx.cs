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
using System.Collections.Specialized;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfSurvey.
	/// </summary>
	public class wfSurvey : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblHeadingOne;
		protected System.Web.UI.WebControls.Label lblHeadingTwo;
		protected System.Web.UI.WebControls.Image imgBCGLogo;
		protected System.Web.UI.WebControls.Label lblInstructions;
		protected System.Web.UI.WebControls.Label lblQuestionThirteen;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionThirteen;
		protected System.Web.UI.WebControls.Label lblQuestionFourteen;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFourteen;
		protected System.Web.UI.WebControls.Label lblQuestionSixteen;
		protected System.Web.UI.WebControls.TextBox edtSixteen;
		protected System.Web.UI.WebControls.Label lblQuestionSeventeen;
		protected System.Web.UI.WebControls.TextBox edtSeventeen;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Panel pnlGrower;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFifteen;
		protected System.Web.UI.WebControls.Label lblQuestionFifteen;
		protected System.Web.UI.WebControls.TextBox edtOtherFifteen;
		protected System.Web.UI.WebControls.TextBox edtOtherFourteen;
		protected System.Web.UI.WebControls.TextBox edtOtherThirteen;
		protected System.Web.UI.WebControls.Label lblQuestionTwelve;
		protected System.Web.UI.WebControls.Label lbl5_12;
		protected System.Web.UI.WebControls.Label lbl4_12;
		protected System.Web.UI.WebControls.Label lbl3_12;
		protected System.Web.UI.WebControls.Label lbl2_12;
		protected System.Web.UI.WebControls.Label lbl1_12;
		protected System.Web.UI.WebControls.Label lblQuestionTwelve_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTwelve_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTwelve_Two;
		protected System.Web.UI.WebControls.Label lblQuestionTwelve_Two;
		protected System.Web.UI.WebControls.Label lblQuestionEleven;
		protected System.Web.UI.WebControls.Label lbl5_11;
		protected System.Web.UI.WebControls.Label lbl4_11;
		protected System.Web.UI.WebControls.Label lbl3_11;
		protected System.Web.UI.WebControls.Label lbl2_11;
		protected System.Web.UI.WebControls.Label lbl1_11;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionEleven_Three;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionEleven_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionEleven_One;
		protected System.Web.UI.WebControls.Label lblQuestionEleven_One;
		protected System.Web.UI.WebControls.Label lblQuestionEleven_Two;
		protected System.Web.UI.WebControls.Label lblQuestionEleven_Three;
		protected System.Web.UI.WebControls.Label lblQuestionSix;
		protected System.Web.UI.WebControls.Label lbl5_6;
		protected System.Web.UI.WebControls.Label lbl4_6;
		protected System.Web.UI.WebControls.Label lbl3_6;
		protected System.Web.UI.WebControls.Label lbl2_6;
		protected System.Web.UI.WebControls.Label lbl1_6;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSix_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSix_One;
		protected System.Web.UI.WebControls.Label lblQuestionSix_One;
		protected System.Web.UI.WebControls.Label lblQuestionSix_Two;
		protected System.Web.UI.WebControls.Label lblQuestionEight;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionEight;
		protected System.Web.UI.WebControls.TextBox edtOtherEight;
		protected System.Web.UI.WebControls.Label lblQuestionTen;
		protected System.Web.UI.WebControls.TextBox edtOtherTen;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTen;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionNine;
		protected System.Web.UI.WebControls.Label lblQuestionNine;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Nine;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Eight;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Seven;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Six;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Five;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Four;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Three;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Two;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Four;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Five;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Six;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Seven;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Eight;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Nine;
		protected System.Web.UI.WebControls.Label lblQuestionSeven;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Ten;
		protected System.Web.UI.WebControls.Label lblQuestionSeven_Ten;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven_Three;
		protected System.Web.UI.WebControls.Label lbl1_7;
		protected System.Web.UI.WebControls.Label lbl2_7;
		protected System.Web.UI.WebControls.Label lbl3_7;
		protected System.Web.UI.WebControls.Label lbl4_7;
		protected System.Web.UI.WebControls.Label lbl5_7;
		protected System.Web.UI.WebControls.Label lblQuestionFour;
		protected System.Web.UI.WebControls.Label lblQuestionSevenElaborate;
		protected System.Web.UI.WebControls.TextBox edtQuestionSevenElaborate;
		protected System.Web.UI.WebControls.Label lbSeven_Other;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionThree;
		protected System.Web.UI.WebControls.Label lblQuestionThree;
		protected System.Web.UI.WebControls.TextBox edtOtherFour;
		protected System.Web.UI.WebControls.TextBox edtOtherThree;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.Label Label3;
		protected System.Web.UI.WebControls.Label Label4;
		protected System.Web.UI.WebControls.Label Label5;
		protected System.Web.UI.WebControls.Label Label6;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Three;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Four;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Three;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Two;
		protected System.Web.UI.WebControls.Label lblQuestionFive_One;
		protected System.Web.UI.WebControls.TextBox edtQuestionFive_One;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Five;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Four;
		protected System.Web.UI.WebControls.Label lblQuestionFive;
		protected System.Web.UI.WebControls.TextBox edtQuestionFive_Two;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Six;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTwo;
		protected System.Web.UI.WebControls.Label lblQuestionTwo;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionOne;
		protected System.Web.UI.WebControls.Label lblQuestionOne;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Five;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Six;
		protected System.Web.UI.WebControls.TextBox edtOtherSeven;
		protected System.Web.UI.WebControls.Button btnSubmit;
	
		private void Page_Load(object sender, System.EventArgs e)
			{
			// Put user code to initialize the page here
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
			this.btnSubmit.Click += new System.EventHandler(this.btnSubmit_Click);
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion


		//-------------------------------------------------------------------------
		//Appends the question and the answer to the string collection
		//-------------------------------------------------------------------------
		private void GetCheckBoxListSelection(CheckBoxList cblCurrent, TextBox edtCurrentOther, string szQuestion, ref StringCollection scUsersSelections)
			{
			for(int iIndex = 0; iIndex < cblCurrent.Items.Count; iIndex++)
				{
				if(cblCurrent.Items[iIndex].Selected == true)
					{
					if(edtCurrentOther != null && cblCurrent.Items[iIndex].Value == "Other: ")
						{
						scUsersSelections.Add(szQuestion+" "+cblCurrent.Items[iIndex].Value+edtCurrentOther.Text);
						}
					else
						{
						scUsersSelections.Add(szQuestion+" "+cblCurrent.Items[iIndex].Value);
						}
					}
				}
			}
		//-------------------------------------------------------------------------
		//Converts the string collection to one string
		//-------------------------------------------------------------------------
		private string ConvertStringCollectionToString(StringCollection scUsersSelections)
			{
			System.Text.StringBuilder sbUsersResults = new System.Text.StringBuilder();
			for(int iIndex = 0; iIndex < scUsersSelections.Count; iIndex++)
				{
				sbUsersResults.Append(scUsersSelections[iIndex]+"\r\n");
				}
			return sbUsersResults.ToString();
			}
		//-------------------------------------------------------------------------
		//Gets all the user's responses
		//-------------------------------------------------------------------------
		private string GetBodyText()
			{
			string szBodyText = "";
			StringCollection scUsersResults = new StringCollection();
			GetCheckBoxListSelection(chkQuestionOne, null, lblQuestionOne.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionTwo, null, lblQuestionTwo.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionThree, edtOtherThree, lblQuestionThree.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionFour, edtOtherFour, lblQuestionFour.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionFive.Text);
			GetCheckBoxListSelection(chkQuestionFive_One, null, lblQuestionFive_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Two, null, lblQuestionFive_Two.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Three, null, lblQuestionFive_Three.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Four, null, lblQuestionFive_Four.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Five, null, lblQuestionFive_Five.Text+" "+edtQuestionFive_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Six, null, lblQuestionFive_Six.Text+" "+edtQuestionFive_Two.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionSix.Text);
			GetCheckBoxListSelection(chkQuestionSix_One, null, lblQuestionSix_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSix_Two, null, lblQuestionSix_Two.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionSeven.Text);
			GetCheckBoxListSelection(chkQuestionSeven_One, null, lblQuestionSeven_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Two, null, lblQuestionSeven_Two.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Three, null, lblQuestionSeven_Three.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Four, null, lblQuestionSeven_Four.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Five, null, lblQuestionSeven_Five.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Six, null, lblQuestionSeven_Six.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Seven, null, lblQuestionSeven_Seven.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Eight, null, lblQuestionSeven_Eight.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Nine, null, lblQuestionSeven_Nine.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSeven_Ten, null, lblQuestionSeven_Ten.Text, ref scUsersResults);
			if(edtOtherSeven.Text != "")
				{
				scUsersResults.Add("Other: "+edtOtherSeven.Text);
				}
			scUsersResults.Add(lblQuestionSevenElaborate.Text+" "+edtQuestionSevenElaborate.Text);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionEight, edtOtherEight, lblQuestionEight.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionNine, null, lblQuestionNine.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionTen, edtOtherTen, lblQuestionTen.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionEleven.Text);
			GetCheckBoxListSelection(chkQuestionEleven_One, null, lblQuestionEleven_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionEleven_Two, null, lblQuestionEleven_Two.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionEleven_Three, null, lblQuestionEleven_Three.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionTwelve.Text);
			GetCheckBoxListSelection(chkQuestionTwelve_One, null, lblQuestionTwelve_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionTwelve_Two, null, lblQuestionTwelve_Two.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionThirteen, edtOtherThirteen, lblQuestionThirteen.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionFourteen, edtOtherFourteen, lblQuestionFourteen.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionFifteen, edtOtherFifteen, lblQuestionFifteen.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionSixteen.Text+" : "+edtSixteen.Text);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionSeventeen.Text+" : "+edtSeventeen.Text);
			scUsersResults.Add("\r\n");
				
			szBodyText = ConvertStringCollectionToString(scUsersResults);
			return szBodyText;
			}
		//-------------------------------------------------------------------------
		//Send the results as an email to james and the bcg office
		//-------------------------------------------------------------------------
		private void btnSubmit_Click(object sender, System.EventArgs e)
			{
			try
				{
				string szBodyText = GetBodyText();
				System.Collections.Specialized.NameValueCollection settings = 
					(System.Collections.Specialized.NameValueCollection)System.
					Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
				string szRecieveEmailFrom = Convert.ToString(settings["SurveyEmailFrom"]);
				string szSurveyEmailOne = Convert.ToString(settings["SurveyEmailToOne"]);
				string szSurveyEmailTwo = Convert.ToString(settings["SurveyEmailToTwo"]);
				string szSurveyEmailThree = Convert.ToString(settings["SurveyEmailToThree"]);
				if(EmailClass.SendEmail(szSurveyEmailOne, szRecieveEmailFrom, "YP 2005 Survey", szBodyText, null, System.Web.Mail.MailPriority.Normal) == true &&
					EmailClass.SendEmail(szSurveyEmailTwo, szRecieveEmailFrom, "YP 2005 Survey", szBodyText, null, System.Web.Mail.MailPriority.Normal) == true &&
					EmailClass.SendEmail(szSurveyEmailThree, szRecieveEmailFrom, "YP 2005 Survey", szBodyText, null, System.Web.Mail.MailPriority.Normal) == true)
					{
					FunctionsClass.DisplayMessage(Page, "Thank you very much for taking the time to complete this survey", "wfLogin.aspx");
					}
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
