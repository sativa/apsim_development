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

namespace YieldProphet
{
	/// <summary>
	/// Summary description for wfSurvey.
	/// </summary>
	public class wfSurvey : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Image imgBCGLogo;
		protected System.Web.UI.WebControls.Label lblHeadingOne;
		protected System.Web.UI.WebControls.Label lblInstructions;
		protected System.Web.UI.WebControls.Label lblQuestionOne;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionOne;
		protected System.Web.UI.WebControls.TextBox edtOtherTwo;
		protected System.Web.UI.WebControls.TextBox edtOtherOne;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTwo;
		protected System.Web.UI.WebControls.Label lblQuestionTwo;
		protected System.Web.UI.WebControls.Button btnSubmit;
		protected System.Web.UI.WebControls.Label lblQuestionThree;
		protected System.Web.UI.WebControls.Label lblQuestionFour;
		protected System.Web.UI.WebControls.Label lblQuestionFour_One;
		protected System.Web.UI.WebControls.Label lblNotLimited_4;
		protected System.Web.UI.WebControls.Label lblVeryLimited_4;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Three;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionThree;
		protected System.Web.UI.WebControls.TextBox edtOtherThree;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_One;
		protected System.Web.UI.WebControls.Label lbl1_4;
		protected System.Web.UI.WebControls.Label lbl2_4;
		protected System.Web.UI.WebControls.Label lbl4_4;
		protected System.Web.UI.WebControls.Label lbl3_4;
		protected System.Web.UI.WebControls.Label lbl5_4;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Four;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Five;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Nine;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Eight;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Seven;
		protected System.Web.UI.WebControls.Label lblQuestionFour_Six;
		protected System.Web.UI.WebControls.Label lblFour_Other;
		protected System.Web.UI.WebControls.TextBox edtOtherFour;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Three;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Seven;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Six;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Nine;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Eight;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Five;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFour_Four;
		protected System.Web.UI.WebControls.TextBox edtQuestionFourElaborate;
		protected System.Web.UI.WebControls.Label lblQuestionFive;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Three;
		protected System.Web.UI.WebControls.Label lblQuestionFive_Two;
		protected System.Web.UI.WebControls.Label lblQuestionFive_One;
		protected System.Web.UI.WebControls.Label lblVeryHappy_5;
		protected System.Web.UI.WebControls.Label lblVeryUnhappy_5;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Three;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_Two;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionFive_One;
		protected System.Web.UI.WebControls.Label lblQuestionSix;
		protected System.Web.UI.WebControls.Label lbl5_5;
		protected System.Web.UI.WebControls.Label lbl4_5;
		protected System.Web.UI.WebControls.Label lbl3_5;
		protected System.Web.UI.WebControls.Label lbl2_5;
		protected System.Web.UI.WebControls.Label lbl1_5;
		protected System.Web.UI.WebControls.Label lblVeryHappy_6;
		protected System.Web.UI.WebControls.Label lblVeryUnhappy_6;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSix_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSix_Two;
		protected System.Web.UI.WebControls.Label lblQuestionSix_Two;
		protected System.Web.UI.WebControls.Label lblQuestionSix_One;
		protected System.Web.UI.WebControls.Label lblQuestionSeven;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionSeven;
		protected System.Web.UI.WebControls.TextBox edtOtherSeven;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionEight;
		protected System.Web.UI.WebControls.Label lblQuestionEight;
		protected System.Web.UI.WebControls.Label lblVeryUseful_9;
		protected System.Web.UI.WebControls.Label lblNotAtAllUseful_9;
		protected System.Web.UI.WebControls.Label lbl5_9;
		protected System.Web.UI.WebControls.Label lbl4_9;
		protected System.Web.UI.WebControls.Label lbl3_9;
		protected System.Web.UI.WebControls.Label lbl2_9;
		protected System.Web.UI.WebControls.Label lbl1_9;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionNine_One;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionNine_Two;
		protected System.Web.UI.WebControls.Label lblQuestionNine_Two;
		protected System.Web.UI.WebControls.Label lblQuestionNine_One;
		protected System.Web.UI.WebControls.Label lblQuestionNine;
		protected System.Web.UI.WebControls.TextBox edtOtherEight;
		protected System.Web.UI.WebControls.TextBox edtOtherTen;
		protected System.Web.UI.WebControls.CheckBoxList chkQuestionTen;
		protected System.Web.UI.WebControls.Label lblQuestionTen;
		protected System.Web.UI.WebControls.Label lblQuestionEleven;
		protected System.Web.UI.WebControls.Label lblQuestionTwelve;
		protected System.Web.UI.WebControls.Label lbl5_6;
		protected System.Web.UI.WebControls.Label lbl4_6;
		protected System.Web.UI.WebControls.Label lbl3_6;
		protected System.Web.UI.WebControls.Label lbl2_6;
		protected System.Web.UI.WebControls.Label lbl1_6;
		protected System.Web.UI.WebControls.TextBox edtTwelve;
		protected System.Web.UI.WebControls.TextBox edtEleven;
		protected System.Web.UI.WebControls.Label lblQuestionFourElaborate;
		protected System.Web.UI.WebControls.Label lblHeadingTwo;
	
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
			GetCheckBoxListSelection(chkQuestionOne, edtOtherOne, lblQuestionOne.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionTwo, edtOtherTwo, lblQuestionTwo.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionThree, edtOtherThree, lblQuestionThree.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionFour.Text);
			GetCheckBoxListSelection(chkQuestionFour_One, null, lblQuestionFour_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Two, null, lblQuestionFour_Two.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Three, null, lblQuestionFour_Three.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Four, null, lblQuestionFour_Four.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Five, null, lblQuestionFour_Five.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Six, null, lblQuestionFour_Six.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Seven, null, lblQuestionFour_Seven.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Eight, null, lblQuestionFour_Eight.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFour_Nine, null, lblQuestionFour_Nine.Text, ref scUsersResults);
			if(edtOtherFour.Text != "")
				{
				scUsersResults.Add("Other: "+edtOtherFour.Text);
				}
			scUsersResults.Add(lblQuestionFourElaborate.Text+" "+edtQuestionFourElaborate.Text);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionFive.Text);
			GetCheckBoxListSelection(chkQuestionFive_One, null, lblQuestionFive_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Two, null, lblQuestionFive_Two.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionFive_Three, null, lblQuestionFive_Three.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionSix.Text);
			GetCheckBoxListSelection(chkQuestionSix_One, null, lblQuestionSix_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionSix_Two, null, lblQuestionSix_Two.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionSeven, edtOtherSeven, lblQuestionSeven.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionEight, edtOtherEight, lblQuestionEight.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionNine.Text);
			GetCheckBoxListSelection(chkQuestionNine_One, null, lblQuestionNine_One.Text, ref scUsersResults);
			GetCheckBoxListSelection(chkQuestionNine_Two, null, lblQuestionNine_Two.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			GetCheckBoxListSelection(chkQuestionTen, edtOtherTen, lblQuestionTen.Text, ref scUsersResults);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionEleven.Text+" : "+edtEleven.Text);
			scUsersResults.Add("\r\n");
			scUsersResults.Add(lblQuestionTwelve.Text+" : "+edtTwelve.Text);
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
				if(EmailClass.SendEmail("james.hunt@aanet.com.au", "Dean.Holzworth@csiro.au", "YP 2004 Survey", szBodyText, null, System.Web.Mail.MailPriority.Normal) == true &&
					EmailClass.SendEmail("info@bcg.org.au", "Dean.Holzworth@csiro.au", "YP 2004 Survey", szBodyText, null, System.Web.Mail.MailPriority.Normal) == true)
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
