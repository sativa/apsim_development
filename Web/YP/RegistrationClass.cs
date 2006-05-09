using System;
using System.Text;
using System.Data;
using Janus.Web.GridEX;

namespace YP2006
{
	/// <summary>
	/// Summary description for RegistrationClass.
	/// </summary>
	public class RegistrationClass
	{
		public RegistrationClass()
		{

		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private DataTable ReturnPaddocks(GridEX grdPaddocks)
		{
			DataTable dtPaddocks = new DataTable("Paddocks");
			dtPaddocks.Columns.Add("PaddockName");
			dtPaddocks.Columns.Add("SoilType");
			dtPaddocks.Columns.Add("NearestTown");
			dtPaddocks.Columns.Add("Distance");
			
			DataRow drPaddock;
			GridEXRow grdRow;
			grdPaddocks.UpdateOnLeave = true;
			for(int iIndex = 0; iIndex < grdPaddocks.RowCount; iIndex++)
			{
				grdRow = grdPaddocks.GetRow(iIndex);
				if(grdRow.Cells["PaddockName"].Value != null && grdRow.Cells["SoilType"].Value != null)
				{
					drPaddock = dtPaddocks.NewRow();
					drPaddock["PaddockName"] = grdRow.Cells["PaddockName"].Text;
					drPaddock["SoilType"] = grdRow.Cells["SoilType"].Text;
					drPaddock["NearestTown"] = grdRow.Cells["NearestTown"].Text;
					drPaddock["Distance"] = grdRow.Cells["Distance"].Text;
					dtPaddocks.Rows.Add(drPaddock);
				}
			}

			return dtPaddocks;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public bool SendRegistrationEmail(string szBody)
		{
			bool bSent = false;
			System.Collections.Specialized.NameValueCollection settings = 
				(System.Collections.Specialized.NameValueCollection)System.
				Configuration.ConfigurationSettings.GetConfig("CSIRO/YieldProphet");
			string szRecieveEmailFrom = Convert.ToString(settings["RegistrationEmailFrom"]);
			string szRegistrationEmailOne = Convert.ToString(settings["RegistrationEmailToOne"]);
			string szRegistrationEmailTwo = Convert.ToString(settings["RegistrationEmailToTwo"]);
			string szRegistrationEmailThree = Convert.ToString(settings["RegistrationEmailToThree"]);
			if(EmailClass.SendEmail(szRegistrationEmailOne, szRecieveEmailFrom, "YP2006 Registration", szBody, 
				null, System.Web.Mail.MailPriority.Normal) == true  &&
				EmailClass.SendEmail(szRegistrationEmailTwo, szRecieveEmailFrom, "YP2006 Registration", szBody, 
				null, System.Web.Mail.MailPriority.Normal) == true && 
				EmailClass.SendEmail(szRegistrationEmailThree, szRecieveEmailFrom, "YP2006 Registration", szBody, 
				null, System.Web.Mail.MailPriority.Normal) == true)
			{
				bSent = true;
			}
			return bSent;
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public string ReturnEmailBody(string szFirstName, string szSecondName, string szBusinessName,
			string szPostalAddressOne, string szPostalAddressTwo, string szPostalAddressThree, string szTown,
			string szPostCode, string szPhone, string szMobile, string szFax, string szEmail,
			string szConsultantName, string szConsultantPhone, string szConsultantEmail, GridEX grdPaddocks, 
			string szRegistrationType)
		{

			StringBuilder sbBody = new StringBuilder("");

			if(szRegistrationType != null && szRegistrationType != "")
			{
				sbBody.Append("REGISTRATION TYPE: "+szRegistrationType+"\n\n");
			}
			if(szFirstName != null && szFirstName != "")
			{
				sbBody.Append("First name: "+szFirstName+"\n");
			}
			if(szSecondName != null && szSecondName != "")
			{
				sbBody.Append("Second name: "+szSecondName+"\n");
			}
			if(szBusinessName != null && szBusinessName != "")
			{
				sbBody.Append("Business name: "+szBusinessName+"\n");
			}
			if(szPostalAddressOne != null && szPostalAddressOne != "")
			{
				sbBody.Append("Address line 1: "+szPostalAddressOne+"\n");
			}
			if(szPostalAddressTwo != null && szPostalAddressTwo != "")
			{
				sbBody.Append("Address line 2: "+szPostalAddressTwo+"\n");
			}
			if(szPostalAddressThree != null && szPostalAddressThree != "")
			{
				sbBody.Append("Address line 3: "+szPostalAddressThree+"\n");
			}
			if(szTown != null && szTown != "")
			{
				sbBody.Append("Town: "+szTown+"\n");
			}
			if(szPostCode != null && szPostCode != "")
			{
				sbBody.Append("Post code: "+szPostCode+"\n");
			}
			if(szPhone != null && szPhone != "")
			{
				sbBody.Append("Phone: "+szPhone+"\n");
			}
			if(szMobile != null && szMobile != "")
			{
				sbBody.Append("Mobile: "+szMobile+"\n");
			}
			if(szFax != null && szFax != "")
			{
				sbBody.Append("Fax: "+szFax+"\n");
			}
			if(szEmail != null && szEmail != "")
			{
				sbBody.Append("Email: "+szEmail+"\n");
			}
			if(szConsultantName != null && szConsultantName != "")
			{
				sbBody.Append("Consultant's email: "+szConsultantName+"\n");
			}
			if(szConsultantPhone != null && szConsultantPhone != "")
			{
				sbBody.Append("Consultant's phone: "+szConsultantPhone+"\n");
			}
			if(szConsultantEmail != null && szConsultantEmail != "")
			{
				sbBody.Append("Consultant's email: "+szConsultantEmail+"\n");
			}
			sbBody.Append("\n");

			if(grdPaddocks != null)
			{
				DataTable dtPaddocks = ReturnPaddocks(grdPaddocks);
				foreach(DataRow drPaddock in dtPaddocks.Rows)
				{
					sbBody.Append("Paddock name: "+drPaddock["PaddockName"].ToString()+"\n");
					sbBody.Append("Soil type: "+drPaddock["SoilType"].ToString()+"\n");
					sbBody.Append("Nearest town: "+drPaddock["NearestTown"].ToString()+"\n");
					sbBody.Append("Directions: "+drPaddock["Distance"].ToString()+"\n");
					sbBody.Append("\n");
				}
			}
			
			return sbBody.ToString();
		}
	}//END OF CLASS
}//END OF REGISTRATION
