using System;
using System.Text;
using System.Web.UI.WebControls;

namespace YP2006
{
	/// <summary>
	/// Summary description for HelpClass.
	/// </summary>
	public class HelpClass
	{
		public HelpClass()
		{
		}


		#region Paddock Pages
		
		#region Paddocks Applications Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockApplicationPage(ImageButton imgHelpNitrogen, 
			ImageButton imgHelpIrrigation, ImageButton btnHelpPaddockApplicationsPage)
		{
			SetHelp(imgHelpNitrogen, "Nitrogen Applications", "You use the Nitrogen Applicaton grid to enter the nitrogen applications for the selected paddock.  The application rate field must be a positive number and the application date field must be in the DD/MM/YYYY format.  Both fields must contain data.");
			SetHelp(imgHelpIrrigation, "Irrigation Applications", "You use the Irrigation Applicaton grid to enter the irrigation applications for the selected paddock.  The application amount field and the efficency field must both be positive numbers and the date field must be in the DD/MM/YYYY format.  All three fields must contain data.");
			SetHelp(btnHelpPaddockApplicationsPage, "Paddock\'s Application Page", "The paddock\'s application page is where you enter your irrigation and nitrogen applications for the year");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Paddocks Crop Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockCropPage(ImageButton imgHelpSowDate, 
			ImageButton imgHelpCrop, ImageButton imgHelpCultivar, ImageButton btnHelpPaddockCropPage)
		{
			SetHelp(imgHelpSowDate, "Sowing Date", "You use the sowing date field to set the date that your paddock was sown, the sowing date must be in the DD/MM/YYYY format. To be able to enter data in the sowing date field the Have you sown yet checkbox must be checked.");
			SetHelp(imgHelpCrop, "Crop", "You use the crop drop down to set the type of crop you have sown. To be able to enter data in the crop drop down the Have you sown yet checkbox must be checked.");
			SetHelp(imgHelpCultivar, "Cultivar/Variety", "You use the cultivar drop down to set the type of cultivar you have sown. To be able to enter data in the cultivar drop down the Have you sown yet checkbox must be checked.");
			SetHelp(btnHelpPaddockCropPage, "Paddock\'s Crop Page", "The paddock\'s crop page is where you enter the details concerning the current season\'s crops.  You will probably find that you only visit this page when you are first setting up your paddock for the season.");
		}
		//---------------------------------------------------------------------	
		#endregion

		
		#region Paddocks Information Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockInformationPage(ImageButton imgHelpPaddockName, 
			ImageButton imgHelpStartOfGrowingSeason, ImageButton imgHelpRegion, 
			ImageButton imgHelpWeatherStation, ImageButton imgHelpLinkRainfall, 
			ImageButton imgHelpWeatherStationRainfall, ImageButton imgHelpPaddockInformationPage)
		{
			SetHelp(imgHelpPaddockName, "Paddock Name", "The name of your paddock, this name will appear on reports generated for this paddock.  Each of your paddock's names must be unique.");
			SetHelp(imgHelpStartOfGrowingSeason, "Start of Growing Season", "This function ensures that Yield Prophet runs the simulation for the correct year, and tallies growing season rainfall correctly.  For most regions, this should be set to 1 April of the current year.  The date must be entered in the DD/MM/YYYY format");
			SetHelp(imgHelpRegion, "Region", "Select the most appropriate region for your farm to give you access to the relevant soils and weather stations.  New South Wales is divided into two regions (North and South), regions north of Dubbo are considered \'Northern New South Wales\' and those south of Dubbo are considered \'Southern New South Wales\'.");
			SetHelp(imgHelpWeatherStation, "Closest Weather Station", "Yield Prophet primarily uses the long-term rainfall data from the Bureau of Meteorology weather station to calculate the probability curves generated in the agronomic and climate reports.  Not all weather stations have good long term rainfall data, and for some growers stations may be selected which aren’t necessarily the closest to that grower’s paddock, but are the closest station with good data. ");
			SetHelp(imgHelpLinkRainfall, "Link Rainfall to Existing Paddock", "The link rainall to existing paddock option allows you to link this paddock\'s rainfall information to that of another paddock.  This means if your paddocks are close to each other you only have to enter new rainfall infomation once.");
			SetHelp(imgHelpWeatherStationRainfall, "Use Weather Station\'s Rainfall", "Selecting the use weather station\'s rainfall option eliminates the need for you to enter rainfall for your paddock.  Only use this option if your paddock is very close to the nearest weather station.");
			SetHelp(imgHelpPaddockInformationPage, "Paddock\'s Information Page", "The paddock\'s information page is where all the base information for your paddock is set.  You will probably find that you only visit this page when you are first setting up your paddock.");
		}
		//---------------------------------------------------------------------	
		#endregion

	
		#region Paddocks Rainfall Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockRainfallPage(ImageButton imgHelpYear, 
			ImageButton imgHelpRainfallGrid, ImageButton imgHelpRainfall1April, 
			ImageButton btnHelpRaifallYear, ImageButton imgHelpPaddockRainfallPage)
		{
			SetHelp(imgHelpYear, "Rainfall Year", "Allows you toggle the year which you view and edit rainfall data for.  When you change the year, the current year\'s rainfall is automatically saved.");
			SetHelp(imgHelpRainfallGrid, "Rainfall Grid", "The rainfall grid displays every day in the year, if you wish to record a rainfall event just enter a positive number into the corresponding date field.");
			SetHelp(imgHelpRainfall1April, "Total Rainfall since the First of April", "Displays  the total of all the rainfall events that have occurred since the first of April of the year you are currently viewing to the end of the year you are currently viewing");
			SetHelp(btnHelpRaifallYear, "Total Rainfall in a Calendar Year", "Displays the total of all the rainfall events that have occurred in the currently selected calendar year");
			SetHelp(imgHelpPaddockRainfallPage, "Paddock/'s Rainfall Page", "The paddock\'s rainfall page is where all rainfall events that occur on your paddock are recorded.  It is very important to keep this information as up to date as possible as it plays a large role in the Yield Prophet system.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Paddocks Information Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockSoilPage(ImageButton btnHelpSoilType, 
			ImageButton btnHelpPaddockSoilPage, ImageButton btnHelpRootingDepth, 
			ImageButton btnHelpEC, ImageButton btnHelpInitialConditions, 
			ImageButton btnWaterType, ImageButton btnHelpGridOne, 
			ImageButton btnHelpGridTwo, ImageButton btnHelpGraph)
		{
			SetHelp(btnHelpSoilType, "Soil Type", "Your soil type must be chosen carefully to best reflect the hydraulic properties of your soil.  This may be based on the properties of generic soil types (e.g. Wimmera Clay, Mallee Clay Loam) or from specific measurements or estimations of your own paddocks. You should chose a soil type that is closest to your paddocks type, and that best approximates the soil moisture distribution down the profile that you would expect given your paddock's recent history (previous crop, rainfall, fallow and weeds). If you are uncertain of which soil type is best for your paddock, check with your nominated Yield Prophet consultant or the Yield Prophet coordinator.");
			SetHelp(btnHelpPaddockSoilPage, "Paddock\'s Soil Page", "The paddock\'s soil page is where all the information relating to your paddock\'s soil is set.  You will probably find that you only visit this page when you are first setting up your paddock.");
			SetHelp(btnHelpRootingDepth, "Maximum Rooting Depth", "The maximum depth, in cm, to which roots can grow in your paddock.  The default is 180cm");
			SetHelp(btnHelpEC, "Use EC to Constrain Crop Growth", "Enabling this option will cause Yield Prophet to reduce the rate at which roots can grow through soil based on the levels of EC in your soil");
			SetHelp(btnHelpInitialConditions, "Initial Conditions Date", "This is the date on which your paddock was sampled to determine moisture and nitrogen content. It is very important to set this date correctly, as rainfall and nitrogen applications prior to this date are disregarded by Yield Prophet as soil water and nitrogen are re-set to the measured values on the initial conditions date. The date must be entered in the DD/MM/YYYY format.");
			SetHelp(btnWaterType, "Starting Water Type", "Toggling this option allows you to enter your starting water in either gravimetric or volumetric.  Please not that changing this option once you have entered your data will <B>NOT</B> automatically covert the soil water values for you");
			SetHelp(btnHelpGridOne, "Initial Conditions Grid", "The tables in this section are where you must enter the data from the measurements made on the soil samples taken from your paddock prior to sowing. Enter the depths of the segmentation of your soil cores as described in the left column, and corresponding data in the columns to the right.  Make sure you are entering the correct units!  If you are unsure about entering data, please consult the Yield Prophet Coordinator.");
			SetHelp(btnHelpGridTwo, "Initial Conditions Grid", "The tables in this section are where you must enter the data from the measurements made on the soil samples taken from your paddock prior to sowing. Enter the depths of the segmentation of your soil cores as described in the left column, and corresponding data in the columns to the right.  Make sure you are entering the correct units!  If you are unsure about entering data, please consult the Yield Prophet Coordinator.");
			SetHelp(btnHelpGraph, "Soil Water Chart", "The chart at the bottom of this page is a representation of the amount of soil moisture that is available to your crop on the date that your initial conditions were sampled. It calculates this from the soil moisture measurements made on your soil and the soil type that you select at the top of the page.  The units on this chart are volumetric percentage, and are thus higher than the gravimetric values that you enter. The \'SW\' line is the volumetric soil water content of your paddock.  The \'DUL\' is the volumetric soil water content of the selected soil when it is holding as much water as it can after draining.  The \'LL\' is the volumetric soil water content of the selected soil when the selected crop has extracted as much water as possible from the soil.  The amount of water that your soil can hold that is available to a crop (PAWC - plant available water capacity) is represented by the area between the \'DUL\' and \'LL\' lines (think of this as the size of your \'soil water bucket\'). The amount of water available to your crop at the time of sampling (PAW - plant available water) is the difference between the \'SW\' and \'LL\' line (think of this as level of water in your \'soil water bucket\".");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Paddocks Menu Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPaddockMenuPage(ImageButton btnHelpPaddockMenuPage)
		{
			SetHelp(btnHelpPaddockMenuPage, "Grower\'s Paddock Menu Page", "This page allows you to select the paddock related action you wish to undertake.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#endregion



		#region Report Pages

		#region Consultant Report Favourite Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForConsultantReporFavouritePage(ImageButton btnHelpConsultantFavouriteReportPage)
		{
			SetHelp(btnHelpConsultantFavouriteReportPage, "Consultant\'s Favourite Reports Page", "This page allows you to generate, edit or delete reports you have set as your favourite reports.  Setting a report as a favourite report allows you to generate the same report again at a future date with out having to re-enter in all the information required by the report pages.  Please note that if you make any changes to your paddock, not including rainfall information, you will have to update any reports linked to that paddock by pressing the \'edit favourite report\' button followed by the \'save\' button on the following page.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Report Favourite Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForReporFavouritePage(ImageButton btnHelpReportFavouritePage)
		{
			SetHelp(btnHelpReportFavouritePage, "Grower\'s Favourite Reports Page", "This page allows you to generate, edit or delete reports you have set as your favourite reports.  Setting a report as a favourite report allows you to generate the same report again at a future date with out having to re-enter in all the information required by the report pages.  Please note that if you make any changes to your paddock, not including rainfall information, you will have to update any reports linked to that paddock by pressing the \'edit favourite report\' button followed by the \'save\' button on the following page.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Report Menu Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForReportMenuPage(ImageButton btnHelpReportMenuPage)
		{
			SetHelp(btnHelpReportMenuPage, "Grower\'s Report Menu Page", "This page allows you to select the report related action you wish to undertake");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Consultant Report Menu Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForConsultantReportMenuPage(ImageButton btnHelpConsultantReportMenuPage)
		{
			SetHelp(btnHelpConsultantReportMenuPage, "Grower\'s Report Menu Page", "This page allows you to select the report related action you wish to undertake");
		}
		//---------------------------------------------------------------------	

		#endregion


		#region Report Successful Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForReportSuccessfulPage(ImageButton btnHelpReportSuccessfulPage)
		{
			SetHelp(btnHelpReportSuccessfulPage, "Report Successful Page", "This page informs you that your report request(s) were successful.  You will be notified via email once your reports have been generated.  The time it takes for a report to be generated is highly variable and is affected by the number of report requests in the system.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Display Report Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForDisplayReportPage(ImageButton btnHelpDisplayReportPage)
		{
			SetHelp(btnHelpDisplayReportPage, "Display Report Page", "This page displays your reports online.  The next and previous buttons allow you to cycle between your selected reports.  If you wish to save the report, right click on the report and select the \'Save picture as\' option.  Alternatively if you wish to print the report, right click on the report and select the \'Print picture\' option");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Report View Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForReportViewPage(ImageButton btnHelpYear, 
			ImageButton btnHelpGrid, ImageButton btnHelpReportViewPage)
		{
			SetHelp(btnHelpYear, "Report Year", "Your reports are stored by calendar year. Change the selected year to view your reports from different years.");
			SetHelp(btnHelpGrid, "Report Grid", "The report grid displays all of your reports for the currently selected calendar year.  You can selected multiple reports by holding either the Shift key or Cntl key whilst you click on the reports you wish to select.");
			SetHelp(btnHelpReportViewPage, "View Reports Page", "This page allows you to view or delete the reports you have generated.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Report View Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForConsultantReportViewPage(ImageButton btnHelpYear, 
			ImageButton btnHelpGrid, ImageButton btnHelpFind, 
			ImageButton btnHelpConstulantReportViewPage)
		{
			SetHelp(btnHelpYear, "Report Year", "Your reports are stored by calendar year. Change the selected year to view your reports from different years.");
			SetHelp(btnHelpGrid, "Report Grid", "The report grid displays all of your growers and their reports for the currently selected calendar year.  You can selected multiple reports by holding either the Shift key or Cntl key whilst you click on the reports you wish to select.");
			SetHelp(btnHelpFind, "Find User", "Searches the list of growers to find the desired grower.  The search can either be by the grower's full name, or part of their name, for example a search of \'ste\' will find users with the name of \'stephen\' and \'steven\'");
			SetHelp(btnHelpConstulantReportViewPage, "View Reports Page", "This page allows you to view or delete the reports you or your growers have generated.");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Report Generate Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForReportGeneratePage(ImageButton btnHelpGrid, 
			ImageButton btnHelpReportType, ImageButton btnHelpConsultantReportGeneratePage)
		{
			SetHelp(btnHelpGrid, "Report Grid", "The report grid displays all of your paddocks.  You can selected multiple paddocks by holding either the Shift key or Cntl key whilst you click on the paddocks you wish to select.");
			SetHelp(btnHelpReportType, "Report Type", "Select one the report types available to you.");
			SetHelp(btnHelpConsultantReportGeneratePage, "Geneate Reports Page", "This page allows you to generate new reports");
		}
		//---------------------------------------------------------------------	
		#endregion


		#region Consultant Report Generate Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForConsultantReportGeneratePage(ImageButton btnHelpGrid, 
			ImageButton btnHelpReportType, ImageButton btnHelpFind, 
			ImageButton btnHelpSelectAll, ImageButton btnHelpConsultantReportGeneratePage)
		{
			SetHelp(btnHelpFind, "Find User", "Searches the list of growers to find the desired grower.  The search can either be by the grower's full name, or part of their name, for example a search of \'ste\' will find users with the name of \'stephen\' and \'steven\'");
			SetHelp(btnHelpGrid, "Report Grid", "The report grid displays all of your growers and their paddocks.  You can selected multiple paddocks by holding either the Shift key or Cntl key whilst you click on the paddocks you wish to select.");
			SetHelp(btnHelpReportType, "Report Type", "Select one the report types available to you.");
			SetHelp(btnHelpSelectAll, "Select All", "By checking this checkbox, all of your grower's paddocks will be registered as being selected");
			SetHelp(btnHelpConsultantReportGeneratePage, "Geneate Reports Page", "This page allows you to generate new reports, for any of your growers");
		}
		//---------------------------------------------------------------------	
		#endregion


		#endregion



		#region Main Menu Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForMainMenuPage(ImageButton btnHelpMainMenuPage)
		{
			SetHelp(btnHelpMainMenuPage, "Grower\'s Main Menu Page", "This page provides you with a menu from where you can choose the type of actions you wish to undertake");
		}
		//---------------------------------------------------------------------	
		#endregion



		#region Manage Growers Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForManageGrowersPage(ImageButton btnHelpFind, 
			ImageButton btnHelpManageGrowerPage)
		{
			SetHelp(btnHelpFind, "Find User", "Searches the list of growers to find the desired grower.  The search can either be by the grower's full name, or part of their name, for example a search of \'ste\' will find users with the name of \'stephen\' and \'steven\'");
			SetHelp(btnHelpManageGrowerPage, "Manage Growers Page", "This page is where you can navigate between your growers and view either their report information or their paddock information.");
		}
		//---------------------------------------------------------------------	
		#endregion



		#region Personal Pages

		#region Personal Details Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPersonalDetailsPage(ImageButton imgHelpName, ImageButton imgHelpEmail, 
			ImageButton imgHelpStartPage, ImageButton btnHelpPersonalDetailsPage)
		{
			SetHelp(imgHelpName, "Display Name", "Your display name is used to identify you within the Yield Prophet web site as well as on Yield Prophet reports");
			SetHelp(imgHelpEmail, "Email", "The email address that is used to contact you concerning Yield Prophet report completion");
			SetHelp(imgHelpStartPage, "Start Page", "The first page that will be displayed after you log in successfully, the default page is the Main Menu page.");
			SetHelp(btnHelpPersonalDetailsPage, "Personal Details Page", "This page allows you to view and modify your personal settings.");
		}
		//---------------------------------------------------------------------	
		#endregion



		#region Personal Password Page
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public void SetHelpForPersonalPasswordPage(ImageButton imgHelpPassword, 
			ImageButton imgHelpPasswordReenter, ImageButton btnHelpPasswordPage)
		{
			SetHelp(imgHelpPassword, "Password", "Your passord is used to log you into the system and is case sensitive.");
			SetHelp(imgHelpPasswordReenter, "Password re-enter", "You are required to re-enter your password to ensure that you entered it exactly as you had intended");
			SetHelp(btnHelpPasswordPage, "Password Page", "This Page allows you to change your password.");	
		}
		//---------------------------------------------------------------------	
		#endregion

		#endregion



		#region Display Help Functions
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void SetHelp(ImageButton imgHelp, string szHelpHeading, string szHelpText)
		{
			RemoveFormattingForWindow(ref szHelpText);
			RemoveFormattingForWindow(ref szHelpHeading);
			SetDisplayHelpWindow(imgHelp, ref szHelpHeading, ref szHelpText);
			RemoveFormattingForToolTip(ref szHelpText);
			SetToolTipHelp(imgHelp, ref szHelpText);
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void SetDisplayHelpWindow(ImageButton imgHelp, ref string szHelpHeading, ref string szHelpText)
		{
			imgHelp.Attributes.Add("OnClick", "window.open('wfHelp.aspx?Heading=Help : "+szHelpHeading+"&Text="+szHelpText+"','_blank', 'height=580, width=403');return false;"); 
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void SetToolTipHelp(ImageButton imgHelp, ref string szHelpText)
		{
			imgHelp.ToolTip = szHelpText;
		}
		//---------------------------------------------------------------------
		#endregion



		#region Format String Functions
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void RemoveFormattingForWindow(ref string szHelpText)
		{
			StringBuilder sbHelpText = new StringBuilder(szHelpText);
			
			sbHelpText = sbHelpText.Replace("<", "`");
			sbHelpText = sbHelpText.Replace(">", "~");
			sbHelpText = sbHelpText.Replace("\'", "^");

			szHelpText = sbHelpText.ToString();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static private void RemoveFormattingForToolTip(ref string szHelpText)
		{
			StringBuilder sbHelpText = new StringBuilder("");
			bool bWrite = true;
			szHelpText = szHelpText.Replace("^", "\'");
			
			for(int iIndex = 0; iIndex < szHelpText.Length; ++iIndex)
			{
				if(szHelpText[iIndex] == '`')
				{
					bWrite = false;
					continue;
				}
				if(szHelpText[iIndex] == '~')
				{
					bWrite = true; 
					continue;
				}

				if(bWrite == true)
				{
					sbHelpText.Append(szHelpText[iIndex]); 
				}
			}
			szHelpText = sbHelpText.ToString();
		}
		//---------------------------------------------------------------------
		//
		//---------------------------------------------------------------------
		static public string ReAddFormatting(string szHelpText)
		{
			StringBuilder sbHelpText = new StringBuilder(szHelpText);

			sbHelpText = sbHelpText.Replace("`", "<");
			sbHelpText = sbHelpText.Replace("~", ">");
			sbHelpText = sbHelpText.Replace("^", "\'");

			return sbHelpText.ToString();
		}
		//---------------------------------------------------------------------
		#endregion


	}//END OF CLASS
}//END OF NAMESPACE
