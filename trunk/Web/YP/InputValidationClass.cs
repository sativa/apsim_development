using System;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for InputValidationClass.
	/// </summary>
	public class InputValidationClass
		{
		public InputValidationClass()
			{}
			
		/*----------------------------ASCII TABLE----------------------------------
		0 = 48
		1 = 49
		2 = 50
		3 = 51
		4 = 52
		5 = 53
		6 = 54
		7 = 55
		8 = 56
		9 = 57
		+ = 43
		. = 46
		-------------------------------------------------------------------------*/
		//-------------------------------------------------------------------------
		//Replaces any character which will cause problems for the database
		//Using an Access Database, the only character that causes a problem is the 
		//single quote charcter.
		//IF THE DATABASE CHANGES TO AN MSSQL DATABASE, THIS FUNCTION MAY HAVE TO 
		//BE EXTENDED!!!!!
		//-------------------------------------------------------------------------
		public static string ValidateString(string szInputString)
			{
			//Replaces the single quote character with two single quote characters, which
			//will save into the database correctly.
			//EG: O'Brien becomes O''Brien which appears in the database
			//as O'Brien.
			szInputString = szInputString.Replace("'", "''");
			return szInputString;		
			}
		//-------------------------------------------------------------------------
		//Takes in an input string and then checks that it is in a valid depth format
		//EG: 10-23 or 10 - 23 or 14- 30 will all pass but
		// aa-aa or 11-1s or 111 or 40-20 or aaa wont
		//If the input string is valid it will be returned if isn't a null value
		//will be returned
		//-------------------------------------------------------------------------	
		public static string ValidateDepthString(string szInputString)
			{
			string szValidatedDepthString = null;
			char[] cSeperator = {'-'};
			string[] szInputParts = szInputString.Split(cSeperator);
			//Checks that there are two strings left
			if(szInputParts.Length == 2)
				{
				//removes white space
				szInputParts[0] = szInputParts[0].Trim();
				szInputParts[1] = szInputParts[1].Trim();
				//Checks if both strings are integers
				if(IsInputAPositiveInteger(szInputParts[0]) && IsInputAPositiveInteger(szInputParts[1]))
					{
					//Checks that the first integer is smaller than the second
					if(Convert.ToInt32(szInputParts[0]) < Convert.ToInt32(szInputParts[1]))
						{
						szValidatedDepthString = szInputParts[0] + "-" + szInputParts[1];
						}
					}
				}
			return szValidatedDepthString;		
			}
		//-------------------------------------------------------------------------
		//Checks that the input string doesn't contain a character that can not
		//be used in a filename
		//-------------------------------------------------------------------------	
		public static bool IsInputAValidFileLocationString(string szInputString)
			{
			szInputString = szInputString.Replace("'", "''");
			bool bValidLocationString = true;
			for(int iIndex = 0; iIndex < szInputString.Length; iIndex++)
				{
				if((szInputString[iIndex] == '/' || szInputString[iIndex] == '\\'
					|| szInputString[iIndex] == ':' || szInputString[iIndex] == '*'
					|| szInputString[iIndex] == '?' || szInputString[iIndex] == '"'
					|| szInputString[iIndex] == '<' || szInputString[iIndex] == '>'
					|| szInputString[iIndex] == '|' || szInputString[iIndex] == '\''
					|| szInputString[iIndex] == '#') == true)
					{
					bValidLocationString = false;
					break;
					}
				}
			return bValidLocationString;			
			}
		//-------------------------------------------------------------------------
		//Checks to see if the inputstring can be converted to a positive decimal
		//-------------------------------------------------------------------------
		public static bool IsInputAPositiveDecimal(string szInputString)
		{
			bool bPositiveDecimal = true;
			szInputString.Replace("+", "");
			int iNumberOfDecimalCharacters = 0;
			//Checks that all the characters are numbers (48 to 57) or
			//decimal place (46)
			for(int iIndex = 0; iIndex < szInputString.Length; iIndex++)
				{
				if(((szInputString[iIndex] >= 48 && szInputString[iIndex] <= 57)
					|| szInputString[iIndex] == 46) == false)
					{
					bPositiveDecimal = false;
					break;
					}
				//Makes sure that there aren't two decimal places in the string
				if(szInputString[iIndex] == 46)
					{
					iNumberOfDecimalCharacters++;
					if(iNumberOfDecimalCharacters > 1)
						{
						bPositiveDecimal = false;
						break;
						}
					}
				}
			return bPositiveDecimal;		
			}
		//-------------------------------------------------------------------------
		//Checks to see if the input string is a valid integer.
		//-------------------------------------------------------------------------
		public static bool IsInputAPositiveInteger(string szInputString)
			{
			bool bPositiveInteger = true;
			szInputString.Replace("+", "");
			//Checks that all the characters are numbers (48 to 57)
			for(int iIndex = 0; iIndex < szInputString.Length; iIndex++)
				{
				if((szInputString[iIndex] >= 48 && szInputString[iIndex] <= 57) == false)
					{
					bPositiveInteger = false;
					break;
					}
				}
			return bPositiveInteger;		
			}
		//-------------------------------------------------------------------------
		//Checks to make sure that the year, month and day combination passed in
		//will form a valid date
		//-------------------------------------------------------------------------	
		public static bool IsValidDate(int iYear, int iMonth, int iDay)
			{
			bool bValidDate = true;
			//Rejects the 29th of Feb on any year that isn't a leap year
			if(iDay == 29 && !DateTime.IsLeapYear(iYear))
				{
				if(iMonth == 2)
					{
					bValidDate = false;
					}
				}	
			//Rejects the 30th of Feb
			if(iDay == 30)
				{
				if(iMonth == 2)
					{
					bValidDate = false;
					}
				}
			//Rejects the 31 of Feb, Apr, Jun, Sept, Nov
			if(iDay == 31)
				{
				if(iMonth == 2 || iMonth  == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11)
					{
					bValidDate = false;
					}
				}
			//Rejects all other invalid dates	
			if(iDay > 31 || iDay < 1 || iMonth > 12 || iMonth < 1 || iYear < 1)
				{
				bValidDate = false;
				}
			return bValidDate;
			}
		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
