using System;

namespace UrbanWater
	{
	/// <summary>
	/// Summary description for InputValidationClass.
	/// </summary>
	public class InputValidationClass
		{
		
		/*-------------------------------ASCII TABLE-------------------------------
		46 = .
		48 = 0
		49 = 1
		50 = 2
		51 = 3
		52 = 4
		53 = 5
		54 = 6
		55 = 7
		56 = 8
		57 = 9
		-------------------------------------------------------------------------*/
		
		public InputValidationClass()
			{}
				
		//-------------------------------------------------------------------------
		//Checks to ensure that the passed in string is a positive integer
		//-------------------------------------------------------------------------
		public static bool IsInputAPositiveDecimal(string szInputString)
			{
			bool bPositiveDecimal = true;
			int iNumberOfDecimalPoints = 0;
			//For every character in the string
			for(int iIndex = 0; iIndex < szInputString.Length; iIndex++)
				{
				//Checks to see if the character isn't a number (0-9) or a
				//decimal point
				if(((szInputString[iIndex] >= 48 && szInputString[iIndex] <= 57)
					|| szInputString[iIndex] == 46) == false)
					{
					bPositiveDecimal = false;
					break;
					}
				//Checks to make sure that only one decimal point has been entered
				else if(szInputString[iIndex] == 46)
					{
					iNumberOfDecimalPoints++;
					if(iNumberOfDecimalPoints > 1)
						{
						bPositiveDecimal = false;
						break;
						}
					}
				}
			return bPositiveDecimal;		
			}
			
		}//END OF CLASS
	}//END OF NAMESPACE
