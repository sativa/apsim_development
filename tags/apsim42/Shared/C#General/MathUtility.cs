using System;
using System.Collections;

namespace CSGeneral
	{
	/// <summary>
	/// Various math utilities.
	/// </summary>
	public class MathUtility
		{
		//------------------------------------------------
		// Returns true if specified value is 'missing'
		// -----------------------------------------------
		public static double MissingValue
			{
			get {return 999999;}
			}

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static bool FloatsAreEqual(double value1, double value2)
			{
			return FloatsAreEqual(value1, value2, 0.00001);
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static bool FloatsAreEqual(double value1, double value2, double tolerance)
			{
			return (Math.Abs(value1 - value2) < tolerance);
			}	
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static double[] Multiply(double[] value1, double[] value2)
			{
			double[] results = new double[value1.Length];
			if(value1.Length == value2.Length)
				{
				results = new double[value1.Length];
				for(int iIndex = 0; iIndex < value1.Length; iIndex++)
					{
					if (value1[iIndex] == MissingValue || value2[iIndex] == MissingValue)
						results[iIndex] = MissingValue;
					else
						results[iIndex] = (value1[iIndex] * value2[iIndex]);
					}
				}
				return results;	
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static double[] Multiply_Value(double[] value1, double value2)
			{
			double[] results = null;
			results = new double[value1.Length];
			for(int iIndex = 0; iIndex < value1.Length; iIndex++)
				{
				if (value1[iIndex] == MissingValue)
					results[iIndex] = MissingValue;
				else
					results[iIndex] = (value1[iIndex] * value2);
				}
			return results;	
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static double[] Divide(double[] value1, double[] value2)
			{
			double[] results = null;
			if(value1.Length == value2.Length)
				{
				results = new double[value1.Length];
				for(int iIndex = 0; iIndex < value1.Length; iIndex++)
					{
					if (value1[iIndex] == MissingValue || value2[iIndex] == MissingValue)
						results[iIndex] = MissingValue;
					else if(value2[iIndex] != 0)
						{
						results[iIndex] = (value1[iIndex] / value2[iIndex]);
						}
					else
						{
						results[iIndex] = value1[iIndex];
						}
					}
				}
			return results;	
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public static double[] Divide_Value(double[] value1, double value2)
			{
			double[] results = new double[value1.Length];
			//Avoid divide by zero problems
			if(value2 != 0)
				{
				for(int iIndex = 0; iIndex < value1.Length; iIndex++)
					{
					if (value1[iIndex] == MissingValue)
						results[iIndex] = MissingValue;
					else
						results[iIndex] = (value1[iIndex] / value2);
					}
				}
				else
				{
				results = value1;
				}
			return results;	
			}
		//-------------------------------------------------------------------------
		// Sum an array of numbers 
		//-------------------------------------------------------------------------
		public static double Sum(IEnumerable Values)
			{
			return Sum(Values, 0, 0, 0.0);
			}

		//-------------------------------------------------------------------------
		// Sum an array of numbers starting at startIndex up to (but not including) endIndex
		// beginning with an initial value
		//-------------------------------------------------------------------------
		public static double Sum(IEnumerable Values, int iStartIndex, int iEndIndex, 
			                     double dInitialValue)
			{
			double result = dInitialValue;
			if(iStartIndex < 0)
				{
				throw new Exception("MathUtility.Sum: End index or start index is out of range");
				}
			int iIndex = 0;
			foreach (double Value in Values)
				{
				if ((iStartIndex == 0 && iEndIndex == 0) ||
					(iIndex >= iStartIndex && iIndex < iEndIndex) && Value != MissingValue)
					result += Value;
				iIndex++;
				}

			return result;	
			}
		//-------------------------------------------------------------------------
		//Linearly interpolates a value y for a given value x and a given
		//set of xy co-ordinates.
		//When x lies outside the x range_of, y is set to the boundary condition.
		//Returns true for Did_interpolate if interpolation was necessary.
		//-------------------------------------------------------------------------
		public static double LinearInterpReal(double dX, double[] dXCoordinate, double[] dYCoordinate, ref bool  bDidInterpolate)
			{
			bDidInterpolate = false;
			//find where x lies in the x coordinate
			if(dXCoordinate.Length == 0 || dYCoordinate.Length == 0 || dXCoordinate.Length != dYCoordinate.Length)
				{
				throw new Exception("MathUtility.LinearInterpReal: Lengths of passed in arrays are incorrect");
				}
			
			for(int iIndex = 0; iIndex < dXCoordinate.Length; iIndex++)
				{
				if(dX <= dXCoordinate[iIndex])
					{
					//Chcek to see if dX is exactly equal to dXCoordinate[iIndex]
					//If so then don't calcuate dY.  This was added to remove roundoff error.
					if(dX == dXCoordinate[iIndex])
						{
						bDidInterpolate = false;
						return dYCoordinate[iIndex];
						}
					//Found position
					else if(iIndex == 0)
						{
						bDidInterpolate = true;
						return dYCoordinate[iIndex];
						}
					else
						{
						//interpolate - y = mx+c
						if((dXCoordinate[iIndex] - dXCoordinate[iIndex-1]) == 0)
							{
							bDidInterpolate = true;
							return dYCoordinate[iIndex-1];
							}
						else
							{
							bDidInterpolate = true;
							return ((dYCoordinate[iIndex] - dYCoordinate[iIndex-1]) / (dXCoordinate[iIndex] - dXCoordinate[iIndex-1]) * (dX - dXCoordinate[iIndex-1]) + dYCoordinate[iIndex-1]);
							}		
						}	
					}
				else if(iIndex == (dXCoordinate.Length-1))
					{
					bDidInterpolate = true;
					return dYCoordinate[iIndex];
					}
				}// END OF FOR LOOP
				return 0.0;
			}


		// ---------------------------------------------
		// Reverse the contents of the specified array.
		// ---------------------------------------------
		static public double[] Reverse(double[] Values)
			{
			double[] ReturnValues = new double[Values.Length];

            int Index = 0;
			for (int Layer = Values.Length-1; Layer >= 0; Layer--)
				{
				ReturnValues[Index] = Values[Layer];
				Index++;
				}
			return ReturnValues;
			}
		
		//-------------------------------------------------------------------------
		}
	}
