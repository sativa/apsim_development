using System;
using System.Collections;

namespace CSGeneral
	{
	/// <summary>
	/// Various math utilities.
	/// </summary>
	public class MathUtility
		{
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
			double[] results = null;
			if(value1.Length == value2.Length)
				{
				results = new double[value1.Length];
				for(int iIndex = 0; iIndex < value1.Length; iIndex++)
					{
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
					//Avoid divide by zero problems
					if(value2[iIndex] != 0)
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
		//
		//-------------------------------------------------------------------------
		public static double Accumulate(double[] dValues, int iStartIndex, int iEndIndex, double dInitialValue)
			{
			double result = dInitialValue;
			if(iEndIndex > dValues.Length || iStartIndex < 0)
				{
				throw new Exception("MathUtility.Accumulate: End index or start index is out of range");
				}
			for(int iIndex = iStartIndex; iIndex < iEndIndex; iIndex++)
				{
				result += dValues[iIndex];
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
		
		
		//-------------------------------------------------------------------------
		}
	}
