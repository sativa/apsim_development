using System;

namespace CSGeneral
	{
	/// <summary>
	/// Various math utilities.
	/// </summary>
	public class MathUtility
		{

		public static bool FloatsAreEqual(double value1, double value2)
			{
			return FloatsAreEqual(value1, value2, 0.00001);
			}	
		public static bool FloatsAreEqual(double value1, double value2, double tolerance)
			{
			return (Math.Abs(value1 - value2) < tolerance);
			}	
	
	
		}
	}
