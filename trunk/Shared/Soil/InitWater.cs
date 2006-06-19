using System;
using VBGeneral;

namespace CSGeneral
	{
	//------------------------------------
	// Encapsulates an initial water type
	// -----------------------------------
	public class InitWater : SoilBase
		{
		private Soil ParentSoil;

		// -----------
		// Constructor
		// -----------
		public InitWater(Soil soil)
			: base(soil.Data.Child("InitWater"))
			{
			ParentSoil = soil;
			}


		// ---------------------------------------
		// Return the method being used currently.
		// ---------------------------------------
		public enum MethodType {Percent, DepthWetSoil, Layered};
		public MethodType Method
			{
			get {
                if (Data.ChildList("layer").Count > 0)
                    return MethodType.Layered;
                else if (Data.Child("DepthWetSoilMethod") != null)
                    return MethodType.DepthWetSoil;
			    else
				    return MethodType.Percent;
				}
			}


		// ---------------------------------------
		// Properties to return settings.
		// ---------------------------------------
		public int Percent
			{
			get {return Convert.ToInt32(Convert.ToDouble(Data.get_ChildValueWithError("PercentMethod|Percent")) * 100);}
			}
		public bool FilledFromTop
			{
			get {return (Data.get_ChildValueWithError("PercentMethod|Distributed").ToLower() == "filled from top");}
			}
		public int DepthWetSoil
			{
			get {return Convert.ToInt32(Data.get_ChildValueWithError("DepthWetSoilMethod|Depth"));}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] SW
			{
			get {
				double[] ll15 = ParentSoil.LL15;
				double[] dul = ParentSoil.DUL;
				double[] pawc = ParentSoil.PAWC();
				double[] sw = new double[ll15.Length];
				switch (Method)
					{
					case MethodType.Percent: 
						{
						if (FilledFromTop)
							{
							double AmountWater = MathUtility.Sum(pawc) * (Percent / 100.0);
							for (int Layer = 0; Layer < ll15.Length; Layer++)
								{
								if (AmountWater >= pawc[Layer])
									{
									sw[Layer] = dul[Layer];
									AmountWater = AmountWater - pawc[Layer];
									}
								else
									{
									double Prop = AmountWater / pawc[Layer];
									sw[Layer] = Prop * (dul[Layer] - ll15[Layer]) + ll15[Layer];
									AmountWater = 0;
									}
								}
							}
						else
							{	
							for (int Layer = 0; Layer < ll15.Length; Layer++)
								sw[Layer] = Percent / 100.0 * (dul[Layer] - ll15[Layer]) + ll15[Layer];
							}
						break;
						}
					case MethodType.DepthWetSoil:
						{
						double[] Thickness = ParentSoil.Thickness;
						double DepthSoFar = 0;
						for (int Layer = 0; Layer < ll15.Length; Layer++)
							{
							if (DepthWetSoil > DepthSoFar + Thickness[Layer])
								sw[Layer] = dul[Layer];
							else
								{
								double Prop = Math.Max(DepthWetSoil - DepthSoFar, 0) / Thickness[Layer];
								sw[Layer] = Prop * (dul[Layer] - ll15[Layer]) + ll15[Layer];
								}
							DepthSoFar += Thickness[Layer];
							}
						break;
						}
					case MethodType.Layered:
						sw = getLayered("", "sw");
						break;
					}
				return sw;
				}
			}

		// ----------------------------------
		// Get water as mm
		// ----------------------------------
		public double[] SWMM
			{
			get {return MathUtility.Multiply(SW, Thickness);}
			set {MathUtility.Divide(SW, Thickness);}
			}

		// ----------------------------------
		// Set water via the percent method.
		// ----------------------------------
		public void SetUsingPercent(int Percent, bool FilledFromTop)
			{
			APSIMData Data = ParentSoil.Data.Child("InitWater");
			Data.Clear();
			double Prop = Percent / 100.0;
			Data.set_ChildValue("PercentMethod|Percent", Prop.ToString("f2"));
			string Distributed = "Filled from top";
			if (!FilledFromTop)
				Distributed = "Evenly distributed";
			Data.set_ChildValue("PercentMethod|Distributed", Distributed);
			}


		// ----------------------------------
		// Set water via the depth wet soil method.
		// ----------------------------------
		public void SetUsingDepthWetSoil(int Depth)
			{
			Data.Clear();
			Data.set_ChildValue("DepthWetSoilMethod|Depth", Depth.ToString());
			}


		// ----------------------------------
		// Set water via the layered method.
		// ----------------------------------
		public void SetUsingLayered(double[] sw)
			{
			Data.Clear();
			setLayered("", "sw", sw, "f2");
			}

		}
	}
