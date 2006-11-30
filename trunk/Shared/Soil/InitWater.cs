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
                if (Data.ChildNames("layer").Length > 0)
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
			get {return Convert.ToInt32(Convert.ToDouble(Data.get_ChildValue("PercentMethod\\Percent")) * 100);}
			}
		public bool FilledFromTop
			{
			get {return (Data.get_ChildValue("PercentMethod\\Distributed").ToLower() == "filled from top");}
			}
		public int DepthWetSoil
			{
			get {return Convert.ToInt32(Data.get_ChildValue("DepthWetSoilMethod\\Depth"));}
			}
        public string RelativeTo
            {
            get {
                string value = Data.get_ChildValue("RelativeTo");
                if (value == "")
                    value = "ll15";
                return value;
                }
            set {
                Data.set_ChildValue("RelativeTo", value);
                }
            }

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] SW
			{
			get {
                double[] ll;
                double[] pawc;
                if (RelativeTo == "ll15")
                    {
                    ll = ParentSoil.LL15;
                    pawc = ParentSoil.PAWC();
                    }
                else
                    {
                    ll = ParentSoil.LL(RelativeTo);
                    pawc = ParentSoil.PAWC(RelativeTo);
                    }
                
				double[] dul = ParentSoil.DUL;
				double[] sw = new double[ll.Length];
				switch (Method)
					{
					case MethodType.Percent: 
						{
						if (FilledFromTop)
							{
							double AmountWater = MathUtility.Sum(pawc) * (Percent / 100.0);
							for (int Layer = 0; Layer < ll.Length; Layer++)
								{
								if (AmountWater >= pawc[Layer])
									{
									sw[Layer] = dul[Layer];
									AmountWater = AmountWater - pawc[Layer];
									}
								else
									{
									double Prop = AmountWater / pawc[Layer];
									sw[Layer] = Prop * (dul[Layer] - ll[Layer]) + ll[Layer];
									AmountWater = 0;
									}
								}
							}
						else
							{	
							for (int Layer = 0; Layer < ll.Length; Layer++)
								sw[Layer] = Percent / 100.0 * (dul[Layer] - ll[Layer]) + ll[Layer];
							}
						break;
						}
					case MethodType.DepthWetSoil:
						{
						double[] Thickness = ParentSoil.Thickness;
						double DepthSoFar = 0;
						for (int Layer = 0; Layer < ll.Length; Layer++)
							{
							if (DepthWetSoil > DepthSoFar + Thickness[Layer])
								sw[Layer] = dul[Layer];
							else
								{
								double Prop = Math.Max(DepthWetSoil - DepthSoFar, 0) / Thickness[Layer];
								sw[Layer] = Prop * (dul[Layer] - ll[Layer]) + ll[Layer];
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
            Data.DeleteByType("DepthWetSoilMethod");
            Data.DeleteByType("layer");
			double Prop = Percent / 100.0;
			Data.set_ChildValue("PercentMethod\\Percent", Prop.ToString("f2"));
			string Distributed = "Filled from top";
			if (!FilledFromTop)
				Distributed = "Evenly distributed";
			Data.set_ChildValue("PercentMethod\\Distributed", Distributed);
			}


		// ----------------------------------
		// Set water via the depth wet soil method.
		// ----------------------------------
        public void SetUsingDepthWetSoil(int Depth)
			{
            Data.DeleteByType("PercentMethod");
            Data.DeleteByType("layer");
            Data.set_ChildValue("DepthWetSoilMethod\\Depth", Depth.ToString());
			}


		// ----------------------------------
		// Set water via the layered method.
		// ----------------------------------
		public void SetUsingLayered(double[] sw)
			{
            if (sw.Length > 0)
                {
                Data.DeleteByType("DepthWetSoilMethod");
                Data.DeleteByType("PercentMethod");
                setLayered("", "sw", sw);
                }
			}

		}
	}
