using System;
using VBGeneral;

namespace CSGeneral
	{
	//------------------------------------
	// Encapsulates an initial water type
	// -----------------------------------
	public class InitNitrogen : SoilBase
		{
		private Soil ParentSoil;

		// -----------
		// Constructor
		// -----------
		public InitNitrogen(Soil soil)
			: base(soil.Data)
			{
			ParentSoil = soil;
			}


		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] NO3
			{
			get {return getLayered("InitNitrogen", "no3");}
			set {setLayered("InitNitrogen", "no3", value);}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] NH4
			{
			get {return getLayered("InitNitrogen", "nh4");}
			set {setLayered("InitNitrogen", "nh4", value);}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] NO3KgHa
			{
			get {return ToKgHa(NO3);}
			set {setLayered("InitNitrogen", "no3", ToPpm(value));}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double[] NH4KgHa
			{
			get {return ToKgHa(NH4);}
			set {setLayered("InitNitrogen", "nh4", ToPpm(value));}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double TotalNO3KgHa
			{
			get {return MathUtility.Sum(ToKgHa(NO3));}
			set {
				double[] no3 = NO3KgHa;
				double TotalNO3Required = value;
				double TotalNO3Currently = MathUtility.Sum(no3);
				if (TotalNO3Currently == 0)
					{
					double AmountInEachLayer = TotalNO3Required / ParentSoil.Thickness.Length;
					for (int i = 0; i != no3.Length; i++)
						no3[i] = AmountInEachLayer;
					}
				else
					{
					double Prop = TotalNO3Required / TotalNO3Currently;
					for (int i = 0; i != no3.Length; i++)
						no3[i] = no3[i] * Prop;
					}
				NO3KgHa = no3;
				}
			}

		// ------------------------------------
		// Return the soil water for this class
		// ------------------------------------
		public double TotalNH4KgHa
			{
			get {return MathUtility.Sum(ToKgHa(NH4));}
			set {
				double[] nh4 = NH4KgHa;
				double TotalNH4Required = value;
				double TotalNH4Currently = MathUtility.Sum(nh4);
				if (TotalNH4Currently == 0)
					{
					double AmountInEachLayer = TotalNH4Required / ParentSoil.Thickness.Length;
					for (int i = 0; i != nh4.Length; i++)
						nh4[i] = AmountInEachLayer;
					}
				else
					{
					double Prop = TotalNH4Required / TotalNH4Currently;
					for (int i = 0; i != nh4.Length; i++)
						nh4[i] = nh4[i] * Prop;
					}
				NH4KgHa = nh4;
				}
			}


		// ----------------------------------------------
		// Convert from ppm to kg/ha
		//		ppm = kg/ha * 100 / (BD * Thickness(mm))
		// ----------------------------------------------
		private double[] ToKgHa(double[] ppm)
			{
			double[] BD = ParentSoil.BD;
			double[] Thickness = ParentSoil.Thickness;
			double[] KgHa = new double[ppm.Length];

			for (int i = 0; i != ppm.Length; i++)
				KgHa[i] = ppm[i] / 100 * (BD[i] * Thickness[i]);

			return KgHa; 
			}


		// ----------------------------------------------
		// Convert from ppm to kg/ha
		//		ppm = kg/ha * 100 / (BD * Thickness(mm))
		// ----------------------------------------------
		private double[] ToPpm(double[] KgHa)
			{
			double[] BD = ParentSoil.BD;
			double[] Thickness = ParentSoil.Thickness;
			double[] Ppm = new double[KgHa.Length];

			for (int i = 0; i != KgHa.Length; i++)
				Ppm[i] = KgHa[i] * 100 / (BD[i] * Thickness[i]);

			return Ppm; 
			}
	
		}
	}
