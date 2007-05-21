using System;
using VBGeneral;
using CSGeneral;

namespace Soils
	{
	//------------------------------------
	// Encapsulates an initial water type
	// -----------------------------------
	public class InitNitrogen
		{
		private Soil ParentSoil;
        private APSIMData Data;

		public InitNitrogen(APSIMData data)
			{
            // -----------
            // Constructor
            // -----------
            ParentSoil = new Soil(data.Parent);
            Data = data;

            // Ensure this component has valid thickness, no3 and nh4 values - always.
            Thickness = ParentSoil.Thickness;
            double[] no3 = NO3;
            for (int i = 0; i != no3.Length; i++)
                {
                if (no3[i] == MathUtility.MissingValue)
                    no3[i] = 0.1;
                }
            NO3 = no3;
            double[] nh4 = NH4;
            for (int i = 0; i != nh4.Length; i++)
                {
                if (nh4[i] == MathUtility.MissingValue)
                    nh4[i] = 0.1;
                }
            NH4 = nh4;
            }
        public double[] Thickness
            {
            get { return Utility.getLayered(Data, "profile", "thickness", ""); }
            set { Utility.setLayered(Data, "profile", "thickness", "", value); }
            }
        public double[] NO3
			{
            // ------------------------------------
            // Return the nitrate for this class
            // ------------------------------------
            get { return Utility.getLayered(Data, "profile", "no3", ""); }
            set { Utility.setLayered(Data, "profile", "no3", "", value); }
			}
		public double[] NH4
			{
            // ------------------------------------
            // Return the ammonia for this class
            // ------------------------------------
            get { return Utility.getLayered(Data, "profile", "nh4", ""); }
            set { Utility.setLayered(Data, "profile", "nh4", "", value); }
			}
        public double[] NO3MapedToSoil
            {
            get
                {
                double[] DefaultValues = new double[ParentSoil.Thickness.Length];
                for (int i = 0; i != DefaultValues.Length; i++)
                    DefaultValues[i] = 1.0;  // a small number
                return Utility.MapSampleToSoilUsingMass(NO3, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }
        public double[] NH4MapedToSoil
            {
            get
                {
                double[] DefaultValues = new double[ParentSoil.Thickness.Length];
                for (int i = 0; i != DefaultValues.Length; i++)
                    DefaultValues[i] = 0.2;  // a small number
                return Utility.MapSampleToSoilUsingMass(NH4, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }

		public double[] NO3KgHa
			{
            // ------------------------------------
            // Return the nitrate(kg/ha) for this class
            // ------------------------------------
            get { return ToKgHa(NO3); }
            set { Utility.setLayered(Data, "profile", "no3", "", ToPpm(value)); }
			}
		public double[] NH4KgHa
			{
            // ------------------------------------
            // Return the ammonia (kg/ha) for this class
            // ------------------------------------
            get { return ToKgHa(NH4); }
            set { Utility.setLayered(Data, "profile", "nh4", "", ToPpm(value)); }
			}
		public double TotalNO3KgHa
			{
            // ------------------------------------
            // Return the total no3 (kg/ha) for this class
            // ------------------------------------
            get { return MathUtility.Sum(ToKgHa(NO3)); }
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
		public double TotalNH4KgHa
			{
            // ------------------------------------
            // Return the total ammonia (kg/ha) for this class
            // ------------------------------------
            get { return MathUtility.Sum(ToKgHa(NH4)); }
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

		private double[] ToKgHa(double[] ppm)
			{
            // ----------------------------------------------
            // Convert from ppm to kg/ha
            //		ppm = kg/ha * 100 / (BD * Thickness(mm))
            // ----------------------------------------------
            double[] BD = Utility.MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
			double[] KgHa = new double[Thickness.Length];

			//for (int i = 0; i != ppm.Length; i++)
            for (int i = 0; i != Thickness.Length; i++)
				KgHa[i] = ppm[i] / 100 * (BD[i] * Thickness[i]);

			return KgHa; 
			}
		private double[] ToPpm(double[] KgHa)
			{
            // ----------------------------------------------
            // Convert from ppm to kg/ha
            //		ppm = kg/ha * 100 / (BD * Thickness(mm))
            // ----------------------------------------------
            double[] BD = Utility.MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
            double[] Ppm = new double[Thickness.Length];

            for (int i = 0; i != Thickness.Length; i++)
				Ppm[i] = KgHa[i] * 100 / (BD[i] * Thickness[i]);

			return Ppm; 
			}
        }
	}
