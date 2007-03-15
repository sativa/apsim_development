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

		public InitNitrogen(Soil soil)
			{
            // -----------
            // Constructor
            // -----------
            ParentSoil = soil;
            Data = soil.Data.Child("InitNitrogen");
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
            double[] BD = ParentSoil.BD;
			double[] Thickness = ParentSoil.Thickness;
			double[] KgHa = new double[ppm.Length];

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
            double[] BD = ParentSoil.BD;
			double[] Thickness = ParentSoil.Thickness;
			double[] Ppm = new double[KgHa.Length];

			for (int i = 0; i != KgHa.Length; i++)
				Ppm[i] = KgHa[i] * 100 / (BD[i] * Thickness[i]);

			return Ppm; 
			}


        internal void ValidateAgainstLayerStructure()
            {
            double[] no3 = NO3;
            double[] nh4 = NH4;
            int NumLayersCurrent = no3.Length;
            int NumLayers = ParentSoil.Thickness.Length;
            
            Array.Resize(ref no3, NumLayers);
            Array.Resize(ref nh4, NumLayers);
            if (NumLayersCurrent > 0 && NumLayersCurrent < NumLayers)
                {
                // we don't have enough layers - add extra ones.
                for (int i = NumLayersCurrent; i < NumLayers; i++)
                    {
                    no3[i] = no3[NumLayersCurrent - 1];
                    nh4[i] = nh4[NumLayersCurrent - 1];
                    }
                }

            NO3 = no3;
            NH4 = nh4;
            }


        }
	}
