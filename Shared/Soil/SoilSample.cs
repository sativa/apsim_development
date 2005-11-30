using System;
using System.Xml;
using VBGeneral;
using System.Collections;

namespace CSGeneral
	{
	// -------------------------------------------------
	// A class for encapsulating a soil sample
	// A soil sample can have SW values in volumetric %
	// or gravimetric % or wet dry %. Default is Volumetric.
	// Assumes that data.parent is the parent soil.
	// the parent soil is used to get BD and other
	// soil variables when mapping a sample to a soil.
	// -------------------------------------------------
	public class SoilSample : SoilBase
		{
		private Soil ParentSoil;
		//public SoilSample()	: base(new APSIMData("SoilSample", ""))	{}
		public SoilSample(APSIMData data) : base(data)
			{
			if (Data.Parent == null)
				throw new Exception("Sample '" + Data.Name + "' has no parent soil.");
			ParentSoil = new Soil(Data.Parent);
			}


		#region Water format methods
		public enum StoredWaterFormatType {VolumetricPercent, GravimetricPercent, GravimetricWetDry};
		public StoredWaterFormatType StoredWaterFormat
			{
			get {
				if (Data.get_ChildValue("WaterFormat") == "GravimetricPercent")
					return StoredWaterFormatType.GravimetricPercent;
				else if (Data.get_ChildValue("WaterFormat") == "GravimetricWetDry") 
					return StoredWaterFormatType.GravimetricWetDry;
				else
                    return StoredWaterFormatType.VolumetricPercent;
				}
			}
		private void SetStoredWaterFormat(StoredWaterFormatType WaterFormat)
			{
			if (WaterFormat == StoredWaterFormatType.GravimetricWetDry)
				{
				double[] MissingValues = new double[Thickness.Length];
				for (int i = 0; i != MissingValues.Length; i++)
					MissingValues[i] = MathUtility.MissingValue;
				setLayered("water", "sw", MissingValues);
				Data.set_ChildValue("WaterFormat", "GravimetricWetDry");
				}
			else
				{
				double[] MissingValues = new double[Thickness.Length];
				for (int i = 0; i != MissingValues.Length; i++)
					MissingValues[i] = MathUtility.MissingValue;
				setLayered("water", "wet", MissingValues);
				setLayered("water", "dry", MissingValues);

				if (WaterFormat == StoredWaterFormatType.GravimetricPercent)
					Data.set_ChildValue("WaterFormat", "GravimetricPercent");
				else if (WaterFormat == StoredWaterFormatType.VolumetricPercent)
					Data.set_ChildValue("WaterFormat", "VolumetricPercent");
				}
			}

		private double[] GetLayeredAsVol(string PropertyType, string PropertyName)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				return getLayered(PropertyType, PropertyName);
			else
				{
				double[] BD = MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
				return MathUtility.Multiply(GetLayeredAsGrav(PropertyType, PropertyName), BD);
				}
			}
		private double[] GetLayeredAsGrav(string PropertyType, string PropertyName)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				{
				double[] BD = MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
				return MathUtility.Divide(getLayered(PropertyType, PropertyName),
											BD);
				}
			else if (StoredWaterFormat == StoredWaterFormatType.GravimetricPercent)
				return getLayered(PropertyType, PropertyName);
			else
				{
				double[] Gravimetric = new double[Thickness.Length];
				for (int layer = 0; layer != Thickness.Length; layer++)
					Gravimetric[layer] = (Wet[layer] - Dry[layer]) / Dry[layer];
				return Gravimetric;
				}
			}
		#endregion


		#region Data get/set properties
		public DateTime SampleDate
			{
			get {
				string DateString = GetStringValue("", "date");
				if (DateString == "")
					return DateTime.Today;
				else
					return DateTime.Parse(DateString);
				}
			set	{
				SetValue("", "date", value.ToString());
				}
			}

		public double[] SW
			{
			get {return GetLayeredAsVol("water", "sw");}
			set {
				setLayered("water", "sw", value);
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				}                           				
			}
		public double[] SWGrav
			{
			get {return GetLayeredAsGrav("water", "sw");}
			set {
				setLayered("water", "sw", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				}                           				
			}
		public double[] Wet
			{
			get {return getLayered("water", "wet");}
			set {
				setLayered("water", "wet", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricWetDry);
				}                           				
			}
		public double[] Dry
			{
			get {return getLayered("water", "dry");}
			set {
				setLayered("water", "dry", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricWetDry);
				}                           				
			}

		public double[] NO3
			{
			get {return getLayered("nitrogen", "no3");}
			set {setLayered("nitrogen", "no3", value);}
			}
		public double[] NH4
			{
			get {return getLayered("nitrogen", "nh4");}
			set {setLayered("nitrogen", "nh4", value);}
			}

		public double[] OC
			{
			get {return getLayered("other", "oc");}
			set {setLayered("nitrogen", "oc", value);}
			}
		public double[] PH
			{
			get {return getLayered("other", "ph");}
			set {setLayered("nitrogen", "ph", value);}
			}
		public double[] EC
			{
			get {return getLayered("other", "ec");}
			set {setLayered("other", "ec", value);}
			}
		public double[] ESP
			{
			get {return getLayered("other", "esp");}
			set {setLayered("other", "esp", value);}
			}
		#endregion
                    

		#region Sample / soil mapping methods

		private double[] SWMapedToSoil
			{
			get {return MapSampleToSoilUsingSpatial(SW, Thickness, ParentSoil.InitialWater.SW, ParentSoil.Thickness);}
			}
		private double[] NO3MapedToSoil
			{
			get {return MapSampleToSoilUsingMass(NO3, Thickness, ParentSoil.InitialNitrogen.NO3, ParentSoil.Thickness, ParentSoil.BD);}
			}
		private double[] NH4MapedToSoil
			{
			get {return MapSampleToSoilUsingMass(NH4, Thickness, ParentSoil.InitialNitrogen.NH4, ParentSoil.Thickness, ParentSoil.BD);}
			}
		private double[] OCMapedToSoil
			{
			get {return MapSampleToSoilUsingMass(OC, Thickness, ParentSoil.OC, ParentSoil.Thickness, ParentSoil.BD);}
			}
		private double[] PHMapedToSoil
			{
			get {return MapSampleToSoilUsingSpatial(PH, Thickness, ParentSoil.PH, ParentSoil.Thickness);}
			}
		private double[] ECMapedToSoil
			{
			get {
				double[] soilec = null;
				if (ParentSoil.EC.Length == 0)
					{
					soilec = new double[ParentSoil.Thickness.Length];
					for (int i = 0; i != soilec.Length; i++)
						soilec[i] = 0;
					}
				else
					{
					soilec = ParentSoil.EC;
					}
				double[] ec = MapSampleToSoilUsingSpatial(EC, Thickness, soilec, ParentSoil.Thickness);
				return ec;
				}
			}

		// ----------------------------------------------------------------
		// Interpolate some BD values that match this sample's thicknesses.
		// ----------------------------------------------------------------
		private static double[] MapSoilToSampleUsingSpatial(double[] FromValues, double[] FromThickness, double[] ToThickness)
			{
			// need to work out a bd for this sample from the characterisation bd passed in.
			double[] FromMass = MathUtility.Multiply(FromValues, FromThickness);
			double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
			return MathUtility.Divide(ToMass, ToThickness);
			}


		// ------------------------------------------------------------------------
		// Map the specified values to the linked soil by first converting to
		// a mass value i.e. by first multiplying Values by BD. 
		// The result will be a set of values that correspond to the linked soil layers.
		// ------------------------------------------------------------------------
		private static double[] MapSampleToSoilUsingMass(double[] FromValues, double[] FromThickness,
														   double[] ToValues, double[] ToThickness, double[] ToBD)
			{
			if (ToValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness, ToValues, ToThickness);
				return MassRedistribute(FromValues, FromThickness, ToThickness, ToBD);	
				}
			else
				return new double[0];
			}


		// ------------------------------------------------------------------------
		// Map the specified values to the linked soil using a simple spatial
		// interpolation. The result will be a set of values that correspond 
		// to the linked soil layers.
		// ------------------------------------------------------------------------
		private static double[] MapSampleToSoilUsingSpatial(double[] FromValues, double[] FromThickness,
															  double[] ToValues, double[] ToThickness)
			{
			if (ToValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness,
										ToValues, ToThickness);
				FromValues = MathUtility.Multiply(FromValues, FromThickness);
				FromValues = SpatialRedistribute(FromValues, FromThickness, ToThickness);	
				return MathUtility.Divide(FromValues, ToThickness);
				}
			else
				return new double[0];
			}

		
		//-------------------------------------------------------------------------
		//Spatial mass redistribution algorithm.
		//-------------------------------------------------------------------------
		private static double[] SpatialRedistribute(double[] FromMass, double[] FromThickness, 
											 double[] ToThickness)
			{
			if(FromMass.Length != FromThickness.Length)
				{
				throw new Exception("Cannot redistribute soil sample layer structure to soil layer structure. "+
			                        "The number of values in the sample doesn't match the number of layers in the sample.");
				}

			// Remapping is achieved by first constructing a map of
			// cumulative mass vs depth
			// The new values of mass per layer can be linearly
			// interpolated back from this shape taking into account
			// the rescaling of the profile.

			double[] CumDepth = new double[FromMass.Length+1];
			double[] CumMass = new double[FromMass.Length+1];
			CumDepth[0] = 0.0;
			CumMass[0] = 0.0;
			for(int Layer = 0; Layer < FromThickness.Length; Layer++)
				{
				CumDepth[Layer+1] = CumDepth[Layer] + FromThickness[Layer];
				CumMass[Layer+1] = CumMass[Layer] + FromMass[Layer];
				}
					
			//look up new mass from interpolation pairs
			double[] ToMass = new double[ToThickness.Length];
			for(int Layer = 1; Layer <= ToThickness.Length; Layer++)
				{
				double LayerBottom = MathUtility.Sum(ToThickness, 0, Layer, 0.0);
				double LayerTop = LayerBottom - ToThickness[Layer-1];
				bool DidInterpolate = false;
				double CumMassTop = MathUtility.LinearInterpReal(LayerTop, CumDepth, 
					CumMass, ref DidInterpolate);
				double CumMassBottom = MathUtility.LinearInterpReal(LayerBottom, CumDepth, 
					CumMass, ref DidInterpolate);
				ToMass[Layer-1] = CumMassBottom - CumMassTop;
				}
			return ToMass;
			}	


		//-------------------------------------------------------------------------
		//Mass Redistribution algorithm
		//-------------------------------------------------------------------------
		private static double[] MassRedistribute(double[] FromValues, double[] FromThickness,
										  double[] ToThickness, double[] ToBd)
			{
			// Firstly we need to convert the values passed in, into a mass using
			// bulk density.

			double[] FromBd = MapSoilToSampleUsingSpatial(ToBd, ToThickness, FromThickness);
			double[] FromMass = new double[FromValues.Length];
			for(int Layer = 0; Layer < FromValues.Length; Layer++)
				FromMass[Layer] = FromValues[Layer] * FromBd[Layer] * FromThickness[Layer] / 100;
			
			// spatially interpolate mass.
			double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
			
			//now convert mass back to original values.
			double[] ToValues = new double[ToMass.Length];
			for(int Layer = 0; Layer < ToMass.Length; Layer++)
				ToValues[Layer] = ToMass[Layer] * 100.0 / ToBd[Layer] / ToThickness[Layer];

 			return ToValues;
			}


		//-------------------------------------------------------------------------
		// Remaps the thicknesses and values to more closely match the specified 
		// soil thickness and values. This algorithm removes all missing values
		// and their associated depths.
		// e.g. IF             SoilThickness  Values   SampleThickness	SampleValues
		//                           0-100		2         0-100				10
		//                         100-250	    3		100-600				11
		//                         250-500		4		
		//                         500-750		5
		//                         750-900		6
		//						   900-1200		7
		//                        1200-1500		8
		//                        1500-1800		9
		//
		// will produce:		SampleThickness			Values
		//						     0-100				  10
		//						   100-600				  11
		//						   600-750				   5
		//						   750-900				   6
		//						   900-1200				   7
		//						  1200-1500				   8
		//						  1500-1800				   9
		//
		//-------------------------------------------------------------------------
		private static void CreateVariableForMapping(ref double[] SampleValues, ref double[] SampleThickness, 
														double[] SoilValues, double[] SoilThickness)	
			{
			double[] ReturnThickness = new double[SampleThickness.Length + SoilThickness.Length + 1];
			double[] ReturnValues = new double[SampleThickness.Length + SoilThickness.Length + 1];

			// Copy values and thicknesses to return arrays until a missing value is found.
			double CumSampleDepth = 0.0;
			int SampleLayer = 0;
			for (SampleLayer = 0; ((SampleLayer != SampleThickness.Length) && (double)SampleValues[SampleLayer] != MathUtility.MissingValue); SampleLayer++)
				{
				ReturnThickness[SampleLayer] = SampleThickness[SampleLayer];
				ReturnValues[SampleLayer] = SampleValues[SampleLayer];
				CumSampleDepth += (double)SampleThickness[SampleLayer];
				}

			//Work out if we need to create a dummy layer so that the sample depths line up 
			//with the soil depths
			double CumSoilDepth = 0.0;
			for (int SoilLayer = 0; SoilLayer < SoilThickness.Length; SoilLayer++)
				{
				CumSoilDepth += SoilThickness[SoilLayer];
				if(CumSoilDepth > CumSampleDepth)
					{
					ReturnThickness[SampleLayer] = CumSoilDepth - CumSampleDepth;
					ReturnValues[SampleLayer] = SoilValues[SoilLayer];
					SampleLayer++;
					CumSampleDepth = CumSoilDepth;
					}
				}
			
			// Copy Values from our return arrays back to the parameters passed in.
			SampleThickness = new double[SampleLayer];
			SampleValues = new double[SampleLayer];
			for (int i = 0; i != SampleLayer; i++)
				{
                SampleThickness[i] = ReturnThickness[i];
				SampleValues[i] = ReturnValues[i];
				}
			}
		#endregion


		public double[] PAW(string CropName)
			{
			// return plant available water by layer (mm) given
			// depth, lower limit and dul all in (mm).
			if (ParentSoil.CropExists(CropName))
				{
				double[] sw = SW;
				double[] ll = MapSoilToSampleUsingSpatial(ParentSoil.LL(CropName), ParentSoil.Thickness, Thickness);
				double[] sat = MapSoilToSampleUsingSpatial(ParentSoil.SAT, ParentSoil.Thickness, Thickness);
				double[] thickness = Thickness;
				double[] paw = new double[thickness.Length];
				for(int i = 0; i != thickness.Length; i++)
					{
					sw[i] = Math.Max(sw[i], ll[i]);
					sw[i] = Math.Min(sw[i], sat[i]);
					paw[i] = ((sw[i]  - ll[i])* thickness[i]);
					}
				return paw;
				}
			throw new Exception("Soil is not parameterised for crop: " + CropName);
			}

		public void UpgradeToVersion3()
			{
			string SWUnit = Data.get_ChildValue("swunit");
			if (SWUnit != "")
				{
				if (SWUnit.ToLower() == "volumetric")
					SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				else
					SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				Data.Delete("swunit");
				}

			double[] oc = getLayered("nitrogen", "oc");
			if (oc.Length == 0)
				{
				setLayered("other", "oc", oc);    // moves to other

				for (int i = 0; i != oc.Length; i++)
					oc[i] = MathUtility.MissingValue;
				setLayered("nitrogen", "oc", oc); // deletes old values.
				}

			double[] ph = getLayered("nitrogen", "ph");
			if (ph.Length == 0)
				{
				setLayered("other", "ph", ph);    // moves to other

				for (int i = 0; i != ph.Length; i++)
					ph[i] = MathUtility.MissingValue;
				setLayered("nitrogen", "ph", ph); // deletes old values.
				}
		
			}

		
		}
	}
