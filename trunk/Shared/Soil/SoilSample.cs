using System;
using System.Xml;
using VBGeneral;
using System.Collections;
using CSGeneral;

namespace Soils
	{
	// -------------------------------------------------
	// A class for encapsulating a soil sample
	// A soil sample can have SW values in volumetric %
	// or gravimetric % or wet dry %. Default is Volumetric.
	// Assumes that data.parent is the parent soil.
	// the parent soil is used to get BD and other
	// soil variables when mapping a sample to a soil.
	// -------------------------------------------------
	public class SoilSample
		{
		private Soil ParentSoil;
        private APSIMData Data;
		public SoilSample(APSIMData data)
			{
            Data = data;
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
				Utility.setLayered(Data, "water", "sw", "", MissingValues);
				Data.set_ChildValue("WaterFormat", "GravimetricWetDry");
				}
			else
				{
				double[] MissingValues = new double[Thickness.Length];
				for (int i = 0; i != MissingValues.Length; i++)
					MissingValues[i] = MathUtility.MissingValue;
				Utility.setLayered(Data, "water", "wet", "", MissingValues);
                Utility.setLayered(Data, "water", "dry", "", MissingValues);

				if (WaterFormat == StoredWaterFormatType.GravimetricPercent)
					Data.set_ChildValue("WaterFormat", "GravimetricPercent");
				else if (WaterFormat == StoredWaterFormatType.VolumetricPercent)
					Data.set_ChildValue("WaterFormat", "VolumetricPercent");
				}
			}

		private double[] GetLayeredAsVol(string PropertyType)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				return Utility.getLayered(Data, "profile", PropertyType, "");
			else
				{
				double[] BD = MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
				return MathUtility.Multiply(GetLayeredAsGrav(PropertyType), BD);
				}
			}
		private double[] GetLayeredAsGrav(string PropertyType)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				{
				double[] BD = MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
				return MathUtility.Divide(Utility.getLayered(Data, "profile", PropertyType, ""),
											BD);
				}
			else if (StoredWaterFormat == StoredWaterFormatType.GravimetricPercent)
				return Utility.getLayered(Data, "profile", PropertyType, "");
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
				string DateString = Utility.GetStringValue(Data, "", "date");
				if (DateString == "")
					return DateTime.Today;
				else
					return DateTime.Parse(DateString);
				}
			set	{
				Utility.SetValue(Data, "", "date", value.ToString());
				}
			}
        public double[] Thickness
            {
            get { return Utility.getLayered(Data, "profile", "thickness", ""); }
            set { Utility.setLayered(Data, "profile", "thickness", "", value); }
            }
		public double[] SW
			{
			get {return GetLayeredAsVol("sw");}
			set {
                Utility.setLayered(Data, "profile", "sw", "", value);
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				}                           				
			}
		public double[] SWGrav
			{
			get {return GetLayeredAsGrav("sw");}
			set {
                Utility.setLayered(Data, "profile", "sw", "", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				}                           				
			}
		public double[] Wet
			{
			get {return Utility.getLayered(Data, "profile", "wet", "");}
			set {
                Utility.setLayered(Data, "profile", "wet", "", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricWetDry);
				}                           				
			}
		public double[] Dry
			{
			get {return Utility.getLayered(Data, "profile", "dry", "");}
			set {
                Utility.setLayered(Data, "profile", "dry", "", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricWetDry);
				}                           				
			}
		public double[] NO3
			{
			get {return Utility.getLayered(Data, "profile", "no3", "");}
			set {Utility.setLayered(Data, "profile", "no3", "", value);}
			}
		public double[] NH4
			{
            get { return Utility.getLayered(Data, "profile", "nh4", ""); }
            set { Utility.setLayered(Data, "profile", "nh4", "", value); }
			}
		public double[] OC
			{
            get { return Utility.getLayered(Data, "profile", "oc", ""); }
            set { Utility.setLayered(Data, "profile", "oc", "", value); }
			}
		public double[] PH
			{
            get { return Utility.getLayered(Data, "profile", "ph", ""); }
            set { Utility.setLayered(Data, "profile", "ph", "", value); }
			}
		public double[] EC
			{
            get { return Utility.getLayered(Data, "profile", "ec", ""); }
            set { Utility.setLayered(Data, "profile", "ec", "", value); }
			}
		public double[] ESP
			{
            get { return Utility.getLayered(Data, "profile", "esp", ""); }
            set { Utility.setLayered(Data, "profile", "esp", "", value); }
			}
        public double[] SWMapedToSoil
            {
            get
                {
                double[] DefaultValues = ParentSoil.LL15;
                return MapSampleToSoilUsingSpatial(SW, Thickness, DefaultValues, ParentSoil.Thickness);
                }
            }
        public double[] NO3MapedToSoil
            {
            get
                {
                double[] DefaultValues = new double[ParentSoil.Thickness.Length];
                for (int i = 0; i != DefaultValues.Length; i++)
                    DefaultValues[i] = 1.0;  // a small number
                return MapSampleToSoilUsingMass(NO3, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }
        public double[] NH4MapedToSoil
            {
            get
                {
                double[] DefaultValues = new double[ParentSoil.Thickness.Length];
                for (int i = 0; i != DefaultValues.Length; i++)
                    DefaultValues[i] = 0.2;  // a small number
                return MapSampleToSoilUsingMass(NH4, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }
        public double[] OCMapedToSoil
            {
            get
                {
                double[] DefaultValues = null;
                if (ParentSoil.OC.Length == 0)
                    {
                    DefaultValues = new double[ParentSoil.Thickness.Length];
                    for (int i = 0; i != DefaultValues.Length; i++)
                        DefaultValues[i] = 0.02;
                    }
                else
                    DefaultValues = ParentSoil.OC;
                return MapSampleToSoilUsingMass(OC, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }
        public double[] PHMapedToSoil
            {
            get
                {
                double[] DefaultValues = ParentSoil.PH;
                return MapSampleToSoilUsingSpatial(PH, Thickness, DefaultValues, ParentSoil.Thickness);
                }
            }
        public double[] ECMapedToSoil
            {
            get
                {
                double[] DefaultValues = null;
                if (ParentSoil.EC.Length == 0)
                    {
                    DefaultValues = new double[ParentSoil.Thickness.Length];
                    for (int i = 0; i != DefaultValues.Length; i++)
                        DefaultValues[i] = 0;
                    }
                else
                    DefaultValues = ParentSoil.EC;

                return MapSampleToSoilUsingSpatial(EC, Thickness, DefaultValues, ParentSoil.Thickness);
                }
            }
        #endregion
                    
		#region Soil mapping methods

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
														 double[] DefaultValues, double[] ToThickness, double[] ToBD)
			{
			if (DefaultValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness, DefaultValues, ToThickness);
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
															double[] DefaultValues, double[] ToThickness)
			{
			if (DefaultValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness,
										DefaultValues, ToThickness);
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
			else
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);

            double[] oc = Utility.getLayered(Data, "profile", "oc", "");
			if (oc.Length > 0)
				{
                Utility.setLayered(Data, "profile", "oc", "", oc);    // moves to other

				for (int i = 0; i != oc.Length; i++)
					oc[i] = MathUtility.MissingValue;
                Utility.setLayered(Data, "profile", "oc", "", oc); // deletes old values.
				}

            double[] ph = Utility.getLayered(Data, "profile", "ph", "");
			if (ph.Length > 0)
				{
                Utility.setLayered(Data, "profile", "ph", "", ph);    // moves to other

				for (int i = 0; i != ph.Length; i++)
					ph[i] = MathUtility.MissingValue;
                Utility.setLayered(Data, "profile", "ph", "", ph); // deletes old values.
				}
		
			}

        // -------------------------------------
        // Convert old soil file to new format 7
        // -------------------------------------
        public void UpgradeToVersion7()
            {
            APSIMData Result = new APSIMData("soilsample", Data.Name);
            foreach (APSIMData Child in Data.get_Children(null))
                {
                if (Child.Type.ToLower() == "water" ||
                    Child.Type.ToLower() == "nitrogen" ||
                    Child.Type.ToLower() == "other" ||
                    Child.Type.ToLower() == "soilcrop")
                    UpgradeToNodeVersion7(Child, Result);
                else if (Child.Type.ToLower() != "swunit")
                    Result.Add(Child);
                }
            APSIMData DataParent = Data.Parent;
            DataParent.DeleteNode(Data);
            DataParent.Add(Result);
            }

        private void UpgradeToNodeVersion7(APSIMData Data, APSIMData Result)
            {
            // ---------------------------------------------------------
            // Upgrade node putting all required child nodes into result
            // ---------------------------------------------------------
            APSIMData Profile = Result.Child("profile");
            if (Profile == null)
                Profile = Result.Add(new APSIMData("profile", ""));
            if (Data.Attribute("swunit").ToLower() == "gravimetricpercent" ||
                Data.Parent.get_ChildValue("swunit") == "gravimetric")
                Result.set_ChildValue("WaterFormat", "GravimetricPercent");

            int LayerNumber = 0;
            foreach (APSIMData Child in Data.get_Children(null))
                {
                if (Child.Type.ToLower() == "layer")
                    {
                    LayerNumber++;
                    int NumLayersInProfile = Profile.get_Children("layer").Length;
                    for (int i = NumLayersInProfile; i < LayerNumber; i++)
                        Profile.Add(new APSIMData("layer", ""));
                    APSIMData Layer = Profile.get_Children("layer")[LayerNumber - 1];
                    foreach (APSIMData Value in Child.get_Children(null))
                        {
                        if (Value.Value != "" && Convert.ToDouble(Value.Value) != MathUtility.MissingValue)
                            {
                            APSIMData LayerData = Layer.Add(Value);
                            if (Data.Type.ToLower() == "soilcrop")
                                LayerData.Name = Data.Name;
                            }
                        }
                    }
                else
                    Result.Add(Child);
                }
            }


        //private static void MergeSoilSample2WithSoil(SoilSample SoilSample2, Soil Soil,
        //                                             string CropName, bool UseEC, int RootDepth)
        //    {
        //    Soil.OC = SoilSample2.OCMapedToSoil;
        //    Soil.PH = SoilSample2.PHMapedToSoil;
        //    //Soil.CL = SoilSample2.CLMapedToSoil;
        //    if (UseEC)
        //        {
        //        Soil.EC = SoilSample2.ECMapedToSoil;
        //        Soil.ApplyECXFFunction(CropName);
        //        }
        //    if (RootDepth > 0)
        //        {
        //        RootDepth *= 10;   // cm to mm
        //        Soil.ApplyMaxRootDepth(CropName, RootDepth);
        //        }
        //    }

		}
	}
