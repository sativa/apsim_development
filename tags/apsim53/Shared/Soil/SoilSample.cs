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

		public string WaterFormat
            {
            get
                {
                string Format = Data.get_ChildValue("WaterFormat");
                if (Format == "")
                    Format = "VolumetricPercent";
                return Format;
                }
            set
                {
                Data.set_ChildValue("WaterFormat", value);
                }
            }
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

        #region Water methods
        public double[] SW
			{
			get {
                if (WaterFormat == "VolumetricPercent")
                    return Utility.getLayered(Data, "profile", "sw", "");
                else
                    {
                    double[] BD = Utility.MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
                    return MathUtility.Multiply(SWGrav, BD);
                    }
                }
			set {
                Utility.setLayered(Data, "profile", "sw", "", value);
                Utility.DeleteLayered(Data, "profile", "wet", "");
                Utility.DeleteLayered(Data, "profile", "dry", "");
                WaterFormat = "VolumetricPercent";
				}                           				
			}
		public double[] SWGrav
			{
			get {
                if (WaterFormat == "VolumetricPercent")
                    {
                    double[] BD = Utility.MapSoilToSampleUsingSpatial(ParentSoil.BD, ParentSoil.Thickness, Thickness);
                    return MathUtility.Divide(SW, BD);
                    }
                else if (WaterFormat == "GravimetricPercent")
                    return Utility.getLayered(Data, "profile", "sw", "");
                else
                    {
                    double[] Gravimetric = new double[Thickness.Length];
                    for (int layer = 0; layer != Thickness.Length; layer++)
                        {
                        if (Wet[layer] == MathUtility.MissingValue || Dry[layer] == MathUtility.MissingValue)
                            Gravimetric[layer] = MathUtility.MissingValue;
                        else
                            Gravimetric[layer] = (Wet[layer] - Dry[layer]) / Dry[layer];
                        }
                    return Gravimetric;
                    }
                }
			set {
                Utility.setLayered(Data, "profile", "sw", "", value);
                Utility.DeleteLayered(Data, "profile", "wet", "");
                Utility.DeleteLayered(Data, "profile", "dry", "");
                WaterFormat = "GravimetricPercent";
				}                           				
			}
		public double[] Wet
			{
			get {return Utility.getLayered(Data, "profile", "wet", "");}
			set {
                Utility.setLayered(Data, "profile", "wet", "", value);
				WaterFormat = "GravimetricWetDry";
				}                           				
			}
		public double[] Dry
			{
			get {return Utility.getLayered(Data, "profile", "dry", "");}
			set {
                Utility.setLayered(Data, "profile", "dry", "", value);
                WaterFormat = "GravimetricWetDry";
				}                           				
			}
        #endregion

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
		public double[] CL
			{
            get { return Utility.getLayered(Data, "profile", "cl", ""); }
            set { Utility.setLayered(Data, "profile", "cl", "", value); }
			}			
        public double[] SWMapedToSoil
            {
            get
                {
                double[] DefaultValues = ParentSoil.LL15;
                return Utility.MapSampleToSoilUsingSpatial(SW, Thickness, DefaultValues, ParentSoil.Thickness);
                }
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
                return Utility.MapSampleToSoilUsingMass(OC, Thickness, DefaultValues, ParentSoil.Thickness, ParentSoil.BD);
                }
            }
        public double[] PHMapedToSoil
            {
            get
                {
                double[] DefaultValues = ParentSoil.PH;
                return Utility.MapSampleToSoilUsingSpatial(PH, Thickness, DefaultValues, ParentSoil.Thickness);
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

                return Utility.MapSampleToSoilUsingSpatial(EC, Thickness, DefaultValues, ParentSoil.Thickness);
                }
            }
         public double[] PAW(string CropName)
			{
			// return plant available water by layer (mm) given
			// depth, lower limit and dul all in (mm).
			if (ParentSoil.CropExists(CropName))
				{
				double[] sw = SW;
                double[] ll = Utility.MapSoilToSampleUsingSpatial(ParentSoil.LL(CropName), ParentSoil.Thickness, Thickness);
                double[] sat = Utility.MapSoilToSampleUsingSpatial(ParentSoil.SAT, ParentSoil.Thickness, Thickness);
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
        #region Upgrade version
        public void UpgradeToVersion3()
			{
			string SWUnit = Data.get_ChildValue("swunit");
			if (SWUnit != "")
				{
                if (SWUnit.ToLower() == "volumetric")
                    WaterFormat = "VolumetricPercent";
                else
                    WaterFormat = "GravimetricPercent";
				Data.Delete("swunit");
				}
			else
                WaterFormat = "GravimetricPercent";

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
        #endregion

		}
	}
