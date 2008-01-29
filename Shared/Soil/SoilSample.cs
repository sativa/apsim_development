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
        private XmlNode Data;
		public SoilSample(XmlNode data, Soil ParentSoil)
			{
            Data = data;
            this.ParentSoil = ParentSoil;
			}

		public string WaterFormat
            {
            get
                {
                string Format = XmlHelper.Value(Data, "WaterFormat");
                if (Format == "")
                    Format = "VolumetricPercent";
                return Format;
                }
            set
                {
                XmlHelper.SetValue(Data, "WaterFormat", value);
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
                double[] DefaultValues;
                if (ParentSoil.CropsMeasured.Length > 0)
                    DefaultValues = ParentSoil.LL(ParentSoil.CropsMeasured[0]);
                else
                    DefaultValues = ParentSoil.LL15;

                double[] SampleSW; 
                double[] SampleThickness;
                SampleSW = new double[SW.Length + 2];
                SampleThickness = new double[Thickness.Length + 2];
                SW.CopyTo(SampleSW, 0);
                Thickness.CopyTo(SampleThickness, 0);
                int SecondBottomLayer = SampleThickness.Length - 2;
                int BottomLayer = SampleThickness.Length - 1;
                double BottomMeasuredSW = SW[Thickness.Length - 1];
                SampleThickness[SecondBottomLayer] = Thickness[Thickness.Length - 1];
                SampleSW[SecondBottomLayer] = 0.8 * BottomMeasuredSW;
                SampleThickness[BottomLayer] = Thickness[Thickness.Length - 1];
                SampleSW[BottomLayer] = 0.4 * BottomMeasuredSW;

                SampleSW = Utility.MapSampleToSoilUsingSpatial(SampleSW, SampleThickness, DefaultValues, ParentSoil.Thickness);

                double[] CumMeasuredThickness = Utility.ToCumThickness(Thickness);
                double BottomMeasuredThickness = CumMeasuredThickness[CumMeasuredThickness.Length - 1];
                double[] CumThickness = Utility.ToCumThickness(ParentSoil.Thickness);
                for (int i = 0; i != SampleSW.Length; i++)
                    {
                    if (CumThickness[i] > BottomMeasuredThickness)
                        SampleSW[i] = Math.Max(SampleSW[i], DefaultValues[i]);
                    }
                return SampleSW;
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
                if (EC.Length != 0)
                    {
                    double[] SampleEC = EC;
                    double[] SampleThickness = Thickness;
                    MakeSampleDeepUsingLastMeasuredValue(ref SampleThickness, ref SampleEC);
                    return Utility.MapSampleToSoilUsingSpatial(SampleEC, SampleThickness, DefaultValues, ParentSoil.Thickness);
                    }
                else
                    return DefaultValues;
                }
            }

        private static void MakeSampleDeepUsingLastMeasuredValue(ref double[] Thickness, ref double[] Values)
            {
            // Find last measured value
            int BottomMeasuredLayer;
            for (BottomMeasuredLayer = Values.Length - 1; BottomMeasuredLayer >= 0;  BottomMeasuredLayer--)
                {
                if (Values[BottomMeasuredLayer] != MathUtility.MissingValue)
                    break;
                }
            if (BottomMeasuredLayer < Values.Length)
                {
                // Fill up the array with last measured value. Remember there could be missing values!
                for (int Layer = BottomMeasuredLayer + 1; Layer < Values.Length; Layer++)
                    {
                    Thickness[Layer] = Thickness[BottomMeasuredLayer];
                    Values[Layer] = Values[BottomMeasuredLayer];
                    }

                // Now go and duplicate the bottom measured layer into a deep layer.
                Array.Resize(ref Thickness, Thickness.Length + 1);
                Array.Resize(ref Values, Values.Length + 1);
                int DeepLayer = Thickness.Length - 1;
                Thickness[DeepLayer] = Thickness[BottomMeasuredLayer];
                Values[DeepLayer] = Values[BottomMeasuredLayer];
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
			string SWUnit = XmlHelper.Value(Data, "swunit");
			if (SWUnit != "")
				{
                if (SWUnit.ToLower() == "volumetric")
                    WaterFormat = "VolumetricPercent";
                else
                    WaterFormat = "GravimetricPercent";
				Data.RemoveChild(XmlHelper.Find(Data, "swunit"));
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
            XmlNode Result = XmlHelper.CreateNode(Data.OwnerDocument, "soilsample", Data.Name);
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                {
                if (XmlHelper.Type(Child).ToLower() == "water" ||
                    XmlHelper.Type(Child).ToLower() == "nitrogen" ||
                    XmlHelper.Type(Child).ToLower() == "other" ||
                    XmlHelper.Type(Child).ToLower() == "soilcrop")
                    UpgradeToNodeVersion7(Child, Result);
                else if (XmlHelper.Type(Child).ToLower() != "swunit")
                    Result.AppendChild(Child);
                }
            XmlNode DataParent = Data.ParentNode;
            DataParent.RemoveChild(Data);
            DataParent.AppendChild(Result);
            }
        private void UpgradeToNodeVersion7(XmlNode Data, XmlNode Result)
            {
            // ---------------------------------------------------------
            // Upgrade node putting all required child nodes into result
            // ---------------------------------------------------------
            XmlNode Profile = XmlHelper.Find(Result, "profile");
            if (Profile == null)
                Profile = Result.AppendChild(XmlHelper.CreateNode(Result.OwnerDocument, "profile", ""));
            if (XmlHelper.Attribute(Data, "swunit").ToLower() == "gravimetricpercent" ||
                XmlHelper.Value(Data.ParentNode, "swunit") == "gravimetric")
                XmlHelper.SetValue(Result, "WaterFormat", "GravimetricPercent");

            int LayerNumber = 0;
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                {
                if (XmlHelper.Type(Child).ToLower() == "layer")
                    {
                    LayerNumber++;
                    int NumLayersInProfile = XmlHelper.ChildNodes(Profile, "layer").Count;
                    for (int i = NumLayersInProfile; i < LayerNumber; i++)
                        Profile.AppendChild(XmlHelper.CreateNode(Profile.OwnerDocument, "layer", ""));
                    XmlNode BottomMeasuredLayer = XmlHelper.ChildNodes(Profile, "layer")[LayerNumber - 1];
                    foreach (XmlNode Value in XmlHelper.ChildNodes(Child, ""))
                        {
                        if (Value.InnerText != "" && Convert.ToDouble(Value.InnerText) != MathUtility.MissingValue)
                            {
                            XmlNode LayerData = BottomMeasuredLayer.AppendChild(Value);
                            if (XmlHelper.Type(Data).ToLower() == "soilcrop")
                                XmlHelper.SetName(LayerData, Data.Name);
                            }
                        }
                    }
                else
                    Result.AppendChild(Child);
                }
            }
        #endregion


        public XmlNode ExportToSim(XmlNode ParentNode)
            {
            string NitrogenComponentName = XmlHelper.Name(ParentNode).Replace(" Water", " Nitrogen");
            XmlNode NitrogenSimNode = XmlHelper.Find(ParentNode.ParentNode, NitrogenComponentName);

            if (MathUtility.ValuesInArray(SW))
                {
                // Make sure the soil water values are between airdry and sat
                double[] sw = SWMapedToSoil;
                for (int i = 0; i != sw.Length; i++)
                    {
                    sw[i] = Math.Max(sw[i], ParentSoil.Airdry[i]);
                    sw[i] = Math.Min(sw[i], ParentSoil.SAT[i]);
                    }
                XmlHelper.SetValue(ParentNode, "initdata/sw", Utility.LayeredToString(sw));
                }
            if (MathUtility.ValuesInArray(NO3))
                {
                if (NitrogenSimNode == null)
                    throw new Exception("Cannot find soiln2 node");

                XmlHelper.SetValue(NitrogenSimNode, "initdata/no3ppm", Utility.LayeredToString(NO3MapedToSoil));
                }
            if (MathUtility.ValuesInArray(NH4))
                {
                if (NitrogenSimNode == null)
                    throw new Exception("Cannot find soiln2 node");
                XmlHelper.SetValue(NitrogenSimNode, "initdata/nh4ppm", Utility.LayeredToString(NH4MapedToSoil));
                }
            if (MathUtility.ValuesInArray(OC))
                {
                if (NitrogenSimNode == null)
                    throw new Exception("Cannot find soiln2 node");

                XmlHelper.SetValue(NitrogenSimNode, "initdata/oc", Utility.LayeredToString(OCMapedToSoil));
                }
            if (MathUtility.ValuesInArray(PH))
                {
                if (NitrogenSimNode == null)
                    throw new Exception("Cannot find soiln2 node");

                XmlHelper.SetValue(NitrogenSimNode, "initdata/ph", Utility.LayeredToString(PHMapedToSoil));
                }
            if (ParentSoil.UseEC && MathUtility.ValuesInArray(EC))
                {
                foreach (string CropName in ParentSoil.Crops)
                    {
                    XmlNode CropSimNode = XmlHelper.Find(ParentNode.ParentNode, CropName);
                    if (CropSimNode != null)
                        {
                        string XfString = XmlHelper.Value(CropSimNode, "initdata/xf");
                        string[] XfStrings = XfString.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                        double[] xf = new double[XfStrings.Length];
                        for (int i = 0; i != XfStrings.Length; i++)
                            xf[i] = Convert.ToDouble(XfStrings[i]);
                        ApplyECXFFunction(ParentSoil.Thickness, ECMapedToSoil, ref xf);

                        XmlHelper.SetValue(CropSimNode, "initdata/xf", Utility.LayeredToString(xf));

                        }
                    }
                }

            return ParentNode;
            }
        private static void ApplyECXFFunction(double[] Thickness, double[] EC, ref double[] xf)
            {
            // -------------------------------------------------
            // Using the soil's EC values - create an XF profile
            // -------------------------------------------------
            if (EC.Length > 0 && MathUtility.ValuesInArray(EC))
                {
                for (int i = 0; i != Thickness.Length; i++)
                    {
                    if (EC[i] <= 0.68)
                        xf[i] = 1.0;
                    else
                        {
                        xf[i] = 2.06 / (1 + 2 * EC[i]) - 0.351;
                        xf[i] = CSGeneral.MathUtility.Constrain(0.0, 1.0, xf[i]);
                        }
                    }
                }
            }


        }
	}
