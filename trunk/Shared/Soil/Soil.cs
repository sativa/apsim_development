using System;
using System.Collections.Specialized;
using System.Collections.Generic;
using System.IO;
using VBGeneral;


namespace CSGeneral
{
	// ---------------------------------
	// A class for encapsulating a soil
	// ---------------------------------
	public class Soil : SoilBase
		{
		private APSIMData PredLLCoeff = null;
        private APSIMData PredKLCoeff = null;
		public Soil(APSIMData data)	: base(data) 
            {
            InitWater w = InitialWater;
            InitNitrogen n = InitialNitrogen;
            }

		// -----------------------------------
		// General soil descriptive properties
		// -----------------------------------
        public string Name
            {
            get { return Data.Name; }
            set { Data.Name = value; }
            }
        public string State
            {
            get { return GetStringValue("", "state"); }
            set { SetValue("", "state", value); }
            }
        public string Region
			{
			get {return GetStringValue("", "region");}
			set {SetValue("", "region", value);}
			}

        public string NearestTown
            {
            get { return GetStringValue("", "nearesttown"); }
            set { SetValue("", "nearesttown", value); }
            }
        public string Site
			{
			get {return GetStringValue("", "site");}
			set {SetValue("", "site", value);}
			}

		public string Classification
			{
			get {return GetStringValue("", "SoilType");}
			set {SetValue("", "SoilType", value);}
			}

		public string DataSource
			{
			get {return GetStringValue("", "datasource");}
			set {SetValue("", "DataSource", value);}
			}
		public string Comment
			{
			get {return GetStringValue("", "comment");}
			set {SetValue("", "comment", value);}
			}

		public string NaturalVegetation
			{
			get {return GetStringValue("", "naturalvegetation");}
			set {SetValue("", "naturalvegetation", value);}
			}

		public string AttachmentFileName
			{	
			get {return GetStringValue("attachment", "filename");}
			set {
				if (File.Exists(value))
					{
					SetValue("attachment", "filename", Path.GetFileName(value));
			        FileStream fs = File.OpenRead(value);
					int NumBytes = Convert.ToInt32(fs.Length);
					byte[] buffer = new byte[NumBytes+1];
					fs.Read(buffer, 0, NumBytes);
					string Contents = Convert.ToBase64String(buffer);
					SetValue("attachment", "bytes", Contents);
					fs.Close();
					fs = null;
					}
				else
					SetValue("", "attachment", "");
				}
			}
		public string Attachment
			{
			get {return GetStringValue("attachment", "bytes");}
			}

		public string CreateAttachment()
			{
			if (AttachmentFileName != "")
				{
				string TempFileName = Path.GetTempPath() + AttachmentFileName;
		        FileStream fs = new FileStream(TempFileName, FileMode.Create);
				byte[] buffer = Convert.FromBase64String(GetStringValue("attachment", "bytes"));
				fs.Write(buffer, 0, buffer.Length);
				fs.Close();
				fs = null;
				return TempFileName;
				}
			else
				return "";
			}

		public double[] LL15
			{
            get { return getLayered("profile", "ll15", ""); }
            set { setLayered("profile", "ll15", "", value); }
			}
		public double[] Airdry
			{
            get { return getLayered("profile", "airdry", ""); }
            set { setLayered("profile", "airdry", "", value); }
			}
		public double[] DUL
			{
            get { return getLayered("profile", "dul", ""); }
            set { setLayered("profile", "dul", "", value); }
			}
		public double[] SAT
			{
            get { return getLayered("profile", "sat", ""); }
            set { setLayered("profile", "sat", "", value); }
			}
		public double[] BD
			{
            get { return getLayered("profile", "bd", ""); }
            set { setLayered("profile", "bd", "", value); }
			}
        public double[] Rocks
            {
            get { return getLayered("profile", "Rocks", ""); }
            set { setLayered("profile", "Rocks", "", value); }
            }
        public double[] SWCON
			{
            get { return getLayered("profile", "swcon", ""); }
            set { setLayered("profile", "swcon", "", value); }
			}
		public double[] MWCON
			{
            get { return getLayered("profile", "mwcon", ""); }
            set { setLayered("profile", "mwcon", "", value); }
			}
		public double[] FBIOM
			{
            get { return getLayered("profile", "fbiom", ""); }
            set { setLayered("profile", "fbiom", "", value); }
			}
		public double[] FINERT
			{
            get { return getLayered("profile", "finert", ""); }
            set { setLayered("profile", "finert", "", value); }
			}
		public double[] OC
			{
            get { return getLayered("profile", "oc", ""); }
            set { setLayered("profile", "oc", "", value); }
			}
        public bool PHStoredAsWater() { return (GetStringValue("profile", "phunits") != "CaCl"); }
        public double[] PH          // pH in water = (pH in CaCl X 1.1045) - 0.1375
			{
			get {
                if (PHStoredAsWater())
                    return getLayered("profile", "ph", "");
                else
                    return MathUtility.Subtract_Value(MathUtility.Multiply_Value(getLayered("profile", "ph", ""), 1.1045), 0.1375);
                }
            set {
                SetValue("profile", "phunits", "");
                setLayered("profile", "ph", "", value); 
                }
			}
        public double[] PHCaCl         
            {
            get {
                if (PHStoredAsWater())
                    return MathUtility.Add_Value(MathUtility.Divide_Value(getLayered("profile", "ph", ""), 1.1045), 0.1375);
                else
                    return getLayered("profile", "ph", "");
                }                  
            set {
                setLayered("profile", "ph", "", value);
                SetValue("profile", "phunits", "CaCl");
                }
            }
		public double[] EC
			{
            get { return getLayered("profile", "ec", ""); }
            set { setLayered("profile", "ec", "", value); }
			}
		public double[] CL
			{
            get { return getLayered("profile", "cl", ""); }
            set { setLayered("profile", "cl", "", value); }
			}
		public double[] Boron
			{
            get { return getLayered("profile", "boron", ""); }
            set { setLayered("profile", "boron", "", value); }
			}
		public double[] CEC
			{
            get { return getLayered("profile", "cec", ""); }
            set { setLayered("profile", "cec", "", value); }
			}
		public double[] Ca
			{
            get { return getLayered("profile", "ca", ""); }
            set { setLayered("profile", "ca", "", value); }
			}
		public double[] Mg
			{
            get { return getLayered("profile", "Mg", ""); }
            set { setLayered("profile", "Mg", "", value); }
			}
		public double[] Na
			{
            get { return getLayered("profile", "Na", ""); }
            set { setLayered("profile", "Na", "", value); }
			}
		public double[] K
			{
            get { return getLayered("profile", "K", ""); }
            set { setLayered("profile", "K", "", value); }
			}
		public double[] ESP
			{
            get { return getLayered("profile", "esp", ""); }
            set { setLayered("profile", "esp", "", value); }
			}
		public double[] ParticleSizeSand
			{
            get { return getLayered("profile", "ParticleSizeSand", ""); }
            set { setLayered("profile", "ParticleSizeSand", "", value); }
			}
		public double[] ParticleSizeSilt
			{
            get { return getLayered("profile", "ParticleSizeSilt", ""); }
            set { setLayered("profile", "ParticleSizeSilt", "", value); }
			}
		public double[] ParticleSizeClay
			{
            get { return getLayered("profile", "ParticleSizeClay", ""); }
            set { setLayered("profile", "ParticleSizeClay", "", value); }
			}
        public string[] Texture
            {
            get { return getLayeredAsStrings("profile", "texture", ""); }
            set { setLayeredAsStrings("profile", "texture", "", value); }
            }
		public double[] LabileP
			{
            get { return getLayered("profile", "LabileP", ""); }
            set { setLayered("profile", "LabileP", "", value); }
			}
		public double[] BandedP
			{
            get { return getLayered("profile", "BandedP", ""); }
            set { setLayered("profile", "BandedP", "", value); }
			}
		public double[] RockP
			{
            get { return getLayered("profile", "RockP", ""); }
            set { setLayered("profile", "RockP", "", value); }
			}
		public double[] Sorption
			{
            get { return getLayered("profile", "Sorption", ""); }
            set { setLayered("profile", "Sorption", "", value); }
			}


		// ------------------------------------------------------
		// Crop properties
		// ------------------------------------------------------
		public string[] Crops
			{
			get {
                String[] ReturnListCollection = CropsMeasured;
				StringCollection PredCrops = PredictedCrops;
				string[] ReturnList = new string[ReturnListCollection.Length + PredCrops.Count];
				ReturnListCollection.CopyTo(ReturnList, 0);
				PredCrops.CopyTo(ReturnList, ReturnListCollection.Length);
				return ReturnList;
				}
			}
        public string[] CropsMeasured
            {
            get
                {
                APSIMData Profile = Data.Child("profile");
                if (Profile != null)
                    {
                    APSIMData Layer = Profile.ChildByType("layer");
                    if (Layer != null)
                        {
                        String[] ReturnListCollection = Layer.ChildNames("ll");
                        string[] ReturnList = new string[ReturnListCollection.Length];
                        ReturnListCollection.CopyTo(ReturnList, 0);
                        return ReturnList;
                        }
                    }
                return new string[0];
                }
            }
        public bool CropExists(string CropName)
			{
            foreach (string MeasuredCrop in CropsMeasured)
                {
                if (MeasuredCrop.ToLower() == CropName.ToLower())
                    return true;
                }
            return false;
			}
		public bool CropIsPredicted(string CropName)
			{
			return !CropExists(CropName);
			}
        public void AddCrop(string CropName)
            {
            double[] ll = LL15;
            double[] kl = new double[ll.Length];
            double[] xf = new double[ll.Length];
            for (int i = 0; i != ll.Length; i++)
                {
                kl[i] = 0.06;
                xf[i] = 1.0;
                }
            SetCrop(CropName, ll, kl, xf);
            }
        public void DeleteCrop(string CropName)
			{
            DeleteLayered("profile", "ll", CropName);
            DeleteLayered("profile", "kl", CropName);
            DeleteLayered("profile", "xf", CropName);
            }
		public double[] LL(string CropName)
			{
			if (CropExists(CropName))
                return getLayered("profile", "ll", CropName);
			else
				return PredictedLL(CropName);
			}
		public double[] KL(string CropName)
			{
			if (CropExists(CropName))
                return getLayered("profile", "kl", CropName);
			else
				return PredictedKL(CropName);
			}
		public double[] XF(string CropName)
			{
			if (CropExists(CropName))
                return getLayered("profile", "xf", CropName);
			else
				return PredictedXF(CropName);
			}
		public void SetCrop(string CropName, double[] ll, double[] kl, double[] xf)
			{
            setLayered("profile", "ll", CropName, ll);
            setLayered("profile", "kl", CropName, kl);
            setLayered("profile", "xf", CropName, xf);
			}

        struct SavedCrop
            {
            public string Name;
            public double[] ll;
            public double[] kl;
            public double[] xf;

            };
		public void SetCropOrder(string[] CropNames)
			{
            List<SavedCrop> Crops = new List<SavedCrop>();
			for (int i = 0; i != CropNames.Length; i++)
				{
                SavedCrop Crop;
                Crop.Name = CropNames[i];
                Crop.ll = LL(CropNames[i]);
                Crop.kl = KL(CropNames[i]);
                Crop.xf = XF(CropNames[i]);
                Crops.Add(Crop);
                DeleteCrop(CropNames[i]);
                }
            foreach (SavedCrop Crop in Crops)
                SetCrop(Crop.Name, Crop.ll, Crop.kl, Crop.xf);
			}


		// ------------------------------------------------------
		// Predicted crop properties
		// ------------------------------------------------------
		private bool OpenPredLLCoeffFile()
			{
			if (PredLLCoeff == null)
				{
				string CoeffFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "Soil", "PredLLCoeffFile");
				if (File.Exists(CoeffFileName))
					{
					PredLLCoeff = new APSIMData();
					PredLLCoeff.LoadFromFile(CoeffFileName);
					}
				}
			return (PredLLCoeff != null);
			}
        private bool OpenPredKLCoeffFile()
            {
            if (PredKLCoeff == null)
                {
                string CoeffFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "Soil", "PredKLCoeffFile");
                if (File.Exists(CoeffFileName))
                    {
                    PredKLCoeff = new APSIMData();
                    PredKLCoeff.LoadFromFile(CoeffFileName);
                    }
                }
            return (PredKLCoeff != null);
            }


		private StringCollection PredictedCrops
			{
			get {
				StringCollection PredCrops = new StringCollection();
				if (OpenPredLLCoeffFile() && CumThicknessMidPoints.Length > 0)
					{
					// get a list of all possible predicted crops.
                    string SoilNameNoSpaces = Classification.Replace(" ", "");
					double[] SoilDepthCentre = this.CumThicknessMidPoints;
				
					foreach (APSIMData PredSoil in  PredLLCoeff.get_Children(null))
						if (PredSoil.Name.ToLower() == SoilNameNoSpaces.ToLower())
							foreach (string CropName in PredSoil.ChildNames(null))
								if (!CropExists(CropName))
									{
									double[] a = null;
									double[] b = null;
									double[] CoeffDepthCentre = null;
									PredictedCoeffs(CropName, ref a, ref b, ref CoeffDepthCentre);
									if (SoilDepthCentre[SoilDepthCentre.Length-1] <= CoeffDepthCentre[CoeffDepthCentre.Length-1])
										PredCrops.Add(CropName);
									}
					}
				return PredCrops;
				}
			}

		private void PredictedCoeffs(string CropName, ref double[] a, ref double[] b, ref double[] CoeffDepthCentre)
			{
			// Get all coefficients and convert to double arrays.
			StringCollection AStrings = new StringCollection();
			StringCollection BStrings = new StringCollection();
			StringCollection LayerCentreStrings = new StringCollection();
            string SoilNameNoSpaces = Classification.Replace(" ", "");
			foreach (APSIMData Layer in PredLLCoeff.Child(SoilNameNoSpaces).Child(CropName).get_Children("layer"))
				{
				AStrings.Add(Layer.get_ChildValue("a"));
				BStrings.Add(Layer.get_ChildValue("b"));
				LayerCentreStrings.Add(Layer.get_ChildValue("LayerCentre"));
				}

			if (AStrings.Count == 0 || AStrings.Count != BStrings.Count || AStrings.Count != LayerCentreStrings.Count)
                throw new Exception("Invalid predicted LL coeffs found for soil: " + Classification + " and crop: " + CropName);
			a = new double[AStrings.Count];
			b = new double[BStrings.Count];
			CoeffDepthCentre = new double[LayerCentreStrings.Count];
			for (int i = 0; i != AStrings.Count; i++)
				{
				a[i] = Convert.ToDouble(AStrings[i]);
				b[i] = Convert.ToDouble(BStrings[i]);
				CoeffDepthCentre[i] = Convert.ToDouble(LayerCentreStrings[i]);
				}
			}

		private double[] PredictedLL(string CropName)
			{
			if (OpenPredLLCoeffFile() && Utility.IndexOfCaseInsensitive(PredictedCrops, CropName) != -1)
				{
				double[] a = null;
				double[] b = null;
				double[] CoeffDepthCentre = null;
				PredictedCoeffs(CropName, ref a, ref b, ref CoeffDepthCentre);

				// Get some soil numbers we're going to need.
				double[] SoilDepthCentre = this.CumThicknessMidPoints;
				double[] SoilDUL = MathUtility.Multiply_Value(this.DUL, 100);

                // Find the lowest measured crop LL in 3rd layer.
                double LowestLL3rdLayer = FindLowestCropLL3rdLayer();

				// only continue if our soil depth depth centers are within range of
				// the coefficient depth centers.
				if (SoilDepthCentre[SoilDepthCentre.Length-1] <= CoeffDepthCentre[CoeffDepthCentre.Length-1])
					{
					double[] PredLL = new double[SoilDepthCentre.Length];
					for (int i = 0; i != SoilDepthCentre.Length; i++)
						{
						bool DidInterpolate = false;
						double A = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, a, ref DidInterpolate);
						double B = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, b, ref DidInterpolate);
						PredLL[i] = SoilDUL[i] * (A + B * SoilDUL[i]) / 100.0;

						// Bound the predicted LL values.
						PredLL[i] = Math.Max(PredLL[i], this.LL15[i]);
						PredLL[i] = Math.Min(PredLL[i], this.DUL[i]);
						}

					// make the top 2 layers the same as the smallest measured crop LL[2]
                    if (PredLL.Length >= 3)
                        {
                        if (LowestLL3rdLayer != MathUtility.MissingValue)
                            {
                            PredLL[0] = LowestLL3rdLayer;
                            PredLL[1] = LowestLL3rdLayer;
                            }
                        else
                            {
                            PredLL[0] = PredLL[2];
                            PredLL[1] = PredLL[2];
                            }
                        }
                    
                    return PredLL;
					}
				}
			return new double[0];
			}

        private double FindLowestCropLL3rdLayer()
            {
            double LowestLL3rdLayer = MathUtility.MissingValue;
            foreach (string CropName in CropsMeasured)
                {
                double[] ll = LL(CropName);
                if (ll.Length >= 3)
                    LowestLL3rdLayer = Math.Min(LowestLL3rdLayer, ll[2]);
                }
            return LowestLL3rdLayer;
            }

		private double[] PredictedKL(string CropName)
			{
            if (OpenPredKLCoeffFile())
                {
                APSIMData CropKlValues = PredKLCoeff.Child(CropName);
                if (CropKlValues != null)
                    {
                    StringCollection LayerCentreStrings = new StringCollection();
                    StringCollection KLStrings = new StringCollection();
                    foreach (APSIMData Layer in CropKlValues.get_Children("layer"))
                        {
                        KLStrings.Add(Layer.get_ChildValue("kl"));
                        LayerCentreStrings.Add(Layer.get_ChildValue("LayerCentre"));
                        }
                    double[] kl = new double[KLStrings.Count];
                    double[] CoeffDepthCentre = new double[LayerCentreStrings.Count];
                    for (int i = 0; i != KLStrings.Count; i++)
                        {
                        kl[i] = Convert.ToDouble(KLStrings[i]);
                        CoeffDepthCentre[i] = Convert.ToDouble(LayerCentreStrings[i]);
                        }

                    double[] SoilDepthCentre = this.CumThicknessMidPoints;
                    double[] Values = new double[SoilDepthCentre.Length];
                    bool DidInterpolate = true;
                    for (int i = 0; i != SoilDepthCentre.Length; i++)
                        Values[i] = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, kl, ref DidInterpolate);
                    return Values;
                    }
                }
            return new double[0];
			}

        private double[] PredictedXF(string CropName)
			{
            if (CropsMeasured.Length > 0 && OpenPredLLCoeffFile())
                return XF(CropsMeasured[0]);
			else
                {
                int NumLayers = Thickness.Length;
                double[] Values = new double[NumLayers];
                for (int i = 0; i != NumLayers; i++)
                    Values[i] = 1.0;
                return Values;
                }
			}

		// -----------------------------------
		// APSIM soil properties
		// -----------------------------------
		public double U
			{
			get {return GetDoubleValue("", "U");}
			}
		public double Cona
			{
			get {return GetDoubleValue("", "Cona");}
			}
        public void SetUCona(double U, double Cona)
            {
            SetValue("", "Cona", Cona);
            SetValue("", "U", U);
            SetValue("", "SummerCona", "");
            SetValue("", "WinterCona", "");
            SetValue("", "SummerU", "");
            SetValue("", "WinterU", "");
            SetValue("", "SummerDate", "");
            SetValue("", "WinterDate", "");
            }

        public void SetSummerWinterUCona(double SummerU, double WinterU, 
                                         double SummerCona, double WinterCona,   
                                         string SummerDate, string WinterDate)
            {
            SetValue("", "Cona", "");
            SetValue("", "U", "");
            SetValue("", "SummerCona", SummerCona);
            SetValue("", "WinterCona", WinterCona);
            SetValue("", "SummerU", SummerU);
            SetValue("", "WinterU", WinterU);
            SetValue("", "SummerDate", SummerDate);
            SetValue("", "WinterDate", WinterDate);
            }
        public double WinterU
            {
            get { return GetDoubleValue("", "WinterU"); }
            }
        public double SummerU
            {
            get { return GetDoubleValue("", "SummerU"); }
            }
        public double WinterCona
            {
            get { return GetDoubleValue("", "WinterCona"); }
            }
        public double SummerCona
            {
            get { return GetDoubleValue("", "SummerCona"); }
            }
        public string WinterDate
            {
            get { return GetStringValue("", "WinterDate"); }
            }
        public string SummerDate
            {
            get { return GetStringValue("", "SummerDate"); }
            }

		public double Salb
			{
			get {return GetDoubleValue("", "Salb");}
			set {SetValue("", "Salb", value);}
			}
		public double DiffusConst
			{
			get {return GetDoubleValue("", "DiffusConst");}
			set {SetValue("", "DiffusConst", value);}
			}
		public double DiffusSlope
			{
			get {return GetDoubleValue("", "DiffusSlope");}
			set {SetValue("", "DiffusSlope", value);}
			}
		public double CN2Bare
			{
			get {return GetDoubleValue("", "CN2Bare");}
			set {SetValue("", "CN2Bare", value);}
			}
		public double CNRed
			{
			get {return GetDoubleValue("", "CNRed");}
			set {SetValue("", "CNRed", value);}
			}
		public double CNCov
			{
			get {return GetDoubleValue("", "CNCov");}
			set {SetValue("", "CNCov", value);}
			}
		public double RootCN
			{
			get {return GetDoubleValue("", "RootCN");}
			set {SetValue("", "RootCN", value);}
			}
		public double RootWT
			{
			get {return GetDoubleValue("", "RootWT");}
			set {SetValue("", "RootWT", value);}
			}
		public double SoilCN
			{
			get {return GetDoubleValue("", "SoilCN");}
			set {SetValue("", "SoilCN", value);}
			}
		public double EnrACoeff
			{
			get {return GetDoubleValue("", "EnrACoeff");}
			set {SetValue("", "EnrACoeff", value);}
			}
		public double EnrBCoeff
			{
			get {return GetDoubleValue("", "EnrBCoeff");}
			set {SetValue("", "EnrBCoeff", value);}
			}
		public double ResidueCP
			{
			get {return GetDoubleValue("", "ResidueCP");}
			set {SetValue("", "ResidueCP", value);}
			}
		public double RootCP
			{
			get {return GetDoubleValue("", "RootCP");}
			set {SetValue("", "RootCP", value);}
			}
		public double RateDissolRock
			{
			get {return GetDoubleValue("", "RateDissolRock");}
			set {SetValue("", "RateDissolRock", value);}
			}


		// ------------------------------------------------------------------
		// return plant available water content by layer (mm)
		// ------------------------------------------------------------------
		public double[] PAWC(string CropName)
			{
			return PAWC(this.LL(CropName), this.XF(CropName));
			}
		// ------------------------------------------------------------------
		// return plant available water content by layer (mm) given
		// depth, lower limit and dul all in (mm).
		// ------------------------------------------------------------------
		public double[] PAWC()
			{
            double[] xf = new double[LL15.Length];
            for (int i = 0; i != xf.Length; i++)
                xf[i] = 1;

			return PAWC(this.LL15, xf);
			}
		// ------------------------------------------------------------------
		// return plant available water content by layer (mm)
		// ------------------------------------------------------------------
		private double[] PAWC(double[] LL, double[] XF)
			{
			double[] Thickness = this.Thickness;
			double[] DUL = this.DUL;
			double[] PAWC = new double[Thickness.Length];

			if (Thickness.Length != DUL.Length || Thickness.Length != LL.Length)
				return new double[0];

			for (int layer = 0; layer != Thickness.Length; layer++)
				if (DUL[layer] == MathUtility.MissingValue ||
					LL[layer] == MathUtility.MissingValue ||
                    XF[layer] == 0 ||
                    (layer > 0 && PAWC[layer-1] == 0) )
					PAWC[layer] = 0;
				else
					PAWC[layer] = (DUL[layer] - LL[layer]) * Thickness[layer];
			return PAWC;
			}
		// ------------------------------------------------------------------
		// return plant available water by layer (mm)
		// ------------------------------------------------------------------
		public double[] PAW(string cropName)
			{
			double[] Thickness = this.Thickness;
			double[] LL = this.LL(cropName);
			double[] SW = InitialWater.SW;
			double[] PAW = new double[Thickness.Length];

			// calculate depth increments.
			if (Thickness.Length > 0)
				{
				for (int layer = 0; layer != SW.Length; layer++)
					PAW[layer] = Math.Max((SW[layer] - LL[layer]), 0.0) * Thickness[layer];
				}
			return PAW;
			}


		// ----------------------------------------------------------------
		// Export the soil to a PAR file.
		// ----------------------------------------------------------------
		public void ExportToPar(string FileName, string SectionName, bool AppendToFile)
			{
			string Template =
				"[$SECTIONNAME$.soilwat2.parameters]\r\n"+
				"   diffus_const = [soil.DiffusConst]    ! coeffs for unsaturated water flow\r\n"+
                "   diffus_slope = [soil.DiffusSlope]\r\n" +
                "   cn2_bare     = [soil.Cn2Bare]    ! bare soil runoff curve number\r\n" +
                "   cn_red       = [soil.CnRed]    ! potetial reduction in curve number due to residue\r\n" +
                "   cn_cov       = [soil.CnCov]   ! cover for maximum reduction in curve number\r\n" +
                "   salb         = [soil.Salb]  ! bare soil albedo\r\n";
            if (SummerCona != MathUtility.MissingValue)
                {
                Template += "   SummerCona   = [soil.SummerCona]   ! stage 2 evap coef. for summer\r\n" +
                            "   WinterCona   = [soil.WinterCona]   ! stage 2 evap coef. for winter\r\n" +
                            "   SummerU      = [soil.SummerU]      ! stage 1 soil evaporation coefficient for summer (mm)\r\n" +
                            "   WinterU      = [soil.WinterU]      ! stage 1 soil evaporation coefficient for winter (mm)\r\n" +
                            "   SummerDate   = [soil.SummerDate]      ! Start date of summer\r\n" +
                            "   WinterDate   = [soil.WinterDate]      ! Start date of winter\r\n";
                }
            else
                Template += "   cona         = [soil.Cona]   ! stage 2 evap coef.\r\n" +
                            "   u            = [soil.U]     ! stage 1 soil evaporation coefficient (mm)\r\n";
            Template +=
                "\r\n" +
                "[foreach Soil.profile]\r\n" +
                "   dlayer  =[foreach profile.layer as Layer]  [Layer.thickness.3][endfor]   ! layer thickness mm soil\r\n" +
                "   air_dry =[foreach profile.layer as Layer]    [Layer.airdry.3][endfor]   ! air dry mm water/mm soil\r\n" +
                "   ll15    =[foreach profile.layer as Layer]    [Layer.ll15.3][endfor]   ! lower limit mm water/mm soil\r\n" +
                "   dul     =[foreach profile.layer as Layer]    [Layer.dul.3][endfor]   ! drained upper limit mm water/mm soil\r\n" +
                "   sat     =[foreach profile.layer as Layer]    [Layer.sat.3][endfor]   ! saturation mm water/mm soil\r\n" +
                "   sw      =$SW$   ! starting soil water mm water/mm soil\r\n" +
                "   swcon   =[foreach profile.layer as Layer]    [Layer.swcon.3][endfor]   ! drainage coefficient\r\n" +
                "   bd      =[foreach profile.layer as Layer]    [Layer.bd.3][endfor]   ! bulk density gm dry soil/cc moist soil\r\n";
            if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
                Template +=
                "   mwcon   =[foreach profile.layer as Layer]    [Layer.mwcon.3][endfor]   \r\n\r\n";
            
            Template +=
                "[endfor]\r\n" +//END OF WATER FOR LOOP
                "\r\n" +
                "[$SECTIONNAME$.soiln2.parameters]\r\n" +//TITLE
                "   root_cn      = [soil.rootcn]     ! C:N ratio of initial root residues\r\n" +
                "   root_wt      = [soil.rootwt]   ! root residues as biomass (kg/ha)\r\n" +
                "   soil_cn      = [soil.soilcn]   ! C:N ratio of soil\r\n" +
                "   enr_a_coeff  = [soil.enracoeff]\r\n" +
                "   enr_b_coeff  = [soil.enrbcoeff]\r\n" +
                "   profile_reduction =  off\r\n" +
                "\r\n" +
                "[foreach Soil.profile]\r\n" +
                "   oc      =[foreach profile.layer as Layer]\r\n      [Layer.oc.3][endfor]   ! Soil Organic Carbon\r\n" +
                "   ph      =$PH$   ! pH of soil\r\n" +
                "   fbiom   =[foreach profile.layer as Layer]\r\n      [Layer.fbiom.3][endfor]   ! Organic C Biomass Fraction\r\n" +
                "   finert  =[foreach profile.layer as Layer]\r\n      [Layer.finert.3][endfor]   ! Inert Organic C Fraction\r\n" +
                "   no3ppm  =$NO3$   ! Nitrate Concentration\r\n" +
                "   nh4ppm  =$NH4$   ! Ammonium Concentration\r\n" +
                "[endfor]\r\n" +//END OF NITROGEN FOR LOOP
                "\r\n" +
                "[if [soil.ResidueCP] > 0]\r\n" +
                "[$SECTIONNAME$.soilp.parameters]\r\n" +
                "   residue_cp         =  [soil.residuecp]   () !c:p ratio of residues at initialisation\r\n" +
                "   root_cp            =  [soil.rootcp]      () !c:p ratio of roots at initialisation\r\n" +
                "   rate_dissol_rock_P =  [soil.RateDissolRock] (/yr)   !rate at which rock P source becomes available\r\n" +
                "\r\n" +
                "[foreach Soil.profile]\r\n" +
                "   labile_P  = [foreach profile.layer]    [layer.labilep.3][endfor]   (mg/kg)\r\n" +
                "   banded_P  = [foreach profile.layer]    [layer.bandedP.3][endfor]   (kg/ha) ! banded p content for each layer\r\n" +
                "   rock_P    = [foreach profile.layer]    [layer.rockP.3][endfor]   (kg/ha)   !rock p content for each layer ie no water soluble\r\n" +
                "   sorption  =[foreach profile.layer]  [layer.sorption.3][endfor]   ()   !P sorbed at 0.2ppm\r\n" +
                "[endfor]\r\n" +
                "[endif]\r\n" +
                "[endfile]\r\n\r\n";


            foreach (string CropName in Crops)
                {
                Template += "[$SECTIONNAME$." + CropName + ".parameters]\r\n";
                if (CropIsPredicted(CropName))
                    Template += "   !These crop numbers are predicted\r\n";

			    string LLLine = "";
			    string KLLine = "";
			    string XFLine = "";
			    double[] ll = LL(CropName);
			    double[] kl = KL(CropName);
			    double[] xf = XF(CropName);
			    for (int i = 0; i != ll.Length; i++)
				    {
				    LLLine += "    " + ll[i].ToString("f3");
				    KLLine += "      " + kl[i].ToString("f3");
				    XFLine += "      " + xf[i].ToString("f3");
				    }
                Template += "   <ll>" + LLLine + "</ll>\r\n";
                if (CropName.ToLower() == "ozcot")
                    Template += "   Title = XXX\r\n" +
                                "   asoil = 3.0\r\n";

                Template += "   <kl>" + KLLine + "</kl>\r\n";
                Template += "   <xf>" + XFLine + "</xf>\r\n";
                }

			string SWLine = "";
			string NO3Line = "";
			string NH4Line = "";
            string PHLine = "";
			double[] sw = InitialWater.SW;
			double[] no3 = InitialNitrogen.NO3;
			double[] nh4 = InitialNitrogen.NH4;
            double[] ph = PH;

			int NumLayers = Thickness.Length;
			for (int i = 0; i != NumLayers; i++)
				{
				SWLine += "    " + sw[i].ToString("f3");
				NO3Line += "      " + no3[i].ToString("f3");
				NH4Line += "      " + nh4[i].ToString("f3");
                PHLine += "      " + ph[i].ToString("f3");
				}

			Template = Template.Replace("$SW$", SWLine);
			Template = Template.Replace("$NO3$", NO3Line);
			Template = Template.Replace("$NH4$", NH4Line);
            Template = Template.Replace("$PH$", PHLine);
			Template = Template.Replace("$SECTIONNAME$", SectionName);

			string szSoilFileTemplate = "[file " + Path.GetFileName(FileName) + "]\r\n" + Template;
			Macro SoilMacro = new Macro();
			StringCollection scSoilFiles = SoilMacro.Go(Data, szSoilFileTemplate,
														Path.GetDirectoryName(FileName),
														AppendToFile);
			}


		public void ExportToSim(TextWriter Out)
			{
            string Template =
                "<component name=\"[soil.name] Water\" executable=\"%apsuite\\apsim\\soilwat2\\lib\\soilwat2.dll\">\r\n" +
                "   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soilwat2\\soilwat2.ini</include>\r\n" +
                "      <diffus_const>[soil.DiffusConst]</diffus_const>\r\n" +
                "      <diffus_slope>[soil.DiffusSlope]</diffus_slope>\r\n" +
                "      <cn2_bare>[soil.Cn2Bare]</cn2_bare>\r\n" +
                "      <cn_red>[soil.CnRed]</cn_red>\r\n" +
                "      <cn_cov>[soil.CnCov]</cn_cov>\r\n" +
                "      <salb>[soil.Salb]</salb>\r\n";
            if (SummerCona != MathUtility.MissingValue)
                {
                Template +=
                "      <SummerCona>[soil.SummerCona]</SummerCona>\r\n" +
                "      <WinterCona>[soil.WinterCona]</WinterCona>\r\n" +
                "      <SummerU>[soil.SummerU]</SummerU>\r\n" +
                "      <WinterU>[soil.WinterU]</WinterU>\r\n" +
                "      <SummerDate>[soil.SummerDate]</SummerDate>\r\n" +
                "      <WinterDate>[soil.WinterDate]</WinterDate>\r\n";
                }
            else
                Template +=
                "      <cona>[soil.Cona]</cona>\r\n" +
                "      <u>[soil.U]</u>\r\n";

            Template +=
                "[foreach soil.profile]\r\n" +
                "      <dlayer> [foreach profile.layer as l][l.thickness] [endfor] </dlayer>\r\n" +
                "      <sat>[foreach profile.layer as l][l.sat] [endfor]</sat>\r\n" +
                "      <dul>[foreach profile.layer as l][l.dul] [endfor]</dul>\r\n" +
                "      <ll15>[foreach profile.layer as l][l.ll15] [endfor]</ll15>\r\n" +
                "      <air_dry>[foreach profile.layer as l][l.airdry] [endfor]</air_dry>\r\n" +
                "      <swcon>[foreach profile.layer as l][l.swcon] [endfor]</swcon>\r\n" +
                "      <bd>[foreach profile.layer as l][l.bd] [endfor]</bd>\r\n";
            if (MWCON.Length > 0 && MWCON[0] != MathUtility.MissingValue)
                Template +=
                "      <mwcon>[foreach profile.layer as l][l.mwcon] [endfor]</mwcon>\r\n";

            Template +=
                "      <sw>$SW$</sw>\r\n" +
                "[endfor]\r\n" +
                "   </initdata>\r\n" +
                "</component>\r\n" +
                "<component name=\"[soil.name] Nitrogen\" executable=\"%apsuite\\apsim\\soiln2\\lib\\soiln2.dll\">\r\n" +
                "   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soiln2\\soiln2.ini</include>\r\n" +
                "      <soiltype>[soil.soiltype]</soiltype>\r\n" +
                "      <root_cn>[soil.RootCN]</root_cn>\r\n" +
                "      <root_wt>[soil.RootWT]</root_wt>\r\n" +
                "      <soil_cn>[soil.SoilCN]</soil_cn>\r\n" +
                "      <enr_a_coeff>[soil.EnrACoeff]</enr_a_coeff>\r\n" +
                "      <enr_b_coeff>[soil.EnrBCoeff]</enr_b_coeff>\r\n" +
                "      <profile_reduction>off</profile_reduction>\r\n" +
                "[foreach soil.profile]\r\n" +
                "      <oc>[foreach profile.layer as l] [l.oc][endfor]</oc>\r\n" +
                "      <ph>$PH$</ph>\r\n" +
                "      <fbiom>[foreach profile.layer as l] [l.fbiom][endfor]</fbiom>\r\n" +
                "      <finert>[foreach profile.layer as l] [l.finert][endfor]</finert>\r\n" +
                "      <rocks>[foreach profile.layer as l] [l.rocks][endfor]</rocks>\r\n" +
                "      <ureappm>0  0    0    0    0    0    0</ureappm>\r\n" +
                "      <no3ppm>$NO3$</no3ppm>\r\n" +
                "      <nh4ppm>$NH4$</nh4ppm>\r\n" +
                "[endfor]\r\n" +
                "   </initdata>\r\n" +
                "</component>\r\n" +
                "[if [soil.ResidueCP] > 0]\r\n" +
                "<component name=\"[soil.name] Phosphorus\" executable=\"%apsuite\\apsim\\soilp\\lib\\soilp.dll\">\r\n" +
                "   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soilp\\soilp.ini</include>\r\n" +
                "[foreach soil.profile]\r\n" +
                "      <Labile_P>[foreach profile.layer as l] [l.LabileP][endfor]</Labile_P>\r\n" +
                "      <banded_P>[foreach profile.layer as l] [l.bandedP][endfor]</banded_P>\r\n" +
                "      <rock_P>[foreach profile.layer as l] [l.rockP][endfor]</rock_P>\r\n" +
                "      <sorption>[foreach profile.layer as l] [l.sorption][endfor]</sorption>\r\n" +
                "[endfor]\r\n" +
                "      <Residue_CP>[soil.ResidueCP]</Residue_CP>\r\n" +
                "      <Root_CP>[soil.RootCP]</Root_CP>\r\n" +
                "      <rate_dissol_rock_P>[soil.RateDissolRock]</rate_dissol_rock_P>\r\n" +
                "   </initdata>\r\n" +
                "</component>\r\n" +
                "[endif]\r\n";

            string errors = CheckForErrors();
            if (errors != "")
                throw new Exception(errors);

			string SWLine = "";
			string NO3Line = "";
			string NH4Line = "";
            string PHLine = "";
			double[] sw = InitialWater.SW;
			double[] no3 = InitialNitrogen.NO3;
			double[] nh4 = InitialNitrogen.NH4;
            double[] ph = PH;

			int NumLayers = Thickness.Length;
			for (int i = 0; i != NumLayers; i++)
				{
				SWLine += "    " + sw[i].ToString("f3");
				NO3Line += "      " + no3[i].ToString("f3");
				NH4Line += "      " + nh4[i].ToString("f3");
                PHLine += "      " + ph[i].ToString("f3");
				}

			Template = Template.Replace("$SW$", SWLine);
			Template = Template.Replace("$NO3$", NO3Line);
			Template = Template.Replace("$NH4$", NH4Line);
            Template = Template.Replace("$PH$", PHLine);

            Macro SoilMacro = new Macro();
            string Contents = SoilMacro.Go(Data, Template);
            Contents = Contents.Replace("<soiltype>[soil.soiltype]</soiltype>", "");
			Out.Write(Contents);
			}

		public void ExportCropToSim(TextWriter Out, string CropName)
			{
			string Template =
				"<ll>$LL$</ll>\r\n" +
				"<kl>$KL$</kl>\r\n" +
				"<xf>$XF$</xf>";

			string LLLine = "";
			string KLLine = "";
			string XFLine = "";
			double[] ll = LL(CropName);
			double[] kl = KL(CropName);
			double[] xf = XF(CropName);

			for (int i = 0; i != ll.Length; i++)
				{
				LLLine += "    " + ll[i].ToString("f3");
				KLLine += "      " + kl[i].ToString("f3");
				XFLine += "      " + xf[i].ToString("f3");
				}
            if (LLLine == "" || KLLine == "" || XFLine == "")
                throw new Exception("Soil is not parameterised for crop: " + CropName);
			Template = Template.Replace("$LL$", LLLine);
			Template = Template.Replace("$KL$", KLLine);
			Template = Template.Replace("$XF$", XFLine);
            Out.Write(Template);
			}


		// ------------------------
		// Initial water property
		// ------------------------
		public InitWater InitialWater
			{
			get {
				if (Data.Child("InitWater") == null)
                    Data.Add(new APSIMData("<InitWater><PercentMethod><Percent>100</Percent><Distributed>filled from top</Distributed></PercentMethod></InitWater>"));
				return new InitWater(this);
				}
			}


		// ------------------------
		// Initial water property
		// ------------------------
		public InitNitrogen InitialNitrogen
			{
			get {
				if (Data.Child("InitNitrogen") == null)
					Data.Add(new APSIMData("InitNitrogen", ""));
				return new InitNitrogen(this);
				}
			}


		// -----------------------------------
		// Convert old soil file to new format
		// -----------------------------------
		public void UpgradeToVersion2()
			{
			if (Data.Child("InitWater") == null)
				{
				Data.Add(new APSIMData("InitWater", ""));
				double[] OldSW = getLayered("profile", "sw", "");
				if (OldSW != null && OldSW.Length > 0)
					InitialWater.SetUsingLayered(OldSW);
				}

			if (Data.Child("InitNitrogen") == null)
				{
				Data.Add(new APSIMData("InitNitrogen", ""));
				double[] OldNO3 = getLayered("profile", "no3", "");
				if (OldNO3 != null && OldNO3.Length > 0)
					InitialNitrogen.NO3 = OldNO3;
				double[] OldNH4 = getLayered("profile", "nh4", "");
				if (OldNH4 != null && OldNH4.Length > 0)
					InitialNitrogen.NH4 = OldNH4;
				}
            setLayered("profile", "sw", "", new double[0]);
            setLayered("profile", "no3", "", new double[0]);
            setLayered("profile", "nh4", "", new double[0]);

			// Look for missing crop names.
			foreach (string Crop in Crops)
				{
				if (Crop.ToLower() == "soilcrop")
					DeleteCrop("soilcrop");
				}
			}

		// -----------------------------------
		// Convert old soil file to new format
		// -----------------------------------
		public void UpgradeToVersion3()
			{
			}

        // -------------------------------------
        // Convert old soil file to new format 7
        // -------------------------------------
        public void UpgradeToVersion7()
            {
            APSIMData Result = new APSIMData("soil", Data.Name);
            foreach (APSIMData Child in Data.get_Children(null))
                {
                if (Child.Type.ToLower() == "water" || 
                    Child.Type.ToLower() == "nitrogen" ||
                    Child.Type.ToLower() == "other" ||
                    Child.Type.ToLower() == "soilcrop")
                    UpgradeToNodeVersion7(Child, Result);
                else
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
                        if (Convert.ToDouble(Value.Value) != MathUtility.MissingValue)
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


		//-------------------------------------------------------------------------
		// This checks the soil for errors and returns an error message if a
		// problem was found. A blank string returned indicates no problems.
		//-------------------------------------------------------------------------
		public string CheckForErrors()
			{
			string ErrorMessages = "";
			ErrorMessages += CheckForMissing(this.Thickness, "THICKNESS");
			ErrorMessages += CheckForMissing(this.Airdry, "AIRDRY");
			ErrorMessages += CheckForMissing(this.DUL, "DUL");
			ErrorMessages += CheckForMissing(this.SAT, "SAT");
			ErrorMessages += CheckForMissing(this.SWCON, "SWCON");
			ErrorMessages += CheckForMissing(this.InitialWater.SW, "SW");
			ErrorMessages += CheckForMissing(this.BD, "BD");
			ErrorMessages += CheckForMissing(this.OC, "OC");
			ErrorMessages += CheckForMissing(this.PH, "PH");
			ErrorMessages += CheckForMissing(this.FBIOM, "FBIOM");
			ErrorMessages += CheckForMissing(this.FINERT, "FINERT");
			ErrorMessages += CheckForMissing(this.InitialNitrogen.NO3, "NO3");
			ErrorMessages += CheckForMissing(this.InitialNitrogen.NH4, "NH4");
			ErrorMessages += CheckForMissing(this.Salb, "SALB");
			ErrorMessages += CheckForMissing(this.CN2Bare, "CN2BARE");
			ErrorMessages += CheckForMissing(this.CNRed, "CNRED");
			ErrorMessages += CheckForMissing(this.CNCov, "CNCOV");
			ErrorMessages += CheckForMissing(this.DiffusConst, "DIFFUSCONST");
			ErrorMessages += CheckForMissing(this.DiffusSlope, "DIFFUSSLOPE");
			ErrorMessages += CheckForMissing(this.RootCN, "ROOTCN");
			ErrorMessages += CheckForMissing(this.RootWT, "ROOTWT");
			ErrorMessages += CheckForMissing(this.SoilCN, "SOILCN");
			ErrorMessages += CheckForMissing(this.EnrACoeff, "ENRACOEFF");
			ErrorMessages += CheckForMissing(this.EnrBCoeff, "ENRBCOEFF");

			foreach (string Crop in this.Crops)
				{
				ErrorMessages += CheckForMissing(this.LL(Crop), "LL-" + Crop);
				if (Crop.ToLower() != "ozcot")
					{
					ErrorMessages += CheckForMissing(this.KL(Crop), "KL-" + Crop);
					ErrorMessages += CheckForMissing(this.XF(Crop), "XF-" + Crop);
					}
				}

			// Do some more rigorous checks.
			if (ErrorMessages == "")
				ErrorMessages += CheckProfile();

			return ErrorMessages;
			}


		// -----------------------------------------------------
		// Checks the specified array for missing values.
		// Returns an error message if a problem was found.
		// -----------------------------------------------------
		private string CheckForMissing(double[] Values, string PropertyName)
			{
			bool AllMissing = true;
			for (int i = 0; i != Values.Length; i++)
				AllMissing = (AllMissing && Values[i] == MathUtility.MissingValue);
			if (AllMissing)
				return "There are no values for " + PropertyName + "\r\n";

			for (int i = 0; i != Values.Length; i++)
				{
				int RealLayerNumber = i + 1;
				if (Values[i] == MathUtility.MissingValue)
					return "A missing value was found in layer " + RealLayerNumber.ToString() + " for " + PropertyName + "\r\n";
				}
			return "";
			}


		// -----------------------------------------------------
		// Checks the specified value for missing number.
		// Returns an error message if a problem was found.
		// -----------------------------------------------------
		private string CheckForMissing(double Value, string PropertyName)
			{
			if (Value == MathUtility.MissingValue)
				return "Missing a value for " + PropertyName + "\r\n";
			return "";
			}


		// ------------------------------------------------------------------
		// Checks validity of soil water parameters for a soil profile layer
		// This is a port of the soilwat2_check_profile routine.
		// ------------------------------------------------------------------
		private string CheckProfile()
			{
			string errorMessages = "";
			const double min_sw = 0.0;
			const double specific_bd =  2.65; // (g/cc)

			double[] thickness = this.Thickness;
			double[] airdry = this.Airdry;
			double[] ll15 = this.LL15;
			double[] dul = this.DUL;
			double[] sat = this.SAT;
			double[] bd = this.BD;
			double[] sw = this.InitialWater.SW;

			for (int layer = 0; layer != thickness.Length; layer++)
				{
				double max_sw = 1.0 - bd[layer] / specific_bd;
				int RealLayerNumber = layer + 1;

				if (airdry[layer] < min_sw)
					errorMessages += " Air dry lower limit of " + airdry[layer].ToString("f3")
                                  +  " in layer " + RealLayerNumber.ToString() + " is below acceptable value of " + min_sw.ToString("f3")
								  + "\r\n";

				if (ll15[layer] < airdry[layer])
					errorMessages += "15 bar lower limit of " + ll15[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry[layer].ToString("f3")
								  + "\r\n";

				foreach (string Crop in Crops)
					{
					double[] ll = LL(Crop);
					double[] kl = KL(Crop);
					double[] xf = XF(Crop);

					if (kl[layer] > 1)
						errorMessages += "KL value of " + kl[layer].ToString("f3")
									+  " in layer " + RealLayerNumber.ToString() + " is greater than 1"
									+ "\r\n";

					if (xf[layer] > 1)
						errorMessages += "XF value of " + xf[layer].ToString("f3")
									+  " in layer " + RealLayerNumber.ToString() + " is greater than 1"
									+ "\r\n";

					if (ll[layer] < airdry[layer])
						errorMessages += "Crop lower limit of " + ll[layer].ToString("f3")
									  + " for crop " + Crop
							          +  " in layer " + RealLayerNumber.ToString() + " is below air dry value of " + airdry[layer].ToString("f3")
									  + "\r\n";

					if (ll[layer] > DUL[layer])
						errorMessages += "Crop lower limit of " + ll[layer].ToString("f3")
									  + " for crop " + Crop
							          +  " in layer " + RealLayerNumber.ToString() + " is above drained upper limit of " + dul[layer].ToString("f3")
									  + "\r\n";

					}

				if (dul[layer] < ll15[layer])
					errorMessages += "Drained upper limit of " + dul[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is at or below lower limit of " + ll15[layer].ToString("f3")
								  + "\r\n";

				if (sat[layer] < dul[layer])
					errorMessages += "Saturation of " + sat[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is at or below drained upper limit of " + dul[layer].ToString("f3")
								  + "\r\n";

				if (sat[layer] > max_sw)
					{
					double max_bd = (1.0 - sat[layer]) * specific_bd;
					errorMessages += "Saturation of " + sat[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is above acceptable value of  " + max_sw.ToString("f3")
								  +  ". You must adjust bulk density to below " + max_bd.ToString("f3")
								  +  " OR saturation to below " + max_sw.ToString("f3")
								  + "\r\n";
					}

				if (sw[layer] > sat[layer])
					errorMessages += "Soil water of " + sw[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is above saturation of " + sat[layer].ToString("f3")
								  + "\r\n";

				if (sw[layer] < airdry[layer])
					errorMessages += "Soil water of " + sw[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is below air-dry value of " + airdry[layer].ToString("f3")
								  + "\r\n";

				if (bd[layer] > 2.65)
					errorMessages += "BD value of " + bd[layer].ToString("f3")
						          +  " in layer " + RealLayerNumber.ToString() + " is greater than the theoretical maximum of 2.65"
								  + "\r\n";
                if (OC[layer] < 0.01)
                    errorMessages += "OC value of " + OC[layer].ToString("f3")
                                  +  " in layer " + RealLayerNumber.ToString() + " is less than 0.01"
                                  + "\r\n";
                if (PH[layer] < 3.5)
                    errorMessages += "PH value of " + PH[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is less than 3.5"
                                  + "\r\n";
                if (PH[layer] > 11)
                    errorMessages += "PH value of " + PH[layer].ToString("f3")
                                  + " in layer " + RealLayerNumber.ToString() + " is greater than 11"
                                  + "\r\n";
				}
			return errorMessages;
			}

		// -------------------------------------------------
		// Using the soil's EC values - create an XF profile
		// -------------------------------------------------
		public void ApplyECXFFunction(string CropName)
			{
			if (EC.Length > 0)
				{
				double[] xf = new double[Thickness.Length];
				for (int i = 0; i != this.CumThickness.Length; i++)
					{
					if (EC[i] <= 0.68)
						xf[i] = 1.0;
					else
						{
						xf[i] = 2.06 / (1 + 2 * EC[i]) - 0.351;
						xf[i] = CSGeneral.MathUtility.Constrain(0.0, 1.0, xf[i]);
						}
					}

				double[] ll = LL(CropName);
				double[] kl = KL(CropName);

				SetCrop(CropName, ll, kl, xf);
				}
			}


		// -------------------------------------------------
		// Using the soil's EC values - create an XF profile
		// -------------------------------------------------
		public void ApplyMaxRootDepth(string CropName, int RootingDepth)
			{
			double[] xf = XF(CropName);
			if (xf.Length > 0)
				{
				for (int i = 0; i != this.CumThickness.Length; i++)
					{
					if (CumThickness[i] > RootingDepth)
						{
						double PreviousCumThickness = 0.0;
						if(i > 0)
							PreviousCumThickness = CumThickness[i-1];

						if (PreviousCumThickness > RootingDepth)
							xf[i] = 0.0;
						else
							{
							double Proportion = (RootingDepth - PreviousCumThickness) / Thickness[i];
							xf[i] = xf[i] * Proportion;
							xf[i] = CSGeneral.MathUtility.Constrain(0.0, 1.0, xf[i]);
							}
						}
					}

				double[] ll = LL(CropName);
				double[] kl = KL(CropName);

				SetCrop(CropName, ll, kl, xf);
				}
			}
        
        //---------------------------------------------------
        // Adjust the DUL curve, given a max water Capacity
        // start from bottom most layer that has water in it and reduced to zero or until
        // the whole profile equals the given max water capacity
        //---------------------------------------------------
        public void ApplyMaxWaterCapacity(int maxWaterCapacity)
            {
            double[] localDUL = DUL;
            double[] localLL15 = LL15;
            double[] localThickness = Thickness;
            //find lowest layer that has water
            int bottomLayer;
            int layer;
            for (layer = 0; layer < localDUL.Length; layer++)
                {
                double test=((localDUL[layer]* Thickness[layer])-(localLL15[layer]*Thickness[layer]));
                if (((localDUL[layer]* Thickness[layer])-(localLL15[layer]*Thickness[layer])) == 0) break;
                }
            bottomLayer=layer-1;
            double currentCapacitySum = MathUtility.Sum(PAWC());
            double capacityDifference =  maxWaterCapacity-currentCapacitySum ;
            if (!(capacityDifference == 0))
                {
                if (capacityDifference > 0)
                    {
                    double newThickness = localThickness[bottomLayer] + (((Math.Abs(capacityDifference)) / (localDUL[bottomLayer] - localLL15[bottomLayer])));
                    localThickness[bottomLayer] = newThickness;
                    }
                else
                    {
                    for (int j = bottomLayer; j >= 0; j--)
                        {
                        double waterInLayer=((localThickness[j]) * (localDUL[j] - localLL15[j]));
                        if ((Math.Abs(capacityDifference) > waterInLayer))
                            {
                            capacityDifference = (Math.Abs(capacityDifference) - (localThickness[j]) * (localDUL[j] - localLL15[j]));
                            localThickness[j] = 0;
                            }
                        else
                            {
                            double newThickness = ((Math.Abs(capacityDifference)) / (localDUL[j] - localLL15[j]));
                            localThickness[j] = localThickness[j]-newThickness;
                            break;
                            }
                        }
                    }
                }
            Thickness=localThickness;
            }

        // -------------------------------------------------
        // Adjust the DUL curve, given a max soil depth(cutoff)
        // Leave the DUL number for all whole layers above the cutoff layer
        // Work out the proportional DUL number for the layer that has the cutoff within it
        // Set the LL15 to the DUL for the whole layers below the cutoff
        // -------------------------------------------------
        public void ApplyMaxSoilDepth(int soilDepth)
            {
            double[] localDUL = DUL;
            double[] localLL15 = LL15;
            for (int i = 0; i != this.CumThickness.Length; i++)
                {
                if (CumThickness[i] > soilDepth)
                    {
                    double PreviousCumThickness = 0.0;
                    if (i > 0)
                        PreviousCumThickness = CumThickness[i - 1];

                    if (PreviousCumThickness > soilDepth)
                        {
                        localLL15[i]=localDUL[i];
                        }
                    else
                        {
                        double Proportion = (soilDepth - PreviousCumThickness) / Thickness[i];
                        localDUL[i] = (LL15[i]+((localDUL[i]-LL15[i]) * Proportion));
                        }
                    }
                }
            DUL = localDUL;
            LL15 = localLL15;
            }

		// ----------------------
		// Methods for cell notes
		// ----------------------
		public void DeleteNote(string GridName, int Col, int Row)
			{
			string NoteNameToDelete = "";
			foreach (APSIMData Note in Data.get_Children("note"))
				{
				if (Note.get_ChildValue("grid") == GridName &&
					Convert.ToInt32(Note.get_ChildValue("col")) == Col &&
					Convert.ToInt32(Note.get_ChildValue("row")) == Row)
					NoteNameToDelete = Note.Name;
				}
			if (NoteNameToDelete != "")
				Data.Delete(NoteNameToDelete);
			}

		public struct Note
			{
			public string GridName;
			public int Col, Row;
			public string Text;
			};

		public void AddNote(string GridName, int Col, int Row, string Text)
			{
			APSIMData Note = new APSIMData("note", "note");
			Note.set_ChildValue("grid", GridName);
			Note.set_ChildValue("col", Col.ToString());
			Note.set_ChildValue("row", Row.ToString());
			Note.set_ChildValue("text", Text);
			Data.Add(Note);
			}

		public Note[] GetNotes()
			{
			Note[] ReturnList = new Note[Data.get_Children("note").Length];
			int i = 0;
			foreach (APSIMData NoteNode in Data.get_Children("note"))
				{
				ReturnList[i].GridName = NoteNode.get_ChildValue("grid");
				ReturnList[i].Col = Convert.ToInt32(NoteNode.get_ChildValue("col"));
				ReturnList[i].Row = Convert.ToInt32(NoteNode.get_ChildValue("row"));
				ReturnList[i].Text = NoteNode.get_ChildValue("text");

				i++;
				}
			return ReturnList;
			}



        internal void AddLayerToBottom()
            {
            // ----------------------------------
            // Add another layer to the profile.
            // ----------------------------------
            APSIMData Profile = Data.Child("profile");
            if (Profile != null)
                {
                APSIMData[] Layers = Profile.get_Children("layer");

                Profile.Add(Layers[Thickness.Length - 1]);

                // we need InitWater and InitNitrogen to make themselves valid as we've
                // changed the number of layers.
                if (Data.Child("initwater") != null)
                    {
                    InitWater Water = new InitWater(this);
                    Water.ValidateAgainstLayerStructure();
                    }
                if (Data.Child("initnitrogen") != null)
                    {
                    InitNitrogen Nitrogen = new InitNitrogen(this);
                    Nitrogen.ValidateAgainstLayerStructure();
                    }
                }
            }

        internal void DeleteLayerFromBottom()
            {
            // ----------------------------------
            // Add another layer to the profile.
            // ----------------------------------
            APSIMData Profile = Data.Child("profile");
            if (Profile != null)
                {
                APSIMData[] Layers = Profile.get_Children("layer");

                Profile.DeleteNode(Layers[Thickness.Length - 1]);

                // we need InitWater and InitNitrogen to make themselves valid as we've
                // changed the number of layers.
                if (Data.Child("initwater") != null)
                    {
                    InitWater Water = new InitWater(this);
                    Water.ValidateAgainstLayerStructure();
                    }
                if (Data.Child("initnitrogen") != null)
                    {
                    InitNitrogen Nitrogen = new InitNitrogen(this);
                    Nitrogen.ValidateAgainstLayerStructure();
                    }
                }
            }
        }
	}
