using System;
using System.Collections.Specialized;
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
		public Soil(APSIMData data)	: base(data) {}

		// -----------------------------------
		// General soil descriptive properties
		// -----------------------------------
		public string Region
			{
			get {return GetStringValue("", "region");}
			set {SetValue("", "region", value);}
			}

		public string Site
			{
			get {return GetStringValue("", "site");}
			set {SetValue("", "site", value);}
			}

		public string Name
			{
			get {return Data.Name;}
			set {Data.Name = value;}
			}
		public string Order
			{
			get {return GetStringValue("", "order");}
			set {SetValue("", "order", value);}
			}
		public string NearestTown
			{
			get {return GetStringValue("", "nearesttown");}
			set {SetValue("", "nearesttown", value);}
			}
		public string Comment
			{
			get {return GetStringValue("", "comment");}
			set {SetValue("", "comment", value);}
			}
		public string GPS
			{
			get {return GetStringValue("", "gps");}
			set {SetValue("", "gps", value);}
			}
		public string GPSDatum
			{
			get {return GetStringValue("", "gpsdatum");}
			set {SetValue("", "gpsdatum", value);}
			}
		public string MapId
			{
			get {return GetStringValue("", "mapid");}
			set {SetValue("", "mapid", value);}
			}

		public string NaturalVegetation
			{
			get {return GetStringValue("", "naturalvegetation");}
			set {SetValue("", "naturalvegetation", value);}
			}

		// ---------------------------------------
		// Methods to convert to from gravimetric
		// and volumetric percent.
		// ---------------------------------------
		private double[] GetLayeredAsVol(string PropertyType, string PropertyName)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				return getLayered(PropertyType, PropertyName);
			else
				return MathUtility.Multiply(getLayered(PropertyType, PropertyName),
											BD);
			}
		private double[] GetLayeredAsGrav(string PropertyType, string PropertyName)
			{
			if (StoredWaterFormat == StoredWaterFormatType.VolumetricPercent)
				return MathUtility.Divide(getLayered(PropertyType, PropertyName),
											BD);
			else
				return getLayered(PropertyType, PropertyName);
			}

		// ------------------------------------
		// Layered soil properties.
		// The water methods always return vol%
		// unless otherwise named.
		// ------------------------------------
		public enum StoredWaterFormatType {VolumetricPercent, GravimetricPercent};
		public StoredWaterFormatType StoredWaterFormat
			{
			get {
				if (Data.get_ChildValue("WaterFormat") == "GravimetricPercent")
					return StoredWaterFormatType.GravimetricPercent;
				else
                    return StoredWaterFormatType.VolumetricPercent;
				}
			}
		private void SetStoredWaterFormat(StoredWaterFormatType WaterFormat)
			{
			if (WaterFormat == StoredWaterFormatType.GravimetricPercent)
				Data.set_ChildValue("WaterFormat", "GravimetricPercent");
			else
				Data.set_ChildValue("WaterFormat", "VolumetricPercent");
			}
		public double[] LL15
			{
			get {return GetLayeredAsVol("water", "ll15");}
			set {
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				setLayered("water", "ll15", value);
				}
			}
		public double[] LL15Grav
			{
			get {return GetLayeredAsGrav("water", "ll15");}
			set {
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				setLayered("water", "ll15", value);
				}
			}
		public double[] Airdry
			{
			get {return GetLayeredAsVol("water", "airdry");}
			set {
				setLayered("water", "airdry", value);
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				}
			}
		public double[] AirdryGrav
			{
			get {return GetLayeredAsGrav("water", "airdry");}
			set {
				setLayered("water", "airdry", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				}
			}
		public double[] DUL
			{
			get {return GetLayeredAsVol("Water", "dul");}
			set {
				setLayered("Water", "dul", value);
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				}
			}
		public double[] DULGrav
			{
			get {return GetLayeredAsGrav("Water", "dul");}
			set {
				setLayered("Water", "dul", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				}
			}
		public double[] SAT
			{
			get {return GetLayeredAsVol("water", "sat");}
			set {
				setLayered("Water", "sat", value);
				SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
				}
			}
		public double[] SATGrav
			{
			get {return GetLayeredAsGrav("water", "sat");}
			set {
				setLayered("Water", "sat", value);
				SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
				}
			}
		public double[] BD
			{
			get {return getLayered("Water", "bd");}
			set {setLayered("Water", "bd", value);}
			}
		public double[] SWCON
			{
			get {return getLayered("Water", "swcon");}
			set {setLayered("Water", "swcon", value);}
			}
		public double[] MWCON
			{
			get {return getLayered("Water", "mwcon");}
			set {setLayered("Water", "mwcon", value);}
			}
		public double[] FBIOM
			{
			get {return getLayered("nitrogen", "fbiom");}
			set {setLayered("nitrogen", "fbiom", value);}
			}
		public double[] FINERT
			{
			get {return getLayered("nitrogen", "finert");}
			set {setLayered("nitrogen", "finert", value);}
			}
		public double[] OC
			{
			get {return getLayered("nitrogen", "oc");}
			set {setLayered("nitrogen", "oc", value);}
			}
		public double[] PH
			{
			get {return getLayered("nitrogen", "ph");}
			set {setLayered("nitrogen", "ph", value);}
			}
		public double[] EC
			{
			get {return getLayered("other", "ec");}
			set {setLayered("other", "ec", value);}
			}
		public double[] CL
			{
			get {return getLayered("other", "cl");}
			set {setLayered("other", "cl", value);}
			}
		public double[] CEC
			{
			get {return getLayered("other", "cec");}
			set {setLayered("other", "cec", value);}
			}
		public double[] Ca
			{
			get {return getLayered("other", "ca");}
			set {setLayered("other", "ca", value);}
			}
		public double[] Mg
			{
			get {return getLayered("other", "Mg");}
			set {setLayered("other", "Mg", value);}
			}
		public double[] Na
			{
			get {return getLayered("other", "Na");}
			set {setLayered("other", "Na", value);}
			}
		public double[] K
			{
			get {return getLayered("other", "K");}
			set {setLayered("other", "K", value);}
			}
		public double[] ESP
			{
			get {return getLayered("other", "esp");}
			set {setLayered("other", "esp", value);}
			}
		public double[] ParticleSizeSand
			{
			get {return getLayered("other", "ParticleSizeSand");}
			set {setLayered("other", "ParticleSizeSand", value);}
			}
		public double[] ParticleSizeSilt
			{
			get {return getLayered("other", "ParticleSizeSilt");}
			set {setLayered("other", "ParticleSizeSilt", value);}
			}
		public double[] ParticleSizeClay
			{
			get {return getLayered("other", "ParticleSizeClay");}
			set {setLayered("other", "ParticleSizeClay", value);}
			}
		public double[] LabileP
			{
			get {return getLayered("Phosphorus", "LabileP");}
			set {setLayered("Phosphorus", "LabileP", value);}
			}
		public double[] BandedP
			{
			get {return getLayered("Phosphorus", "BandedP");}
			set {setLayered("Phosphorus", "BandedP", value);}
			}
		public double[] RockP
			{
			get {return getLayered("Phosphorus", "RockP");}
			set {setLayered("Phosphorus", "RockP", value);}
			}
		public double[] Sorption
			{
			get {return getLayered("Phosphorus", "Sorption");}
			set {setLayered("Phosphorus", "Sorption", value);}
			}


		// ------------------------------------------------------
		// Crop properties
		// ------------------------------------------------------
		public string[] Crops
			{
			get {
				StringCollection ReturnListCollection = Data.ChildList("SoilCrop");
				StringCollection PredCrops = PredictedCrops;
				string[] ReturnList = new string[ReturnListCollection.Count + PredCrops.Count];
				ReturnListCollection.CopyTo(ReturnList, 0);
				PredCrops.CopyTo(ReturnList, ReturnListCollection.Count);
				return ReturnList;
				}
			}
		public bool CropExists(string CropName)
			{
            return (Data.Child(CropName) != null);
			}
		public bool CropIsPredicted(string CropName)
			{
			return !CropExists(CropName);
			}
		public void AddCrop(string CropName)
			{
			Data.Add(new APSIMData("SoilCrop", CropName));
			}
		public void DeleteCrop(string CropName)
			{
			Data.Delete(CropName);
			}
		public double[] LL(string CropName)
			{
			if (CropExists(CropName))
				return GetLayeredAsVol(CropName, "ll");
			else
				return PredictedLL(CropName);
			}
		public double[] LLGrav(string CropName)
			{
			if (CropExists(CropName))
				return GetLayeredAsGrav(CropName, "ll");
			else
				return MathUtility.Divide(PredictedLL(CropName), BD);
			}
		public double[] KL(string CropName)
			{
			if (CropExists(CropName))
				return getLayered(CropName, "kl");
			else
				return PredictedLL(CropName);
			}
		public double[] XF(string CropName)
			{
			if (CropExists(CropName))
				return getLayered(CropName, "xf");
			else
				return PredictedXF(CropName);
			}
		public void SetCrop(string CropName, double[] ll, double[] kl, double[] xf)
			{
			SetStoredWaterFormat(StoredWaterFormatType.VolumetricPercent);
			setLayered("SoilCrop", CropName, "ll", ll);
			setLayered("SoilCrop", CropName, "kl", kl);
			setLayered("SoilCrop", CropName, "xf", xf);
			}
		public void SetCropGrav(string CropName, double[] ll, double[] kl, double[] xf)
			{
			SetCrop(CropName, ll, kl, xf);
			SetStoredWaterFormat(StoredWaterFormatType.GravimetricPercent);
			}
		public void SetCropOrder(string[] CropNames)
			{
			for (int DestIndex = 0; DestIndex != CropNames.Length-1; DestIndex++)
				{
                int SourceIndex = -1;
				string [] CurrentCrops = Crops;
				for (int c = 0; c != CurrentCrops.Length; c++)
					if (CurrentCrops[c].ToLower() == CropNames[DestIndex].ToLower())
						SourceIndex = c;

				if (SourceIndex == -1)
					throw new Exception("Cannot find crop: " + CropNames[DestIndex] + " in method SetCropOrder");

				for (int i = SourceIndex; i > DestIndex; i--)
					Data.MoveUp(CropNames[DestIndex], "soilcrop");
				}
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


		private StringCollection PredictedCrops
			{	
			get {
				StringCollection PredCrops = new StringCollection();
				if (Data.ChildList("SoilCrop").Count > 0 && OpenPredLLCoeffFile())
					{
					// get a list of all possible predicted crops.
					string SoilNameNoSpaces = Order.Replace(" ", "");
					foreach (APSIMData PredSoil in  PredLLCoeff.get_Children(null))
						if (PredSoil.Name.ToLower() == SoilNameNoSpaces.ToLower())
							foreach (string Crop in PredSoil.ChildList(null))
								if (!CropExists(Crop))
									PredCrops.Add(Crop);
					}
				return PredCrops;
				}
			}

		private double[] PredictedLL(string CropName)
			{
			if (Crops.Length > 0 && OpenPredLLCoeffFile())
				{
				// Get all coefficients and convert to double arrays.
				StringCollection AStrings = new StringCollection();
				StringCollection BStrings = new StringCollection();
				StringCollection LayerCentreStrings = new StringCollection();
				string SoilNameNoSpaces = Order.Replace(" ", "");
				foreach (APSIMData Layer in PredLLCoeff.Child(SoilNameNoSpaces).Child(CropName).get_Children("layer"))
					{
					AStrings.Add(Layer.get_ChildValue("a"));
					BStrings.Add(Layer.get_ChildValue("b"));
					LayerCentreStrings.Add(Layer.get_ChildValue("LayerCentre"));
					}

				if (AStrings.Count == 0 || AStrings.Count != BStrings.Count || AStrings.Count != LayerCentreStrings.Count)
					throw new Exception("Invalid predicted LL coeffs found for soil: " + Order + " and crop: " + CropName);
				double[] a = new double[AStrings.Count];
				double[] b = new double[BStrings.Count];
				double[] CoeffDepthCentre = new double[LayerCentreStrings.Count];
				for (int i = 0; i != AStrings.Count; i++)
					{
					a[i] = Convert.ToDouble(AStrings[i]);
					b[i] = Convert.ToDouble(BStrings[i]);
					CoeffDepthCentre[i] = Convert.ToDouble(LayerCentreStrings[i]);
					}

				// Get some soil numbers we're going to need.
				double[] SoilDepthCentre = this.CumThicknessMidPoints;
				double[] SoilDUL = this.DUL;
				double[] FirstCropLL = LL(Crops[0]);

				// only continue if our soil depth depth centers are within range of
				// the coefficient depth centers.
				if (SoilDepthCentre[SoilDepthCentre.Length-1] <= CoeffDepthCentre[CoeffDepthCentre.Length-1])
					{
					double[] PredLL = new double[SoilDepthCentre.Length];
					for (int i = 0; i != a.Length; i++)
						{
						bool DidInterpolate = false;
						double A = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, a, ref DidInterpolate);
						double B = MathUtility.LinearInterpReal(SoilDepthCentre[i], CoeffDepthCentre, b, ref DidInterpolate);
						PredLL[i] = SoilDUL[i] * (A + B * SoilDUL[i]);

						// make the top 2 layers the same as the first measured crop LL
						if (i == 0 || i == 1)
							PredLL[i] = FirstCropLL[i];
						}
					return PredLL;
					}
				}
			return new double[0];
			}

		private double[] PredictedKL(string CropName)
			{
			if (Crops.Length > 0 && OpenPredLLCoeffFile())
				return KL(Crops[0]);
			else
				return new double[0];
			}

		private double[] PredictedXF(string CropName)
			{
			if (Crops.Length > 0 && OpenPredLLCoeffFile())
				return XF(Crops[0]);
			else
				return new double[0];
			}

		// -----------------------------------
		// APSIM soil properties
		// -----------------------------------
		public double U
			{
			get {return GetDoubleValue("Water", "U");}
			set {SetValue("Water", "U", value);}
			}
		public double Cona
			{
			get {return GetDoubleValue("Water", "Cona");}
			set {SetValue("Water", "Cona", value);}
			}
		public double Salb
			{
			get {return GetDoubleValue("Water", "Salb");}
			set {SetValue("Water", "Salb", value);}
			}
		public double DiffusConst
			{
			get {return GetDoubleValue("Water", "DiffusConst");}
			set {SetValue("Water", "DiffusConst", value);}
			}
		public double DiffusSlope
			{
			get {return GetDoubleValue("Water", "DiffusSlope");}
			set {SetValue("Water", "DiffusSlope", value);}
			}
		public double CN2Bare
			{
			get {return GetDoubleValue("Water", "CN2Bare");}
			set {SetValue("Water", "CN2Bare", value);}
			}
		public double CNRed
			{
			get {return GetDoubleValue("Water", "CNRed");}
			set {SetValue("Water", "CNRed", value);}
			}
		public double CNCov
			{
			get {return GetDoubleValue("Water", "CNCov");}
			set {SetValue("Water", "CNCov", value);}
			}
		public double RootCN
			{
			get {return GetDoubleValue("Nitrogen", "RootCN");}
			set {SetValue("Nitrogen", "RootCN", value);}
			}
		public double RootWT
			{
			get {return GetDoubleValue("Nitrogen", "RootWT");}
			set {SetValue("Nitrogen", "RootWT", value);}
			}
		public double SoilCN
			{
			get {return GetDoubleValue("Nitrogen", "SoilCN");}
			set {SetValue("Nitrogen", "SoilCN", value);}
			}
		public double EnrACoeff
			{
			get {return GetDoubleValue("Nitrogen", "EnrACoeff");}
			set {SetValue("Nitrogen", "EnrACoeff", value);}
			}
		public double EnrBCoeff
			{
			get {return GetDoubleValue("Nitrogen", "EnrBCoeff");}
			set {SetValue("Nitrogen", "EnrBCoeff", value);}
			}
		public double ResidueCP
			{
			get {return GetDoubleValue("Phosphorus", "ResidueCP");}
			set {SetValue("Phosphorus", "ResidueCP", value);}
			}
		public double RootCP
			{
			get {return GetDoubleValue("Phosphorus", "RootCP");}
			set {SetValue("Phosphorus", "RootCP", value);}
			}
		public double RateDissolRock
			{
			get {return GetDoubleValue("Phosphorus", "RateDissolRock");}
			set {SetValue("Phosphorus", "RateDissolRock", value);}
			}


		// ------------------------------------------------------------------
		// return plant available water content by layer (mm)
		// ------------------------------------------------------------------
		public double[] PAWC(string CropName)
			{
			return PAWC(this.LL(CropName));
			}
		// ------------------------------------------------------------------
		// return plant available water content by layer (mm) given
		// depth, lower limit and dul all in (mm).
		// ------------------------------------------------------------------
		public double[] PAWC()
			{
			return PAWC(this.LL15);
			}
		// ------------------------------------------------------------------
		// return plant available water content by layer (mm)
		// ------------------------------------------------------------------
		private double[] PAWC(double[] LL)
			{
			double[] Thickness = this.Thickness;
			double[] DUL = this.DUL;
			double[] PAWC = new double[Thickness.Length];

			if (Thickness.Length != DUL.Length || Thickness.Length != LL.Length)
				return new double[0];

			for (int layer = 0; layer != Thickness.Length; layer++)
				if (DUL[layer] == MathUtility.MissingValue ||
					LL[layer] == MathUtility.MissingValue)
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
				"[foreach Soil.Water as water]\r\n"+
				"   diffus_const = [water.DiffusConst]    ! coeffs for unsaturated water flow\r\n"+
				"   diffus_slope = [water.DiffusSlope]\r\n"+
				"   cn2_bare     = [water.Cn2Bare]    ! bare soil runoff curve number\r\n"+
				"   cn_red       = [water.CnRed]    ! potetial reduction in curve number due to residue\r\n"+
				"   cn_cov       = [water.CnCov]   ! cover for maximum reduction in curve number\r\n"+
				"   salb         = [water.Salb]  ! bare soil albedo\r\n"+
				"   cona         = [water.Cona]   ! stage 2 evap coef.\r\n"+
				"   u            = [water.U]     ! stage 1 soil evaporation coefficient (mm)\r\n"+
				"\r\n"+
				"   dlayer  =[foreach water.layer as Layer]  [Layer.thickness.3][endfor]   ! layer thickness mm soil\r\n"+
				"   air_dry =[foreach water.layer as Layer]    [Layer.airdry.3][endfor]   ! air dry mm water/mm soil\r\n"+
				"   ll15    =[foreach water.layer as Layer]    [Layer.ll15.3][endfor]   ! lower limit mm water/mm soil\r\n"+
				"   dul     =[foreach water.layer as Layer]    [Layer.dul.3][endfor]   ! drained upper limit mm water/mm soil\r\n"+
				"   sat     =[foreach water.layer as Layer]    [Layer.sat.3][endfor]   ! saturation mm water/mm soil\r\n"+
				"   sw      =$SW$   ! starting soil water mm water/mm soil\r\n"+
				"   swcon   =[foreach water.layer as Layer]    [Layer.swcon.3][endfor]   ! drainage coefficient\r\n"+
				"   bd      =[foreach water.layer as Layer]    [Layer.bd.3][endfor]   ! bulk density gm dry soil/cc moist soil\r\n"+
				"[if [water.1.mwcon] > 0]\r\n"+
				"   mwcon   =[foreach water.layer as Layer]    [Layer.mwcon.3][endfor]   \r\n\r\n"+
				"[endif]\r\n"+
				"[endfor]\r\n"+//END OF WATER FOR LOOP
				"\r\n"+
				"[foreach Soil.SoilCrop as crop]\r\n"+
				"[$SECTIONNAME$.[crop.name].parameters]\r\n"+//TITLE
				"   [if [crop.predicted] = 'yes']"+
				"   !These crop numbers are predicted\r\n"+
				"   [endif]"+
				"   ll      =[foreach crop.layer as Layer]\r\n      [Layer.ll.3][endfor]\r\n\r\n"+
				"[if [crop.name] = ozcot]\r\n"+
				"   Title = XXX\r\n"+
				"   asoil = 3.0\r\n"+
				"[else]\r\n"+
				"   kl      =[foreach crop.layer as Layer]\r\n      [Layer.kl.3][endfor]\r\n\r\n"+
				"   xf      =[foreach crop.layer as Layer]\r\n      [Layer.xf.3][endfor]\r\n\r\n"+
				"[endif]\r\n"+
				"[endfor]\r\n"+//END OF CROP FOR LOOP
				"\r\n"+
				"[$SECTIONNAME$.soiln2.parameters]\r\n"+//TITLE
				"[foreach Soil.Nitrogen as nitrogen]\r\n"+
				"   root_cn      = [nitrogen.rootcn]     ! C:N ratio of initial root residues\r\n"+
				"   root_wt      = [nitrogen.rootwt]   ! root residues as biomass (kg/ha)\r\n"+
				"   soil_cn      = [nitrogen.soilcn]   ! C:N ratio of soil\r\n"+
				"   enr_a_coeff  = [nitrogen.enracoeff]\r\n"+
				"   enr_b_coeff  = [nitrogen.enrbcoeff]\r\n"+
				"   profile_reduction =  off\r\n"+
				"\r\n"+
				"   oc      =[foreach nitrogen.layer as Layer]\r\n      [Layer.oc.3][endfor]   ! Soil Organic Carbon\r\n"+
				"   ph      =[foreach nitrogen.layer as Layer]\r\n      [Layer.ph.3][endfor]   ! pH of soil\r\n"+
				"   fbiom   =[foreach nitrogen.layer as Layer]\r\n      [Layer.fbiom.3][endfor]   ! Organic C Biomass Fraction\r\n"+
				"   finert  =[foreach nitrogen.layer as Layer]\r\n      [Layer.finert.3][endfor]   ! Inert Organic C Fraction\r\n"+
				"   no3ppm  =$NO3$   ! Nitrate Concentration\r\n"+
				"   nh4ppm  =$NH4$   ! Ammonium Concentration\r\n"+
				"[endfor]\r\n"+//END OF NITROGEN FOR LOOP
				"\r\n"+
				"[foreach Soil.Phosphorus]\r\n"+
				"[$SECTIONNAME$.soilp.parameters]\r\n"+
				"   residue_cp         =  [phosphorus.residuecp]   () !c:p ratio of residues at initialisation\r\n"+
				"   root_cp            =  [phosphorus.rootcp]      () !c:p ratio of roots at initialisation\r\n"+
				"   rate_dissol_rock_P =  [phosphorus.RateDissolRock] (/yr)   !rate at which rock P source becomes available\r\n"+
				"\r\n"+
				"   labile_P  = [foreach phosphorus.layer]    [layer.labilep.3][endfor]   (mg/kg)\r\n"+
				"   banded_P  = [foreach phosphorus.layer]    [layer.bandedP.3][endfor]   (kg/ha) ! banded p content for each layer\r\n"+
				"   rock_P    = [foreach phosphorus.layer]    [layer.rockP.3][endfor]   (kg/ha)   !rock p content for each layer ie no water soluble\r\n"+
				"   sorption  =[foreach phosphorus.layer]  [layer.sorption.3][endfor]   ()   !P sorbed at 0.2ppm\r\n"+
				"[endfor]\r\n"+
				"[endfile]\r\n\r\n";


			string SWLine = "";
			string NO3Line = "";
			string NH4Line = "";
			double[] sw = InitialWater.SW;
			double[] no3 = InitialNitrogen.NO3;
			double[] nh4 = InitialNitrogen.NH4;

			int NumLayers = Thickness.Length;
			for (int i = 0; i != NumLayers; i++)
				{
				SWLine += "    " + sw[i].ToString("f3");
				NO3Line += "      " + no3[i].ToString("f3");
				NH4Line += "      " + nh4[i].ToString("f3");
				}

			Template = Template.Replace("$SW$", SWLine);
			Template = Template.Replace("$NO3$", NO3Line);
			Template = Template.Replace("$NH4$", NH4Line);
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
				"[foreach soil.water]\r\n" +
				"<component name=\"[soil.name] Water\" executable=\"%apsuite\\apsim\\soilwat2\\lib\\soilwat2.dll\">\r\n" +
				"   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soilwat2\\soilwat2.ini</include>\r\n" +
				"      <diffus_const>[water.DiffusConst]</diffus_const>\r\n" +
                "      <diffus_slope>[water.DiffusSlope]</diffus_slope>\r\n" +
                "      <cn2_bare>[water.Cn2Bare]</cn2_bare>\r\n" +
                "      <cn_red>[water.CnRed]</cn_red>\r\n" +
                "      <cn_cov>[water.CnCov]</cn_cov>\r\n" +
                "      <salb>[water.Salb]</salb>\r\n" +
                "      <cona>[water.Cona]</cona>\r\n" +
                "      <u>[water.U]</u>\r\n" +
                "      <dlayer> [foreach water.layer as l][l.thickness] [endfor] </dlayer>\r\n" +
                "      <sat>[foreach water.layer as l][l.sat] [endfor]</sat>\r\n" +
                "      <dul>[foreach water.layer as l][l.dul] [endfor]</dul>\r\n" +
                "      <ll15>[foreach water.layer as l][l.ll15] [endfor]</ll15>\r\n" +
                "      <air_dry>[foreach water.layer as l][l.airdry] [endfor]</air_dry>\r\n" +
                "      <swcon>[foreach water.layer as l][l.swcon] [endfor]</swcon>\r\n" +
                "      <bd>[foreach water.layer as l][l.bd] [endfor]</bd>\r\n" +
                "      [if [water.1.mwcon] > 0]\r\n" +
                "      <mwcon>[foreach water.layer as l][l.mwcon] [endfor]</mwcon>\r\n" +
                "      [endif]\r\n" +
				"      <sw>$SW$</sw>\r\n" + 
				"   </initdata>\r\n" +
				"</component>\r\n" +
				"[endfor]\r\n" +
				"[foreach soil.Nitrogen]\r\n" +
				"<component name=\"[soil.name] Nitrogen\" executable=\"%apsuite\\apsim\\soiln2\\lib\\soiln2.dll\">\r\n" +
				"   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soiln2\\soiln2.ini</include>\r\n" +
                "      <root_cn>20.</root_cn>\r\n" +
                "      <root_wt>1000.</root_wt>\r\n" +
                "      <soil_cn>14.5</soil_cn>\r\n" +
                "      <enr_a_coeff>7.4</enr_a_coeff>\r\n" +
                "      <enr_b_coeff>0.2</enr_b_coeff>\r\n" +
                "      <profile_reduction>off</profile_reduction>\r\n" +
                "      <oc>[foreach nitrogen.layer as l] [l.oc][endfor]</oc>\r\n" +
                "      <ph>[foreach nitrogen.layer as l] [l.ph][endfor]</ph>\r\n" +
                "      <fbiom>[foreach nitrogen.layer as l] [l.fbiom][endfor]</fbiom>\r\n" +
                "      <finert>[foreach nitrogen.layer as l] [l.finert][endfor]</finert>\r\n" +
                "      <ureappm>0  0    0    0    0    0    0</ureappm>\r\n" +
                "      <no3ppm>$NO3$</no3ppm>\r\n" +
                "      <nh4ppm>$NH4$</nh4ppm>\r\n" +
                "   </initdata>\r\n" +
                "</component>\r\n" +
                "[endfor]\r\n" +
                "[foreach soil.phosphorus]\r\n" +
				"[if [phosphorus.ResidueCP] > 0]\r\n" +
				"<component name=\"[phosphorus.name]\" executable=\"%apsuite\\apsim\\soilp\\lib\\soilp.dll\">\r\n" +
				"   <initdata>\r\n" +
                "      <include>%apsuite\\apsim\\soilp\\soilp.ini</include>\r\n" +
                "      <Labile_P>[foreach phosphorus.layer as l] [l.LabileP][endfor]</Labile_P>\r\n" +
                "      <banded_P>[foreach phosphorus.layer as l] [l.bandedP][endfor]</banded_P>\r\n" +
                "      <rock_P>[foreach phosphorus.layer as l] [l.rockP][endfor]</rock_P>\r\n" +
                "      <sorption>[foreach phosphorus.layer as l] [l.sorption][endfor]</sorption>\r\n" +
                "      <Residue_CP>[phosphorus.ResidueCP]</Residue_CP>\r\n" +
                "      <Root_CP>[phosphorus.RootCP]</Root_CP>\r\n" +
                "      <rate_dissol_rock_P>[phosphorus.RateDissolRock]</rate_dissol_rock_P>\r\n" +
                "   </initdata>\r\n" +
                "</component>\r\n" +
				"[endif]\r\n" + 
                "[endfor]";

			string SWLine = "";
			string NO3Line = "";
			string NH4Line = "";
			double[] sw = InitialWater.SW;
			double[] no3 = InitialNitrogen.NO3;
			double[] nh4 = InitialNitrogen.NH4;

			int NumLayers = Thickness.Length;
			for (int i = 0; i != NumLayers; i++)
				{
				SWLine += "    " + sw[i].ToString("f3");
				NO3Line += "      " + no3[i].ToString("f3");
				NH4Line += "      " + nh4[i].ToString("f3");
				}
		
			try
				{
				Template = Template.Replace("$SW$", SWLine);
				Template = Template.Replace("$NO3$", NO3Line);
				Template = Template.Replace("$NH4$", NH4Line);
				Macro SoilMacro = new Macro();
				Out.Write(SoilMacro.Go(Data, Template));				
				}
			catch (Exception err)
				{
				Out.Write(err.Message);
				}
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
					Data.Add(new APSIMData("InitWater", ""));
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
				double[] OldSW = getLayered("water", "sw");
				if (OldSW != null && OldSW.Length > 0)
					InitialWater.SetUsingLayered(OldSW);
				}

			if (Data.Child("InitNitrogen") == null)
				{
				Data.Add(new APSIMData("InitNitrogen", ""));
				double[] OldNO3 = getLayered("nitrogen", "no3");
				if (OldNO3 != null && OldNO3.Length > 0)
					InitialNitrogen.NO3 = OldNO3;
				double[] OldNH4 = getLayered("nitrogen", "nh4");
				if (OldNH4 != null && OldNH4.Length > 0)
					InitialNitrogen.NH4 = OldNH4;
				}
			setLayered("water", "sw", new double[0]);
            setLayered("nitrogen", "no3", new double[0]);
			setLayered("nitrogen", "nh4", new double[0]);

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
			ErrorMessages += CheckForMissing(this.Cona, "CONA");
			ErrorMessages += CheckForMissing(this.U, "U");
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
				double[] kl = LL(CropName);

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
				double[] kl = LL(CropName);

				SetCrop(CropName, ll, kl, xf);
				}
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
			Note[] ReturnList = new Note[Data.get_Children("note").Count];
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


		}
	}
