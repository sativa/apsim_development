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
		public Soil(APSIMData data)	: base(data)
			{
			}


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



		// ------------------------
		// Layered soil properties.
		// ------------------------
		public double[] LL15
			{
			get {return getLayered("water", "ll15");}
			set {setLayered("water", "ll15", value);}
			}
		public double[] Airdry
			{				  
			get {return getLayered("water", "airdry");}
			set {setLayered("water", "airdry", value);}
			}
//		public double[] SW
//			{
//			get {return InitialWater.SW;}
//			}
		public double[] DUL
			{
			get {return getLayered("Water", "dul");}
			set {setLayered("Water", "dul", value);}
			}
		public double[] SAT
			{
			get {return getLayered("water", "sat");}
			set {setLayered("Water", "sat", value);}
			}
		public double[] BD
			{
			get {return getLayered("Water", "bd");}
			set {setLayered("Water", "bd", value);}
			}
//		public double[] NO3
//			{
//			get {return getLayered("nitrogen", "no3");}
//			set {setLayered("nitrogen", "no3", value);}
//			}
//		public double[] NH4
//			{
//			get {return getLayered("nitrogen", "nh4");}
//			set {setLayered("nitrogen", "nh4", value);}
//			}
		public double[] SWCON
			{
			get {return getLayered("nitrogen", "swcon");}
			set {setLayered("nitrogen", "swcon", value);}
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

                                        
		// ------------------------------------------------------
		// Crop properties
		// ------------------------------------------------------
		public string[] Crops
			{
			get {
				StringCollection ReturnListCollection = Data.ChildList("SoilCrop");
				string[] ReturnList = new string[ReturnListCollection.Count];
				ReturnListCollection.CopyTo(ReturnList, 0);
				return ReturnList;
				}
			}
		public bool CropExists(string CropName)
			{
            StringCollection Crops = Data.ChildList("SoilCrop");
			return (Crops.IndexOf(CropName.ToLower()) != -1);
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
			return getLayered(CropName, "ll");
			}
		public double[] KL(string CropName)
			{
			return getLayered(CropName, "kl");
			}
		public double[] XF(string CropName)
			{
			return getLayered(CropName, "xf");
			}
		public void SetCrop(string CropName, double[] ll, double[] kl, double[] xf)
			{
			setLayered("SoilCrop", CropName, "ll", ll);
			setLayered("SoilCrop", CropName, "kl", kl);
			setLayered("SoilCrop", CropName, "xf", xf);
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

			for (int layer = 0; layer != LL.Length; layer++)
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
		public void ExportToPar(string FileName)
			{
			string Template = 
				"[soil.soilwat2.parameters]\n"+
				"[foreach Soil.Water as water]\n"+
				"   diffus_const = [water.DiffusConst]   ! coeffs for unsaturated water flow\n"+
				"   diffus_slope = [water.DiffusSlope]\n"+
				"   cn2_bare     = [water.Cn2Bare]    ! bare soil runoff curve number\n"+
				"   cn_red       = [water.CnRed]    ! potetial reduction in curve number due to residue\n"+
				"   cn_cov       = [water.CnCov]   ! cover for maximum reduction in curve number\n"+
				"   salb         = [water.Salb]  ! bare soil albedo\n"+
				"   cona         = [water.Cona]     ! stage 2 evap coef.\n"+
				"   u            = [water.U]     ! stage 1 soil evaporation coefficient (mm)\n"+
				"\n"+
				"   dlayer  =[foreach water.layer as Layer]\n      [Layer.thickness][endfor]   ! layer thickness mm soil\n"+
				"   air_dry =[foreach water.layer as Layer]\n    [Layer.airdry][endfor]   ! air dry mm water/mm soil\n"+
				"   ll15    =[foreach water.layer as Layer]\n    [Layer.ll15][endfor]   ! lower limit mm water/mm soil\n"+
				"   dul     =[foreach water.layer as Layer]\n    [Layer.dul][endfor]   ! drained upper limit mm water/mm soil\n"+
				"   sat     =[foreach water.layer as Layer]\n    [Layer.sat][endfor]   ! saturation mm water/mm soil\n"+
				"   sw      =[foreach water.layer as Layer]\n    [Layer.sw][endfor]   ! starting soil water mm water/mm soil\n"+
				"   swcon   =[foreach water.layer as Layer]\n    [Layer.swcon][endfor]   ! drainage coefficient\n"+
				"   bd      =[foreach water.layer as Layer]\n    [Layer.bd][endfor]   ! bulk density gm dry soil/cc moist soil\n"+
				"[endfor]\n"+//END OF WATER FOR LOOP
				"\n"+
				"[soil.soiln2.parameters]\n"+//TITLE
				"[foreach Soil.Nitrogen as nitrogen]\n"+
				"   root_cn      = [nitrogen.rootcn]     ! C:N ratio of initial root residues\n"+
				"   root_wt      = [nitrogen.rootwt]   ! root residues as biomass (kg/ha)\n"+
				"   soil_cn      = [nitrogen.soilcn]   ! C:N ratio of soil\n"+
				"   enr_a_coeff  = [nitrogen.enracoeff]\n"+
				"   enr_b_coeff  = [nitrogen.enrbcoeff]\n"+
				"   profile_reduction =  off\n"+ 
				"\n"+
				"   oc      =[foreach nitrogen.layer as Layer]\n      [Layer.oc][endfor]   ! Soil Organic Carbon\n"+
				"   ph      =[foreach nitrogen.layer as Layer]\n      [Layer.ph][endfor]   ! pH of soil\n"+
				"   fbiom   =[foreach nitrogen.layer as Layer]\n      [Layer.fbiom][endfor]   ! Organic C Biomass Fraction\n"+
				"   finert  =[foreach nitrogen.layer as Layer]\n      [Layer.finert][endfor]   ! Inert Organic C Fraction\n"+
				"   no3ppm  =[foreach nitrogen.layer as Layer]\n      [Layer.no3][endfor]   ! Nitrate Concentration\n"+
				"   nh4ppm  =[foreach nitrogen.layer as Layer]\n      [Layer.nh4][endfor]   ! Ammonium Concentration\n"+
				"[endfor]\n"+//END OF NITROGEN FOR LOOP
				"\n"+
				"[foreach Soil.SoilCrop as crop]\n"+
				"[soil.[crop.name].parameters]\n"+//TITLE
				"   ll      =[foreach crop.layer as Layer]\n      [Layer.ll][endfor]\n\n"+
				"   kl      =[foreach crop.layer as Layer]\n      [Layer.kl][endfor]\n\n"+
				"   xf      =[foreach crop.layer as Layer]\n      [Layer.xf][endfor]\n\n"+
				"[endfor]\n"+//END OF CROP FOR LOOP
				"[endfile]\n\n";

			//AutoCorrect();
			RoundResultsTo3DecimalPlaces();
			
			string szSoilFileTemplate = "[file " + Path.GetFileName(FileName) + "]\n" + Template;
			Macro SoilMacro = new Macro();
			StringCollection scSoilFiles = SoilMacro.Go(Data, szSoilFileTemplate, 
														Path.GetDirectoryName(FileName));
			}
										  

		// ------------------------------------------------------------------
		// Auto correct the soil profile properties if necessary.
		// Return true if the sw values were bounded.
		// ------------------------------------------------------------------
//		private bool AutoCorrect()
//			{
//			bool neededCorrecting = false;
//			double[] sw = SW;
//			double[] airdry = Airdry;
//			double[] ll15 = LL15;
//			double[] dul = DUL;
//			double[] sat = SAT;
//
//			for (int layer = 0; layer != sw.Length; layer++)
//				{
//				if (sw[layer] < ll15[layer])
//					{
//					neededCorrecting = true;
//					sw[layer] = ll15[layer];
//					}
//				else if (sw[layer] > dul[layer])
//					{
//					neededCorrecting = true;
//					sw[layer] = dul[layer];
//					}
//				}
//			if (neededCorrecting)
//				SW = sw;
//			return neededCorrecting;
//			}


		//-------------------------------------------------------------------------
		//This is function is used to set all the values to three decimal places
		//It basically  makes the .soil file generated for the apsim run machine
		//much easier to read and check
		//-------------------------------------------------------------------------	
		private void RoundResultsTo3DecimalPlaces()
			{
			foreach (APSIMData layer in Data.Child("Water").get_Children("layer"))
				{
				layer.Child("airdry").Value = Math.Round(Convert.ToDouble(layer.Child("airdry").InnerXML), 3).ToString("F3");
				layer.Child("ll15").Value = Math.Round(Convert.ToDouble(layer.Child("ll15").InnerXML), 3).ToString("F3");
				layer.Child("dul").Value = Math.Round(Convert.ToDouble(layer.Child("dul").InnerXML), 3).ToString("F3");
				layer.Child("sat").Value = Math.Round(Convert.ToDouble(layer.Child("sat").InnerXML), 3).ToString("F3");
				layer.Child("swcon").Value = Math.Round(Convert.ToDouble(layer.Child("swcon").InnerXML), 3).ToString("F3");
				layer.Child("sw").Value = Math.Round(Convert.ToDouble(layer.Child("sw").InnerXML), 3).ToString("F3");
				layer.Child("bd").Value = Math.Round(Convert.ToDouble(layer.Child("bd").InnerXML), 3).ToString("F3");
				}
			foreach (APSIMData layer in Data.Child("Nitrogen").get_Children("layer"))
				{
				layer.Child("oc").Value = Math.Round(Convert.ToDouble(layer.Child("oc").InnerXML), 3).ToString("F3");
				layer.Child("ph").Value = Math.Round(Convert.ToDouble(layer.Child("ph").InnerXML), 3).ToString("F3");
				layer.Child("fbiom").Value = Math.Round(Convert.ToDouble(layer.Child("fbiom").InnerXML), 3).ToString("F3");
				layer.Child("finert").Value = Math.Round(Convert.ToDouble(layer.Child("finert").InnerXML), 3).ToString("F3");
				layer.Child("no3").Value = Math.Round(Convert.ToDouble(layer.Child("no3").InnerXML), 3).ToString("F3");
				layer.Child("nh4").Value = Math.Round(Convert.ToDouble(layer.Child("nh4").InnerXML), 3).ToString("F3");
				}
			foreach (APSIMData crop in Data.get_Children("SoilCrop"))
				{
				foreach (APSIMData layer in crop.get_Children("layer"))
					{
					layer.Child("ll").Value = Math.Round(Convert.ToDouble(layer.Child("ll").InnerXML), 3).ToString("F3");
					layer.Child("kl").Value = Math.Round(Convert.ToDouble(layer.Child("kl").InnerXML), 3).ToString("F3");
					layer.Child("xf").Value = Math.Round(Convert.ToDouble(layer.Child("xf").InnerXML), 3).ToString("F3");
					}
				}
			}


		// ------------------------
		// Initial water property
		// ------------------------
		public InitWater InitialWater
			{
			get {return new InitWater(this);}
			}


		// ------------------------
		// Initial water property
		// ------------------------
		public InitNitrogen InitialNitrogen
			{
			get {return new InitNitrogen(this);}
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
			}
		}
	}
