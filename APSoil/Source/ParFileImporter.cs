using System;
using System.IO;
using System.Windows.Forms;
using System.Collections.Specialized;
using VBGeneral;
using CSGeneral;

namespace APSoil
	{
	// ------------------------------------------
	// This class implements APSIM parameter file
	// importing.
	// ------------------------------------------
	public class ParFileImporter
		{
		// ----------------------------
		// Import from specified par file.
		// ----------------------------
		static public void ImportParFile(string FileName, ApsoilController Apsoil)
			{	
			Cursor.Current = Cursors.WaitCursor;
			if (!File.Exists(FileName))
				throw new Exception("Cannot import file " + FileName + ". File doesn't exist");
			string[] Sections = APSIMSettings.INIReadAllSections(FileName);

            // import all water sections.
			foreach (string Section in Sections)
				{
				if (GetStringValue(FileName, Section, "dlayer") != "")
					{
					StringCollection SectionBits = StringManip.SplitStringHonouringQuotes(Section, ".");
					if (SectionBits.Count == 3)
						{
						if (SectionBits[1].ToLower() == "soilwat2")
							{
							string SoilName = SectionBits[0];
							Soil NewSoil = new Soil(new APSIMData("soil", SoilName));
							ReadWaterSection(SoilName, FileName, NewSoil);
							ReadNitrogenSection(SoilName, FileName, NewSoil);
							ReadCropSections(SoilName, FileName, NewSoil);
							ReadPhosphorusSection(SoilName, FileName, NewSoil);
							Apsoil.AddXMLToSelected(NewSoil.Data.XML);
							}
						}
					}
				}

			Cursor.Current = Cursors.Default;
			}


		// ------------------------------
		// Import from specified w2 file.
		// ------------------------------
		static public void ImportW2N2P2(string FileName, ApsoilController Apsoil)
			{	
			Cursor.Current = Cursors.WaitCursor;

			string W2FileName = Path.GetDirectoryName(FileName) + "\\" + Path.GetFileNameWithoutExtension(FileName) + ".w2";
			if (!File.Exists(W2FileName))
				throw new Exception("Cannot import file " + W2FileName + ". File doesn't exist");
			string[] Sections = APSIMSettings.INIReadAllSections(W2FileName);
			
			//Look for a title on the first few line.
			string Title = "";
			StreamReader sr = new StreamReader(W2FileName);
			for (int i = 0; i != 5; i++)
				{
				string TitleLine = sr.ReadLine();
				if (TitleLine.Length > 5 && TitleLine.ToLower().Substring(0, 6) == "!title")
					Title = StringManip.SplitOffAfterDelimiter(ref TitleLine, "=");
				}
			if (Title.Length == 0)
				throw new Exception("Cannot find title line in file " + W2FileName);
			
			// create a new soil.
			Soil NewSoil = new Soil(new APSIMData("soil", Title));

			// Read in all water parameters
			ReadWaterSection("run%", W2FileName, NewSoil);

			// Read in all crop sections.
			ReadCropSections("run%", W2FileName, NewSoil);

			// Read in all nitrogen parameters.
			string N2FileName = Path.GetDirectoryName(FileName) + "\\" + Path.GetFileNameWithoutExtension(FileName) + ".n2";
			if (File.Exists(N2FileName))
				ReadNitrogenSection("run%", N2FileName, NewSoil);

			// Read in all phosphorus parameters.
			string P2FileName = Path.GetDirectoryName(FileName) + "\\" + Path.GetFileNameWithoutExtension(FileName) + ".p2";
			if (File.Exists(P2FileName))
				ReadPhosphorusSection("run%", P2FileName, NewSoil);

			// Add new soil to our soils.
			Apsoil.AddXMLToSelected(NewSoil.Data.XML);

			Cursor.Current = Cursors.Default;
			}
	

		// ------------------------------------------------------------
		// Read in all water parameters from specified section and file
		// ------------------------------------------------------------
		static private void ReadWaterSection(string SectionBit, string FileName, Soil NewSoil)
			{
			string SectionName = SectionBit + ".soilwat2.parameters";
			NewSoil.Cona = GetDoubleValue(FileName, SectionName, "cona");
			NewSoil.U = GetDoubleValue(FileName, SectionName, "u");
			NewSoil.CN2Bare = GetDoubleValue(FileName, SectionName, "cn2_bare");
			NewSoil.Salb = GetDoubleValue(FileName, SectionName, "salb");
			NewSoil.DiffusConst = GetDoubleValue(FileName, SectionName, "diffus_const");
			NewSoil.DiffusSlope = GetDoubleValue(FileName, SectionName, "diffus_slope");
			NewSoil.CNRed = GetDoubleValue(FileName, SectionName, "cn_red");
			NewSoil.CNCov = GetDoubleValue(FileName, SectionName, "cn_cov");

			NewSoil.Thickness = GetDoubleValues(FileName, SectionName, "dlayer");
			NewSoil.LL15 = GetDoubleValues(FileName, SectionName, "ll15");
			NewSoil.Airdry = GetDoubleValues(FileName, SectionName, "air_dry");
			NewSoil.DUL = GetDoubleValues(FileName, SectionName, "dul");
			NewSoil.SAT = GetDoubleValues(FileName, SectionName, "sat");
			NewSoil.InitialWater.SetUsingLayered(GetDoubleValues(FileName, SectionName, "sw"));
			NewSoil.SWCON = GetDoubleValues(FileName, SectionName, "swcon");
			NewSoil.MWCON = GetDoubleValues(FileName, SectionName, "mwcon");
			NewSoil.BD = GetDoubleValues(FileName, SectionName, "bd");
			}


		// ------------------------------------------------------------
		// Read in all crop parameters from all crop sections in 
		// specified file
		// ------------------------------------------------------------
		static private void ReadCropSections(string SectionBit, string FileName, Soil NewSoil)
			{
			string[] Sections = APSIMSettings.INIReadAllSections(FileName);
			foreach (string Section in Sections)
				{
				if (APSIMSettings.INIRead(FileName, Section, "ll") != "")
					{
					// get the crop name
					StringCollection SectionBits = StringManip.SplitStringHonouringQuotes(Section, ".");
					if (SectionBits.Count == 3 && SectionBits[0].ToLower() == SectionBit.ToLower())
						{
						string CropName = SectionBits[1];
						NewSoil.AddCrop(CropName);

						double[] ll;
						string LLValue = GetStringValue(FileName, Section, "ll");
						if (LLValue.ToLower() == "#ll")
							ll = NewSoil.LL15;
						else
							ll = GetDoubleValues(FileName, Section, "ll");
			
						NewSoil.SetCrop(CropName, 
										ll,
										GetDoubleValues(FileName, Section, "kl"),
										GetDoubleValues(FileName, Section, "xf"));
						}
					}
				}
			}

	
		// ------------------------------------------------------------
		// Read in all nitrogen parameters from specified section and file
		// ------------------------------------------------------------
		static private void ReadNitrogenSection(string SectionBit, string FileName, Soil NewSoil)
			{
			string SectionName = SectionBit + ".soiln2.parameters";
			NewSoil.RootCN = GetDoubleValue(FileName, SectionName, "root_cn");
			NewSoil.RootWT = GetDoubleValue(FileName, SectionName, "root_wt");
			NewSoil.SoilCN = GetDoubleValue(FileName, SectionName, "soil_cn");
			NewSoil.EnrACoeff = GetDoubleValue(FileName, SectionName, "enr_a_coeff");
			NewSoil.EnrBCoeff = GetDoubleValue(FileName, SectionName, "enr_b_coeff");

			NewSoil.PH = GetDoubleValues(FileName, SectionName, "ph");
			NewSoil.OC = GetDoubleValues(FileName, SectionName, "oc");
			NewSoil.FBIOM = GetDoubleValues(FileName, SectionName, "fbiom");
			NewSoil.FINERT = GetDoubleValues(FileName, SectionName, "finert");
			NewSoil.InitialNitrogen.NO3 = GetDoubleValues(FileName, SectionName, "no3ppm");
			NewSoil.InitialNitrogen.NH4 = GetDoubleValues(FileName, SectionName, "nh4ppm");
			}


		// -----------------------------------------------------------------
		// Read in all phosphorus parameters from specified section and file
		// -----------------------------------------------------------------
		static private void ReadPhosphorusSection(string SectionBit, string FileName, Soil NewSoil)
			{
			string SectionName = SectionBit + ".soilp.parameters";
			NewSoil.LabileP = GetDoubleValues(FileName, SectionName, "labile_P");
			NewSoil.BandedP = GetDoubleValues(FileName, SectionName, "banded_P");
			NewSoil.RockP = GetDoubleValues(FileName, SectionName, "rock_P");
			NewSoil.Sorption = GetDoubleValues(FileName, SectionName, "sorption");
			NewSoil.ResidueCP = GetDoubleValue(FileName, SectionName, "residue_cp");
			NewSoil.RootCP = GetDoubleValue(FileName, SectionName, "root_cp");
			NewSoil.RateDissolRock = GetDoubleValue(FileName, SectionName, "rate_dissol_rock_p");
			}


		// ----------------------------------------------------------
		// Get string value from specified table for specified field.
		// ----------------------------------------------------------
		static private string GetStringValue(string FileName, string SectionName, string Key)
			{
			string Value = APSIMSettings.INIRead(FileName, SectionName, Key);
			StringManip.SplitOffAfterDelimiter(ref Value, "!");
			StringManip.SplitOffAfterDelimiter(ref Value, "(");
			if (Value.StartsWith("$"))
				Value = ResolveVariableMacro(FileName, Value);

			return Value;
			}


		// ----------------------------------------------------------
		// Get string value from specified table for specified field.
		// ----------------------------------------------------------
		static private double GetDoubleValue(string FileName, string SectionName, string Key)
			{
			string Value = GetStringValue(FileName, SectionName, Key);
			if (Value  == "")
				return MathUtility.MissingValue;
			else
				{
				try
					{	
					return Convert.ToDouble(Value);
					}
				catch (Exception)
					{
					throw new Exception("Cannot convert value to a floating point number" +
					                    ". Filename: " + FileName +
					                    ". Section: " + SectionName +
										". Key: " + Key);
					}
				}

			}


		// ----------------------------------------------------------
		// Get string value from specified table for specified field.
		// ----------------------------------------------------------
		static private double[] GetDoubleValues(string FileName, string SectionName, string Key)
			{
			string Value = APSIMSettings.INIRead(FileName, SectionName, Key);
			StringManip.SplitOffAfterDelimiter(ref Value, "!");
			StringManip.SplitOffAfterDelimiter(ref Value, "(");

			if (Value == "")
				return new double[0];
			else
				{
				StringCollection Values = StringManip.SplitStringHonouringQuotes(Value, " ");
				double[] ReturnValues = new double[Values.Count];
				for (int i = 0; i != Values.Count; i++)
					{
					if (Values[i].StartsWith("$"))
						Values[i] = ResolveVariableMacro(FileName, Values[i]);
		 			try
						{
						ReturnValues[i] = Convert.ToDouble(Values[i]);
						}
					catch (Exception)
						{
						throw new Exception("Cannot convert value to a floating point number" +
											". Filename: " + FileName +
											". Section: " + SectionName +
											". Key: " + Key);
						}
					}
				return ReturnValues;
				}
			}	


		// ------------------------------------------------------------
		// Resolve the specified macro to a name and return its value.
		// NB It looks in a '*variables' section.
		// ------------------------------------------------------------
		static private string ResolveVariableMacro(string FileName, string MacroName)
			{
			string MacroLine = APSIMSettings.INIRead(FileName, "*variables", MacroName);
			StringManip.SplitOffAfterDelimiter(ref MacroLine, "!");
			if (MacroLine == "")
				throw new Exception("Cannot resolve macro: " + MacroName + " in file:" + FileName);

			StringCollection MacroBits = StringManip.SplitStringHonouringQuotes(MacroLine, " ");
			if (MacroBits.Count != 3)
				throw new Exception("Invalid variables line: " + MacroLine);
				
			return MacroBits[1];			
			}



	
		}
	}
