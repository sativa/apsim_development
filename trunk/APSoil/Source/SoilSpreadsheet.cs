using System;
using System.Data;
using System.Collections.Specialized;
using System.Windows.Forms;
using ExcelUtility;
using CSGeneral;
using VBGeneral;

namespace APSoil
	{
	public class SoilSpreadsheet
		{
		public SoilSpreadsheet()
			{
			}


		static public void ImportFromFile(string FileName, APSIMData Soils)
			{
			// Import all files
			Cursor.Current = Cursors.WaitCursor;

			DataTable Table = ExcelHelper.GetDataFromSheet(FileName, "SoilData");
			
			int Row = 0;
            while (Row < Table.Rows.Count)
				Soils.Add(CreateSoilFromSpreadsheet(Table, ref Row).Data);
			
			Cursor.Current = Cursors.Default;
			}

		static public void ExportToFile(string FileName, APSIMData Soils)
			{
			// export the specified soils to the specified XLS file.
			DataTable Table = new DataTable("SoilData");
			int Row = 0;
			foreach (APSIMData Data in Soils.get_Children("soil"))
				CreateTableFromSoil(new Soil(Data), Table, ref Row);
			ExcelHelper.SendDataToSheet(FileName, "SoilData", Table);			
			}


		static private void CreateTableFromSoil(Soil MySoil, DataTable Data, ref int Row)
			{
			int NumLayers = MySoil.Thickness.Length;
			DataTableUtility.AddValue(Data, "Name", MySoil.Name, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Region", MySoil.Region, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Site", MySoil.Site, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Order", MySoil.Order, Row, NumLayers);
			DataTableUtility.AddValue(Data, "NearestTown", MySoil.NearestTown, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Comment", MySoil.Comment, Row, NumLayers);
			DataTableUtility.AddValue(Data, "GPS", MySoil.GPS, Row, NumLayers);
			DataTableUtility.AddValue(Data, "GPSDatum", MySoil.GPSDatum, Row, NumLayers);
			DataTableUtility.AddValue(Data, "MapId", MySoil.MapId, Row, NumLayers);
			DataTableUtility.AddValue(Data, "NaturalVegetation", MySoil.NaturalVegetation, Row, NumLayers);

			DataTableUtility.AddValue(Data, "U", MySoil.U, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Cona", MySoil.Cona, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Salb", MySoil.Salb, Row, NumLayers);
			DataTableUtility.AddValue(Data, "DiffusConst", MySoil.DiffusConst, Row, NumLayers);
			DataTableUtility.AddValue(Data, "DiffusSlope", MySoil.DiffusSlope, Row, NumLayers);
			DataTableUtility.AddValue(Data, "CN2Bare", MySoil.CN2Bare, Row, NumLayers);
			DataTableUtility.AddValue(Data, "CNRed", MySoil.CNRed, Row, NumLayers);
			DataTableUtility.AddValue(Data, "CNCov", MySoil.CNCov, Row, NumLayers);
			DataTableUtility.AddValue(Data, "RootCN", MySoil.RootCN, Row, NumLayers);
			DataTableUtility.AddValue(Data, "RootWT", MySoil.RootWT, Row, NumLayers);
			DataTableUtility.AddValue(Data, "SoilCN", MySoil.SoilCN, Row, NumLayers);
			DataTableUtility.AddValue(Data, "EnrACoeff", MySoil.EnrACoeff, Row, NumLayers);
			DataTableUtility.AddValue(Data, "EnrBCoeff", MySoil.EnrBCoeff, Row, NumLayers);

			DataTableUtility.AddColumn(Data, "Depth", MySoil.Thickness, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "BD", MySoil.BD, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "LL15", MySoil.LL15, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "Airdry", MySoil.Airdry, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "DUL", MySoil.DUL, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "SAT", MySoil.SAT, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "SW", MySoil.InitialWater.SW, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "NO3", MySoil.InitialNitrogen.NO3, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "NH4", MySoil.InitialNitrogen.NH4, Row, NumLayers);

			foreach (string Crop in MySoil.Crops)
				{
				if (!MySoil.CropIsPredicted(Crop))
					{
					DataTableUtility.AddColumn(Data, "ll(" + Crop + ")", MySoil.LL(Crop), Row, NumLayers);
					DataTableUtility.AddColumn(Data, "kl(" + Crop + ")", MySoil.KL(Crop), Row, NumLayers);
					DataTableUtility.AddColumn(Data, "xf(" + Crop + ")", MySoil.XF(Crop), Row, NumLayers);
					}
				}

			DataTableUtility.AddColumn(Data, "SWCON", MySoil.SWCON, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "MWCON", MySoil.MWCON, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "FBIOM", MySoil.FBIOM, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "FINERT", MySoil.FINERT, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "OC", MySoil.OC, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "EC", MySoil.EC, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "PH", MySoil.PH, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "CL", MySoil.CL, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "CEC", MySoil.CEC, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "Ca", MySoil.Ca, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "Mg", MySoil.Mg, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "Na", MySoil.Na, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "K", MySoil.K, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "ESP", MySoil.ESP, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "ParticleSizeSand", MySoil.ParticleSizeSand, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "ParticleSizeSilt", MySoil.ParticleSizeSilt, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "ParticleSizeClay", MySoil.ParticleSizeClay, Row, NumLayers);

			Row += NumLayers;
			}


		static private Soil CreateSoilFromSpreadsheet(DataTable Table, ref int Row)
			{
			// Create a new soil from the the specified table.
			// At end of this method, row will be pointing to the next
			// soil.
			string name = GetStringValue(Table, "Name", Row);
			if (name == "")
				throw new Exception("Cannot find a soil name");

			Soil NewSoil = new Soil(new APSIMData("Soil", name));
			NewSoil.Region = GetStringValue(Table, "Region", Row);
			NewSoil.Site = GetStringValue(Table, "Site", Row);
			NewSoil.Order = GetStringValue(Table, "Order", Row);
			NewSoil.NearestTown = GetStringValue(Table, "NearestTown", Row);
			NewSoil.Comment = GetStringValue(Table, "Comment", Row);
			NewSoil.GPS = GetStringValue(Table, "GPS", Row);
			NewSoil.GPSDatum = GetStringValue(Table, "GPSDatum", Row);
			NewSoil.MapId = GetStringValue(Table, "MapId", Row);
			NewSoil.NaturalVegetation = GetStringValue(Table, "NaturalVegetation", Row);

			NewSoil.U = GetDoubleValue(Table, "U", Row);
			NewSoil.Cona = GetDoubleValue(Table, "Cona", Row);
			NewSoil.Salb = GetDoubleValue(Table, "Salb", Row);
			NewSoil.DiffusConst = GetDoubleValue(Table, "DiffusConst", Row);
			NewSoil.DiffusSlope = GetDoubleValue(Table, "DiffusSlope", Row);
			NewSoil.CN2Bare = GetDoubleValue(Table, "CN2Bare", Row);
			NewSoil.CNRed = GetDoubleValue(Table, "CNRed", Row);
			NewSoil.CNCov = GetDoubleValue(Table, "CNCov", Row);
			NewSoil.RootCN = GetDoubleValue(Table, "RootCN", Row);
			NewSoil.RootWT = GetDoubleValue(Table, "RootWT", Row);
			NewSoil.SoilCN = GetDoubleValue(Table, "SoilCN", Row);
			NewSoil.EnrACoeff = GetDoubleValue(Table, "EnrACoeff", Row);
			NewSoil.EnrBCoeff = GetDoubleValue(Table, "EnrBCoeff", Row);

			// Work out how many layers we're dealing with.
			int NumLayers = 0;
			while (Row+NumLayers < Table.Rows.Count &&
				   name.ToLower() == Table.Rows[Row+NumLayers]["Name"].ToString().ToLower())
				NumLayers++;

			// Store thickness.
			if (Table.Columns.IndexOf("dlayer") != -1)
				NewSoil.Thickness = DataTableUtility.GetColumnAsDoubles(Table, "dlayer", NumLayers, Row);
			else
				{
				double[] Thickness = DataTableUtility.GetColumnAsDoubles(Table, "depth", NumLayers, Row);
				double DepthSoFar = 0;
				for (int Layer = 0; Layer != NumLayers; Layer++)
					{
					Thickness[Layer] *= 10.0;
					if (Layer > 0)
						Thickness[Layer] -= DepthSoFar;
                    DepthSoFar += Thickness[Layer];
					}
				NewSoil.Thickness = Thickness;
				}			
			
			// Store rest of soil layered variables.
			NewSoil.BD = GetDoubleValues(Table, "bd", NumLayers, Row);
			NewSoil.LL15 = GetDoubleValues(Table, "LL15", NumLayers, Row);
			NewSoil.Airdry = GetDoubleValues(Table, "Airdry", NumLayers, Row);
			NewSoil.DUL = GetDoubleValues(Table, "DUL", NumLayers, Row);
			NewSoil.SAT = GetDoubleValues(Table, "SAT", NumLayers, Row);
			NewSoil.InitialWater.SetUsingLayered(GetDoubleValues(Table, "SW", NumLayers, Row));
			
			NewSoil.SWCON = GetDoubleValues(Table, "SWCON", NumLayers, Row);
			NewSoil.MWCON = GetDoubleValues(Table, "MWCON", NumLayers, Row);
			NewSoil.FBIOM = GetDoubleValues(Table, "FBIOM", NumLayers, Row);
			NewSoil.FINERT = GetDoubleValues(Table, "FINERT", NumLayers, Row);
			NewSoil.OC = GetDoubleValues(Table, "OC", NumLayers, Row);
			NewSoil.EC = GetDoubleValues(Table, "EC", NumLayers, Row);
			NewSoil.PH = GetDoubleValues(Table, "PH", NumLayers, Row);
			NewSoil.CL = GetDoubleValues(Table, "CL", NumLayers, Row);
			NewSoil.CEC = GetDoubleValues(Table, "CEC", NumLayers, Row);
			NewSoil.Ca = GetDoubleValues(Table, "Ca", NumLayers, Row);
			NewSoil.Mg = GetDoubleValues(Table, "Mg", NumLayers, Row);
			NewSoil.Na = GetDoubleValues(Table, "Na", NumLayers, Row);
			NewSoil.K = GetDoubleValues(Table, "K", NumLayers, Row);
			NewSoil.ESP = GetDoubleValues(Table, "ESP", NumLayers, Row);
			NewSoil.ParticleSizeSand = GetDoubleValues(Table, "ParticleSizeSand", NumLayers, Row);
			NewSoil.ParticleSizeSilt = GetDoubleValues(Table, "ParticleSizeSilt", NumLayers, Row);
			NewSoil.ParticleSizeClay = GetDoubleValues(Table, "ParticleSizeClay", NumLayers, Row);
			NewSoil.InitialNitrogen.NO3 = GetDoubleValues(Table, "NO3", NumLayers, Row);
			NewSoil.InitialNitrogen.NH4 = GetDoubleValues(Table, "NH4", NumLayers, Row);

			// Now get a list of all crop names.
			StringCollection Crops = new StringCollection();
			for (int i = 0; i != Table.Columns.Count; i++)
				{
				string ColumnName = Table.Columns[i].ColumnName;
				if (ColumnName.Length > 2 && ColumnName.Substring(0, 3).ToLower() == "ll(")
					Crops.Add(StringManip.SplitOffBracketedValue(ref ColumnName, '(', ')'));
				}													 

			// Now import all crop stuff.
			for (int i = 0; i != Crops.Count; i++)
				{
				NewSoil.AddCrop(Crops[i]);
				double[] ll = GetDoubleValues(Table, "LL(" + Crops[i] + ")", NumLayers, Row);
				double[] kl = GetDoubleValues(Table, "KL(" + Crops[i] + ")", NumLayers, Row);
				double[] xf = GetDoubleValues(Table, "XF(" + Crops[i] + ")", NumLayers, Row);
				NewSoil.SetCrop(Crops[i], ll, kl, xf);
				}

			Row += NumLayers;
			return NewSoil;
			}

		static private string GetStringValue(DataTable Table, string FieldName, int Row)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1)
				return Table.Rows[Row][FieldName].ToString();
			else
				return "";
			}

		static private double GetDoubleValue(DataTable Table, string FieldName, int Row)
			{
			// Get string value from specified table for specified field.
			string Value = GetStringValue(Table, FieldName, Row);
			if (Value == "")
				return MathUtility.MissingValue;
			else
				return Convert.ToDouble(Value);
			}

		static private double[] GetDoubleValues(DataTable Table, string FieldName, int NumValues, int Row)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1)
				return DataTableUtility.GetColumnAsDoubles(Table, FieldName, NumValues, Row);
			else
				return new double[0];
			}

		}
	}
