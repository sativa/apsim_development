using System;
using System.Data;
using System.Collections.Specialized;
using System.Windows.Forms;
using ExcelUtility;
using CSGeneral;
using VBGeneral;
using System.IO;
using System.Collections;

namespace APSoil
	{
	public class SoilSpreadsheet
		{
		public SoilSpreadsheet()
			{
			}


		static public void ImportFromFile(string FileName, ApsoilController Apsoil)
			{
			// Import all files
			Cursor.Current = Cursors.WaitCursor;
            StringCollection TopNode = new StringCollection();
            TopNode.Add("soils");
            Apsoil.SelectedPaths = TopNode;

			DataTable Table = ExcelHelper.GetDataFromSheet(FileName, "SoilData");
            StringCollection NewXml = new StringCollection();
			int Row = 0;
            while (Row < Table.Rows.Count)
                NewXml.Add(CreateSoilXmlFromSpreadsheet(Table, ref Row));

			StringCollection NewSelections = new StringCollection();
            foreach (string Xml in NewXml)
                {
                APSIMData NewData = new APSIMData(Xml);
				APSIMData NewNode = CreateNodeFromPath(Apsoil, Apsoil.AllData, NewData.Name);
                NewSelections.Add(BaseController.GetFullPathForData(NewNode));
                NewNode.InnerXML = NewData.InnerXML;
				}
            Apsoil.SelectedPaths = NewSelections;			
			Cursor.Current = Cursors.Default;
			}


		static public void ExportToFile(string FileName, ArrayList Soils)
			{
            Cursor.Current = Cursors.WaitCursor;
            
            File.Delete(FileName);
            DataTable Table = new DataTable("SoilData");
            int Row = 0;
            foreach (APSIMData SelectedData in Soils)
                CreateTableFromData(SelectedData, Table, ApsoilController.GetFullPathForData(SelectedData), ref Row);
            ExcelHelper.SendDataToSheet(FileName, "SoilData", Table);
            Cursor.Current = Cursors.Default;
            }

		static private void CreateTableFromData(APSIMData Data, DataTable Table, string ChildPath, ref int Row)
			{
            if (Data.Type.ToLower() == "soil")
                CreateTableFromSoil(new Soil(Data), Table, ChildPath, ref Row);

			foreach (APSIMData Child in Data.get_Children(null))
				{
				if (Child.Type.ToLower() == "soil" ||
				    Child.Type.ToLower() == "soils" || Child.Type.ToLower() == "folder")
					CreateTableFromData(Child, Table, ChildPath + "|" + Child.Name, ref Row); // recursion
				}
			}


		static private void CreateTableFromSoil(Soil MySoil, DataTable Data, string ChildPath, ref int Row)
			{
			int NumLayers = MySoil.Thickness.Length;
			DataTableUtility.AddValue(Data, "Name", "\\" + ChildPath.Replace("|", "\\"), Row, NumLayers);
			DataTableUtility.AddValue(Data, "State", MySoil.State, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Region", MySoil.Region, Row, NumLayers);
            DataTableUtility.AddValue(Data, "NearestTown", MySoil.NearestTown, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Site", MySoil.Site, Row, NumLayers);
            DataTableUtility.AddValue(Data, "Classification", MySoil.Order, Row, NumLayers);
			DataTableUtility.AddValue(Data, "DataSource", MySoil.DataSource, Row, NumLayers);
			DataTableUtility.AddValue(Data, "Comments", MySoil.Comment, Row, NumLayers);
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

			DataTableUtility.AddColumn(Data, "Thickness", MySoil.Thickness, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "BD", MySoil.BD, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "Rocks", MySoil.Rocks, Row, NumLayers);
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

            DataTableUtility.AddColumn(Data, "Texture", MySoil.Texture, Row, NumLayers);
            DataTableUtility.AddColumn(Data, "SWCON", MySoil.SWCON, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "MWCON", MySoil.MWCON, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "FBIOM", MySoil.FBIOM, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "FINERT", MySoil.FINERT, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "OC", MySoil.OC, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "EC", MySoil.EC, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "PH", MySoil.PH, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "CL", MySoil.CL, Row, NumLayers);
			DataTableUtility.AddColumn(Data, "Boron", MySoil.Boron, Row, NumLayers);
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


		static private string CreateSoilXmlFromSpreadsheet(DataTable Table, ref int Row)
			{
			// Create a new soil from the the specified table.
			// At end of this method, row will be pointing to the next
			// soil.
			string name = GetStringValue(Table, "Name", Row);
			if (name == "")
				throw new Exception("Cannot find a soil name");

			Soil NewSoil = new Soil(new APSIMData("Soil", name));
            NewSoil.State = GetStringValue(Table, "State", Row);
            NewSoil.Region = GetStringValue(Table, "Region", Row);
            NewSoil.NearestTown = GetStringValue(Table, "NearestTown", Row);
            NewSoil.Site = GetStringValue(Table, "Site", Row);
			NewSoil.Order = GetStringValue(Table, "Classification", Row);
			NewSoil.DataSource = GetStringValue(Table, "DataSource", Row);
			NewSoil.Comment = GetStringValue(Table, "Comments", Row);
			NewSoil.NaturalVegetation = GetStringValue(Table, "NaturalVegetation", Row);

			NewSoil.SetUCona(GetDoubleValue(Table, "U", Row), GetDoubleValue(Table, "Cona", Row));
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
            NewSoil.Thickness = GetDoubleValues(Table, "Thickness", NumLayers, Row, 0);
			
			// Store rest of soil layered variables.
			NewSoil.BD = GetDoubleValues(Table, "bd", NumLayers, Row, 3);
            NewSoil.Rocks = GetDoubleValues(Table, "Rocks", NumLayers, Row, 0);
            NewSoil.LL15 = GetDoubleValues(Table, "LL15", NumLayers, Row, 2);
			NewSoil.Airdry = GetDoubleValues(Table, "Airdry", NumLayers, Row, 2);
			NewSoil.DUL = GetDoubleValues(Table, "DUL", NumLayers, Row, 2);
			NewSoil.SAT = GetDoubleValues(Table, "SAT", NumLayers, Row, 2);
			NewSoil.InitialWater.SetUsingLayered(GetDoubleValues(Table, "SW", NumLayers, Row, 2));

            NewSoil.Texture = GetStringValues(Table, "Texture", NumLayers, Row);
            NewSoil.SWCON = GetDoubleValues(Table, "SWCON", NumLayers, Row, 2);
			NewSoil.MWCON = GetDoubleValues(Table, "MWCON", NumLayers, Row, 2);
			NewSoil.FBIOM = GetDoubleValues(Table, "FBIOM", NumLayers, Row, 2);
			NewSoil.FINERT = GetDoubleValues(Table, "FINERT", NumLayers, Row, 1);
			NewSoil.OC = GetDoubleValues(Table, "OC", NumLayers, Row, 2);
			NewSoil.EC = GetDoubleValues(Table, "EC", NumLayers, Row, 1);
			NewSoil.PH = GetDoubleValues(Table, "PH", NumLayers, Row, 1);
			NewSoil.CL = GetDoubleValues(Table, "CL", NumLayers, Row, 1);
			NewSoil.Boron = GetDoubleValues(Table, "Boron", NumLayers, Row, 1);
            NewSoil.CEC = GetDoubleValues(Table, "CEC", NumLayers, Row, 1);
            NewSoil.Ca = GetDoubleValues(Table, "Ca", NumLayers, Row, 1);
            NewSoil.Mg = GetDoubleValues(Table, "Mg", NumLayers, Row, 1);
            NewSoil.Na = GetDoubleValues(Table, "Na", NumLayers, Row, 1);
            NewSoil.K = GetDoubleValues(Table, "K", NumLayers, Row, 1);
            NewSoil.ESP = GetDoubleValues(Table, "ESP", NumLayers, Row, 1);
            NewSoil.ParticleSizeSand = GetDoubleValues(Table, "ParticleSizeSand", NumLayers, Row, 1);
            NewSoil.ParticleSizeSilt = GetDoubleValues(Table, "ParticleSizeSilt", NumLayers, Row, 1);
            NewSoil.ParticleSizeClay = GetDoubleValues(Table, "ParticleSizeClay", NumLayers, Row, 1);
			NewSoil.InitialNitrogen.NO3 = GetDoubleValues(Table, "NO3", NumLayers, Row, 3);
			NewSoil.InitialNitrogen.NH4 = GetDoubleValues(Table, "NH4", NumLayers, Row, 3);

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
				double[] ll = GetDoubleValues(Table, "LL(" + Crops[i] + ")", NumLayers, Row, 2);
				double[] kl = GetDoubleValues(Table, "KL(" + Crops[i] + ")", NumLayers, Row, 2);
				double[] xf = GetDoubleValues(Table, "XF(" + Crops[i] + ")", NumLayers, Row, 2);

				bool AllMissingValues = true;
				for (int j = 0; j != ll.Length && AllMissingValues; j++)
					AllMissingValues = (AllMissingValues && ll[j] == MathUtility.MissingValue &&
										kl[j] == MathUtility.MissingValue && xf[j] == MathUtility.MissingValue);	

				if (!AllMissingValues)
					{
					NewSoil.AddCrop(Crops[i]);
    				NewSoil.SetCrop(Crops[i], ll, kl, xf);
					}
				}

			Row += NumLayers;
			return NewSoil.Data.XML;
			}

		static private string GetStringValue(DataTable Table, string FieldName, int Row)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1 && Table.Rows[Row][FieldName].ToString() != MathUtility.MissingValue.ToString())
				return Table.Rows[Row][FieldName].ToString();
			else
				return "";
			}

        static private string[] GetStringValues(DataTable Table, string FieldName, int NumValues, int Row)
            {
            // Get string value from specified table for specified field.
            if (Table.Columns.IndexOf(FieldName) != -1)
                return DataTableUtility.GetColumnAsStrings(Table, FieldName, NumValues, Row);
            else
                return new string[0];
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

        static private double[] GetDoubleValues(DataTable Table, string FieldName, int NumValues, int Row, int DecPlaces)
			{
			// Get string value from specified table for specified field.
			if (Table.Columns.IndexOf(FieldName) != -1)
                return MathUtility.Round(DataTableUtility.GetColumnAsDoubles(Table, FieldName, NumValues, Row), DecPlaces);
			else
				return new double[0];
			}

		static private APSIMData CreateNodeFromPath(ApsoilController Apsoil, APSIMData NewData, string SoilPath)
            // ------------------------------------------------------
            // The SoilPath name passed in is a full path to a node
            // in the tree. e.g. \soils\Queensland\soilA
            // This method returns that node if it exists or creates
            // a new node and returns that.
            // ------------------------------------------------------
			{
			bool rootNode = true;
			int PosDelimiter;
            if (SoilPath != "")
                SoilPath = SoilPath.Substring(1);  // skip leading \ character.

			while (SoilPath != "")
				{
				PosDelimiter = SoilPath.IndexOf('\\');
				string PathNodeName;
				if (PosDelimiter >= 0)
					{
					PathNodeName = SoilPath.Substring(0, PosDelimiter);
					SoilPath = SoilPath.Substring(PosDelimiter+1);
					}
				else
					{
					PathNodeName = SoilPath.Substring(PosDelimiter+1);
					SoilPath = "";
					}

				if (rootNode)
					{
                    Apsoil.RenameSelected(PathNodeName);
					rootNode = false;
					}
				else
					{
					APSIMData Child = NewData.Child(PathNodeName);
                    if (Child == null)
                        {
                        if (SoilPath == "")
                            Child = NewData.Add(new APSIMData("soil", PathNodeName));
                        else
                            Child = NewData.Add(new APSIMData("folder", PathNodeName));
                        Apsoil.DataHasBeenAdded(BaseController.GetFullPathForData(Child.Parent), Child.Parent);
    
                        }
					NewData = Child;
					}
				}
			return NewData;
			}

		}
	}
