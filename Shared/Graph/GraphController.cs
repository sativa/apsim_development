using System;
using System.Windows.Forms;
using System.IO;
using VBGeneral;
using CSGeneral;
using System.Text;
using System.Runtime.InteropServices;
using System.Data;
using System.Collections.Specialized;

namespace Graph
	{

	public class GraphController : BaseController
        {
        // -----------------------------------------------------------------------
        // The APSIMData passed into this class' constructor should look like:
        //    <Data>
        //       <ApsimFileReader>
        //          <FileName type="filenames">continuous wheat simulation.out</FileName>
        //          <ParseTitle type="yesno">no</ParseTitle>
        //       </ApsimFileReader>
        //    </Data>
        // For those methods that require a path it should NOT include the root node name
        //    e.g. ApsimFileReader and NOT Data\ApsimFileReader
        // -----------------------------------------------------------------------
        
        private ImageList MyImageList;
        private string[] TopLevelComponents = { "apsimfilereader", "xmlfilereader", "rems", "excelreader" };
        private string[] NonTopLevelComponents = { "probability", "predobs" , "filter", "cumulative", "depth", "diff", "frequency", 
                                                   "kwtest", "regression", "stats", "soi" };
        private StringBuilder contents = new StringBuilder(500000);
        private UInt32 DataContainer = 0;
        private APSIMData GraphData;
        private bool WeCreatedDataContainer = false;

        #region Imports from external DLL's

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern UInt32 CreateDataContainer();

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void DeleteDataContainer(UInt32 DataContainer);
        
        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void SetProperties(UInt32 DataContainer,
                                                string path,
                                                string properties);
        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetErrorMessage(UInt32 DataContainer,
                                                   string path,
                                                   StringBuilder ErrorMessage);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetFieldNames(UInt32 DataContainer,
                                                 string path,
                                                 StringBuilder FieldNames);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetREMSExperimentNames(UInt32 DataContainer,
                                                          string path,
                                                          StringBuilder Names);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetREMSTreatmentNames(UInt32 DataContainer,
                                                         string path,
                                                         StringBuilder Names);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern UInt32 CreateDataForm(IntPtr ParentHandle);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void DeleteDataForm(UInt32 Handle);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern int GetHandleOfForm(UInt32 FormHandle);

        [DllImport("segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void FillDataFormWithData(UInt32 FormHandle,
                                                        UInt32 DataContainer,
                                                        string path);

        [DllImport("segreport.dll",
                        CharSet = CharSet.Ansi,
                        CallingConvention = CallingConvention.StdCall)]
        private static extern void GetXYData(UInt32 DataContainer,
                                             string path,
                                             string x,
                                             string y,
                                             byte[] Data);

        [DllImport("user32.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern bool MoveWindow(
            int hWnd,	// handle of window
            int X,	// horizontal position
            int Y,	// vertical position
            int nWidth,	// width
            int nHeight,	// height
            bool bRepaint 	// repaint flag
           );
        #endregion

        #region Constructor / destructor / setup
        public GraphController(BaseController BaseController, string GraphDataPath) : base("", "", "")
            {
            // --------------------------------------
            // constructor
            // --------------------------------------
            DataContainer = CreateDataContainer();
            WeCreatedDataContainer = true;
            AllData = BaseController.AllData;
            SelectedPaths = BaseController.SelectedPaths;
            GraphData = AllData.Find(GraphDataPath);
            SetProperties(DataContainer, "", GraphData.XML);
            }
        public GraphController(ImageList images, UInt32 datacontainer) : base("", "", "")
			{
            // --------------------------------------
            // constructor
            // --------------------------------------
            MyImageList = images;
            DataContainer = datacontainer;
            WeCreatedDataContainer = false;
            }
        protected override void Dispose(bool Disposing)
            {
            if (WeCreatedDataContainer && DataContainer != 0)
                {
                DeleteDataContainer(DataContainer);
                DataContainer = 0;
                }
            }

        #endregion

        #region Override methods from base class
        public override ImageList SmallImageList
			{
            // ----------------------------------------------
            // Provide access to our imagelist of small icons
            // ----------------------------------------------
            get
				{
				return MyImageList;
				}
			}
		public override int SmallImageIndex(string ComponentName)
			{
            // --------------------------------------------
            // Return access to an imagelist of small icons
            // --------------------------------------------
            if (ComponentName.ToLower() == "data")
                return 0;
            else if (ComponentName.ToLower() == "apsimfilereader")
                return 1;
            else if (ComponentName.ToLower() == "xmlfilereader")
                return 2;
            else if (ComponentName.ToLower() == "rems")
                return 3;
            else if (ComponentName.ToLower() == "excelreader")
                return 4;
            else if (ComponentName.ToLower() == "probability")
                return 5;
            else if (ComponentName.ToLower() == "predobs")
                return 6;
            else if (ComponentName.ToLower() == "filter")
                return 7;
            else if (ComponentName.ToLower() == "cumulative")
                return 8;
            else if (ComponentName.ToLower() == "depth")
                return 9;
            else if (ComponentName.ToLower() == "diff")
                return 10;
            else 
                return 0;
            }
        bool IsTopLevelComponent(string ComponentName)
            {
            // --------------------------------------------------------------
            // Return true if specified component is a top level component
            // --------------------------------------------------------------
            foreach (string Component in TopLevelComponents)
                {
                if (ComponentName.ToLower() == Component.ToLower())
                    return true;
                }
            return false;
            }
        bool IsNonTopLevelComponent(string ComponentName)
            {
            // --------------------------------------------------------------
            // Return true if specified component is a non top level component
            // --------------------------------------------------------------
            foreach (string Component in NonTopLevelComponents)
                {
                if (ComponentName.ToLower() == Component.ToLower())
                    return true;
                }
            return false;
            }		
		public override bool IsComponentVisible(APSIMData Component)
			{
            // -------------------------------------------------
            // Return true if the specified component is visible
            // to the user.
            // -------------------------------------------------
            return (IsTopLevelComponent(Component.Name) || IsNonTopLevelComponent(Component.Name));
            }
        public override bool AllowComponentAdd(string ChildComponentType, string ParentComponentType)
			{
            // -------------------------------------------------
            // Return true if the specified component type can
            // be added as a child to the specified parent type.
            // -------------------------------------------------
            if (ParentComponentType.ToLower() == "data" || ParentComponentType.ToLower() == "folder")
                return IsTopLevelComponent(ChildComponentType);
            else
                return IsNonTopLevelComponent(ChildComponentType);
			}
		public override BaseView CreateUI(string UIType)
			{
            // -------------------------------------------------
            // Open a new user interface based on the specified
            // component type.
            // -------------------------------------------------
            if (IsTopLevelComponent(UIType) || IsNonTopLevelComponent(UIType))
                return new GraphDataUI();
            else
                return null;
			}
        public override string ImageFileForType(string ComponentType)
            {
            return APSIMSettings.ApsimDirectory() + "\\ApsimUI\\Images\\GraphData.jpg";
            }
        protected override bool IsDataReadOnly()
            {
            return (base.IsDataReadOnly() || Path.GetFileName(FileName).ToLower() == "graph.xml");
            }
        #endregion

        #region Data methods
        public string GetErrorMessage(string Path)
            {
            // -----------------------------------------------------
            // Return any error message for the data component as
            // specified by path.
            // -----------------------------------------------------
            GetErrorMessage(DataContainer, "Data\\" + Path, contents);
            return contents.ToString();
            }
        public void SetProperties(string Path, string Properties)
            {
            // -----------------------------------------------------
            // Set the properties for the data component as specified
            // by path.
            // -----------------------------------------------------
            SetProperties(DataContainer, "Data\\" + Path, Properties);
            }
        public string[] GetAllDataSets()
            {
            // -----------------------------------------------------
            // Return a list of all data sets.
            // -----------------------------------------------------
            StringCollection DataSetNames = new StringCollection();
            GetAllDataSets(GraphData, DataSetNames);
            string[] Names = new string[DataSetNames.Count];

            // Make the dataset names relative to the graph data node rather than the entire simulation.
            for (int i = 0; i != DataSetNames.Count; i++)
                Names[i] = DataSetNames[i].Replace(GraphData.FullPath + "\\", "");
            return Names;
            }
        private void GetAllDataSets(APSIMData Node, StringCollection DataSetNames)
            {
            // ---------------------------------------------------------
            // Return a list of all data sets - internal implementation
            // ---------------------------------------------------------
            foreach (APSIMData Child in Node.get_Children(null))
                {
                if (Array.IndexOf(TopLevelComponents, Child.Name.ToLower()) != -1 ||
                    Array.IndexOf(NonTopLevelComponents, Child.Name.ToLower()) != -1)
                    {
                    string DataPath = Child.FullPath;
                    DataSetNames.Add(DataPath);
                    GetAllDataSets(Child, DataSetNames);
                    }
                }
            }
        public string[] GetFieldNamesForDataSet(string Path)
            {
            // ------------------------------------------------------
            // Return a list of all field names for specified dataset
            // ------------------------------------------------------
            GetFieldNames(DataContainer, "Data\\" + Path, contents);
            return contents.ToString().Split("\t".ToCharArray());
            }
        public DataTable GetXYData(string Path, string x, string y)
            {
            // ------------------------------------------------------
            // Return an xy datatable for specified dataset for the 
            // 2 x and y columns.
            // ------------------------------------------------------
            DataTable Data = new DataTable();

            byte[] ByteStream = new byte[50000];
            GetXYData(DataContainer, "Data\\" + Path, x, y, ByteStream);

            MemoryStream Mem = new MemoryStream(ByteStream);
            BinaryReader In = new BinaryReader(Mem);
            
            FillDataTable(Data, x, In);
            FillDataTable(Data, y, In);

            return Data;
            }
        private void FillDataTable(DataTable Data, string ColumnName, BinaryReader In)
            {
            // ----------------------------------------------------------
            // Internal method for extracting a column of numbers/strings
            // from a byte stream.
            // ----------------------------------------------------------
            int DataType = In.ReadInt32();
            if (DataType == 1)
                Data.Columns.Add(ColumnName, typeof(float));
            else if (DataType == 2)
                Data.Columns.Add(ColumnName, typeof(DateTime));
            else
                Data.Columns.Add(ColumnName, typeof(string));
            int NumValues = In.ReadInt32();

            // Make sure there are enough values in the table.
            while (Data.Rows.Count < NumValues)
                Data.Rows.Add(Data.NewRow());

            for (int Row = 0; Row != NumValues; Row++)
                {
                if (DataType == 1)
                    {
                    float Value = In.ReadSingle();
                    Data.Rows[Row][ColumnName] = Value;
                    }
                else if (DataType == 2)
                    {
                    Int16 Year = In.ReadInt16();
                    Int16 Month = In.ReadInt16();
                    Int16 Day = In.ReadInt16();
                    Data.Rows[Row][ColumnName] = new DateTime(Year, Month, Day);
                    }
                else
                    {
                    string Value = In.ReadString();
                    Data.Rows[Row][ColumnName] = Value;
                    }
                }
            }
        #endregion

        #region Data window methods
        public UInt32 CreateDataWindow(IntPtr ParentHandle)
            {
            // ------------------------------------------------
            // Create an empty data window control parented to
            // the specified handle.
            // ------------------------------------------------
            return CreateDataForm(ParentHandle);
            }
        public void DeleteDataWindow(UInt32 DataWindow)
            {
            // ------------------------------------------------
            // Delete the specified data window control
            // ------------------------------------------------

            DeleteDataForm(DataWindow);
            }
        public void SizeDataWindow(UInt32 DataWindow, int Width, int Height)
            {
            // ------------------------------------------------
            // Tell windows to resize the specified data window
            // ------------------------------------------------
            MoveWindow(GetHandleOfForm(DataWindow), 0, 0, Width, Height, true);            
            }
        public void RefreshDataWindow(UInt32 DataWindow, string path)
            {
            // ------------------------------------------------
            // Refresh the data in the specified data window.
            // ------------------------------------------------
            FillDataFormWithData(DataWindow, DataContainer, "Data\\" + path);
            }
        #endregion

        #region Generic UI functions
        public override void CreateCellEditorForRow(APSIMData Prop, FarPoint.Win.Spread.SheetView Grid, int Row)
            {
            if (Prop.Attribute("type") == "fieldname")
                {
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
                Combo.Editable = true;
                Grid.Cells[Row, 1].CellType = Combo;
                }
            else if (Prop.Attribute("type") == "fieldnames")
                {
                Grid.Rows[Row].Height = 80;
                Grid.Cells[Row, 1].CellType = new CheckedListBoxCellType();
                }
            else if (Prop.Attribute("type") == "experiment" || Prop.Attribute("type") == "treatment")
                {
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
                Grid.Cells[Row, 1].CellType = Combo;
                }
            else
                base.CreateCellEditorForRow(Prop, Grid, Row);
            }


        public override void PopulateCellEditor(APSIMData Prop, FarPoint.Win.Spread.CellType.BaseCellType Editor)
            {
            string ParentPath = "Data\\" + Data.Parent.FullPath.Replace(GraphData.FullPath + "\\", "");
            string OurPath = "Data\\" + Data.FullPath.Replace(GraphData.FullPath + "\\", "");
            if (Prop.Attribute("type") == "fieldname")
                {
                GetFieldNames(DataContainer, ParentPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)Editor;
                Combo.Items = Names;
                }
            else if (Prop.Attribute("type") == "fieldnames")
                {
                GetFieldNames(DataContainer, ParentPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                CheckedListBoxCellType ListBox = (CheckedListBoxCellType)Editor;
                ListBox.Items = Names;
                }
            else if (Prop.Attribute("type") == "experiment")
                {
                GetREMSExperimentNames(DataContainer, OurPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType) Editor;
                Combo.Items = Names;
                }
            else if (Prop.Attribute("type") == "treatment")
                {
                GetREMSTreatmentNames(DataContainer, OurPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)Editor;
                Combo.Items = Names;
                }
            }
        # endregion

		}
	}

