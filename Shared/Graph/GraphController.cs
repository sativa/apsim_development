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
        private ImageList MyImageList;
        private string[] TopLevelComponents = { "apsimfilereader", "xmlfilereader", "rems", "excelreader" };
        private string[] NonTopLevelComponents = { "probability", "predobs" , "filter", "cumulative", "depth", "diff", "frequency", 
                                                   "kwtest", "regression", "stats", "soi" };
        private StringBuilder contents = new StringBuilder(500000);
        UInt32 DataContainer = 0;

        #region Imports from external DLL's
        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern UInt32 CreateDataContainer(string properties);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void DeleteDataContainer(UInt32 DataContainer);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void SetProperties(UInt32 DataContainer,
                                                string path,
                                                string properties);
        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void FindData(UInt32 DataContainer,
                                           string path,
                                           StringBuilder data);
        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetErrorMessage(UInt32 DataContainer,
                                                   string path,
                                                   StringBuilder ErrorMessage);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetFieldNames(UInt32 DataContainer,
                                                 string path,
                                                 StringBuilder FieldNames);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetREMSExperimentNames(UInt32 DataContainer,
                                                          string path,
                                                          StringBuilder Names);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void GetREMSTreatmentNames(UInt32 DataContainer,
                                                         string path,
                                                         StringBuilder Names);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern UInt32 CreateDataForm(IntPtr ParentHandle);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void DeleteDataForm(UInt32 Handle);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern int GetHandleOfForm(UInt32 FormHandle);

        [DllImport("d:\\development\\bin\\segreport.dll",
                   CharSet = CharSet.Ansi,
                   CallingConvention = CallingConvention.StdCall)]
        private static extern void FillDataFormWithData(UInt32 FormHandle,
                                                        UInt32 DataContainer,
                                                        string path);

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
        public GraphController(ImageList images)	
			: base("", "", "")
			{
            // --------------------------------------
            // constructor
            // --------------------------------------
            MyImageList = images;
            NewDataEvent += new NotifyEventHandler(OnNewDataEvent);
			}
        protected override void Dispose(bool disposing)
            {
            // --------------------------------------
            // destructor
            // --------------------------------------
            if (!disposed && disposing)
                DeleteDataContainer(DataContainer);
            base.Dispose(disposing);
            }
        void OnNewDataEvent()
            {
            // -------------------------------------------
            // New XML data has been opened.
            // Create a new data container passing the
            // new data to it.
            // -------------------------------------------
            if (AllowDataChanges)
                {
                DeleteDataContainer(DataContainer);
                DataContainer = CreateDataContainer(AllData.XML);
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
		public override bool IsComponentVisible(string ComponentName)
			{
            // -------------------------------------------------
            // Return true if the specified component is visible
            // to the user.
            // -------------------------------------------------
            return (IsTopLevelComponent(ComponentName) || IsNonTopLevelComponent(ComponentName));
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
            StringBuilder message = new StringBuilder(50000);
            GetErrorMessage(DataContainer, Path, message);
            return message.ToString();
            }
        public void SetProperties(string Path, string Properties)
            {
            SetProperties(DataContainer, Path, Properties);
            }
        public void GetAllDataSets(StringCollection DataSetNames)
            {
            GetAllDataSets(Data, DataSetNames);
            // Make the dataset names relative to the graph data node rather than the entire simulation.
            for (int i = 0; i != DataSetNames.Count; i++)
                DataSetNames[i] = DataSetNames[i].Replace(Data.FullPath + "//", "");
            }
        private void GetAllDataSets(APSIMData Node, StringCollection DataSetNames)
            {
            foreach (APSIMData Child in Node.get_Children(null))
                {
                if (Child.ChildNames(null).Length > 0)
                    {
                    string DataPath = Child.FullPath;
                    DataSetNames.Add(DataPath);
                    GetAllDataSets(Child, DataSetNames);
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
            FillDataFormWithData(DataWindow, DataContainer, path);
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
            if (Prop.Attribute("type") == "fieldname")
                {
                GetFieldNames(DataContainer, Data.Parent.FullPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)Editor;
                Combo.Items = Names;
                }
            else if (Prop.Attribute("type") == "fieldnames")
                {
                GetFieldNames(DataContainer, Data.Parent.FullPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                CheckedListBoxCellType ListBox = (CheckedListBoxCellType)Editor;
                ListBox.Items = Names;
                }
            else if (Prop.Attribute("type") == "experiment")
                {
                GetREMSExperimentNames(DataContainer, Data.FullPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType) Editor;
                Combo.Items = Names;
                }
            else if (Prop.Attribute("type") == "treatment")
                {
                GetREMSTreatmentNames(DataContainer, Data.FullPath, contents);
                string st = contents.ToString();
                string[] Names = st.Split('\t');
                FarPoint.Win.Spread.CellType.ComboBoxCellType Combo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)Editor;
                Combo.Items = Names;
                }
            }
        # endregion

		}
	}

