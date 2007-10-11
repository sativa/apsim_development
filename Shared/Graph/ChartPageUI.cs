using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using VBUserInterface;
using System.IO;
using CSUserInterface;
using System.ComponentModel.Design;
using Steema.TeeChart;
using System.Drawing.Design;
using System.Reflection;

namespace Graph
    {
    public partial class ChartPageUI : BaseView
        {
        private DataProcessor DataProcessor;
        private APSIMData Data;
        private ArrowRenderer Arrow = new ArrowRenderer(10, 1f, true);
        enum PageMode {Normal, Arrow, Design};
        PageMode Mode;
        Greatis.FormDesigner.Toolbox Toolbox = new Greatis.FormDesigner.Toolbox(); 

        public ChartPageUI()
            {
            InitializeComponent();
            IMenuCommandService mcs = (IMenuCommandService)FormDesigner.DesignerHost.GetService(typeof(IMenuCommandService));
            mcs.AddVerb(new DesignerVerb("Delete", new EventHandler(OnRemoveMenuItem), StandardCommands.VerbFirst));
            mcs.AddVerb(new DesignerVerb("Rename", new EventHandler(OnRenameMenuItem), StandardCommands.VerbLast));
            mcs.AddVerb(new DesignerVerb("Exit edit mode", new EventHandler(OnNormalClick), StandardCommands.VertSpaceConcatenate));

            Toolbox.ToolboxCtrl.Designer = FormDesigner;
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ApsimFileReaderUI), "ApsimFileReader"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ApsimFileReaderUI), "XmlFileReader"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "Probability"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FilterUI), "Filter"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(EmptyGraphUI), "Cumulative"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(EmptyGraphUI), "Depth"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "Diff"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FrequencyUI), "Frequency"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "KWTest"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(RegressionUI), "Regression"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(PredObsUI), "PredObs"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(StatsUI), "Stats"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(SOIUI), "SOI"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(RecordFilterUI), "RecordFilter"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(REMSUI), "REMS"), "Data");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(XYPickerUI), "XY"), "Visual");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ChartUI), "Chart"), "Visual");
            Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(DataGridUI), "DataGrid"), "Visual");
            Toolbox.TopMost = true;
            Toolbox.ToolboxCtrl.AutoSize = true;
            Toolbox.AutoSize = true;
            Toolbox.FormBorderStyle = FormBorderStyle.SizableToolWindow;
            Toolbox.Height = 450;
            Toolbox.Width = 120;
            }
        private ToolboxItem CreateToolboxItem(Type t, string DisplayName)
            {
            ToolboxItem Item = new ToolboxItem(t);
            Item.DisplayName = DisplayName;
            return Item;
            }
        public DataProcessor Processor
            {
            get { return DataProcessor; }
            set { DataProcessor = value; }
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            Data = Controller.ApsimData.Find(NodePath);
            Mode = PageMode.Normal;

            //Special case where this dataui is dropped on an outputfile.
            //We want to give the filename to the child outputfile automatically.
            if (Data.Parent != null && Data.Parent.Parent != null &&
                Data.Parent.Parent.Type.ToLower() == "outputfile" &&
                Data.Child("outputfile") != null)
                {
                string FullFileName = Path.GetDirectoryName(Controller.FileName) + "\\" 
                                      + BaseActions.CalcFileName(Data.Parent.Parent);
                Data.Child("outputfile").set_ChildValue("FileName", FullFileName);
                }
            // Position ourselves
            if (Data.Attribute("Left") != "")
                Left = Convert.ToInt32(Data.Attribute("Left"));
            if (Data.Attribute("Top") != "")
                Top = Convert.ToInt32(Data.Attribute("Top"));
            if (Data.Attribute("Width") != "")
                Width = Convert.ToInt32(Data.Attribute("Width"));
            if (Data.Attribute("Height") != "")
                Height = Convert.ToInt32(Data.Attribute("Height"));

            // Go create all child controls.
            Controls.Clear();
            foreach (APSIMData Child in Data.get_Children(null))
                Add(Child);
            }

        private void Add(APSIMData NewComponent)
            {
            BaseView View = null;

            APSIMData TypeInfo = Controller.GetComponentTypeInfo(NewComponent.Type);
            string UIType = "";
            if (TypeInfo != null)
                UIType = TypeInfo.get_ChildValue("uitype");
            if (UIType != "")
                View = (BaseView)BaseController.CreateClass(UIType);

            if (View != null)
                {
                View.Name = NewComponent.Name;

                View.Parent = this; 
                View.OnLoad(Controller, NewComponent.FullPath);
                
                if (NewComponent.Attribute("Left") != "")
                    View.Left = Convert.ToInt32(NewComponent.Attribute("Left"));
                if (NewComponent.Attribute("Top") != "")
                    View.Top = Convert.ToInt32(NewComponent.Attribute("Top"));
                if (NewComponent.Attribute("Width") != "")
                    View.Width = Convert.ToInt32(NewComponent.Attribute("Width"));
                if (NewComponent.Attribute("Height") != "")
                    View.Height = Convert.ToInt32(NewComponent.Attribute("Height"));
                View.Visible = (NewComponent.Attribute("Visible").ToLower() != "no");
                if (!Processor.FromApsimReport)
                    Processor.Add(NewComponent.XML);
                }
            }

        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            foreach (BaseView View in Controls)
                View.OnRefresh();
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            OnNormalClick(null, null);
            if (Data != null)
                {
                Data.SetAttribute("Left", Left.ToString());
                Data.SetAttribute("Top", Top.ToString());
                Data.SetAttribute("Width", Width.ToString());
                Data.SetAttribute("Height", Height.ToString());
                foreach (BaseView View in Controls)
                    {
                    Data.Child(View.Name).SetAttribute("Left", View.Left.ToString());
                    Data.Child(View.Name).SetAttribute("Top", View.Top.ToString());
                    Data.Child(View.Name).SetAttribute("Width", View.Width.ToString());
                    Data.Child(View.Name).SetAttribute("Height", View.Height.ToString());
                    if (Processor.FromApsimReport)
                        Processor.Set(Data.Child(View.Name).XML);

                    }
                }
            }

        public void DoRefresh(APSIMData ChangedNode)
            {
            Processor.Set(ChangedNode.XML);
            bool RefreshDone = false;
            Control Ctrl = this;
            while (Ctrl.Parent != null && !RefreshDone)
                {
                if (Ctrl.Parent is BaseView)
                    {
                    (Ctrl.Parent as BaseView).OnRefresh();
                    RefreshDone = true;
                    }
                else
                    Ctrl = Ctrl.Parent;
                }
            if (!RefreshDone)
                OnRefresh();
            }

        #region ContextMenu methods
        private void OnMenuOpening(object sender, CancelEventArgs e)
            {
            // -----------------------------------------------------
            // User has opened the source menu - populate it.
            // -----------------------------------------------------

            Control ControlUnderMouse = GetChildAtPoint(PointToClient(Cursor.Position));

            PopupMenu.Items.Clear();
            if (Mode == PageMode.Normal)
                {
                if (ControlUnderMouse != null && ControlUnderMouse is ChartUI)
                    {
                    ToolStripMenuItem ChartPropertiesMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Chart properties");
                    ChartPropertiesMenuItem.Click += OnChartPropertiesClick;
                    ChartPropertiesMenuItem.Tag = ControlUnderMouse;
                    PopupMenu.Items.Add(new ToolStripSeparator());
                    }
                ToolStripMenuItem ArrowMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Connections");
                ArrowMenuItem.Click += OnArrowClick;
                ToolStripMenuItem EditMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Edit");
                EditMenuItem.Click += OnEditClick;
                }
            else if (Mode == PageMode.Arrow)
                {
                if (ControlUnderMouse != null)
                    {
                    APSIMData ControlData = Data.Child(ControlUnderMouse.Name);
                    string[] DataSetNames = Processor.DataSetNames();
                    foreach (string DataSetName in DataSetNames)
                        {
                        if (DataSetName != ControlUnderMouse.Name)
                            {
                            ToolStripMenuItem MenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Link to " + DataSetName);
                            MenuItem.Checked = (ControlData.ChildByTypeAndValue("source", DataSetName) != null);
                            MenuItem.Click += OnAddLinkMenuItemClick;
                            MenuItem.Tag = DataSetName + " " + ControlUnderMouse.Name;
                            }
                        }
                    foreach (string SourceName in ControlData.get_Values("source"))
                        {
                        if (Array.IndexOf(DataSetNames, SourceName) == -1)
                            ControlData.DeleteNode(ControlData.ChildByTypeAndValue("source", SourceName));
                        }
                    }
                PopupMenu.Items.Add(new ToolStripSeparator());               
                ToolStripMenuItem ExitConnectionsMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Exit connections mode");
                ExitConnectionsMenuItem.Click += OnNormalClick;
                }
            e.Cancel = false;
            }
        private void OnControlAdded(object sender, ComponentEventArgs e)
            {
            // -----------------------------------------------------
            // User has inserted a new component
            // -----------------------------------------------------
            BaseView NewControl = (BaseView)e.Component;
            if (NewControl.Name != "")
                {
                string NewTypeName = Toolbox.ToolboxCtrl.SelectedItem.DisplayName;
                APSIMData NewChild = Data.Add(new APSIMData(NewTypeName, ""));
                NewChild.EnsureNameIsUnique2();
                if (NewTypeName.ToLower() != "page")
                    Processor.Add(NewChild.XML);

                NewControl.Parent = this;
                NewControl.Name = NewChild.Name;
                NewControl.OnLoad(Controller, NewChild.FullPath);
                NewControl.OnRefresh();
                }
            }
        private void OnRemoveMenuItem(object sender, EventArgs e)
            {
            // -----------------------------------------------------
            // User has clicked on remove menu item.
            // -----------------------------------------------------
            ISelectionService ss = (ISelectionService)FormDesigner.DesignerHost.GetService(typeof(ISelectionService));
            Control ComponentToRemove = (Control)ss.PrimarySelection;
            if (MessageBox.Show("Are you sure you want to remove " + ComponentToRemove.Name,
                                "Are you sure", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
                {
                ss.SetSelectedComponents(null);
                string ComponentNameToRemove = ComponentToRemove.Name;
                Data.Delete(ComponentNameToRemove);
                ComponentToRemove.Parent.Controls.Remove(ComponentToRemove);
                Processor.Erase(ComponentNameToRemove);
                Invalidate();
                }
            }
        private void OnRenameMenuItem(object sender, EventArgs e)
            {
            // -----------------------------------------------------
            // User has clicked on remove menu item.
            // -----------------------------------------------------
            ISelectionService Selections = (ISelectionService)FormDesigner.DesignerHost.GetService(typeof(ISelectionService));
            if (Selections.SelectionCount == 1)
                {
                BaseView ComponentToRename = (BaseView)Selections.PrimarySelection;
                string OldName = ComponentToRename.Name;
                string NewName = InputDialog.InputBox("Enter new name:",
                                                      "Rename", OldName, false);
                if (NewName != "" && NewName != OldName)
                    {
                    APSIMData RenamedChild = Data.Child(OldName);
                    RenamedChild.Name = NewName;
                    RenamedChild.EnsureNameIsUnique();
                    NewName = RenamedChild.Name;
                    ComponentToRename.Name = NewName;
                    ComponentToRename.Text = NewName;
                    Processor.Rename(OldName, NewName);
                    ComponentToRename.OnRefresh();
                    }
                }
            }
        private void OnNormalClick(object sender, EventArgs e)
            {
            IComponentChangeService iccs = (IComponentChangeService)FormDesigner.DesignerHost.GetService(typeof(IComponentChangeService));
            iccs.ComponentAdded -= OnControlAdded;
            FormDesigner.Active = false;
            Mode = PageMode.Normal;

            Toolbox.Visible = false;
            Invalidate();
            }
        private void OnArrowClick(object sender, EventArgs e)
            {
            FormDesigner.Active = false;
            Mode = PageMode.Arrow;
            Invalidate();
            }
        private void OnEditClick(object sender, EventArgs e)
            {
            FormDesigner.Active = true;
            Toolbox.Visible = true;
            Mode = PageMode.Design;
            IComponentChangeService iccs = (IComponentChangeService)FormDesigner.DesignerHost.GetService(typeof(IComponentChangeService));
            iccs.ComponentAdded += OnControlAdded;
            }
        private void OnChartPropertiesClick(object sender, EventArgs e)
            {
            ToolStripItem MenuItem = (ToolStripItem)sender;
            ChartUI ChartToEdit = (ChartUI)MenuItem.Tag;
            ChartToEdit.ChartEdit();
            }
        private void OnAddLinkMenuItemClick(object sender, EventArgs e)
            {
            // -----------------------------------------------------
            // User has clicked on an add link menu item.
            // -----------------------------------------------------
            ToolStripMenuItem MenuItem = (ToolStripMenuItem)sender;
            string[] TagNames = MenuItem.Tag.ToString().Split(' ');
            string SourceName = TagNames[0];
            string ThisControlName = TagNames[1];

            APSIMData ChildData = Data.Child(ThisControlName);
            if (MenuItem.Checked)
                ChildData.DeleteNode(ChildData.ChildByTypeAndValue("source", SourceName));
            else
                {
                APSIMData NewSourceNode = new APSIMData("source", "");
                NewSourceNode.Value = SourceName;
                ChildData.Add(NewSourceNode);
                }
            Invalidate();
            DoRefresh(ChildData);
            }
        #endregion

        #region Arrow stuff
        private void OnCanvasPaint(object sender, PaintEventArgs e)
            {
            //e.Graphics.DrawRectangle(SystemPens.ControlLightLight, Bounds);
            //e.Graphics.FillRectangle(SystemBrushes.ControlDark, Bounds /*e.ClipRectangle*/);
            //APSIMData GraphDataNode = Controller.ApsimData.Find(NodePath);
            //if (GraphDataNode.Attribute("EditMode") == "On")
            if (Data != null && Mode == PageMode.Arrow)
                DrawArrows(e);
            }
        private void DrawArrows(PaintEventArgs e)
            {
            foreach (APSIMData Child in Data.get_Children(null))
                foreach (APSIMData Source in Child.get_Children("source"))
                    {
                    if (Source.Value != "")
                        {
                        Control[] FromCtrl = Controls.Find(Source.Value, true);
                        Control[] ToCtrl = Controls.Find(Child.Name, true);
                        if (FromCtrl.Length == 1 && ToCtrl.Length == 1)
                            DrawArrowBetweenControls(FromCtrl[0], ToCtrl[0], e);
                        }
                    }

            }
        private void DrawArrowBetweenControls(Control FromCtrl, Control ToCtrl, PaintEventArgs e)
            {
            float x1, y1, x2, y2;
            if (FromCtrl.Top <= ToCtrl.Top)
                {
                y1 = FromCtrl.Bottom;
                y2 = ToCtrl.Top;
                if (ToCtrl.Left > FromCtrl.Right)
                    {
                    x1 = FromCtrl.Right;
                    x2 = ToCtrl.Left;
                    }
                else if (ToCtrl.Left > FromCtrl.Left)
                    {
                    x1 = FromCtrl.Left;
                    x2 = ToCtrl.Left;
                    }
                else
                    {
                    x1 = FromCtrl.Left;
                    x2 = ToCtrl.Right;
                    }

                }
            else
                {
                y1 = FromCtrl.Top;
                y2 = ToCtrl.Bottom;
                if (ToCtrl.Left > FromCtrl.Right)
                    {
                    x1 = FromCtrl.Right;
                    x2 = ToCtrl.Left;
                    }
                else if (ToCtrl.Left > FromCtrl.Left)
                    {
                    x1 = FromCtrl.Left;
                    x2 = ToCtrl.Left;
                    }
                else
                    {
                    x1 = FromCtrl.Left;
                    x2 = ToCtrl.Right;
                    }
                }
            if (!FromCtrl.Visible)
                {
                x1 = 1;
                y1 = 1;
                }
            Arrow.DrawArrow(e.Graphics, Pens.Blue, Brushes.LightBlue, x1, y1, x2, y2);
            }
        #endregion



        }
    }

