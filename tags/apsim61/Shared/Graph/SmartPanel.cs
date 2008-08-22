using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;
using CSUserInterface;
using System.IO;

namespace Graph
    {
    public partial class SmartPanel : GraphView
        {
        private APSIMData GraphTools;
        private ArrowRenderer Arrow = new ArrowRenderer(10, 1f, true);
        private APSIMData GraphDataNode;

        public SmartPanel()
            {
            InitializeComponent();
            }
        public Color CanvasColour
            {
            get { return Canvas.BackColor; }
            set { Canvas.BackColor = value; }
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);

            Canvas.Controls.Clear();

            string GraphToolsFileName = APSIMSettings.ApsimDirectory() + "\\ApsimUI\\graph.xml";
            GraphTools = new APSIMData();
            GraphTools.LoadFromFile(GraphToolsFileName);
            GraphTools = GraphTools.Child("Low level components");
            GraphDataNode = Controller.ApsimData.Find(NodePath);

            //Special case where this dataui is dropped on an outputfile.
            //We want to give the filename to the child outputfile automatically.
            GraphDataNode = Controller.ApsimData.Find(NodePath);
            if (GraphDataNode.Parent.Type.ToLower() == "outputfile" &&
                GraphDataNode.Child("outputfile") != null)
                {
                string FullFileName = Path.GetDirectoryName(Controller.FileName) + "\\" + BaseActions.CalcFileName(GraphDataNode.Parent);
                GraphDataNode.Child("outputfile").set_ChildValue("FileName", FullFileName);
                }
            Canvas.OnAdded += OnAdded;
            Canvas.OnDeleted += OnDeleted;
            Canvas.OnRenamed += OnRenamed;
            Canvas.OnMenuPopup += OnMenuPopup;
            Canvas.OnLoad(Controller, NodePath, GraphTools);
            }
        public override void OnRefresh()
            {
            base.OnRefresh();
            Canvas.OnRefresh();
            }
        public override void OnSave()
            {
            base.OnSave();
            Canvas.OnAdded -= OnAdded;
            Canvas.OnDeleted -= OnDeleted;
            Canvas.OnRenamed -= OnRenamed;
            Canvas.OnMenuPopup -= OnMenuPopup;
            Canvas.OnSave();
            }
        private void OnAdded(APSIMData Data, GraphView NewView)
            {
            NewView.Processor = Processor;
            if (Data.Type.ToLower() != "page")
                Processor.Add(Data.XML);
            }
        private void OnDeleted(string Name)
            {
            Processor.Erase(Name);
            }
        private void OnRenamed(string OldName, string NewName)
            {
            Processor.Rename(OldName, NewName);
            }
        private void OnMenuPopup(ContextMenuStrip Menu, Control ControlUnderMouse)
            {
            if (Dock != DockStyle.None)
                return;
            if (ControlUnderMouse == null)
                {
                ToolStripMenuItem DragableMenuItem = (ToolStripMenuItem)Menu.Items.Add("Dragable");
                DragableMenuItem.Click += OnDragableMenuItem;
                }
            else
                {
                APSIMData ThisControlData = GraphDataNode.Child(ControlUnderMouse.Name);
                foreach (APSIMData Source in ThisControlData.get_Children("source"))
                    {
                    ToolStripMenuItem LinkMenuItem = (ToolStripMenuItem)Menu.Items.Add(Source.Name + " link");
                    ToolStripDropDownMenu LinkMenu = new ToolStripDropDownMenu();
                    LinkMenuItem.DropDown = LinkMenu;

                    foreach (APSIMData Child in GraphDataNode.get_Children(null))
                        {
                        if (Child.Name != ControlUnderMouse.Name)
                            {
                            ToolStripMenuItem MenuItem = (ToolStripMenuItem)LinkMenu.Items.Add(Child.Name);
                            MenuItem.Checked = (Child.Name == Source.Value);
                            MenuItem.Click += OnAddLinkMenuItemClick;
                            MenuItem.Tag = Source.Name + " " + ControlUnderMouse.Name;
                            }
                        }
                    }
                }

            }

        private void OnDragableMenuItem(object sender, EventArgs e)
            {
            // -----------------------------------------------------
            // User has clicked on dragable menu item.
            // -----------------------------------------------------
            ToolStripMenuItem MenuItem = (ToolStripMenuItem)sender;
            }

        private void OnCanvasPaint(object sender, PaintEventArgs e)
            {
            //e.Graphics.DrawRectangle(SystemPens.ControlLightLight, Bounds);
            //e.Graphics.FillRectangle(SystemBrushes.Window, e.ClipRectangle);
            //APSIMData GraphDataNode = Controller.ApsimData.Find(NodePath);
            //if (GraphDataNode.Attribute("EditMode") == "On")
            //    DrawArrows(e);
            }
        private void DrawArrows(PaintEventArgs e)
            {
            foreach (APSIMData Child in GraphDataNode.get_Children(null))
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

        private void OnAddLinkMenuItemClick(object sender, EventArgs e)
            {
            // -----------------------------------------------------
            // User has clicked on an add link menu item.
            // -----------------------------------------------------
            ToolStripMenuItem MenuItem = (ToolStripMenuItem)sender;
            string[] TagNames = MenuItem.Tag.ToString().Split(' ');
            string SourceName = TagNames[0];
            string ThisControlName = TagNames[1];

            APSIMData Data = GraphDataNode.Child(ThisControlName);
            if (MenuItem.Checked)
                Data.Child(SourceName).Value = "";
            else
                Data.Child(SourceName).Value = MenuItem.Text;
            Invalidate();
            DoRefresh(Data);
            }

        private void button1_Click(object sender, EventArgs e)
            {
            formDesigner1.StartDesign();
            }



        }
    }

