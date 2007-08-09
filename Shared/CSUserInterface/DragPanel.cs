using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CSUserInterface
    {
    public partial class DragPanel : Panel
        {
        private int PosX;
        private int PosY;
        private bool IsDraggable;
        private Control ControlBeingDragged;

        public DragPanel()
            {
            InitializeComponent();
            }

        public bool DraggableControls
            {
            get { return IsDraggable; }
            set
                {
                IsDraggable = value;
                if (IsDraggable)
                    {
                    foreach (Control Child in Controls)
                        {
                        Control DragBar = GetDragBar(Child);
                        DragBar.MouseDown += new MouseEventHandler(OnMouseDown);
                        }
                    }
                else
                    {
                    foreach (Control Child in Controls)
                        {
                        Control DragBar = GetDragBar(Child);
                        DragBar.MouseDown -= new MouseEventHandler(OnMouseDown);
                        }
                    }
                }
            }
        public bool IsDragging
            {
            get { return ControlBeingDragged != null; }
            }

        private Control GetDragBar(Control Ctrl)
            {
            foreach (Control Child in Ctrl.Controls)
                if (Child.Name == "DragBar")
                    return Child;
            return Ctrl;
            }

        private void OnMouseDown(object sender, MouseEventArgs e)
            {
            ControlBeingDragged = (Control) sender;
            if (ControlBeingDragged.Name == "DragBar")
                ControlBeingDragged = ControlBeingDragged.Parent;                
            DoDragDrop(sender, DragDropEffects.Move);
            }

        private void OnDragEnter(object sender, DragEventArgs e)
            {
            e.Effect = DragDropEffects.Move;
            }

        private void OnDragOver(object sender, DragEventArgs e)
            {
            Point clientp = PointToClient(new Point(e.X, e.Y));
            PosX = clientp.X;
            PosY = clientp.Y;
            ControlBeingDragged.SetBounds(PosX, PosY, ControlBeingDragged.Width, ControlBeingDragged.Height);
            }

        private void OnDragDrop(object sender, DragEventArgs e)
            {
            ControlBeingDragged.SetBounds(PosX, PosY, ControlBeingDragged.Width, ControlBeingDragged.Height);
            ControlBeingDragged = null;
            Invalidate();
            }


        }
    }
