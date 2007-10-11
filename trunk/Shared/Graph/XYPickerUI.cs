using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;
using FarPoint.Win.Spread.CellType;

namespace Graph
    {
    public partial class XYPickerUI : BaseView
        {
        private APSIMData XYNode;
        private bool InRefresh = false;
        private ChartPageUI ParentUI;

        public XYPickerUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();

            GroupBox.Text = Name;
            InRefresh = true;
            XYNode = Controller.ApsimData.Find(NodePath);
            string DataSource = XYNode.get_ChildValue("Source");
            if (DataSource != "")
                {
                string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);

                // setup the x drop down
                XDropDown.Items.Clear();
                XDropDown.Items.AddRange(FieldNames);
                XDropDown.Text = XYNode.get_ChildValue("X");


                // setup the Y lists.
                SetupYCombo(Y1, Y1Right, FieldNames, 0);
                SetupYCombo(Y2, Y2Right, FieldNames, 1);
                SetupYCombo(Y3, Y3Right, FieldNames, 2);
                SetupYCombo(Y4, Y4Right, FieldNames, 3);
                SetupYCombo(Y5, Y5Right, FieldNames, 4);

                // setup the radio buttons
                TypeCombo.Text = XYNode.get_ChildValue("SeriesType");
                PointCombo.Text = XYNode.get_ChildValue("PointType");
                }
            InRefresh = false;
            }

        private void SetupYCombo(ComboBox YCombo, CheckBox YRight, 
                                 string[] FieldNames, int Index)
            {
            string[] YFieldNames = XYNode.get_Values("Y");
            string[] Y2FieldNames = XYNode.get_Values("YRight");

            YCombo.Items.Clear();
            YCombo.Items.AddRange(FieldNames);
            if (YFieldNames.Length > Index)
                {
                YCombo.Text = YFieldNames[Index];
                YRight.Checked = false;
                }
            else if (Y2FieldNames.Length > Index)
                {
                YCombo.Text = Y2FieldNames[Index];
                YRight.Checked = true;
                }

            }

        private void OnXChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XYNode.set_ChildValue("X", XDropDown.Text);
                ParentUI.DoRefresh(XYNode);
                }
            }
        private void OnTypeChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XYNode.set_ChildValue("SeriesType", TypeCombo.Text);
                PointCombo.Enabled = (TypeCombo.Text != "Bar");
                ParentUI.DoRefresh(XYNode);
                }
            }
        private void OnPointChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                XYNode.set_ChildValue("PointType", PointCombo.Text);
                ParentUI.DoRefresh(XYNode);
                }
            }


        private void OnYChanged(object sender, EventArgs e)
            {
            if (!InRefresh)
                {
                List<string> Values = new List<string>();
                if (Y1.Text != "" && !Y1Right.Checked)
                    Values.Add(Y1.Text);
                if (Y2.Text != "" && !Y2Right.Checked)
                    Values.Add(Y2.Text);
                if (Y3.Text != "" && !Y3Right.Checked)
                    Values.Add(Y3.Text);
                if (Y4.Text != "" && !Y4Right.Checked)
                    Values.Add(Y4.Text);
                if (Y5.Text != "" && !Y5Right.Checked)
                    Values.Add(Y5.Text);
                string[] YValues = new string[Values.Count];
                Values.CopyTo(YValues);
                XYNode.set_Values("Y", YValues);

                Values.Clear();
                if (Y1.Text != "" && Y1Right.Checked)
                    Values.Add(Y1.Text);
                if (Y2.Text != "" && Y2Right.Checked)
                    Values.Add(Y2.Text);
                if (Y3.Text != "" && Y3Right.Checked)
                    Values.Add(Y3.Text);
                if (Y4.Text != "" && Y4Right.Checked)
                    Values.Add(Y4.Text);
                if (Y5.Text != "" && Y5Right.Checked)
                    Values.Add(Y5.Text);
                string[] Y2Values = new string[Values.Count];
                Values.CopyTo(Y2Values);
                XYNode.set_Values("YRight", Y2Values);
                ParentUI.DoRefresh(XYNode);
                }
            }

        }
    }

