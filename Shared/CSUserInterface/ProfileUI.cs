using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using Soils;
using FarPoint.Win.Spread;
using VBGeneral;

namespace CSUserInterface
    {
    public partial class ProfileUI : VBUserInterface.BaseView
        {
        private Soil MySoil = null;
        //private string ComponentType;
        //private APSIMData FieldInfo;
        public ProfileUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
            }
        public override void OnClose()
            {
            }
		override public void OnRefresh()
			{
            //APSIMData Component = Controller.ApsimData.Find(NodePath);
            //if (Component != null && Component.Parent != null)
            //    {
            //    ComponentType = Component.Type;
            //    APSIMData TypeInfo = Controller.GetComponentTypeInfo(ComponentType);
            //    if (TypeInfo != null && TypeInfo.Child("fields") != null)
            //        {
            //        FieldInfo = TypeInfo.Child("fields");
            //        MySoil = new Soil(Component.Parent);
            //        PopulateProfileGrid();
            //        OperationMode mode = OperationMode.Normal;
            //        if (Controller.ApsimData.IsReadOnly)
            //            mode = OperationMode.ReadOnly;
        				
            //        SoilProfile.OperationMode = mode;
            //        }
            //    }
            }

        private void PopulateProfileGrid()
            {
            //UserChange = false;
            //APSIMData[] Fields = FieldInfo.get_Children(null);

            //SoilProfile.RowCount = MySoil.Thickness.Length;
            //SoilProfile.ColCount = Fields.Length + 1;
            //SoilProfile.ClearRange(0, 0, SoilProfile.RowCount, SoilProfile.ColumnCount, true);

            //GridUtils.SetColumnAsStrings(SoilProfile, 0, Soils.Utility.ToDepthStrings(MySoil.Thickness));
            //for (int Col = 1; Col != SoilProfile.ColumnCount; Col++)
            //    {
            //    SoilProfile.
            //    GridUtils.SetColumnAsStrings(SoilProfile, Col, MySoil.Values(Fields[Col-1]));
            //    }
            //GridUtils.SetColumnAsStrings(SoilProfile, 0, Soils.Utility.ToDepthStrings(MySoil.Thickness));
            //GridUtils.SetColumnAsStrings(SoilProfile, 1, MySoil.Texture);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 2, MySoil.SWCON);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 3, MySoil.MWCON);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 4, MySoil.FBIOM);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 5, MySoil.FINERT);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 6, MySoil.KS);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 7, MySoil.OC);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 8, MySoil.EC);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 10, MySoil.CL);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 11, MySoil.Boron);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 12, MySoil.CEC);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 13, MySoil.Ca);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 14, MySoil.Mg);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 15, MySoil.Na);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 16, MySoil.K);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 17, MySoil.ESP);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 18, MySoil.ParticleSizeSand);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 19, MySoil.ParticleSizeSilt);
            //GridUtils.SetColumnAsDoubles(SoilProfile, 20, MySoil.ParticleSizeClay);
            //if (MySoil.PHStoredAsWater())
            //    {
            //    SoilProfile.ColumnHeader.Cells[1, 9].Text = "(water)";
            //    GridUtils.SetColumnAsDoubles(SoilProfile, 9, MySoil.PH);
            //    }
            //else
            //    {
            //    SoilProfile.ColumnHeader.Cells[1, 9].Text = "(CaCl)";
            //    GridUtils.SetColumnAsDoubles(SoilProfile, 9, MySoil.PHCaCl);
            //    UserChange = true;
            //    }
            }
        private void SaveProfileGrid(int ColumnIndex)
            {
            int NumLayers = GridUtils.FindFirstBlankCell(SoilProfile, 0);
            switch (ColumnIndex)
                {
            case 1: MySoil.Texture = GridUtils.GetColumnAsStrings(SoilProfile, 1, NumLayers); break;
            case 2: MySoil.SWCON = GridUtils.GetColumnAsDoubles(SoilProfile, 2, NumLayers); break;
            case 3: MySoil.MWCON = GridUtils.GetColumnAsDoubles(SoilProfile, 3, NumLayers); break;
            case 4: MySoil.FBIOM = GridUtils.GetColumnAsDoubles(SoilProfile, 4, NumLayers); break;
            case 5: MySoil.FINERT = GridUtils.GetColumnAsDoubles(SoilProfile, 5, NumLayers); break;
            case 6: MySoil.KS = GridUtils.GetColumnAsDoubles(SoilProfile, 6, NumLayers); break;
            case 7: MySoil.OC = GridUtils.GetColumnAsDoubles(SoilProfile, 7, NumLayers); break;
            case 8: MySoil.EC = GridUtils.GetColumnAsDoubles(SoilProfile, 8, NumLayers); break;
            case 9: if (SoilProfile.ColumnHeader.Cells[1, 9].Text == "(water)")
                MySoil.PH = GridUtils.GetColumnAsDoubles(SoilProfile, 9, NumLayers);
            else
                MySoil.PHCaCl = GridUtils.GetColumnAsDoubles(SoilProfile, 9, NumLayers);
            break;
            case 10: MySoil.CL = GridUtils.GetColumnAsDoubles(SoilProfile, 10, NumLayers); break;
            case 11: MySoil.Boron = GridUtils.GetColumnAsDoubles(SoilProfile, 11, NumLayers); break;
            case 12: MySoil.CEC = GridUtils.GetColumnAsDoubles(SoilProfile, 12, NumLayers); break;
            case 13: MySoil.Ca = GridUtils.GetColumnAsDoubles(SoilProfile, 13, NumLayers); break;
            case 14: MySoil.Mg = GridUtils.GetColumnAsDoubles(SoilProfile, 14, NumLayers); break;
            case 15: MySoil.Na = GridUtils.GetColumnAsDoubles(SoilProfile, 15, NumLayers); break;
            case 16: MySoil.K = GridUtils.GetColumnAsDoubles(SoilProfile, 16, NumLayers); break;
            case 17: MySoil.ESP = GridUtils.GetColumnAsDoubles(SoilProfile, 17, NumLayers); break;
            case 18: MySoil.ParticleSizeSand = GridUtils.GetColumnAsDoubles(SoilProfile, 18, NumLayers); break;
            case 19: MySoil.ParticleSizeSilt = GridUtils.GetColumnAsDoubles(SoilProfile, 19, NumLayers); break;
            case 20: MySoil.ParticleSizeClay = GridUtils.GetColumnAsDoubles(SoilProfile, 20, NumLayers); break;
                }
            }
        private void OnProfileCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            //if (UserChange)
            //    {
            //    UserChange = false;
            //    SaveProfileGrid(e.Column);
            //    UserChange = true;
            //    }
            }

        }
    }

