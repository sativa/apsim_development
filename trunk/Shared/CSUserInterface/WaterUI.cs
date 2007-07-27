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

namespace CSUserInterface
    {
    public partial class WaterUI : VBUserInterface.BaseView
        {
        private BaseController Controller;
        private string NodePath;
        private Soil MySoil;
        private static int NUMBER_OF_STATIC_COLS = 7;
        private bool UserChange = true;

        public WaterUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller)
            {
            this.Controller = Controller;
            FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
            }
        public override void OnClose()
            {
            }
        override public void OnRefresh(string NodePath)
            {
            this.NodePath = NodePath;
            MySoil = new Soil(Controller.Data.Parent);

            HelpText = "";
            WaterChartControl.LinkedSoil = MySoil;
            PopulateWaterGrid();
            OperationMode mode = OperationMode.Normal;
            if (Controller.ApsimData.IsReadOnly)
                mode = OperationMode.ReadOnly;

            Water.OperationMode = mode;
            }


        #region Water Grid
        private void PopulateWaterGrid()
            {
            UserChange = false;
            Water.ClearRange(0, 0, Water.RowCount, Water.ColumnCount, false);
            GridUtils.SetColumnAsStrings(Water, 0, Soils.Utility.ToDepthStrings(MySoil.Thickness));
            GridUtils.SetColumnAsDoubles(Water, 1, MySoil.BD);
            GridUtils.SetColumnAsDoubles(Water, 2, MySoil.Rocks);
            GridUtils.SetColumnAsDoubles(Water, 3, MySoil.SAT);
            GridUtils.SetColumnAsDoubles(Water, 4, MySoil.DUL);
            GridUtils.SetColumnAsDoubles(Water, 5, MySoil.Airdry);
            GridUtils.SetColumnAsDoubles(Water, 6, MySoil.LL15);

            // Make sure we have the right number of crop columns.
            string[] CropNames = MySoil.Crops;
            int RequiredNumberOfColumns = CropNames.Length * 4 + NUMBER_OF_STATIC_COLS;
            Water.ColumnCount = RequiredNumberOfColumns;

            // Fill all crop columns with numbers.
            for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
                {
                int CropCol = NUMBER_OF_STATIC_COLS + CropNumber * 4;
                string CropName = CropNames[CropNumber];
                Color CropColor = Color.GreenYellow;
                if (CropNumber / 2.0 != CropNumber / 2)
                    CropColor = Color.PaleGreen;
                Color PredCropColor = Color.Pink;
                if (CropNumber / 2.0 != CropNumber / 2)
                    PredCropColor = Color.LightPink;

                // setup the LL column.
                bool Predicted = MySoil.CropIsPredicted(CropName);
                if (Predicted)
                    {
                    Water.ColumnHeader.Cells[0, CropCol].Text = "Predicted " + CropName;
                    FarPoint.Win.Spread.CellType.NumberCellType PredLLFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
                    PredLLFormatter.DecimalPlaces = 2;
                    Water.Columns[CropCol].CellType = PredLLFormatter;
                    }
                else
                    {
                    Water.ColumnHeader.Cells[0, CropCol].Text = CropName;
                    Water.Columns[CropCol].CellType = null;
                    }
                Water.ColumnHeader.Cells[0, CropCol].ColumnSpan = 4;
                Water.ColumnHeader.Cells[0, CropCol].HorizontalAlignment = CellHorizontalAlignment.Center;
                Water.ColumnHeader.Cells[1, CropCol].Text = "LL";
                Water.ColumnHeader.Cells[2, CropCol].Text = "mm/mm";
                GridUtils.SetColumnAsDoubles(Water, CropCol, MySoil.LL(CropName));

                if (Predicted)
                    {
                    Water.Columns[CropCol].BackColor = PredCropColor;
                    Water.Columns[CropCol].Locked = true;
                    }
                else
                    Water.Columns[CropCol].BackColor = CropColor;
                Water.Columns[CropCol].Width = 60;

                // setup the PAWC column.
                Water.ColumnHeader.Cells[1, CropCol + 1].Text = "PAWC";
                Water.ColumnHeader.Cells[2, CropCol + 1].Text = "(mm)";
                Water.Columns[CropCol + 1].Locked = true;
                if (Predicted)
                    Water.Columns[CropCol + 1].BackColor = PredCropColor;
                else
                    Water.Columns[CropCol + 1].BackColor = CropColor;
                Water.Columns[CropCol + 1].Width = 45;
                RefreshPAWCColumn(CropCol + 1);
                FarPoint.Win.Spread.CellType.NumberCellType PAWCFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
                PAWCFormatter.DecimalPlaces = 1;
                Water.Columns[CropCol + 1].CellType = PAWCFormatter;

                // setup the KL column
                Water.ColumnHeader.Cells[1, CropCol + 2].Text = "KL";
                Water.ColumnHeader.Cells[2, CropCol + 2].Text = " ";
                GridUtils.SetColumnAsDoubles(Water, CropCol + 2, MySoil.KL(CropName));
                if (Predicted)
                    {
                    Water.Columns[CropCol + 2].BackColor = PredCropColor;
                    Water.Columns[CropCol + 2].Locked = true;
                    }
                else
                    Water.Columns[CropCol + 2].BackColor = CropColor;
                Water.Columns[CropCol + 2].Width = 45;

                // setup the XF column
                Water.ColumnHeader.Cells[1, CropCol + 3].Text = "XF";
                Water.ColumnHeader.Cells[2, CropCol + 3].Text = " ";
                GridUtils.SetColumnAsDoubles(Water, CropCol + 3, MySoil.XF(CropName));
                if (Predicted)
                    {
                    Water.Columns[CropCol + 3].BackColor = PredCropColor;
                    Water.Columns[CropCol + 3].Locked = true;
                    }
                else
                    Water.Columns[CropCol + 3].BackColor = CropColor;
                Water.Columns[CropCol + 3].Width = 45;
                }

            AddSummaryRow();
            UserChange = true;
            }
        private void SaveWaterGrid()
            {
            for (int col = 0; col != Water.ColumnCount; col++)
                SaveWaterGridColumn(col);
            }
        private void SaveWaterGridColumn(int ColumnIndex)
            {
            if (ColumnIndex == 0)
                AddSummaryRow();
            int NumLayers = GridUtils.FindFirstBlankCell(Water, 0);
            switch (ColumnIndex)
                {
            case 0: MySoil.Thickness = Soils.Utility.ToThickness(GridUtils.GetColumnAsStrings(Water, 0, NumLayers)); break;
            case 1: MySoil.BD = GridUtils.GetColumnAsDoubles(Water, 1, NumLayers); break;
            case 2: MySoil.Rocks = GridUtils.GetColumnAsDoubles(Water, 2, NumLayers); break;
            case 3: MySoil.SAT = GridUtils.GetColumnAsDoubles(Water, 3, NumLayers); break;
            case 4: MySoil.DUL = GridUtils.GetColumnAsDoubles(Water, 4, NumLayers); RefreshPAWCColumns(); break;
            case 5: MySoil.Airdry = GridUtils.GetColumnAsDoubles(Water, 5, NumLayers); break;
            case 6: MySoil.LL15 = GridUtils.GetColumnAsDoubles(Water, 6, NumLayers); break;
            default:
                {
                int CropCol = (ColumnIndex - NUMBER_OF_STATIC_COLS) / 4 * 4 + NUMBER_OF_STATIC_COLS;
                string CropName = Water.ColumnHeader.Cells[0, CropCol].Text;
                if (!CropName.StartsWith("Predicted "))
                    {
                    string[] CropNames = MySoil.Crops;
                    // Save all crop columns
                    double[] ll = GridUtils.GetColumnAsDoubles(Water, CropCol, NumLayers);
                    double[] kl = GridUtils.GetColumnAsDoubles(Water, CropCol + 2, NumLayers);
                    double[] xf = GridUtils.GetColumnAsDoubles(Water, CropCol + 3, NumLayers);
                    MySoil.SetCrop(CropName, ll, kl, xf);
                    RefreshPAWCColumn(CropCol + 1);
                    }
                break;
                }
                }


            WaterChartControl.RefreshView();
            }
        private void RefreshPAWCColumns()
            {
            for (int col = 0; col != Water.ColumnCount; col++)
                if (Water.ColumnHeader.Cells[1, col].Text == "PAWC")
                    RefreshPAWCColumn(col);
            }
        private void RefreshPAWCColumn(int ColumnIndex)
            {
            string CropName = Water.ColumnHeader.Cells[0, ColumnIndex - 1].Text.Replace("Predicted ", "");
            double[] paw = MySoil.PAWC(CropName);
            GridUtils.SetColumnAsDoubles(Water, ColumnIndex, paw);
            Water.Columns[ColumnIndex].Locked = true;
            AddSummaryRow();
            }
        private void AddSummaryRow()
            {
            int NumDepths = GridUtils.FindFirstBlankCell(Water, 0);
            if (NumDepths > 0)
                {
                int SummaryRow = Water.RowCount - 1;

                // set the blank row.
                Water.Cells[SummaryRow, 0, SummaryRow, Water.ColumnCount - 1].Formula = "";
                Water.Cells[SummaryRow, 0, SummaryRow, Water.ColumnCount - 1].Text = "";
                Water.Rows[SummaryRow].BackColor = Color.White;

                // set the summary row.
                Water.Cells[SummaryRow, 0].Text = "Totals";
                for (int col = NUMBER_OF_STATIC_COLS + 1; col < Water.ColumnCount; col += 4)
                    {
                    int ColForFormula = col + 1;
                    int EndRowForFormula = SummaryRow - 1;
                    string SumFormula = "SUM(R1C" + ColForFormula.ToString() + ":R" + EndRowForFormula.ToString() + "C" + ColForFormula.ToString() + ")";
                    Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
                    Water.Cells[SummaryRow, col].Formula = SumFormula;
                    }
                Water.Rows[SummaryRow].BackColor = Color.Yellow;
                }
            }
        private void OnWaterCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                SaveWaterGridColumn(e.Column);
                UserChange = true;
                }
            }
        #endregion


        #region Printing Methods
        private int CurrentStartCol;
        private int SlotHeight;
        private int NumSlots = 3;
        private int NumFixedCols = 6;
        private int NumFreeColsToPrint = 8;
        private int MarginBetweenSlots = 0;
        public void OnBeginPrint(object sender, System.Drawing.Printing.PrintEventArgs e)
            {
            CurrentStartCol = NumFixedCols;
            SlotHeight = 0;
            }
        public void OnPrintPage(object sender, System.Drawing.Printing.PrintPageEventArgs e)
            {
            if (SlotHeight == 0)
                SlotHeight = CalcSlotHeight(e);

            e.Graphics.Clip = new Region(e.MarginBounds);
            e.HasMorePages = true;
            for (int slot = 0; slot < NumSlots && e.HasMorePages; slot++)
                {
                Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top + slot * (SlotHeight + MarginBetweenSlots),
                                            e.MarginBounds.Right - e.MarginBounds.Left,
                                            SlotHeight);
                if (CurrentStartCol >= Water.ColumnCount)
                    {
                    // print the graph.
                    PrintForm.PrintControl(e.Graphics, r, WaterChartControl.WaterChart, 1.0F);

                    e.HasMorePages = false;
                    }
                else
                    {
                    PrintForm.PrintControl(e.Graphics, r, Grid, 1.0F);
                    CurrentStartCol += NumFreeColsToPrint;
                    }
                }
            e.Graphics.ResetClip();
            }

        private int CalcSlotHeight(System.Drawing.Printing.PrintPageEventArgs e)
            {
            Water.PrintInfo.ColStart = 0;
            Water.PrintInfo.ColEnd = 6;
            Water.PrintInfo.RowStart = 0;
            Water.PrintInfo.RowEnd = GridUtils.FindFirstBlankCell(Water, 0) - 1;
            Water.PrintInfo.PrintType = PrintType.CellRange;
            Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top,
                                        e.MarginBounds.Width, 200);
            do
                {
                r.Height = r.Height + 10;
                }
            while (Grid.GetOwnerPrintPageCount(e.Graphics, r, 0) > 1);
            return r.Height;
            }

        public void OnPreDraw(object sender, TMGDevelopment.Windows.Forms.PreDrawEventArgs e)
            {
            if (e.Control == Grid)
                {
                Water.PrintInfo.ColStart = CurrentStartCol;
                Water.PrintInfo.ColEnd = CurrentStartCol + NumFreeColsToPrint - 1;
                Water.PrintInfo.PrintType = PrintType.CellRange;
                Grid.OwnerPrintDraw(e.Graphics, e.Bounds, 0, 1);
                e.OwnerDrawn = true;
                }
            else if (e.Control == WaterChartControl.WaterChart)
                {
                Bitmap b = new Bitmap(e.Bounds.Width, e.Bounds.Height);
                WaterChartControl.WaterChart.DrawToBitmap(b, e.Bounds);
                e.Graphics.DrawImage(b, e.Bounds);
                e.OwnerDrawn = true;
                }
            }
        #endregion

        }
    }

