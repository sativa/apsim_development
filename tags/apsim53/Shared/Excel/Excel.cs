using System;
using System.Data;
using System.Collections.Specialized;
using Microsoft.Office.Core;
using CSGeneral;
using System.IO;

namespace ExcelUtility
	{
	public class ExcelHelper
		{

		static public DataTable GetDataFromSheet(string FileName, string SheetName)
			{
			Excel.Application ExcelApp = null;
			Excel.Workbook Workbook = null;
			Excel.Worksheet Sheet = null;
			DataTable Table = null;

			try
				{
				ExcelApp = new Excel.Application();
				if (ExcelApp == null)
					throw new Exception("Cannot find Excel.");

				Workbook = ExcelApp.Workbooks.Open(FileName, 
					Type.Missing, Type.Missing, Type.Missing, Type.Missing, Type.Missing, 
					Type.Missing, Type.Missing, Type.Missing, Type.Missing, Type.Missing, 
					Type.Missing, Type.Missing, Type.Missing, Type.Missing);
				if (Workbook == null)
					throw new Exception("Cannot open spreadsheet: " + FileName);

				// Go find the right sheet.
				for (int PageIndex = 1; PageIndex <= Workbook.Worksheets.Count; PageIndex++)
					{
					Sheet = (Excel.Worksheet) Workbook.Worksheets.get_Item(PageIndex);
					if (Sheet.Name.ToLower() == SheetName.ToLower())
						break;
					}
				if (Sheet.Name.ToLower() != SheetName.ToLower())
					throw new Exception("Cannot find sheet: " + SheetName + " in spreadsheet file: " + FileName);

                Excel.Range r = Sheet.UsedRange;
                object[,] values = (object[,])r.Value2;
                int NumColumns = GetNumColumns(values);
                int NumRows = GetNumRows(values);

                // ok now we want to read in all rows of data and fill up our data table.
                Table = new DataTable();
                for (int i=1; i <= NumColumns; i++)
                    Table.Columns.Add(values[1, i].ToString());
                for (int RowIndex = 2; RowIndex <= NumRows; RowIndex++)
                    {
                    DataRow Row = Table.NewRow();
                    for (int ColIndex = 1; ColIndex <= NumColumns; ColIndex++)
                        {
                        if (values[RowIndex, ColIndex] != null)
                            Row[ColIndex - 1] = values[RowIndex, ColIndex].ToString();
                        }
                    Table.Rows.Add(Row);
                    }
				}
			catch (Exception)
				{
				if (ExcelApp != null)
					ExcelApp.Quit();
				Sheet = null;
				Workbook = null;
				ExcelApp = null;
				throw;
				}

			ExcelApp.Quit();
			Sheet = null;
			Workbook = null;
			ExcelApp = null;
			return Table;
			}

        private static int GetNumRows(object[,] values)
            {
            for (int Row = 1; Row <= values.GetUpperBound(0); Row++)
                {
                if (values[Row, 1] == null ||
                    values[Row, 1].ToString().Replace(" ", "") == "")
                    return Row - 1;
                }
            return values.GetUpperBound(0);
            }

        private static int GetNumColumns(object[,] values)
            {
            for (int Col = 1; Col <= values.GetUpperBound(1); Col++)
                {
                if (values[1, Col] == null ||
                    values[1, Col].ToString().Replace(" ", "") == "")
                    return Col - 1;
                }
            return values.GetUpperBound(1);
            }


        static public void SendDataToSheet(string FileName, string SheetName, DataTable Table)
			{
			if (File.Exists(FileName))
				throw new Exception("File '" + FileName + "' already exists.");
			Excel.Application ExcelApp = null;
			Excel.Workbook Workbook = null;
			Excel.Worksheet WorkSheet = null;

			try
				{
				ExcelApp = new Excel.Application();
				if (ExcelApp == null)
					throw new Exception("Cannot find Excel.");

				Workbook = ExcelApp.Workbooks.Add(Excel.XlWBATemplate.xlWBATWorksheet);
				WorkSheet = (Excel.Worksheet) Workbook.Worksheets.Add(
                                             Type.Missing, 
                                             Type.Missing, 
                                             Type.Missing, 
                                             Type.Missing);
				WorkSheet.Name = SheetName;

				// ok now we want send all data to sheet.
				ExcelApp.SheetsInNewWorkbook = 1;

                //object[,] values = new object[Table.Rows.Count+1, Table.Columns.Count];
                int[] Lengths = {Table.Rows.Count+1, Table.Columns.Count+1};
                int[] LowerBounds = { 1, 1 };
                object[,] values = (object[,])Array.CreateInstance(typeof(object), Lengths, LowerBounds);
                
                for (int Col = 0; Col != Table.Columns.Count; Col++)
                    values[1, Col+1] = Table.Columns[Col].ToString();

                for (int Row = 0; Row != Table.Rows.Count; Row++)
                    for (int Col = 0; Col != Table.Columns.Count; Col++)
                        values[Row + 2, Col + 1] = Table.Rows[Row][Col].ToString();

                object TopLeft = WorkSheet.Cells[1, 1];
                object BottomRight = WorkSheet.Cells[Table.Rows.Count+1, Table.Columns.Count+1];
                Excel.Range r = (Excel.Range)WorkSheet.get_Range(TopLeft, BottomRight);

                Object[] args2 = new Object[1];
                args2[0] = values; 
                r.Value2 = values;

                //r.Value2 = values;

				ExcelApp.DisplayAlerts = false;
				ExcelApp.ActiveWorkbook.SaveAs(
								FileName, 
								Excel.XlFileFormat.xlXMLSpreadsheet, 
								Type.Missing, 
								Type.Missing, 
								Type.Missing, 
								Type.Missing, 
								Excel.XlSaveAsAccessMode.xlNoChange,
								Type.Missing, 
								Type.Missing, 
								Type.Missing, 
								Type.Missing, 
								Type.Missing);     
				}
			catch (Exception)
				{
				if (ExcelApp != null)
					ExcelApp.Quit();
				WorkSheet = null;
				Workbook = null;
				ExcelApp = null;
				throw;
				}

			ExcelApp.Quit();
			WorkSheet = null;
			Workbook = null;
			ExcelApp = null;
			}


		}
	}
