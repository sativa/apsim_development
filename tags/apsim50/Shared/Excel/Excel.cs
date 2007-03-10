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

				// ok now we want to read in all rows of data and fill up our data table.
				Table = new DataTable();
				StringCollection TitleRow = GetExcelRow(Sheet, 1);
				int NumColumns = TitleRow.Count;
				for (int i=0; i != NumColumns; i++)
                    Table.Columns.Add(TitleRow[i]);
				int RowIndex = 2;
				StringCollection Data = new StringCollection();
				while (GetExcelRow(Sheet, RowIndex, NumColumns, Data))
					{
					DataRow Row = Table.NewRow();
					for (int ColIndex = 0; ColIndex != NumColumns; ColIndex++)
						Row[ColIndex] = Data[ColIndex];
					Table.Rows.Add(Row);
					RowIndex++;
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
				WorkSheet.Activate();
				SetTitleRow(WorkSheet, 0, Table);

				for (int Row = 0; Row != Table.Rows.Count; Row++)
					SetExcelRow(WorkSheet, Row, Table);

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

		private static StringCollection GetExcelRow(Excel.Worksheet Sheet, int Row)
			{
			// Return a row of cell values to caller for specified cell.
			// Row index starts from 1.

			int Col = 1; 
			StringCollection Values = new StringCollection();
			Excel.Range Cell = (Excel.Range) Sheet.Cells[Row, Col];
			while (Cell != null && Cell.Value2 != null)
				{
				if (Cell.Value2 != null)
					Values.Add(Cell.Value2.ToString());
				Col++;
				Cell = (Excel.Range) Sheet.Cells[Row, Col];
				}
		
			return Values;
			}
		private static bool GetExcelRow(Excel.Worksheet Sheet, int Row, int NumValues, StringCollection Values)
			{
			// Return a row of cell values to caller for specified cell.
			// Row index starts from 1.
			Values.Clear();

			int Col = 1; 
			Excel.Range Cell = (Excel.Range) Sheet.Cells[Row, Col];
			if (Cell.Value2 == null)
				return false;
			while (Cell != null && Col <= NumValues)
				{
				if (Cell.Value2 != null)
					Values.Add(Cell.Value2.ToString());
				else
					Values.Add(MathUtility.MissingValue.ToString("f0"));

				Col++;
				Cell = (Excel.Range) Sheet.Cells[Row, Col];
				}
		
			return true;
			}

		private static void SetTitleRow(Excel.Worksheet Sheet, int Row, DataTable Table)
			{
			for (int Col = 0; Col != Table.Columns.Count; Col++)
				{
				Excel.Range Cell = (Excel.Range) Sheet.Cells[Row+1, Col+1];
				Cell.Value2 = Table.Columns[Col].ColumnName;
				Cell.Font.Bold = true;
				}	
			}
		private static void SetExcelRow(Excel.Worksheet Sheet, int Row, DataTable Table)
			{
			for (int Col = 0; Col != Table.Columns.Count; Col++)
				{
				Excel.Range Cell = (Excel.Range) Sheet.Cells[Row+2, Col+1];
				Cell.Value2 = Table.Rows[Row][Col];
				}
			}

		}
	}