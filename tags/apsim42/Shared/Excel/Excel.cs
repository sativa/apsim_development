using System;
using System.Data;
using System.Collections.Specialized;
using Microsoft.Office.Core;
using CSGeneral;

namespace ExcelUtility
	{
	public class ExcelHelper
		{


		// -----------------
		// Import all files
		// -----------------
		static public DataTable ImportFromFile(string FileName, string SheetName)
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

		// ---------------------------------------------------------
		// Return a row of cell values to caller for specified cell.
		// Row index starts from 1.
		// ---------------------------------------------------------
		private static StringCollection GetExcelRow(Excel.Worksheet Sheet, int Row)
			{
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
		// ---------------------------------------------------------
		// Return a row of cell values to caller for specified cell.
		// Row index starts from 1.
		// ---------------------------------------------------------
		private static bool GetExcelRow(Excel.Worksheet Sheet, int Row, int NumValues, StringCollection Values)
			{
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

		}
	}
