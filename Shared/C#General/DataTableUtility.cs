using System;
using System.Data;

namespace CSGeneral
	{
	//------------------------------------------
	// Some utilities for loading and unloading
	// a DataTable
	// -----------------------------------------
	public class DataTableUtility
		{
		// ---------------------------------------------------
		// Add a value to the specified data table
		// ---------------------------------------------------
		static public void AddValue(DataTable Table, string ColumnName, string Value, int StartRow, int Count)
			{
			string[] Values = new string[Count];
			for (int i = 0; i != Count; i++)
				Values[i] = Value;
			AddColumn(Table, ColumnName, Values, StartRow, Count);
			}

		// ---------------------------------------------------
		// Add a value to the specified data table
		// ---------------------------------------------------
		static public void AddValue(DataTable Table, string ColumnName, double Value, int StartRow, int Count)
			{
			string[] Values = new string[Count];
			for (int i = 0; i != Count; i++)
				Values[i] = Value.ToString();
			AddColumn(Table, ColumnName, Values, StartRow, Count);
			}


		// ---------------------------------------------------
		// Add a column of values to the specified data table
		// ---------------------------------------------------
		static public void AddColumn(DataTable Table, string ColumnName, double[] Values, int StartRow, int Count)
			{
			if (Table.Columns.IndexOf(ColumnName) == -1)
				Table.Columns.Add(ColumnName);

			// Make sure there are enough values in the table.
			while (Table.Rows.Count < Values.Length + StartRow)
				Table.Rows.Add(Table.NewRow());

			int Row = StartRow;
			for (int Index = 0; Index != Values.Length; Index++)
				{
				if (Values[Index] != MathUtility.MissingValue)
					Table.Rows[Row][ColumnName] = Values[Index];
				else
					Table.Rows[Row][ColumnName] = "";
				Row++;
				}
			}


		// ---------------------------------------------------
		// Add a column of values to the specified data table
		// ---------------------------------------------------
		static public void AddColumn(DataTable Table, string ColumnName, string[] Values, int StartRow, int Count)
			{
			if (Table.Columns.IndexOf(ColumnName) == -1)
				Table.Columns.Add(ColumnName);

			// Make sure there are enough values in the table.
			while (Table.Rows.Count < Values.Length + StartRow)
				Table.Rows.Add(Table.NewRow());

			int Row = StartRow;
			for (int Index = 0; Index != Values.Length; Index++)
				{
				Table.Rows[Row][ColumnName] = Values[Index];
				Row++;
				}
			}


		// ---------------------------------------------------
		// Get a column of values from the specified data table
		// ---------------------------------------------------
		static public double[] GetColumnAsDoubles(DataTable Table, string ColumnName, int NumValues)
			{
			double [] Values = new double[NumValues];
			for (int Row = 0; Row != Table.Rows.Count && Row != NumValues; Row++)
				{
				if (Table.Rows[Row][ColumnName].ToString() == "")
					Values[Row] = MathUtility.MissingValue;
				else
					Values[Row] = Convert.ToDouble(Table.Rows[Row][ColumnName]);
				}
			return Values;
			}


		// ---------------------------------------------------
		// Get a column of values from the specified data table
		// ---------------------------------------------------
		static public double[] GetColumnAsDoubles(DataTable Table, string ColumnName, int NumValues, int StartRow)
			{
			double [] Values = new double[NumValues];
			int Index = 0;
			for (int Row = StartRow; Row != Table.Rows.Count && Index != NumValues; Row++)
				{
				if (Table.Rows[Row][ColumnName].ToString() == "")
					Values[Index] = MathUtility.MissingValue;
				else
					Values[Index] = Convert.ToDouble(Table.Rows[Row][ColumnName]);
				Index++;
				}
			return Values;
			}


		// ---------------------------------------------------
		// Get a column of values from the specified data table
		// ---------------------------------------------------
		static public string[] GetColumnAsStrings(DataTable Table, string ColumnName, int NumValues)
			{
			string [] Values = new string[NumValues];
			for (int Row = 0; Row != Table.Rows.Count && Row != NumValues; Row++)
				Values[Row] = Convert.ToString(Table.Rows[Row][ColumnName]);
			return Values;
			}


		// ---------------------------------------------------
		// Get a column of values from the specified data table
		// ---------------------------------------------------
		static public string[] GetColumnAsStrings(DataTable Table, string ColumnName, int NumValues, int StartRow)
			{
			string [] Values = new string[NumValues];
			int Index = 0;
			for (int Row = StartRow; Row != Table.Rows.Count && Index != NumValues; Row++)
				{
				Values[Index] = Convert.ToString(Table.Rows[Row][ColumnName]);
				Index++;
				}
			return Values;
			}


		// ---------------------------------------------------------------------
		// Get number of non blank values in column of the specified data table
		// ---------------------------------------------------------------------
		static public int GetNumberOfNonBlankRows(DataTable Table, string ColumnName)
			{
			for (int Row = Table.Rows.Count-1; Row >= 0; Row--)
				{
				if (Table.Rows[Row][ColumnName].ToString() != "")
					return Row + 1;
				}
			return Table.Rows.Count;
			}

		}
	}
