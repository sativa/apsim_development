using System;
using System.Data;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;

namespace CSGeneral
	{
	// ---------------------------------------------
	// A simple type for encapsulating a constant
	// ---------------------------------------------
	public class APSIMConstant
		{
		public APSIMConstant(string name, string val, string units, string comm)
			{
			Name = name;
			Value = val;
			Units = units;
			Comment = comm;
			}

		public string Name;
		public string Value;
		public string Units;
		public string Comment;
		}

	/// <summary>
	/// This class encapsulates an APSIM input file providing methods for
	/// reading data.
	/// </summary>
	public class APSIMInputFile
		{
		private string _FileName;
		private DataTable data = new DataTable();
		private StringCollection Headings;
		private StringCollection Units;
		private ArrayList _Constants = new ArrayList();

		private int YearIndex;
		private int MonthIndex;
		private int DayIndex;
		private int DateIndex;
		private int DayOfMonthIndex;

		// --------------------------------------
		// Return the data
		// --------------------------------------
		public DataTable Data
			{
			get
				{
				return data;
				}
			}



		// -------------------------------------
		// Return constants to caller
		// -------------------------------------
		public ArrayList Constants
			{
			get
				{
				return _Constants;
				}
			}
		// -------------------------------------
		// Return a given constant to caller
		// -------------------------------------
		public APSIMConstant Constant(string ConstantName)
		{
			foreach(APSIMConstant c in _Constants)
			{
				if(StringManip.StringsAreEqual(c.Name, ConstantName))
				{
					return c;
				}
			}
			return null;
		}

		// ----------------------------------
		// Read from the specified file
		// ----------------------------------
		public void ReadFromFile(string FileName)
			{
			if (!File.Exists(FileName))
				throw new Exception("Cannot find file: " + FileName);

			_FileName = FileName;
			StreamReader In = new StreamReader(_FileName);
			ReadApsimHeader(In);
			ReadAllData(In, new DateTime(), new DateTime());
			In.Close();
			}


		// ----------------------------------
		// Read from the specified file 
		// between the specified dates.
		// ----------------------------------
		public void ReadFromFile(string FileName, DateTime StartDate, DateTime EndDate)
			{
			if (!File.Exists(FileName))
				throw new Exception("Cannot find file: " + _FileName);

			_FileName = FileName;
			StreamReader In = new StreamReader(_FileName);
			ReadApsimHeader(In);
			ReadAllData(In, StartDate, EndDate);
			In.Close();
			}


		// ----------------------------------
		// Read in the apsim header.
		// ----------------------------------
		void ReadApsimHeader(StreamReader In)
			{
			string Line;
			string PreviousLine = "";

			Line = In.ReadLine();
			while (In.Peek() >= 0)
				{
            int PosEquals = Line.IndexOf('=');
				if (PosEquals != -1)
					{	
					// constant found.
					string Comment = StringManip.SplitOffAfterDelimiter(ref Line, "!");
					Comment.Trim();
					if (Line.Length > 0)
						{
						string Name = Line.Substring(0, PosEquals).Trim();
						string Value = Line.Substring(PosEquals+1).Trim();
						string Units = StringManip.SplitOffBracketedValue(ref Value, '(', ')');
						_Constants.Add(new APSIMConstant(Name, Value, Units, Comment));
						}
					}
				else
					{
					char[] whitespace = {' ', '\t'};
					int PosFirstNonBlankChar = StringManip.IndexNotOfAny(Line, whitespace);
					if (PosFirstNonBlankChar != -1 && Line[PosFirstNonBlankChar] == '(')
						{
						Headings = StringManip.SplitStringHonouringQuotes(PreviousLine, " \t");
						Units = StringManip.SplitStringHonouringQuotes(Line, " \t");
						break;
						}
					}
				PreviousLine = Line;
				Line = In.ReadLine();
				}

			if (Headings.Count == 0 || Units.Count == 0)
				throw new Exception("Cannot find headings and units in file: " + _FileName);
			
			IdentifyDateColumns();
			}


		// ----------------------------------
		// Read in all data.
		// ----------------------------------
		void ReadAllData(StreamReader In, DateTime StartDate, DateTime EndDate)
			{
			data.Rows.Clear();
			string Line;
			Line = In.ReadLine();
			while (Line != null && Line.Length > 0)
				{
				StringCollection Words = StringManip.SplitStringHonouringQuotes(Line, " \t");
				if (Words.Count !=	Headings.Count)
					throw new Exception("Invalid number of values on line: " + Line + "\r\nin file: " + _FileName);

				DateTime CurrentDate = CalcDate(Words);
				if ((StartDate == new DateTime() && EndDate == new DateTime()) || (CurrentDate >= StartDate && CurrentDate <= EndDate))
					{

					// Add all necessary columns.
					bool addColumns =  (data.Columns.Count == 0);
					if (addColumns)
						{
						// Add a date column to the data table.
						data.Columns.Add(new DataColumn("Date", System.Type.GetType("System.DateTime")));
						}

					DataRow NewMetRow = data.NewRow();
					NewMetRow["date"] = CurrentDate;

					for (int w = 0; w != Words.Count; w++)
						{
						if (!IsADateColumn(w))
							{
							if (addColumns)
								{
								Type ColumnType;
								if (StringManip.IsNumeric(Words[w]))
									ColumnType = Type.GetType("System.Single");

								else if (StringManip.IsDateTime(Words[w]))
									ColumnType = Type.GetType("System.DateTime");

								else 
									ColumnType = Type.GetType("System.String");
									
								data.Columns.Add(new DataColumn(Headings[w], ColumnType));
								}
							NewMetRow[Headings[w]] = Words[w];
							}
						}

					data.Rows.Add(NewMetRow);

					}
				Line = In.ReadLine();
				}

			}


		// ---------------------------------------------
		// Identify the date columns
		// ---------------------------------------------
		void IdentifyDateColumns()
			{
			YearIndex = -1;
			MonthIndex = -1;
			DayIndex = -1;
			DateIndex = -1;
			DayOfMonthIndex = -1;
			for (int h = 0; h != Headings.Count; h++)
				{
				if (StringManip.StringsAreEqual(Headings[h], "year"))
					YearIndex = h;
				if (StringManip.StringsAreEqual(Headings[h], "month"))
					MonthIndex = h;
				if (StringManip.StringsAreEqual(Headings[h], "day"))
					DayIndex = h;
				if (StringManip.StringsAreEqual(Headings[h], "date"))
					DateIndex = h;
				if (StringManip.StringsAreEqual(Headings[h], "dom"))
					DayOfMonthIndex = h;
				}			
			bool ok = (DateIndex != -1);
			if (!ok)
				ok = (YearIndex != -1 && DayIndex != -1);
			if (!ok)
				ok = (YearIndex != -1 && DayOfMonthIndex != -1 && MonthIndex != -1);
			if (!ok)
				throw new Exception("Cannot find date columns in file: " + _FileName + "\r\n" +
					                              "The file must have one of the following combinations:\r\n" +
															"   a date column\r\n" +
															"   a year and day column\r\n" +
															"   a year, month and dom column");				
			}


		// --------------------------------------------
		// Return true if the specified heading index
		// is a date column.
		// --------------------------------------------
		bool IsADateColumn(int HeadingIndex)
			{
			return (HeadingIndex == YearIndex || HeadingIndex == MonthIndex || HeadingIndex == DayIndex ||
				        HeadingIndex == DateIndex || HeadingIndex == DayOfMonthIndex);
			}


		// -------------------------------------
		// Calculate a date from the specified
		// line
		// -------------------------------------
		DateTime CalcDate(StringCollection Words)
			{
			if (DateIndex != -1)
				{
				StringCollection DateWords = StringManip.SplitStringHonouringQuotes(Words[DateIndex], "/");
				if (DateWords.Count != 3)
					throw new Exception("Invalid date format: " + Words[DateIndex] + ". Format for dates is yyyy/mm/dd");
				return new DateTime(Convert.ToInt32(DateWords[0]),
					                              Convert.ToInt32(DateWords[1]),
															Convert.ToInt32(DateWords[2]));

				}
			int Year = Convert.ToInt32(Words[YearIndex]);
			if (DayIndex != -1)
				{
				int Day = Convert.ToInt32(Words[DayIndex]);
				return new DateTime(Year, 1, 1).AddDays(Day-1);
				}
			if (MonthIndex != -1 && DayOfMonthIndex != -1)
				{
				int Month = Convert.ToInt32(Words[MonthIndex]);
				int DayOfMonth = Convert.ToInt32(Words[DayOfMonthIndex]);
				return new DateTime(Year, Month, DayOfMonth);
				}
			return new DateTime();
			}





		}
	}
