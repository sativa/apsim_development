using System;
using System.Collections;
using System.Collections.Specialized;
using CSGeneral;

namespace CSGeneral
{
	/// <summary>
	/// Summary description for Class2.
	/// </summary>
	public class StringManip
		{
		// ------------------------------------------------------------------
		// This method complements the string function IndexOfAny by
		// providing a NOT version. Returns -1 if non of the specified
		// characters are found in specified string.
		// ------------------------------------------------------------------
		public static int IndexNotOfAny(string Text, char[] Delimiters)
			{
			return IndexNotOfAny(Text, Delimiters, 0);
			}
		// ------------------------------------------------------------------
		// This method complements the string function IndexOfAny by
		// providing a NOT version. Returns -1 if non of the specified
		// characters are found in specified string.
		// ------------------------------------------------------------------
		public static int IndexNotOfAny(string Text, char[] Delimiters, int Pos)
			{
			string DelimitersString = new string(Delimiters);
			for (int i = Pos; i < Text.Length; i++)
				{
				if (DelimitersString.IndexOf(Text[i]) == -1)
					return i;
				}
			return -1;
			}

		// ------------------------------------------------------------------
		// This method splits values on a comma but also honours double quotes
		// ensuring something in double quotes is never split.
		//     eg: if text = value1, "value 2, 2a", value3
		//     then: words[0] = value1
		//           words[1] = value2, 2a
		//           words[2] = value3
		// ------------------------------------------------------------------
		public static StringCollection SplitStringHonouringQuotes(string Text, string Delimiters)
			{
			StringCollection ReturnStrings = new StringCollection();

			string DelimitersAndQuote = Delimiters + "\"";
			string DelimitersAndSpace = Delimiters + " ";
			int Start = IndexNotOfAny(Text, DelimitersAndSpace.ToCharArray());
			int Stop;
			while (Start != -1)
				{
				if (Text[Start] == '\"')
					{
					Stop = Text.IndexOf('\"', Start+1);
					if (Stop == -1)
						throw new Exception("Mismatched quotes in string: " + Text);
					Stop++;
					}
				else
					Stop = Text.IndexOfAny(DelimitersAndQuote.ToCharArray(), Start);
							
				if (Stop == -1)
					Stop = Text.Length;

				ReturnStrings.Add(Text.Substring(Start, Stop - Start));
				Start = IndexNotOfAny(Text, DelimitersAndSpace.ToCharArray(), Stop+1);
				}
			return ReturnStrings;
			}

		public static bool StringsAreEqual(string St1, string St2)
			{
         return St1 == St2;
			}
	}
}
