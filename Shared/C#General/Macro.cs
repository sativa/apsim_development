using System;
using VBGeneral;
using System.Collections;
using System.Collections.Specialized;
using System.IO;
namespace CSGeneral
	{
	/// <summary>
	/// This class implements a macro language  e.g.
	/// 
	/// [foreach sim in simulation]
	/// [foreach s in sim.soil]
	/// [foreach scrop in s.crop]
	/// [foreach simcrop in sim.crop]
	/// [if [simcrop.name] = [scrop.name]]
	///   layers = [foreach l in simcrop.layer] [l.dlayer] [endfor]
	///   Soil curve number = [s.soilwat2.cn2]
	/// [endif]
	/// [endfor]
	/// [endfor]
	/// [endfor]
	/// [endfor]

	/// </summary>
	public class Macro
		{
		// -----------------------
		//	constructor
		// -----------------------
		public Macro()
			{
			
			}
		// ------------------------------------------------------------------
		// Go generate all files, putting all files in the specified OutputDirectory.
		// This method returns a list of filenames that were generated.
		// ------------------------------------------------------------------
		public string Go(APSIMData MacroValues, string MacroContents)
			{
			StringCollection AliasNames = new StringCollection();
			APSIMData[] AliasNodes = new APSIMData[100];

			AliasNames.Add(MacroValues.Type);
			AliasNodes[0] = MacroValues;
			
			string Contents = MacroContents;
			ParseComments(ref Contents);
			
			Contents = ParseForEach(Contents, MacroValues, AliasNames, AliasNodes);
			ReplaceGlobalMacros(ref Contents, MacroValues);
			ParseIf(ref Contents);
			return Contents;
			}
		// ------------------------------------------------------------------
		// Go generate all files, putting all files in the specified OutputDirectory.
		// This method returns a list of filenames that were generated.
		// ------------------------------------------------------------------
		public StringCollection Go(APSIMData MacroValues, string MacroContents, string OutputDirectory)
			{
			string Contents = Go(MacroValues, MacroContents);
			return WriteStringToFiles(Contents, OutputDirectory);
			}
		// ------------------------------------------------------------------
		// Parse and remove all foreach macros from specified string.
		// Contents is the full text to parse.
		// 
		// ------------------------------------------------------------------
		string ParseForEach(string Contents,
												APSIMData ValuesNode,
												StringCollection AliasNames,
												APSIMData[] AliasNodes)
			{
			// Locate the next foreach macro.
			int PosForEach = FindForEachMacro(Contents, 0);
			while (PosForEach != -1)
				{
				// ok found a foreach - now parse it to get the variable alias before the 'in' keyword
				// and the variable name after the 'in' keyword.
				// e.g. [foreach scrop in s.crop]
				//			scrop is the alias and s.crop is the variable name.
				string ForEachAlias, NodeName, NodeType;
				int PosAfterForEach;
				ParseForEachMacro(Contents, PosForEach, out ForEachAlias, out NodeName, out NodeType, out PosAfterForEach);

				// find the matching endfor macro.
				int PosEndForEach = FindMatchingEndFor(Contents, PosAfterForEach);

				// parse the endfor macro
				int PosAfterEndFor = ParseEndForMacro(Contents, PosEndForEach);

				// get the bit of text before the foreach macro
				string PreForEachText = Contents.Substring(0, AdjustStartPos(Contents, PosForEach));

				// get the contents of the foreach body
				string ForEachText = Contents.Substring(PosAfterForEach, AdjustStartPos(Contents, PosEndForEach)-PosAfterForEach);

				// get the bit of text after the endfor macro
				string PostForEachText = Contents.Substring(PosAfterEndFor);

				// resolve node name to get a APSIMData node.
				APSIMData MacroNode = ResolveNode(NodeName, AliasNames, AliasNodes);

				// Loop through all matching child nodes and duplicate the foreach body for each child.
				string Body = "";
				StringCollection ChildNodeNames = MacroNode.ChildList(NodeType);
				foreach (string ChildName in ChildNodeNames)
						{
						AliasNames.Add(ForEachAlias);
						AliasNodes[AliasNames.Count-1] = MacroNode.Child(ChildName);

						// recurse back and create a new for each body.
						string NewForEachBody = ParseForEach(ForEachText, ValuesNode, AliasNames, AliasNodes);
						
						// Replace any macros in this new text.
						ReplaceLocalMacros(ref NewForEachBody, AliasNames, AliasNodes);

						// Remove local alias'
						AliasNames.Remove(ForEachAlias);

						Body += NewForEachBody;
						}
				
				ForEachText = Body;

				Contents = PreForEachText + ForEachText + PostForEachText;

				// locate next for_each
				PosForEach = FindForEachMacro(Contents, 0);
				}
			return Contents;
			}
		// ------------------------------------------------------------------
		// Adjust the start position of a macro. This routine will remove
		// unwanted spaces on the front of the macro if the macro has
		// nothing before it on the line.
		// ------------------------------------------------------------------
		int AdjustStartPos(string Contents, int PosStartOfMacro)
			{
			if (PosStartOfMacro > 0)
				{
				int Pos = PosStartOfMacro-1;
				while (Pos > 0 && Contents[Pos] == ' ');
					Pos--;

				if (Contents[Pos] == '\n')
					return Pos + 1;
				else
					return PosStartOfMacro;
				}
			else
				return 0;
			}
		// ------------------------------------------------------------------
		// Adjust the end position of a macro. This routine will remove
		// unwanted spaces and a carriage return on end of the macro 
		// if there is nothing else between the end of the macro and 
		// the end of the line.
		// ------------------------------------------------------------------
		int AdjustEndPos(string Contents, int PosMacro)
			{
			int PosEndOfMacro = PosMacro;
			if (Contents[PosMacro] != ']')
				PosEndOfMacro = Contents.IndexOf(']', PosMacro);
			PosEndOfMacro++;
			int Pos = PosEndOfMacro;
			while (Pos < Contents.Length && (Contents[Pos] == ' ' || Contents[Pos] == '\r'))
				Pos++;

			if (Pos < Contents.Length && Contents[Pos] == '\n')
				return Pos + 1;
			else
				return PosEndOfMacro;
			}
		//---------------------------------------------------------------
		// Find the start of a foreach macro in the specified contents.
		//---------------------------------------------------------------
		int FindForEachMacro(string Contents, int StartPos)
			{
			return Contents.IndexOf("[foreach ", StartPos);
			}
		// -------------------------------------------------
		// Parses a string like: [foreach simulation.soil as s]
		//		Returns:
		//			ForEachAlias = s
		//			NodeName = simulation
		//			NodeType = soil
		// -------------------------------------------------
		void ParseForEachMacro(string Contents, int PosForEach, out string ForEachAlias, 
															out string NodeName, out string NodeType, out int PosAfterForEach)
			{
			PosAfterForEach = Contents.IndexOf("]", PosForEach);
			if (PosAfterForEach == -1)
				throw new Exception("Expected a ']' character while trying to parse a foreach macro");
			string Macro = Contents.Substring(PosForEach+1, PosAfterForEach-PosForEach-1);
			
			char[] delimiters = {' '};
			string[] words = Macro.Split(delimiters, 4);

			if (words[0] != "foreach")
				throw new Exception("Expected a 'foreach' keyword while trying to parse a foreach macro");
			if (words[1] == "")
				throw new Exception("Expected a variable name after the  'foreach' keyword while trying to parse a foreach macro");

			// parse the node name and type
			int PosPeriod = words[1].IndexOf('.');
			if (PosPeriod == -1)
				throw new Exception("Invalid variable name in foreach macro: " + words[1]);
			NodeName = words[1].Substring(0, PosPeriod);
			NodeType = words[1].Substring(PosPeriod+1);

			// Parse the alias if it exists.
			if (words.Length == 4)
				{
				if (words[2] != "as")
					throw new Exception("Expected an 'as' keyword while trying to parse a foreach macro. Got a '" + words[2] + "' instead.");
				if (words[3] == "")
					throw new Exception("Expected an alias after an 'as' keyword while trying to parse a foreach macro");
				ForEachAlias = words[3];
				}
			else if (words.Length == 2)
				ForEachAlias = NodeType;
			else
				throw new Exception("Invalid foreach macro at : " + Contents);
			PosAfterForEach = AdjustEndPos(Contents, PosAfterForEach);
			}
		//---------------------------------------------------------------
		// Parse the endfor macro and return the position to just after
		// the macro.
		//---------------------------------------------------------------
		int ParseEndForMacro(string Contents, int PosEndForEach)
			{
			return AdjustEndPos(Contents, PosEndForEach);
			}
		//---------------------------------------------------------------
		// Find the matching endfor for the specified foreach.
		//---------------------------------------------------------------
		int FindMatchingEndFor(string Contents, int PosForEachBody)
			{
			// There may be nested loops - therefore
			// need to count #for_each and #endfor statements until
			// count = 0
			int PosEndFor = -1;
			int CurrentPos = PosForEachBody;
			int Count = 1;
			while (Count > 0)
				{
				int PosForEach = FindForEachMacro(Contents, CurrentPos);
				PosEndFor = Contents.IndexOf("[endfor]", CurrentPos);
				if (PosForEach != -1 && PosForEach < PosEndFor)
					{
					Count++;
					CurrentPos = PosForEach + 1;
					}
				else
					{
					Count--;
					CurrentPos = PosEndFor;
					}
				CurrentPos++;
				}
			if (PosEndFor == -1)
				throw new Exception("Missing an [endfor] macro");

			return PosEndFor;
			}
		//---------------------------------------------------------------
		// Resolve the specified alias into an APSIMData node.
		//---------------------------------------------------------------
		APSIMData ResolveNode(string Alias, StringCollection AliasNames, APSIMData[] AliasNodes)
			{
			int PosAlias = AliasNames.IndexOf(Alias);
			if (PosAlias == -1)
				throw new Exception("Invalid alias specified in foreach macro: " + Alias);

			return AliasNodes[PosAlias];
			}
		//---------------------------------------------------------------
		// Replace all macros in the specified Contents.
		//---------------------------------------------------------------
		void ReplaceLocalMacros(ref string Contents, StringCollection AliasNames, APSIMData[] AliasNodes)
			{
			char[] delimiters = {'.'};

			int PosStartMacro = Contents.IndexOf('[');
			while (PosStartMacro != -1)
				{
				int PosEndMacro = Contents.IndexOf(']', PosStartMacro);
				string Macro = Contents.Substring(PosStartMacro+1, PosEndMacro-PosStartMacro-1);
				string[] words = Macro.Split(delimiters, 2);
				if (words.Length == 2)
					{
					int PosAlias = AliasNames.IndexOf(words[0]);
					if (PosAlias != -1)
						{
						APSIMData node = AliasNodes[PosAlias];
						try
							{
							string Value = GetValueFromNode(node, words[1]);
							Contents = Contents.Remove(PosStartMacro, Macro.Length+2);
							Contents = Contents.Insert(PosStartMacro, Value);
							}
						catch (Exception)
							{ }
						}
					}
				PosStartMacro = Contents.IndexOf('[', PosStartMacro+1);
				}
			}
		//---------------------------------------------------------------
		// Replace global macros in the specified Contents.
		//---------------------------------------------------------------
		void ReplaceGlobalMacros(ref string Contents, APSIMData Values)
			{
			char[] delimiters = {'.'};

			int PosStartMacro = Contents.IndexOf('[');
			while (PosStartMacro != -1)
				{
				int PosEndMacro = Contents.IndexOf(']', PosStartMacro);
				string Macro = Contents.Substring(PosStartMacro+1, PosEndMacro-PosStartMacro-1);

				try
					{
					string Value = GetValueFromNode(Values, Macro);
					Contents = Contents.Remove(PosStartMacro, Macro.Length+2);
					Contents = Contents.Insert(PosStartMacro, Value);
					}
				catch (Exception)
					{
					}
				PosStartMacro = Contents.IndexOf('[', PosStartMacro+1);
				}
			}
		//---------------------------------------------------------------
		// Return a attribute value or child value from the specified child node
		//---------------------------------------------------------------
		string GetValueFromNode(APSIMData Child, string Macro)
			{
			int PosLastPeriod = Macro.LastIndexOf('.');
			if (PosLastPeriod != -1)
				{
				string ChildName = Macro.Substring(0, PosLastPeriod);
				Macro = Macro.Substring(PosLastPeriod+1);
				Child = Child.FindChild(ChildName, '.');
				}
			
			string Value;
			// try getting an attribute first.
			try
				{
				Value = Child.Attribute(Macro);
				return Value;
				}
			catch (Exception)
				{ }

			// couldn't get an attribute so try getting a value
			Value = Child.Child(Macro).Value;
			return Value;
			}

		//---------------------------------------------------------------
		// Parse all if statements.
		//---------------------------------------------------------------
		void ParseIf(ref string Contents)
			{
			int PosElseIf = 0;
			int PosElse = 0;
			int PosCondition = Contents.IndexOf("[if");
			while (PosCondition != -1)
				{
				int PosEndMacro = Contents.IndexOf(']', PosCondition); 
				int PosEndIf = Contents.IndexOf("[endif]", PosCondition+1);
				int PosEndBlock =Contents.IndexOf("[elseif]", PosCondition+1);
				if (PosEndBlock == -1)
					PosEndBlock = Contents.IndexOf("[else]", PosCondition+1);
				if (PosEndBlock == -1)
					PosEndBlock = PosEndIf;
				if (PosEndBlock == -1)
					throw new Exception("Missing endif for if: " + Contents.Substring(PosCondition));
				bool ok;
				if (PosCondition == PosElse && PosCondition != PosElseIf)
					ok = true;

				else
					{
					int PosSpace = Contents.IndexOf(' ', PosCondition);
					
					ok = EvaluateIf(Contents.Substring(PosSpace, PosEndMacro-PosSpace));
					}
				if (ok)
					{
					PosEndBlock = AdjustStartPos(Contents, PosEndBlock);
					PosEndIf = AdjustEndPos(Contents, PosEndIf);

					// remove everything from the end of block to after the endif.
					Contents = Contents.Remove(PosEndBlock, PosEndIf-PosEndBlock);

					// remove the condition line.
					PosEndMacro = AdjustEndPos(Contents, PosEndMacro);
					PosCondition = AdjustStartPos(Contents, PosCondition);
					Contents = Contents.Remove(PosCondition, PosEndMacro-PosCondition);
					}
				else
					{
					// remove everything from start of condition down to end of block.
					PosCondition = AdjustStartPos(Contents, PosCondition);
					if (PosEndBlock == PosEndIf)
						PosEndBlock = AdjustEndPos(Contents, PosEndBlock);
					else
						PosEndBlock = AdjustStartPos(Contents, PosEndBlock);
					Contents = Contents.Remove(PosCondition, PosEndBlock-PosCondition);
					}

				int PosIf = Contents.IndexOf("[if");
				PosElse = Contents.IndexOf("[else");
				PosElseIf = Contents.IndexOf("[elseif");
				PosCondition = PosIf;
				if (PosCondition == -1)
					PosCondition = PosElse;
				if (PosCondition == -1)
					PosCondition = PosElseIf;
				}
			}
		//---------------------------------------------------------------
		// Parse all comment statements and remove.
		//---------------------------------------------------------------
		void ParseComments(ref string Contents)
			{
			int PosComment = Contents.IndexOf("[comment]");
			while (PosComment != -1)
				{
				PosComment = AdjustStartPos(Contents, PosComment);
				int PosEndComment = Contents.IndexOf("[endcomment]");
				if (PosEndComment == -1)
					throw new Exception("Cannot find matching [endcomment] macro");
				PosEndComment = AdjustEndPos(Contents, PosEndComment);
				Contents = Contents.Remove(PosComment, PosEndComment-PosComment);
				PosComment = Contents.IndexOf("[comment]");
				}
			}
		//---------------------------------------------------------------
		// Evaluate the specified IF macro. Return true if it equates
		// to true.
		//---------------------------------------------------------------
		bool EvaluateIf(string IfMacro)
			{
			StringCollection s = StringManip.SplitStringHonouringQuotes(IfMacro, " ");
			
			if (s.Count != 3)
				throw new Exception("Badly formatted if statement: " + IfMacro);
			string lhs = s[0];
			string op = s[1];
			string rhs = s[2];
			lhs.Trim();
			rhs.Trim();
			if (op == "=")
				return StringManip.StringsAreEqual(lhs, rhs);
			else if (op == "<>")
				return !StringManip.StringsAreEqual(lhs, rhs);
			else
				{
				double lhsValue = Convert.ToDouble(lhs);
				double rhsValue = Convert.ToDouble(rhs);
				if (op == "<")
					return (lhsValue < rhsValue);
				else if (op == "<=")
					return (lhsValue <= rhsValue);
				else if (op == ">")
					return (lhsValue > rhsValue);
				else if (op == ">=")
					return (lhsValue >= rhsValue);
				else
					throw new Exception("Unknown if macro operator: " + op);
				}

			}
		//---------------------------------------------------------------
		// Write specified contents to files.
		//---------------------------------------------------------------
		StringCollection WriteStringToFiles(string Contents, string OutputDirectory)
			{
			StringCollection FileNamesCreated = new StringCollection();
			const string FileMacro = "[file";
			int PosFile = Contents.IndexOf(FileMacro);
			while (PosFile != -1)
				{
				PosFile += FileMacro.Length;
				int PosEndFile = Contents.IndexOf(']', PosFile);
				string Filename = Contents.Substring(PosFile, PosEndFile-PosFile);
				Filename = Filename.Trim();
				if (OutputDirectory != "")
					Filename = OutputDirectory + "\\" + Filename;

				int PosStartFileBody = AdjustEndPos(Contents, PosFile);
				int PosEndFileBody = Contents.IndexOf("[endfile]", PosStartFileBody);
				if (PosEndFileBody ==-1)
					throw new Exception("Cannot find a matching [endfile] keyword for file " + Filename);

				string FileContents = Contents.Substring(PosStartFileBody, PosEndFileBody-PosStartFileBody);

				// Dump the file text into the given file name
				StreamWriter o = new StreamWriter(Filename);
				o.Write(FileContents);
				o.Close();
				FileNamesCreated.Add(Path.GetFullPath(Filename));

				PosFile = Contents.IndexOf(FileMacro, PosEndFileBody);
				}
			return FileNamesCreated;
			}
		}
	}
