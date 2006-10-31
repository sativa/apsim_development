using System;
using System.Collections;
using System.Collections.Specialized;
using Microsoft.VisualBasic;
using VBGeneral;

namespace CSGeneral
{
	//	-------------------------------
	// Base class for all soil classes.
	//	-------------------------------
	public class SoilBase
		{
		private APSIMData MyData;

		protected SoilBase(APSIMData Data)
			{
			MyData = Data;
			}

		// -------------------------------------------------
		// Return the raw data. Sometimes this is necessary
		// e.g. YieldProphet stores the soil in a database.
		// -------------------------------------------------
		public APSIMData Data
			{
			get {return MyData;}
			}


		// ----------------------------------------------------
		// Return a string value to caller for specified child.
		// ----------------------------------------------------
		protected string GetStringValue(string PropertyType, string PropertyName)
			{
			string Key;
			if (PropertyType != "")
				Key = PropertyType + "\\" + PropertyName;
			else
				Key = PropertyName;
			return Data.get_ChildValue(Key);
			}


		// ----------------------------------------------------
		// Return a double value to caller for specified child.
		// ----------------------------------------------------
		protected double GetDoubleValue(string PropertyType, string PropertyName)
			{
			string Value = GetStringValue(PropertyType, PropertyName);
            if (Value == "")
				return MathUtility.MissingValue;			
			else
				return Convert.ToDouble(Value);
			}


		// ----------------------------------------------------
		// Set a string value for specified property.
		// ----------------------------------------------------
		protected void SetValue(string PropertyType, string PropertyName, double Value)
			{
			string StringValue;
			if (Value == MathUtility.MissingValue)
				StringValue = "";
			else
				StringValue = Value.ToString();
			SetValue(PropertyType, PropertyName, StringValue);
			}


		// ----------------------------------------------------
		// Return a double value to caller for specified child.
		// ----------------------------------------------------
		protected void SetValue(string PropertyType, string PropertyName, string Value)
			{
			string Key;
			if (PropertyType != "")
				Key = PropertyType + "\\" + PropertyName;
			else
				Key = PropertyName;

			if (Value != "")
				Data.set_ChildValue(Key, Value);
			else
				{
				if (PropertyType == "")
					{
					if (Data.Child(PropertyName) != null)
						Data.Delete(PropertyName);
					}
				else
					{
					APSIMData Child = Data.Child(PropertyType);
					if (Child != null && Child.Child(PropertyName) != null)
						Child.Delete(PropertyName);
					}
				}
			}

        protected string[] getLayeredAsStrings(string propertyType, string propertyName)
            // ---------------------------------------
            // Return a layered variable as strings
            // ---------------------------------------
            {
            APSIMData PropertyData = Data;
            if (propertyType != "")
                PropertyData = Data.Child(propertyType);
            if (PropertyData == null)
                return new string[0];

            string[] values = new string[PropertyData.ChildNames("layer").Length];
            int index = 0;
            foreach (APSIMData layer in PropertyData.get_Children("layer"))
                {
                if (layer.Child(propertyName) == null)
                    values[index] = "";
                else if (layer.Child(propertyName).InnerXML == "" || layer.Child(propertyName).InnerXML == null)
                    values[index] = "";
                else
                    values[index] = layer.Child(propertyName).InnerXML;
                index++;
                }
            return values;
            }

		
		protected double[] getLayered(string propertyType, string propertyName)
            // ---------------------------------------
            // Return a layered variable as doubles.
            // ---------------------------------------
            {
            string[] StringValues = getLayeredAsStrings(propertyType, propertyName);
			double[] values = new double[StringValues.Length];
            int Index = 0;
			foreach (string StringValue in StringValues)
				{
				if (StringValue == "")
					values[Index] = MathUtility.MissingValue;
				else
					{
					values[Index] = Convert.ToDouble(StringValue);
					if (values[Index] < -100000 || values[Index] > 100000)
						values[Index] = MathUtility.MissingValue;
					}
                Index++;
				}
			return values;
			}


		protected void setLayered(string ParentNodeType, string PropertyName, double[] Values)
            //--------------------------------------------------------------------------
            // Sets the values of the specified node
            //--------------------------------------------------------------------------	
            {
			setLayered(ParentNodeType, "", PropertyName, Values);
			}
		protected void setLayeredAsStrings(string ParentNodeType, string ParentNodeName, string PropertyName, string[] Values)
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as strings.
            //--------------------------------------------------------------------------	
            {
			string NodeNameToLookFor = ParentNodeName;
			if (ParentNodeName == "")
				NodeNameToLookFor = ParentNodeType;
			APSIMData node;
			if (NodeNameToLookFor == "")
				node = Data;	
			else
				{
				if (Data.Child(NodeNameToLookFor) == null)
					Data.Add(new APSIMData(ParentNodeType, ParentNodeName));
				node = Data.Child(NodeNameToLookFor);
				}
	
			int iIndex = 0;

			// make sure we have enough layer nodes.
			int FirstLayerToAdd = node.get_Children("layer").Length + 1;
			for (int LayerNumber = FirstLayerToAdd; LayerNumber <= Values.Length; LayerNumber++)
				node.Add(new APSIMData("layer", LayerNumber.ToString()));

			if (Values.Length > 0)
				{
				// make sure we don't have too many layers.
				int FirstLayerToDelete = node.get_Children("layer").Length;
				for (int LayerNumber = FirstLayerToDelete; LayerNumber > Values.Length; LayerNumber--)
					node.Delete(LayerNumber.ToString());
				foreach (APSIMData layer in node.get_Children("layer"))
					{
					if (Values[iIndex] == "")
						{
						if (layer.Child(PropertyName) != null)
							layer.Delete(PropertyName);
						}
					else
						layer.set_ChildValue(PropertyName, Values[iIndex]);
					iIndex++;
					}
				}
			}

        protected void setLayered(string ParentNodeType, string ParentNodeName, string PropertyName, double[] Values)
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as doubles
            //--------------------------------------------------------------------------	
            {
            string[] StringValues = new string[Values.Length];
            int Index = 0;
            foreach (double Value in Values)
                {
                if (Value == MathUtility.MissingValue)
                    StringValues[Index] = "";
                else
                    StringValues[Index] = Values[Index].ToString();
                Index++;
                }
            setLayeredAsStrings(ParentNodeType, ParentNodeName, PropertyName, StringValues);
            }


		//-------------------------------------------------------------------------
		// Return thickness of each layer - mm
		//-------------------------------------------------------------------------
		public double[] Thickness
			{
			get {return getLayered("water", "thickness");}
			set {setLayered("water", "thickness", value);}
			}


		// ------------------------------------------------------------------
		// e.g. "0-10", "10-30" ...
		// ------------------------------------------------------------------
		public string[] DepthStrings
			{
			get 
				{
				double[] Thickness = this.Thickness;
				string[] Strings = new string[Thickness.Length];
				int DepthSoFar = 0;
				for (int i = 0; i != Thickness.Length; i++)
					{
					int ThisThickness = Convert.ToInt32(Thickness[i]) / 10;   // to cm
					int TopOfLayer = DepthSoFar;
					int BottomOfLayer = DepthSoFar + ThisThickness;
					Strings[i] = TopOfLayer.ToString() + "-" + BottomOfLayer.ToString();
					DepthSoFar = BottomOfLayer;							  
					}
				return Strings;
				}
			set
				{
				string[] Strings = value;
				double[] Thickness = new double[Strings.Length];
				for (int i = 0; i != Strings.Length; i++)
					{
					int PosDash = Strings[i].IndexOf('-');
					if (PosDash == -1)
						throw new Exception("Invalid layer string: " + Strings[i] +
											". String must be of the form: 10-30");

					int TopOfLayer = Convert.ToInt32(Strings[i].Substring(0, PosDash));
					int BottomOfLayer = Convert.ToInt32(Strings[i].Substring(PosDash+1));
					Thickness[i] = (BottomOfLayer - TopOfLayer) * 10;
					}
				this.Thickness = Thickness;
				}
			}


		// ------------------------------------------------
		// Return cumulative thickness for each layer - mm
		// ------------------------------------------------
		public double[] CumThickness
			{
			get {
				double[] Thickness = this.Thickness;
				double[] Depth = new double[Thickness.Length];
				if (Thickness.Length > 0)
					{
					Depth[0] = Thickness[0];
					for (int Layer = 1; Layer != Thickness.Length; Layer++)
						Depth[Layer] = Thickness[Layer] + Depth[Layer-1];
					}
				return Depth;
				}
			}		


		//-------------------------------------------------------------------------
		// Return cumulative thickness midpoints for each layer - mm
		//-------------------------------------------------------------------------
		public double[] CumThicknessMidPoints
			{
			get {
				double[] Depths = CumThickness;
				double[] MidPoints = new double[Depths.Length];
				for (int Layer = 0; Layer != Depths.Length; Layer++)
					{
					if (Layer == 0)
						MidPoints[Layer] = Depths[Layer] / 2.0;
					else
						MidPoints[Layer] = (Depths[Layer] + Depths[Layer-1]) / 2.0;
					}
				return MidPoints;
				}
			}


		// ------------------------------------------------------------
		// Return an array of thickness (Y) values used in depth plots.
		// ------------------------------------------------------------
		static public double[] CalcYForPlotting(double[] Thickness)
			{
			double[] ReturnValues = new double[(Thickness.Length-1)*3+2];

			double CumThickness = 0;
			int Index = 0;
			for (int Layer = 0; Layer != Thickness.Length; Layer++)
				{
				ReturnValues[Index] = CumThickness;
				CumThickness += Thickness[Layer];
				ReturnValues[Index+1] = CumThickness;
				Index += 2;
				if (Layer != Thickness.Length-1)
					{
					ReturnValues[Index] = CumThickness;
					Index++;
					}
				}
			return ReturnValues;
			}
		
		// ------------------------------------------------------------
		// Return an array of (X) values used in depth plots.
		// ------------------------------------------------------------
		static public double[] CalcXForPlotting(double[] Values)
			{
			if (Values.Length <= 0)
				return new double[0];
			double[] ReturnValues = new double[(Values.Length-1)*3+2];

			int Index = 0;
			for (int Layer = 0; Layer != Values.Length; Layer++)
				{
				ReturnValues[Index] = Values[Layer];
				ReturnValues[Index+1] = Values[Layer];
				Index += 2;
				if (Layer != Values.Length-1)
					{
					ReturnValues[Index] = Values[Layer+1];
					Index++;
					}
				}
			return ReturnValues;
			}


		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
