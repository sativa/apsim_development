using System;
using System.Collections;
using System.Collections.Specialized;
using Microsoft.VisualBasic;
using VBGeneral;
using CSGeneral;

namespace Soils
    {
	public class Utility
		{
        // -----------------------------------------
        // Utility class that other soil classes use
        // -----------------------------------------


        public static string GetStringValue(APSIMData Data, string PropertyType, string PropertyName)
			{
            // ----------------------------------------------------
            // Return a string value to caller for specified child.
            // ----------------------------------------------------
            string Key;
			if (PropertyType != "")
				Key = PropertyType + "\\" + PropertyName;
			else
				Key = PropertyName;
			return Data.get_ChildValue(Key);
			}
		public static double GetDoubleValue(APSIMData Data, string PropertyType, string PropertyName)
			{
            // ----------------------------------------------------
            // Return a double value to caller for specified child.
            // ----------------------------------------------------
            string Value = GetStringValue(Data, PropertyType, PropertyName);
            if (Value == "")
				return MathUtility.MissingValue;			
			else
				return Convert.ToDouble(Value);
			}
		public static void SetValue(APSIMData Data, string PropertyType, string PropertyName, double Value)
			{
            // ----------------------------------------------------
            // Set a string value for specified property.
            // ----------------------------------------------------
            string StringValue;
			if (Value == MathUtility.MissingValue)
				StringValue = "";
			else
				StringValue = Value.ToString();
			SetValue(Data, PropertyType, PropertyName, StringValue);
			}
        public static void SetValue(APSIMData Data, string PropertyType, string PropertyName, string Value)
			{
            // ----------------------------------------------------
            // Return a double value to caller for specified child.
            // ----------------------------------------------------
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
        public static string[] getLayeredAsStrings(APSIMData Data, string ParentNodeName, string propertyType, string propertyName)
            {
            // ---------------------------------------
            // Return a layered variable as strings
            // ---------------------------------------
            APSIMData Profile;
            if (ParentNodeName == "")
                Profile = Data;
            else
                Profile = Data.Child(ParentNodeName);
            if (Profile != null)
                {
                string[] values = new string[Profile.ChildNames("layer").Length];
                int index = 0;
                foreach (APSIMData layer in Profile.get_Children("layer"))
                    {
                    APSIMData MatchingValueNode = FindNodeByTypeAndName(layer, propertyType, propertyName);
                    if (MatchingValueNode != null && 
                        MatchingValueNode.InnerXML != null)
                        values[index] = MatchingValueNode.InnerXML;
                    else
                        values[index] = "";
                    index++;
                    }
                return values;
                }
            else
                return new string[0];
            }
        private static APSIMData FindNodeByTypeAndName(APSIMData ParentNode, string Type, string Name)
            {
            // ----------------------------------------------------------
            // Find a child name under Parent that matches the specified
            // type and name. 
            // ----------------------------------------------------------
            foreach (APSIMData Child in ParentNode.get_Children(null))
                {
                bool ThisChildMatches = false;
                if (Type != "" && Name != "")
                    ThisChildMatches = (Child.Type.ToLower() == Type.ToLower() &&
                                        Child.Name.ToLower() == Name.ToLower());
                else if (Type == "" && Name != "")
                    ThisChildMatches = (Child.Name.ToLower() == Name.ToLower());
                else if (Type != "" && Name == "")
                    ThisChildMatches = (Child.Type.ToLower() == Type.ToLower());

                if (ThisChildMatches)
                    return Child;
                }
            return null;
            }
        public static double[] getLayered(APSIMData Data, string ParentNodeName, string propertyType, string propertyName)
            {
            // ---------------------------------------
            // Return a layered variable as doubles.
            // ---------------------------------------
            string[] StringValues = getLayeredAsStrings(Data, ParentNodeName, propertyType, propertyName);
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
        public static void setLayeredAsStrings(APSIMData Data, string ParentNodeName, string PropertyType, string PropertyName, string[] Values)
            {
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as strings.
            //--------------------------------------------------------------------------	
            APSIMData Profile;
            if (ParentNodeName == "")
                Profile = Data;
            else
                Profile = Data.Child(ParentNodeName);
            if (Profile == null)
                Profile = Data.Add(new APSIMData("profile", ""));


            // if values is zero length then the caller wants to delete a property
            // from all layers.
            if (Values.Length == 0)
                {
                int NumLayers = Profile.get_Children("layer").Length;
                Values = new string[NumLayers];
                for (int i = 0; i != NumLayers; i++)
                    Values[i] = "";
                }

            // make sure we have the right amount of layer nodes.
            Profile.EnsureNumberOfChildren("layer", "", Values.Length);

            APSIMData[] Layers = Profile.get_Children("layer");
            for (int i = 0; i != Values.Length; i++)
				{
                APSIMData MatchingValueNode = FindNodeByTypeAndName(Layers[i], PropertyType, PropertyName);
                if (MatchingValueNode == null)
                    {
                    if (Values[i] != "")
                        MatchingValueNode = Layers[i].Add(new APSIMData(PropertyType, PropertyName));
                    }
                else if (Values[i] == "")
                    Layers[i].DeleteNode(MatchingValueNode);
                if (MatchingValueNode != null)
                    MatchingValueNode.Value = Values[i];
				}
			}
        public static void setLayered(APSIMData Data, string ParentNodeName, string PropertyType, string PropertyName, double[] Values)
            {
            //--------------------------------------------------------------------------
            // Sets the values of the specified node as doubles
            //--------------------------------------------------------------------------	
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
            setLayeredAsStrings(Data, ParentNodeName, PropertyType, PropertyName, StringValues);
            }
        public static void DeleteLayered(APSIMData Data, string ParentNodeName, string PropertyType, string PropertyName)
            {
            //--------------------------------------------------------------------------
            // Deletes the values of the specified property
            //--------------------------------------------------------------------------	
            string[] StringValues = new string[0];
            setLayeredAsStrings(Data, ParentNodeName, PropertyType, PropertyName, StringValues);
            }
        public static string[] ToDepthStrings(double[] Thickness)
            {
            // ------------------------------------------------------------------
            // Convert an array of thickness(mm) to depth strings(cm)
            //    e.g. "0-10", "10-30"
            // ------------------------------------------------------------------
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
        public static double[] ToThickness(string[] DepthStrings)
		    {
            double[] Thickness = new double[DepthStrings.Length];
            for (int i = 0; i != DepthStrings.Length; i++)
				{
                int PosDash = DepthStrings[i].IndexOf('-');
				if (PosDash == -1)
                    throw new Exception("Invalid layer string: " + DepthStrings[i] +
										". String must be of the form: 10-30");

                int TopOfLayer = Convert.ToInt32(DepthStrings[i].Substring(0, PosDash));
                int BottomOfLayer = Convert.ToInt32(DepthStrings[i].Substring(PosDash + 1));
				Thickness[i] = (BottomOfLayer - TopOfLayer) * 10;
				}
			return Thickness;
			}
		public static double[] ToCumThickness(double[] Thickness)
			{
            // ------------------------------------------------
            // Return cumulative thickness for each layer - mm
            // ------------------------------------------------
			double[] CumThickness = new double[Thickness.Length];
			if (Thickness.Length > 0)
				{
                CumThickness[0] = Thickness[0];
				for (int Layer = 1; Layer != Thickness.Length; Layer++)
                    CumThickness[Layer] = Thickness[Layer] + CumThickness[Layer - 1];
				}
            return CumThickness;
			}		
		public static double[] ToMidPoints(double[] Thickness)
			{
            //-------------------------------------------------------------------------
            // Return cumulative thickness midpoints for each layer - mm
            //-------------------------------------------------------------------------
            double[] CumThickness = ToCumThickness(Thickness);
            double[] MidPoints = new double[CumThickness.Length];
            for (int Layer = 0; Layer != CumThickness.Length; Layer++)
				{
				if (Layer == 0)
                    MidPoints[Layer] = CumThickness[Layer] / 2.0;
				else
                    MidPoints[Layer] = (CumThickness[Layer] + CumThickness[Layer - 1]) / 2.0;
				}
			return MidPoints;
			}
		static public double[] ToYForPlotting(double[] Thickness)
			{
            // ------------------------------------------------------------
            // Return an array of thickness (Y) values used in depth plots.
            // ------------------------------------------------------------
            double[] ReturnValues = new double[(Thickness.Length - 1) * 3 + 2];

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
		static public double[] ToXForPlotting(double[] Values)
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



		}
	}
