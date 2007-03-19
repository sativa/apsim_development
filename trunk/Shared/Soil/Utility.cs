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
        
        #region Soil mapping methods
        public static double[] MapSoilToSampleUsingSpatial(double[] FromValues, double[] FromThickness, double[] ToThickness)
            {
            // ----------------------------------------------------------------
            // Interpolate some BD values that match this sample's thicknesses.
            // ----------------------------------------------------------------
            double[] FromMass = MathUtility.Multiply(FromValues, FromThickness);
            double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
            return MathUtility.Divide(ToMass, ToThickness);
            }
        public static double[] MapSampleToSoilUsingMass(double[] FromValues, double[] FromThickness,
                                                         double[] DefaultValues, double[] ToThickness, double[] ToBD)
            {
            // ------------------------------------------------------------------------
            // Map the specified values to the linked soil by first converting to
            // a mass value i.e. by first multiplying Values by BD. 
            // The result will be a set of values that correspond to the linked soil layers.
            // ------------------------------------------------------------------------
            if (DefaultValues.Length > 0)
                {
                CreateVariableForMapping(ref FromValues, ref FromThickness, DefaultValues, ToThickness);
                return MassRedistribute(FromValues, FromThickness, ToThickness, ToBD);
                }
            else
                return new double[0];
            }
        public static double[] MapSampleToSoilUsingSpatial(double[] FromValues, double[] FromThickness,
                                                            double[] DefaultValues, double[] ToThickness)
            {
            // ------------------------------------------------------------------------
            // Map the specified values to the linked soil using a simple spatial
            // interpolation. The result will be a set of values that correspond 
            // to the linked soil layers.
            // ------------------------------------------------------------------------
            if (DefaultValues.Length > 0)
                {
                CreateVariableForMapping(ref FromValues, ref FromThickness,
                                        DefaultValues, ToThickness);
                FromValues = MathUtility.Multiply(FromValues, FromThickness);
                FromValues = SpatialRedistribute(FromValues, FromThickness, ToThickness);
                return MathUtility.Divide(FromValues, ToThickness);
                }
            else
                return new double[0];
            }
        private static double[] SpatialRedistribute(double[] FromMass, double[] FromThickness,
                                                    double[] ToThickness)
            {
            //-------------------------------------------------------------------------
            //Spatial mass redistribution algorithm.
            //-------------------------------------------------------------------------
            if (FromMass.Length != FromThickness.Length)
                {
                throw new Exception("Cannot redistribute soil sample layer structure to soil layer structure. " +
                                    "The number of values in the sample doesn't match the number of layers in the sample.");
                }

            // Remapping is achieved by first constructing a map of
            // cumulative mass vs depth
            // The new values of mass per layer can be linearly
            // interpolated back from this shape taking into account
            // the rescaling of the profile.

            double[] CumDepth = new double[FromMass.Length + 1];
            double[] CumMass = new double[FromMass.Length + 1];
            CumDepth[0] = 0.0;
            CumMass[0] = 0.0;
            for (int Layer = 0; Layer < FromThickness.Length; Layer++)
                {
                CumDepth[Layer + 1] = CumDepth[Layer] + FromThickness[Layer];
                CumMass[Layer + 1] = CumMass[Layer] + FromMass[Layer];
                }

            //look up new mass from interpolation pairs
            double[] ToMass = new double[ToThickness.Length];
            for (int Layer = 1; Layer <= ToThickness.Length; Layer++)
                {
                double LayerBottom = MathUtility.Sum(ToThickness, 0, Layer, 0.0);
                double LayerTop = LayerBottom - ToThickness[Layer - 1];
                bool DidInterpolate = false;
                double CumMassTop = MathUtility.LinearInterpReal(LayerTop, CumDepth,
                    CumMass, ref DidInterpolate);
                double CumMassBottom = MathUtility.LinearInterpReal(LayerBottom, CumDepth,
                    CumMass, ref DidInterpolate);
                ToMass[Layer - 1] = CumMassBottom - CumMassTop;
                }
            return ToMass;
            }
        private static double[] MassRedistribute(double[] FromValues, double[] FromThickness,
                                                 double[] ToThickness, double[] ToBd)
            {
            //-------------------------------------------------------------------------
            //Mass Redistribution algorithm
            //-------------------------------------------------------------------------
            // Firstly we need to convert the values passed in, into a mass using
            // bulk density.

            double[] FromBd = MapSoilToSampleUsingSpatial(ToBd, ToThickness, FromThickness);
            double[] FromMass = new double[FromValues.Length];
            for (int Layer = 0; Layer < FromValues.Length; Layer++)
                FromMass[Layer] = FromValues[Layer] * FromBd[Layer] * FromThickness[Layer] / 100;

            // spatially interpolate mass.
            double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);

            //now convert mass back to original values.
            double[] ToValues = new double[ToMass.Length];
            for (int Layer = 0; Layer < ToMass.Length; Layer++)
                ToValues[Layer] = ToMass[Layer] * 100.0 / ToBd[Layer] / ToThickness[Layer];

            return ToValues;
            }
        private static void CreateVariableForMapping(ref double[] SampleValues, ref double[] SampleThickness,
                                                     double[] SoilValues, double[] SoilThickness)
            {
            //-------------------------------------------------------------------------
            // Remaps the thicknesses and values to more closely match the specified 
            // soil thickness and values. This algorithm removes all missing values
            // and their associated depths.
            // e.g. IF             SoilThickness  Values   SampleThickness	SampleValues
            //                           0-100		2         0-100				10
            //                         100-250	    3		100-600				11
            //                         250-500		4		
            //                         500-750		5
            //                         750-900		6
            //						   900-1200		7
            //                        1200-1500		8
            //                        1500-1800		9
            //
            // will produce:		SampleThickness			Values
            //						     0-100				  10
            //						   100-600				  11
            //						   600-750				   5
            //						   750-900				   6
            //						   900-1200				   7
            //						  1200-1500				   8
            //						  1500-1800				   9
            //
            //-------------------------------------------------------------------------
            double[] ReturnThickness = new double[SampleThickness.Length + SoilThickness.Length + 1];
            double[] ReturnValues = new double[SampleThickness.Length + SoilThickness.Length + 1];

            // Copy values and thicknesses to return arrays until a missing value is found.
            double CumSampleDepth = 0.0;
            int SampleLayer = 0;
            for (SampleLayer = 0; ((SampleLayer != SampleThickness.Length) && (double)SampleValues[SampleLayer] != MathUtility.MissingValue); SampleLayer++)
                {
                ReturnThickness[SampleLayer] = SampleThickness[SampleLayer];
                ReturnValues[SampleLayer] = SampleValues[SampleLayer];
                CumSampleDepth += (double)SampleThickness[SampleLayer];
                }

            //Work out if we need to create a dummy layer so that the sample depths line up 
            //with the soil depths
            double CumSoilDepth = 0.0;
            for (int SoilLayer = 0; SoilLayer < SoilThickness.Length; SoilLayer++)
                {
                CumSoilDepth += SoilThickness[SoilLayer];
                if (CumSoilDepth > CumSampleDepth)
                    {
                    ReturnThickness[SampleLayer] = CumSoilDepth - CumSampleDepth;
                    ReturnValues[SampleLayer] = SoilValues[SoilLayer];
                    SampleLayer++;
                    CumSampleDepth = CumSoilDepth;
                    }
                }

            // Copy Values from our return arrays back to the parameters passed in.
            SampleThickness = new double[SampleLayer];
            SampleValues = new double[SampleLayer];
            for (int i = 0; i != SampleLayer; i++)
                {
                SampleThickness[i] = ReturnThickness[i];
                SampleValues[i] = ReturnValues[i];
                }
            }
        #endregion



		}
	}
