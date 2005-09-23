using System;
using System.Xml;
using VBGeneral;
using System.Collections;

namespace CSGeneral
	{
	// -------------------------------------------------
	// A class for encapsulating a soil sample
	// A soil sample can have SW values in volumetric %
	// or gravimetric %. Default is Volumetric.
	// -------------------------------------------------
	public class SoilSample : SoilBase
		{
		private Soil MyLinkedSoil;

		//-------------------------------------------------------------------------
		//Constructor
		//-------------------------------------------------------------------------
		public SoilSample()	: base(new APSIMData("SoilSample", ""))
			{
			}


		//-------------------------------------------------------------------------
		//Constructor
		//-------------------------------------------------------------------------
		public SoilSample(APSIMData data) : base(data)
			{
			}


		// ----------------------------------------------
		// Property to link this sample to a soil.
		// ----------------------------------------------
		public Soil LinkedSoil
			{
			get {return MyLinkedSoil;}
			set {MyLinkedSoil = value;}
			}


		// ---------------------------------------------
		// Throw an exception if there is no linked soil
		// ---------------------------------------------
		private void ThrowExceptionIfNoLinkedSoil()
			{
			if (MyLinkedSoil == null)
				throw new Exception("Soil sample does not have a linked soil");
			}
		//-------------------------------------------------------------------------
		// Returns the SW units
		//-------------------------------------------------------------------------
		public enum SWUnits {Volumetric, Gravimetric};
		public SWUnits SWUnit
			{
			get {if (Data.get_ChildValue("swunit").ToLower() == "volumetric")
					return SWUnits.Volumetric;
				 else
					return SWUnits.Gravimetric;
				}
			set {
				if (value == SWUnits.Volumetric)
					Data.set_ChildValue("swunit", "volumetric");
				else
					Data.set_ChildValue("swunit", "gravimetric");
				}				 
			}


		// ------------------------
		// Layered sample properties.
		// ------------------------
		public double[] SW						// gets and sets volumetric.
			{
			get {
				if (SWUnit == SWUnits.Gravimetric)
					{
					ThrowExceptionIfNoLinkedSoil();
					double[] BD = MapSoilToSampleUsingSpatial(MyLinkedSoil.BD, MyLinkedSoil.Thickness, Thickness);
					return MathUtility.Multiply(getLayered("water", "sw"), BD);
					}
				 else
					return getLayered("water", "sw");
				}
			set {
				setLayered("water", "sw", value);
				SWUnit = SWUnits.Volumetric;
				}                           				
			}
		public double[] SWGrav						// gets and sets gravimetric
			{
			get {
				if (SWUnit == SWUnits.Volumetric)
					{
					ThrowExceptionIfNoLinkedSoil();
					double[] BD = MapSoilToSampleUsingSpatial(MyLinkedSoil.BD, MyLinkedSoil.Thickness, Thickness);
					return MathUtility.Divide(getLayered("water", "sw"), BD);
					}
				 else
					return getLayered("water", "sw");
				}
			set {
				setLayered("water", "sw", value);
				SWUnit = SWUnits.Gravimetric;
				}                           				
			}
		public double[] NO3
			{
			get {return getLayered("nitrogen", "no3");}
			set {setLayered("nitrogen", "no3", value);}
			}
		public double[] NH4
			{
			get {return getLayered("nitrogen", "nh4");}
			set {setLayered("nitrogen", "nh4", value);}
			}
		public double[] OC
			{
			get {return getLayered("nitrogen", "oc");}
			set {setLayered("nitrogen", "oc", value);}
			}
		public double[] PH
			{
			get {return getLayered("nitrogen", "ph");}
			set {setLayered("nitrogen", "ph", value);}
			}
		public double[] EC
			{
			get {return getLayered("other", "ec");}
			set {setLayered("other", "ec", value);}
			}


		
		// -----------------------------------------------------
		// Layered sample properties that have been maped to the
		// linked soil.
		// -----------------------------------------------------
		public double[] SWMapedToSoil
			{
			get {return MapSampleToSoilUsingSpatial(SW, Thickness, MyLinkedSoil.InitialWater.SW, MyLinkedSoil.Thickness);}
			}
		public double[] NO3MapedToSoil
			{
			get {return MapSampleToSoilUsingMass(NO3, Thickness, MyLinkedSoil.InitialNitrogen.NO3, MyLinkedSoil.Thickness, MyLinkedSoil.BD);}
			}
		public double[] NH4MapedToSoil
			{
			get {return MapSampleToSoilUsingMass(NH4, Thickness, MyLinkedSoil.InitialNitrogen.NH4, MyLinkedSoil.Thickness, MyLinkedSoil.BD);}
			}
		public double[] OCMapedToSoil
			{
			get {return MapSampleToSoilUsingMass(OC, Thickness, MyLinkedSoil.OC, MyLinkedSoil.Thickness, MyLinkedSoil.BD);}
			}
		public double[] PHMapedToSoil
			{
			get {return MapSampleToSoilUsingSpatial(PH, Thickness, MyLinkedSoil.PH, MyLinkedSoil.Thickness);}
			}
		public double[] ECMapedToSoil
			{
			get {
				double[] soilec = null;
				if (MyLinkedSoil.EC.Length == 0)
					{
					soilec = new double[MyLinkedSoil.Thickness.Length];
					for (int i = 0; i != soilec.Length; i++)
						soilec[i] = 0;
					}
				else
					{
					soilec = MyLinkedSoil.EC;
					}
				double[] ec = MapSampleToSoilUsingSpatial(EC, Thickness, soilec, MyLinkedSoil.Thickness);
				return ec;
				}
			}


		// ------------------------------------------------------------------
		// return plant available water by layer (mm) given
		// depth, lower limit and dul all in (mm).
		// ------------------------------------------------------------------
		public double[] PAW(string CropName)
			{
			if (MyLinkedSoil.CropExists(CropName))
				{
				double[] sw = SW;
				double[] ll = MapSoilToSampleUsingSpatial(MyLinkedSoil.LL(CropName), MyLinkedSoil.Thickness, Thickness);
				double[] sat = MapSoilToSampleUsingSpatial(MyLinkedSoil.SAT, MyLinkedSoil.Thickness, Thickness);
				double[] thickness = Thickness;
				double[] paw = new double[thickness.Length];
				for(int i = 0; i != thickness.Length; i++)
					{
					sw[i] = Math.Max(sw[i], ll[i]);
					sw[i] = Math.Min(sw[i], sat[i]);
					paw[i] = ((sw[i]  - ll[i])* thickness[i]);
					}
				return paw;
				}
			throw new Exception("Soil is not parameterised for crop: " + CropName);
			}


		// ----------------------------------------------------------------
		// Interpolate some BD values that match this sample's thicknesses.
		// ----------------------------------------------------------------
		private static double[] MapSoilToSampleUsingSpatial(double[] FromValues, double[] FromThickness, double[] ToThickness)
			{
			// need to work out a bd for this sample from the characterisation bd passed in.
			double[] FromMass = MathUtility.Multiply(FromValues, FromThickness);
			double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
			return MathUtility.Divide(ToMass, ToThickness);
			}


		// ------------------------------------------------------------------------
		// Map the specified values to the linked soil by first converting to
		// a mass value i.e. by first multiplying Values by BD. 
		// The result will be a set of values that correspond to the linked soil layers.
		// ------------------------------------------------------------------------
		private static double[] MapSampleToSoilUsingMass(double[] FromValues, double[] FromThickness,
														   double[] ToValues, double[] ToThickness, double[] ToBD)
			{
			if (ToValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness, ToValues, ToThickness);
				return MassRedistribute(FromValues, FromThickness, ToThickness, ToBD);	
				}
			else
				return new double[0];
			}


		// ------------------------------------------------------------------------
		// Map the specified values to the linked soil using a simple spatial
		// interpolation. The result will be a set of values that correspond 
		// to the linked soil layers.
		// ------------------------------------------------------------------------
		private static double[] MapSampleToSoilUsingSpatial(double[] FromValues, double[] FromThickness,
															  double[] ToValues, double[] ToThickness)
			{
			if (ToValues.Length > 0)
				{
				CreateVariableForMapping(ref FromValues, ref FromThickness,
										ToValues, ToThickness);
				FromValues = MathUtility.Multiply(FromValues, FromThickness);
				FromValues = SpatialRedistribute(FromValues, FromThickness, ToThickness);	
				return MathUtility.Divide(FromValues, ToThickness);
				}
			else
				return new double[0];
			}

		
		//-------------------------------------------------------------------------
		//Spatial mass redistribution algorithm.
		//-------------------------------------------------------------------------
		private static double[] SpatialRedistribute(double[] FromMass, double[] FromThickness, 
											 double[] ToThickness)
			{
			if(FromMass.Length != FromThickness.Length)
				{
				throw new Exception("Cannot redistribute soil sample layer structure to soil layer structure. "+
			                        "The number of values in the sample doesn't match the number of layers in the sample.");
				}

			// Remapping is achieved by first constructing a map of
			// cumulative mass vs depth
			// The new values of mass per layer can be linearly
			// interpolated back from this shape taking into account
			// the rescaling of the profile.

			double[] CumDepth = new double[FromMass.Length+1];
			double[] CumMass = new double[FromMass.Length+1];
			CumDepth[0] = 0.0;
			CumMass[0] = 0.0;
			for(int Layer = 0; Layer < FromThickness.Length; Layer++)
				{
				CumDepth[Layer+1] = CumDepth[Layer] + FromThickness[Layer];
				CumMass[Layer+1] = CumMass[Layer] + FromMass[Layer];
				}
					
			//look up new mass from interpolation pairs
			double[] ToMass = new double[ToThickness.Length];
			for(int Layer = 1; Layer <= ToThickness.Length; Layer++)
				{
				double LayerBottom = MathUtility.Sum(ToThickness, 0, Layer, 0.0);
				double LayerTop = LayerBottom - ToThickness[Layer-1];
				bool DidInterpolate = false;
				double CumMassTop = MathUtility.LinearInterpReal(LayerTop, CumDepth, 
					CumMass, ref DidInterpolate);
				double CumMassBottom = MathUtility.LinearInterpReal(LayerBottom, CumDepth, 
					CumMass, ref DidInterpolate);
				ToMass[Layer-1] = CumMassBottom - CumMassTop;
				}
			return ToMass;
			}	


		//-------------------------------------------------------------------------
		//Mass Redistribution algorithm
		//-------------------------------------------------------------------------
		private static double[] MassRedistribute(double[] FromValues, double[] FromThickness,
										  double[] ToThickness, double[] ToBd)
			{
			// Firstly we need to convert the values passed in, into a mass using
			// bulk density.

			double[] FromBd = MapSoilToSampleUsingSpatial(ToBd, ToThickness, FromThickness);
			double[] FromMass = new double[FromValues.Length];
			for(int Layer = 0; Layer < FromValues.Length; Layer++)
				FromMass[Layer] = FromValues[Layer] * FromBd[Layer] * FromThickness[Layer] / 100;
			
			// spatially interpolate mass.
			double[] ToMass = SpatialRedistribute(FromMass, FromThickness, ToThickness);
			
			//now convert mass back to original values.
			double[] ToValues = new double[ToMass.Length];
			for(int Layer = 0; Layer < ToMass.Length; Layer++)
				ToValues[Layer] = ToMass[Layer] * 100.0 / ToBd[Layer] / ToThickness[Layer];

 			return ToValues;
			}


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
		private static void CreateVariableForMapping(ref double[] SampleValues, ref double[] SampleThickness, 
														double[] SoilValues, double[] SoilThickness)	
			{
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
				if(CumSoilDepth > CumSampleDepth)
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


		//-------------------------------------------------------------------------
		//Replaces any missing soil sample values with values from the soil file.
		//This allows the user to input partial results for a soil sample and
		//any missing values will be replaced by values from the soil file
		//-------------------------------------------------------------------------	
		/*private void ReplaceAnyMissingValues(double[] dSoilValues, ref ArrayList alSampleValues,
			double[] dSoilThicknesses, ArrayList alSampleThicknesses)
			{
			double dCumSampleThickness = 0.0;
			double dCumSoilThickness = 0.0;
			int iSoilLayer = 0;

			// calculate depth increments.
			if (alSampleThicknesses.Count > 0)
				{
				if(dSoilThicknesses.Length > 0)
					{
					dCumSoilThickness = dSoilThicknesses[iSoilLayer];
					}
				for (int iSampleLayer = 0; iSampleLayer < alSampleThicknesses.Count; iSampleLayer++)
					{
					//Check to make sure that the layers are as close as can be
					dCumSampleThickness += Convert.ToDouble(alSampleThicknesses[iSampleLayer].ToString());
					while((dCumSoilThickness < dCumSampleThickness) && (iSoilLayer < (dSoilThicknesses.Length-1)))
						{
						iSoilLayer++;
						dCumSoilThickness += dSoilThicknesses[iSoilLayer];	
						}
					if(Convert.ToDouble(alSampleValues[iSampleLayer].ToString()) == MISSING_DOUBLE)
						{
						alSampleValues[iSampleLayer] = dSoilValues[iSoilLayer];
						}
					}
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public double CheckLayerExists(int iLayer, double[] dValues)
			{
			double dValue = 0.0;
			if(iLayer < dValues.Length)
				{
				dValue = dValues[iLayer];
				}
			else
				{
				if(dValues.Length > 0)
					{
					dValue = dValues[dValues.Length-1];
					}
				}
			return dValue;
			}
		  */

		//-------------------------------------------------------------------------	
		}//END OF CLASS
	}//END OF NAMESPACE