using System;
using System.IO;
using NUnit.Framework;
using CSGeneral;
using VBGeneral;
namespace test
	{	
	/// <summary>
	/// Tests the macro class.
	/// </summary>
	/// 

	[TestFixture]
	public class TestMacro
		{
		const string Values = "<simulation name='s'>" +
													"   <soil name='soil1'>"	  +
													"      <crop name='sorghum'>" +
													"         <layer name='1'>" +
													"            <ll>0.23</ll>" +
													"            <kl>1.0</kl>" +
													"         </layer>" +
													"         <layer name='2'>" +
													"            <ll>0.22</ll>" +
													"            <kl>0.9</kl>" +
													"         </layer>" +
													"      </crop>" +
													"      <crop name='wheat'>" +
													"         <layer name='1'>" +
													"            <ll>0.21</ll>" +
													"            <kl>0.8</kl>" +
													"         </layer>" +
													"         <layer name='2'>" +
													"            <ll>0.20</ll>" +
													"            <kl>0.7</kl>" +
													"         </layer>" +
													"      </crop>" +
													"   </soil>" +
													"   <soil name='soil2'>"	  +
													"      <crop name='sorghum'>" +
													"         <layer name='1'>" +
													"            <ll>0.19</ll>" +
													"            <kl>0.6</kl>" +
													"         </layer>" +
													"         <layer name='2'>" +
													"            <ll>0.18</ll>" +
													"            <kl>0.5</kl>" +
													"         </layer>" +
													"      </crop>" +
													"   </soil>" +
													"</simulation>";

		// -------------------------------------------
		// First test of nested foreach statements
		// with and without an alias
		// -------------------------------------------
		[Test]
		public void TestForEach()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[simulation.name] [s.name] [crop.name]\n" +
				"[endfor]\n" + 
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"s soil1 sorghum\n" +
				"s soil1 wheat\n" +
				"s soil2 sorghum\n");
			}
		// -------------------------------------------
		// Test of foreach but writing to files rather
		// than in memory.
		// -------------------------------------------
		[Test]
		public void TestForEachWritesFiles()
			{
			const string Template = 
				"[file test.txt]\n" +
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[simulation.name] [s.name] [crop.name]\n" +
				"[endfor]\n" + 
				"[endfor]\n" +
				"[endfile]\n";

			Macro macro = new Macro();
			macro.Go(new APSIMData(Values), Template, "");
			StreamReader i = new StreamReader("test.txt");
			string Result = i.ReadToEnd();
			Assert.AreEqual(Result, 
				"s soil1 sorghum\n" +
				"s soil1 wheat\n" +
				"s soil2 sorghum\n");
			i.Close();
			File.Delete("test.txt");
			}
		// -------------------------------------------
		// Test badly formatted foreach macro #1
		// -------------------------------------------
		[Test]
		public void TestBadForEachThrows()
			{
			const string Template = 
				"[for each simulation.soil as s]\n" +
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"[for each simulation.soil as s]\n" +
				"[endfor]\n");
			}
		// -------------------------------------------
		// Test badly formatted foreach macro #2
		// -------------------------------------------
		[Test]
		[ExpectedException(typeof(Exception))]
		public void TestBadForEachThrows2()
			{
			const string Template = 
				"[foreach soil as s]\n" +
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			}
		// -------------------------------------------
		// Test badly formatted foreach macro #3
		// -------------------------------------------
		[Test]
		[ExpectedException(typeof(Exception))]
		public void TestBadForEachThrows3()
			{
			const string Template = 
				"[foreach simulation.soil as]\n" +
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			}
		// -------------------------------------------
		// Test missing endfor
		// -------------------------------------------
		[Test]
		[ExpectedException(typeof(Exception))]
		public void TestMissingEndForThrows()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[simulation.name] [s.name] [crop.name]\n" +
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			}
		// -------------------------------------------
		// Test that bad macros are ignored.
		// -------------------------------------------
		[Test]
		public void TestBadMacroIgnored()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[invalid.name] [s.name] [crop.name]\n" +
				"[endfor]\n" +
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"[invalid.name] soil1 sorghum\n" +
				"[invalid.name] soil1 wheat\n" +
				"[invalid.name] soil2 sorghum\n");
			}
		// -------------------------------------------
		// Test that global macros work
		// -------------------------------------------
		[Test]
		public void TestGlobalMacro()
			{
			const string Template = "Sorghum ll = [simulation.soil1.sorghum.1.ll] (mm/mm)"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, "Sorghum ll = 0.23 (mm/mm)");
			}
		// -------------------------------------------
		// Test that if macros work
		// -------------------------------------------
		[Test]
		public void TestIfMacro()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[if [s.name] = soil1]\n" +
						"[simulation.name] [s.name] [crop.name]\n" +
				"[endif]\n" +
				"[endfor]\n" + 
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"s soil1 sorghum\n" +
				"s soil1 wheat\n");
			}
		// -------------------------------------------
		// Test that if macros throw #1
		// -------------------------------------------
		[Test]
		[ExpectedException(typeof(Exception))]
		public void TestIfMacroThrows1()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[if [s.name] = soil1]\n" +
						"[simulation.name] [s.name] [crop.name]\n" +
				"[end if]\n" +
				"[endfor]\n" + 
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			}
		// -------------------------------------------
		// Test that if macros throw #2
		// -------------------------------------------
		[Test]
		[ExpectedException(typeof(Exception))]
		public void TestIfMacroThrows2()
			{
			const string Template = 
				"[foreach simulation.soil as s]\n" +
				"[foreach s.crop]\n" +
				"[if ([s.name] = soil1)]\n" +
						"[simulation.name] [s.name] [crop.name]\n" +
				"[end if]\n" +
				"[endfor]\n" + 
				"[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			}

		// -------------------------------------------
		// Test that white space is removed
		// -------------------------------------------
		[Test]
		public void TestWhiteSpace()
		{
			const string input=
      "<simulations name='Untitled Simulation Set'>\n"+
      "   <simulation name='Untitled'>\n"+
      "      <outputfile name='outputfile'>\n"+
      "         <filename name='filename'>sample.out</filename>\n"+
      "         <frequency name='frequency'>End_of_day</frequency>\n"+
      "         <variable name='year' module='clock' description='Year'/>\n"+
      "         <variable name='day' module='clock' description='Day'/>\n"+
      "         <event name='start_of_day' description='Start of daily simulation time step'/>\n"+
      "      </outputfile>\n"+
      "   </simulation>\n"+
	  "</simulations>";
      
			const string Template = 
					  "[foreach simulations.simulation as sim]\r\n"+
					  "[foreach sim.outputfile as out]\r\n" +
					  "[foreach out.variable as var]\r\n" +
  					  "   [var.name]\r\n" +
					  "[endfor]\r\n" + 
					  "   hello\r\n"+
					  "[endfor]\r\n"+ 
					  "[endfor]\r\n"; 
			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(input), Template);
			Assert.AreEqual(Result, 
				"   year\r\n" +
				"   day\r\n" +
				"   hello\r\n") ;				

		}

		// -------------------------------------------
		// Test that macros work with a comment
		// -------------------------------------------
		[Test]
		public void TestComment()
		{
			const string Template = 
					  "[foreach simulation.soil as s]\n" +
					  "[foreach s.crop]\n" +
					  "[comment] hello [endcomment]\n"+
					  "[if [s.name] = soil1]\n" +
					  "[simulation.name] [s.name] [crop.name]\n" +
					  "[endif]\n" +
					  "[endfor]\n" + 
					  "[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"s soil1 sorghum\n" +
				"s soil1 wheat\n");
		}
		// -------------------------------------------
		// Test that macros work with a comment
		// -------------------------------------------
		[Test]
		public void TestDataPath()
		{
			const string Template = 
					  "[foreach simulation.soil as s]\n" +
					  "[foreach s.crop as crop]\n" +
					  "[s.crop.name]\n" +
					  "[endfor]\n" + 
					  "[endfor]\n"; 

			Macro macro = new Macro();
			string Result = macro.Go(new APSIMData(Values), Template);
			Assert.AreEqual(Result, 
				"sorghum\n" +
				"wheat\n"+
				"sorghum\n");
		}

		}
	}
