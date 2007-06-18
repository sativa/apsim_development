namespace Test
    {
    using System;
    using System.IO;
    using NUnit.Framework;
    using ApsimFile;
    using System.Collections.Specialized;

    [TestFixture] 
    public class TestApsimFile
        {
        private ApsimFile Simulations;

        [SetUp]
        public void Init()
            {
            const string ApsimFileContents =
                "<folder name=\"Simulations\">\r\n" +
                "  <simulation name=\"My Sim\">\r\n" +
                "    <clock>\r\n" +
                "      <start_date>1/01/1940</start_date>\r\n" +
                "      <end_date>31/01/1940</end_date>\r\n" +
                "    </clock>\r\n" +
                "    <report name=\"My report\">\r\n" +
                "      <variable>variable1</variable>\r\n" +
                "      <variable>variable2</variable>\r\n" +
                "    </report>\r\n" +
                "    <met>\r\n" +
                "      <filename>c:\\dummy.met</filename>\r\n" +
                "    </met>\r\n" +
                "  </simulation>\r\n" +
                "  <simulation name=\"My Sim{1}\" InheritedFrom=\"Simulations\\My Sim\">\r\n" +
                "    <clock InheritedFrom=\"Simulations\\My Sim\\clock\">\r\n" +
                "      <start_date>1/02/1940</start_date>\r\n" +
                "      <end_date>31/04/1940</end_date>\r\n" +
                "    </clock>\r\n" +
                "  </simulation>\r\n" +
                "</folder>";
            Simulations = new ApsimFile();
            Simulations.Open(ApsimFileContents, false);
            }

        [Test]
        public void TestChildEnumeration()
            {
            // -----------------------------------------------------------------------
            // Use case: Load a file with 2 simulations and ensure that we can
            // enumerate through all children including the ones in the derived
            // simulation.
            // -----------------------------------------------------------------------
            string[] SimulationsNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(SimulationsNames.Length, 2);
            Assert.AreEqual(SimulationsNames[0], "My Sim");
            Assert.AreEqual(SimulationsNames[1], "My Sim{1}");

            string[] ComponentNames = Simulations.ChildNames("Simulations\\My Sim");
            Assert.AreEqual(ComponentNames.Length, 3);
            Assert.AreEqual(ComponentNames[0], "clock");
            Assert.AreEqual(ComponentNames[1], "My report");
            Assert.AreEqual(ComponentNames[2], "met");

            ComponentNames = Simulations.ChildNames("Simulations\\My Sim{1}");
            Assert.AreEqual(ComponentNames.Length, 3);
            Assert.AreEqual(ComponentNames[0], "clock");
            Assert.AreEqual(ComponentNames[1], "My report");
            Assert.AreEqual(ComponentNames[2], "met");
            }

        [Test]
        public void TestChildMetaData()
            {
            // -----------------------------------------------------------------------
            // Use case: Load a file with 2 simulations and ensure that we can
            // get the metadata for all children including the ones in the derived
            // simulation.
            // -----------------------------------------------------------------------
            string Type;
            ApsimFile.NodeStatus Status;
            string InheritedFrom;
            Simulations.MetaData("Simulations", out Type, out Status, out InheritedFrom);
            Assert.AreEqual(Type, "folder");
            Assert.AreEqual(Status, ApsimFile.NodeStatus.Normal);
            Assert.AreEqual(InheritedFrom, "");

            Simulations.MetaData("Simulations\\My Sim", out Type, out Status, out InheritedFrom);
            Assert.AreEqual(Type, "simulation");
            Assert.AreEqual(Status, ApsimFile.NodeStatus.Normal);
            Assert.AreEqual(InheritedFrom, "");

            Simulations.MetaData("Simulations\\My Sim{1}", out Type, out Status, out InheritedFrom);
            Assert.AreEqual(Type, "simulation");
            Assert.AreEqual(Status, ApsimFile.NodeStatus.Inherited);
            Assert.AreEqual(InheritedFrom, "Simulations\\My Sim");

            Simulations.MetaData("Simulations\\My Sim{1}\\clock", out Type, out Status, out InheritedFrom);
            Assert.AreEqual(Type, "clock");
            Assert.AreEqual(Status, ApsimFile.NodeStatus.Inherited);
            Assert.AreEqual(InheritedFrom, "Simulations\\My Sim\\clock");
            }

        [Test]
        public void TestContents()
            {
            // -----------------------------------------------------------------
            // Use case: Load a file with 2 simulations and ensure that we can
            // get the contents of both a normal node and a derived node.
            // -----------------------------------------------------------------
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim\\clock\\start_date"), "<start_date>1/01/1940</start_date>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim\\clock\\end_date"), "<end_date>31/01/1940</end_date>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\clock\\start_date"), "<start_date>1/02/1940</start_date>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\clock\\end_date"), "<end_date>31/04/1940</end_date>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\My report"),
                "<report name=\"My report\">\r\n" +
                "  <variable>variable1</variable>\r\n" +
                "  <variable>variable2</variable>\r\n" +
                "</report>");
            }

        [Test]
        public void TestSetContents()
            {
            // --------------------------------------------------------------------
            // Use case: Set the contents of a normal node and an inherited node.
            // --------------------------------------------------------------------
            Simulations.SetContents("Simulations\\My Sim{1}\\clock",
                "<start_date>1/09/1999</start_date>" +
                "<end_date>31/09/1999</end_date>");
            Simulations.SetContents("Simulations\\My Sim{1}\\met",
                "<filename>c:\\dummy2.met</filename>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim\\clock"),
                "<clock>\r\n" + 
                "  <start_date>1/01/1940</start_date>\r\n" +
                "  <end_date>31/01/1940</end_date>\r\n" +
                "</clock>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\clock"),
                "<clock InheritedFrom=\"Simulations\\My Sim\\clock\">\r\n" +
                "  <start_date>1/09/1999</start_date>\r\n" +
                "  <end_date>31/09/1999</end_date>\r\n" +
                "</clock>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim\\met"),
                "<met>\r\n" +
                "  <filename>c:\\dummy.met</filename>\r\n" +
                "</met>");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\met"),
                "<met InheritedFrom=\"Simulations\\My Sim\\met\">\r\n" +
                "  <filename>c:\\dummy2.met</filename>\r\n" +
                "</met>");
            }
        [Test]
        public void TestRevertToBase()
            {
            // --------------------------------------------------------------------
            // Use case: Revert an inherited node to it's base counterpart losing
            // the updated contents in the process.
            // --------------------------------------------------------------------
            Simulations.RevertToBase("Simulations\\My Sim{1}\\clock");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}\\clock"),
                "<clock>\r\n" +
                "  <start_date>1/01/1940</start_date>\r\n" +
                "  <end_date>31/01/1940</end_date>\r\n" +
                "</clock>");
            }

        [Test]
        public void TestDeleteNode()
            {
            // --------------------------------------------------------------------
            // Use case: Delete a node from the simulations. Ensure that IsDirtyData
            // returns true.
            // --------------------------------------------------------------------
            Simulations.Delete("Simulations\\My Sim{1}\\clock");
            Assert.AreEqual(Simulations.Contents("Simulations\\My Sim{1}"),
                "<simulation name=\"My Sim{1}\" InheritedFrom=\"Simulations\\My Sim\">\r\n" +
                "</simulation>");
            Assert.IsTrue(Simulations.IsDirty);
            }


        private string SimulationXml =
            "<simulation name=\"My Sim\">" +
            "   <clock>" +
            "      <start_date>1/01/1940</start_date>" +
            "      <end_date>31/01/1940</end_date>" +
            "   </clock>" +
            "   <report>" +
            "      <variable>variable1</variable>" +
            "      <variable>variable2</variable>" +
            "   </report>" +
            "   <met>" +
            "      <filename>c:\\dummy.met</filename>" +
            "   </met>" +
            "</simulation>";

        [Test]
        public void TestCreate2Simulations()
            {
            // -------------------------------------------------------------
            // Use case: Drag a node from standard toolbox and drop on a 
            // simulation set. Then make a copy of that simulation ensuing
            // there is not a simulation name clash.
            // -------------------------------------------------------------
            ApsimFile SimulationSet = new ApsimFile();
            SimulationSet.New();
            SimulationSet.Add("folder", SimulationXml);
            SimulationSet.Add("folder", SimulationXml);

            StringWriter St = new StringWriter();
            SimulationSet.Save(St);
            Assert.AreEqual(St.ToString(),
                "<?xml version=\"1.0\" encoding=\"utf-16\"?>\r\n" +
                "<folder version=\"9\">\r\n" +
                "  <simulation name=\"My Sim\">\r\n" +
                "    <clock>\r\n" +
                "      <start_date>1/01/1940</start_date>\r\n" +
                "      <end_date>31/01/1940</end_date>\r\n" +
                "    </clock>\r\n" +
                "    <report>\r\n" +
                "      <variable>variable1</variable>\r\n" +
                "      <variable>variable2</variable>\r\n" +
                "    </report>\r\n" +
                "    <met>\r\n" +
                "      <filename>c:\\dummy.met</filename>\r\n" +
                "    </met>\r\n" +
                "  </simulation>\r\n" +
                "  <simulation name=\"My Sim{1}\">\r\n" +
                "    <clock>\r\n" +
                "      <start_date>1/01/1940</start_date>\r\n" +
                "      <end_date>31/01/1940</end_date>\r\n" +
                "    </clock>\r\n" +
                "    <report>\r\n" +
                "      <variable>variable1</variable>\r\n" +
                "      <variable>variable2</variable>\r\n" +
                "    </report>\r\n" +
                "    <met>\r\n" +
                "      <filename>c:\\dummy.met</filename>\r\n" +
                "    </met>\r\n" +
                "  </simulation>\r\n" +
                "</folder>");

            string[] ComponentNames = SimulationSet.ChildNames("folder");
            Assert.AreEqual(ComponentNames.Length, 2);
            Assert.AreEqual(ComponentNames[0], "My Sim");
            Assert.AreEqual(ComponentNames[1], "My Sim{1}");
            }

        [Test]
        public void TestCreate2SimulationsWithInheritance()
            {
            // -------------------------------------------------------------
            // Use case: Drag a node from standard toolbox and drop on a 
            // simulation set. Then make an inherited copy of that simulation 
            // ensuing there is not a simulation name clash.
            // -------------------------------------------------------------
            ApsimFile SimulationSet = new ApsimFile();
            SimulationSet.New();
            SimulationSet.Add("folder", SimulationXml);
            SimulationSet.AddInherited("folder", "simulation", "My Sim", "folder\\My Sim");

            StringWriter St = new StringWriter();
            SimulationSet.Save(St);
            Assert.AreEqual(St.ToString(), 
                "<?xml version=\"1.0\" encoding=\"utf-16\"?>\r\n" +
                "<folder version=\"9\">\r\n" +
                "  <simulation name=\"My Sim\">\r\n" +
                "    <clock>\r\n" +
                "      <start_date>1/01/1940</start_date>\r\n" +
                "      <end_date>31/01/1940</end_date>\r\n" +
                "    </clock>\r\n" +
                "    <report>\r\n" +
                "      <variable>variable1</variable>\r\n" +
                "      <variable>variable2</variable>\r\n" +
                "    </report>\r\n" +
                "    <met>\r\n" +
                "      <filename>c:\\dummy.met</filename>\r\n" +
                "    </met>\r\n" +
                "  </simulation>\r\n" +
                "  <simulation name=\"My Sim{1}\" InheritedFrom=\"folder\\My Sim\" />\r\n" +
                "</folder>");
            }
        [Test]
        public void TestInvalidAdd()
            {
            Simulations.Add("Simulations\\My Sim{1}\\clock", "invalidxml");
            }

        [Test]
        public void TestMoveDown()
            {
            StringCollection NodePaths = new StringCollection();
            NodePaths.Add("Simulations\\My Sim");
            Simulations.MoveDown(NodePaths);
            string[] ComponentNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(ComponentNames[0], "My Sim{1}");
            Assert.AreEqual(ComponentNames[1], "My Sim");

            // try moving two nodes down when not possible - shouldn't do anything
            NodePaths.Clear();
            NodePaths.Add("Simulations\\My Sim{1}");
            NodePaths.Add("Simulations\\My Sim");
            Simulations.MoveDown(NodePaths);
            ComponentNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(ComponentNames[0], "My Sim{1}");
            Assert.AreEqual(ComponentNames[1], "My Sim");

            Assert.IsTrue(Simulations.IsDirty);
            }

        [Test]
        public void TestMoveUp()
            {
            StringCollection NodePaths = new StringCollection();
            NodePaths.Add("Simulations\\My Sim{1}");
            Simulations.MoveUp(NodePaths);
            string[] ComponentNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(ComponentNames[0], "My Sim{1}");
            Assert.AreEqual(ComponentNames[1], "My Sim");

            // try moving it up again - shouldn't do anything
            NodePaths.Clear();
            NodePaths.Add("Simulations\\My Sim{1}");
            NodePaths.Add("Simulations\\My Sim");
            Simulations.MoveUp(NodePaths);
            ComponentNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(ComponentNames[0], "My Sim{1}");
            Assert.AreEqual(ComponentNames[1], "My Sim");
            Assert.IsTrue(Simulations.IsDirty);
            }

        [Test]
        public void TestSort()
            {
            Simulations.Sort("Simulations");
            string[] SortedComponentNames = Simulations.ChildNames("Simulations");
            Assert.AreEqual(SortedComponentNames[0], "My Sim{1}");
            Assert.AreEqual(SortedComponentNames[1], "My Sim");
            }

        }
    }
