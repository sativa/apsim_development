using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Net;
using VBUserInterface;
using VBGeneral;
using Soils;
using System.Reflection;
using CSGeneral;
using ApsimFile;
using System.Drawing;
using System.Xml;

namespace CSUserInterface
    {
    public class SoilActions
        {

        public static void FileNew(BaseController Controller)
            {
            if (Controller.FileSaveAfterPrompt())
                {
                SaveFileDialog Dialog = new SaveFileDialog();
                Dialog.Filter = "Soils files (*.soils)|*.soils|" +
                                "All files (*.*)|*.*";
                Dialog.DefaultExt = "soils";
                Dialog.Title = "Provide a filename to save the new soils file to";
                if (Dialog.ShowDialog() == DialogResult.OK)
                    {
                    StreamWriter Out = new StreamWriter(Dialog.FileName);
                    Out.WriteLine("<folder name=\"Soils\" version=\"" + ApsimFile.APSIMChangeTool.CurrentVersion.ToString() + "\"/>");
                    Out.Close();
                    Controller.ApsimData.OpenFile(Dialog.FileName);
                    }
                }
            }
        public static void AddSoil(BaseController Controller)
            {
            Controller.Selection.Add("<soil name=\"NewSoil\"/>");
            }

        #region Import methods
        public static void ImportFromSoils(BaseController Controller)
            {
            OpenFileDialog Dialog = new OpenFileDialog();
            Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*";
            Dialog.Title = "Select 1 or more .soils file(s) to import from";
            Dialog.Multiselect = true;
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                foreach (string FileName in Dialog.FileNames)
                    {
                    XmlDocument Doc = new XmlDocument();
                    Doc.Load(FileName);
                    APSIMChangeTool.Upgrade(Doc.DocumentElement);
                    Controller.Selection.Add(Doc.DocumentElement.OuterXml);
                    }
                }
            }
        public static void ImportFromPar(BaseController Controller)
            {
            OpenFileDialog Dialog = new OpenFileDialog();
            Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
            Dialog.Title = "Select 1 or more .par file(s) to import from";
            Dialog.Multiselect = true;
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                foreach (string FileName in Dialog.FileNames)
                    ParFileImporter.ImportParFile(FileName, Controller);
                }
            }
        public static void ImportFromSpreadsheet(BaseController Controller)
            {
            OpenFileDialog Dialog = new OpenFileDialog();
            Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*";
            Dialog.Title = "Select a spreadsheet to import from";
            if (Dialog.ShowDialog() == DialogResult.OK)
                SoilSpreadsheet.ImportFromFile(Dialog.FileName, Controller);
            }

        public static void ImportFromW2N2(BaseController Controller)
            {
            FolderBrowserDialog Dialog = new FolderBrowserDialog();
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                string[] Files = Directory.GetFiles(Dialog.SelectedPath, "*.w2");
                ParFileImporter.ImportW2N2P2(Files, Controller);
                }
            }
        #endregion
        #region Export methods

        public static void ExportToSoils(BaseController Controller)
            {
            SaveFileDialog Dialog = new SaveFileDialog();
            Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*";
            Dialog.Title = "Enter a .soils file to export to";
            Dialog.DefaultExt = "soils";
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                XmlDocument Doc = new XmlDocument();
                if (!File.Exists(Dialog.FileName))
                    Doc.AppendChild(XmlHelper.CreateNode(Doc, "soils", ""));
                else
                    Doc.Load(Dialog.FileName);

                foreach (string SelectedPath in Controller.SelectedPaths)
                    {
                    ApsimFile.Component Comp = Controller.ApsimData.Find(SelectedPath);
                    XmlDocument NodeDoc = new XmlDocument();
                    NodeDoc.LoadXml(Comp.Contents);
                    Doc.DocumentElement.AppendChild(Doc.ImportNode(NodeDoc.DocumentElement, true));
                    }
                XmlHelper.SetAttribute(Doc.DocumentElement, "version", ApsimFile.APSIMChangeTool.CurrentVersion.ToString());
                Doc.Save(Dialog.FileName);
                MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.",
                                "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
        public static void ExportToPar(BaseController Controller)
            {
            SaveFileDialog Dialog = new SaveFileDialog();
            Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
            Dialog.Title = "Enter a .par file to export to";
            Dialog.DefaultExt = "par";
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                File.Delete(Dialog.FileName);
                foreach (string SelectedPath in Controller.SelectedPaths)
                    ExportToPar(Controller.ApsimData.Find(SelectedPath), Dialog.FileName, Controller);
                MessageBox.Show("Soils have been exported to '" + Dialog.FileName + "'", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
        private static void ExportToPar(ApsimFile.Component Data, string FileName, BaseController Controller)
            {
            if (Data.Type.ToLower() == "folder")
                {
                foreach (ApsimFile.Component Child in Data.ChildNodes)
                    ExportToPar(Child, FileName, Controller);
                }
            else
                {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(Data.Contents);
                Soil SoilToExport = new Soil(Doc.DocumentElement);
                SoilToExport.ExportToPar(FileName, SoilToExport.Name, true);
                }
            }
        public static void ExportToSpreadsheet(BaseController Controller)
            {
            SaveFileDialog Dialog = new SaveFileDialog();
            Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*";
            Dialog.Title = "Enter a spreadsheet file to export to";
            Dialog.DefaultExt = "xls";
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                SoilSpreadsheet.ExportSelectedToFile(Dialog.FileName, Controller);
                MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.",
                                "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }

        #endregion

        public static void CheckSoils(BaseController Controller)
            {
            // User wants to check all soils for consistency
            Cursor.Current = Cursors.WaitCursor;
            string ErrorMessage = "";
            foreach (string SelectedPath in Controller.SelectedPaths)
                CheckSoils(Controller.ApsimData.Find(SelectedPath), ref ErrorMessage);
            if (ErrorMessage == "")
                MessageBox.Show("All soils checked out ok. No problems were encountered",
                                "No problems encountered", MessageBoxButtons.OK,
                                MessageBoxIcon.Information);
            else
                {
                ErrorMessageForm ErrorForm = new ErrorMessageForm();
                ErrorForm.SetText(ErrorMessage);
                ErrorForm.Show();
                }
            Cursor.Current = Cursors.Default;
            }
        private static void CheckSoils(ApsimFile.Component Data, ref string ErrorMessage)
            {
            if (Data.Type.ToLower() == "soil")
                {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(Data.Contents);

                Soil ThisSoil = new Soil(Doc.DocumentElement);
                string Errors = ThisSoil.CheckForErrors();
                if (Errors != "")
                    ErrorMessage += "\r\n" + ThisSoil.Name + "\r\n" + StringManip.IndentText(Errors, 6);
                }
            else if (Data.Type.ToLower() == "folder")
                {
                foreach (ApsimFile.Component Child in Data.ChildNodes)
                    CheckSoils(Child, ref ErrorMessage);
                }
            }

        public static void SortSoils(BaseController Controller)
            {
            Cursor.Current = Cursors.WaitCursor;
            Controller.Selection.Sort();
            Cursor.Current = Cursors.Default;
            }


        public static void Version(BaseController Controller)
            {
            MessageBox.Show(VersionString(), "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        public static string VersionString()
            {
            return "Version " + APSIMSettings.ApsimVersion();
            }
        public static void SoilCropManagement(BaseController Controller)
            {
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Controller.Selection.Contents);
            Soil MySoil = new Soil(Doc.DocumentElement);

            ReorderForm Form = new ReorderForm();
            Form.Text = "Soil / Crop Management";
            Form.TextForAddPrompt = "Enter the name of a new crop";
            Form.SetItems(MySoil.CropsMeasured);
            if (Form.ShowDialog() == DialogResult.OK)
                {
                foreach (string CropName in Form.GetItems())
                    {
                    if (!MySoil.CropExists(CropName))
                        MySoil.AddCrop(CropName);
                    }
                string[] Crops = MySoil.CropsMeasured;
                foreach (string CropName in Crops)
                    {
                    if (VBGeneral.Utility.IndexOfCaseInsensitive(Form.GetItems(), CropName) == -1)
                        MySoil.DeleteCrop(CropName);
                    }
                MySoil.SetCropOrder(Form.GetItems());
                Controller.Selection.Contents = Doc.DocumentElement.OuterXml;
                Controller.Explorer.RefreshCurrentView();
                }
            }
        public static void ChangePHUnits(BaseController Controller)
            {
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Controller.Selection.Contents);
            XmlNode SelectedData = Doc.DocumentElement;
            Soil MySoil = new Soil(SelectedData);
            if (MySoil.PHStoredAsWater())
                MySoil.PHCaCl = MySoil.PH;
            else
                MySoil.PH = MySoil.PHCaCl;
            Controller.Selection.Contents = SelectedData.OuterXml;
            Controller.Explorer.RefreshCurrentView();
            }
        public static void CheckWebForDataUpdate(BaseController Controller)
            {
            Cursor.Current = Cursors.WaitCursor;
            WebRequest request = WebRequest.Create("http://www.apsim.info/apsim/products/apsoil/APSRU-Australia-Soils.soils");
            HttpWebResponse response = (HttpWebResponse)request.GetResponse();
            if (response.StatusDescription == "OK")
                {
                Stream dataStream = response.GetResponseStream();
                StreamReader reader = new StreamReader(dataStream);
                string responseFromServer = reader.ReadToEnd();

                string SoilsFileName = APSIMSettings.ApsimDirectory() + "\\APSRU-Australia-Soils.soils";
                StreamWriter SoilsFile = new StreamWriter(SoilsFileName);
                SoilsFile.Write(responseFromServer);
                SoilsFile.Close();
                reader.Close();
                dataStream.Close();
                MessageBox.Show("The file: " + SoilsFileName + " has been updated to the latest version.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            else
                MessageBox.Show("Cannot connection to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.Error);
            response.Close();
            Cursor.Current = Cursors.Default;
            }
        public static void ReleaseNotes(BaseController Controller)
            {
            string URL = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "APSoil", "ReleaseNotes");
            System.Diagnostics.Process.Start(URL);
            }

        #region PrintSoil 
        public static void PrintSoil(BaseController Controller)
            {
            SoilUI SoilUI = (SoilUI)Controller.Explorer.CurrentView;

            System.Drawing.Printing.PrintDocument PrintDocument = new System.Drawing.Printing.PrintDocument();
            PrintDocument.BeginPrint += new System.Drawing.Printing.PrintEventHandler(SoilUI.OnBeginPrint);
            PrintDocument.PrintPage += new System.Drawing.Printing.PrintPageEventHandler(SoilUI.OnPrintPage);
            PrintDocument.QueryPageSettings += new System.Drawing.Printing.QueryPageSettingsEventHandler(OnQueryPageSettings);

            PrintDialog PrintDialog = new PrintDialog();
            PrintDialog.Document = PrintDocument;

            PrintPreviewDialog PreviewDialog = new PrintPreviewDialog();
            PreviewDialog.Document = PrintDocument;
            PreviewDialog.ShowDialog();
            }
        private static void OnQueryPageSettings(object sender, System.Drawing.Printing.QueryPageSettingsEventArgs e)
            {
            e.PageSettings.Margins.Left = 50;
            e.PageSettings.Margins.Top = 50;
            e.PageSettings.Margins.Right = 50;
            e.PageSettings.Margins.Bottom = 50;
            }
        #endregion 

        #region SIM file writing stuff
        public static XmlNode WriteSoilSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Component.Contents);
            Soils.Soil Soil = new Soils.Soil(Doc.DocumentElement);
            return Soil.ExportToSim(ParentNode);
            }
        public static XmlNode WriteCropSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            // Go find our related soil - must be a sibling. 
            ApsimFile.Component SoilComponent = null;
            ApsimFile.Component Paddock = Component.FindContainingPaddock();
            if ((Paddock == null))
                {
                throw new Exception("Cannot find containing paddock for component: " + Component.Name);
                }
            foreach (ApsimFile.Component Sibling in Paddock.ChildNodes)
                {
                if (Sibling.Type.ToLower() == "soil")
                    {
                    SoilComponent = Sibling;
                    }
                }

            if ((SoilComponent == null))
                {
                throw new Exception("Cannot find a soil component");
                }
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.Contents);
            Soils.Soil Soil = new Soils.Soil(Doc.DocumentElement);
            return Soil.ExportCropToSim(ParentNode, Component.Type);
            }

        public static XmlNode WriteInitWaterSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            // Go find our related soil - should be parent 

            ApsimFile.Component SoilComponent = Component.Parent;
            if ((SoilComponent == null))
                {
                throw new Exception("Cannot find a soil component");
                }
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.Contents);
            Soils.Soil Soil = new Soils.Soil(Doc.DocumentElement);

            // Go create an initwater object. 
            XmlDocument InitWaterDoc = new XmlDocument();
            InitWaterDoc.LoadXml(Component.Contents);
            Soils.InitWater InitWater = new Soils.InitWater(InitWaterDoc.DocumentElement, Soil);
            return InitWater.ExportToSim(ParentNode);

            }

        public static XmlNode WriteSoilSampleSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            // Go find our related soil - should be parent 

            ApsimFile.Component SoilComponent = Component.Parent;
            if ((SoilComponent == null))
                {
                throw new Exception("Cannot find a soil component");
                }
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.Contents);
            Soils.Soil Soil = new Soils.Soil(Doc.DocumentElement);

            // Go create an initwater object. 
            XmlDocument SampleDoc = new XmlDocument();
            SampleDoc.LoadXml(Component.Contents);
            Soils.SoilSample Sample = new Soils.SoilSample(SampleDoc.DocumentElement, Soil);
            return Sample.ExportToSim(ParentNode);

            }

        public static XmlNode WriteInitNitrogenSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            // Go find our related soil - should be parent 
            ApsimFile.Component SoilComponent = Component.Parent;
            if ((SoilComponent == null))
                {
                throw new Exception("Cannot find a soil component");
                }
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.Contents);
            Soils.Soil Soil = new Soils.Soil(Doc.DocumentElement);

            // Find the <component name="nitrogen"> node 
            string NitrogenComponentName = XmlHelper.Name(ParentNode).Replace(" Water", " Nitrogen");
            XmlNode NitrogenSimNode = XmlHelper.Find(ParentNode.ParentNode, NitrogenComponentName);
            if ((NitrogenSimNode == null))
                {
                throw new Exception("Cannot find soiln2 node");
                }

            // Go create an initwater object. 
            XmlDocument InitNitrogenDoc = new XmlDocument();
            InitNitrogenDoc.LoadXml(Component.Contents);
            Soils.InitNitrogen InitNitrogen = new Soils.InitNitrogen(InitNitrogenDoc.DocumentElement, Soil);
            return InitNitrogen.ExportToSim(NitrogenSimNode);

            }
        public static XmlNode WriteManagerSim(ApsimFile.Component Component, XmlNode ParentNode)
            {
            foreach (ApsimFile.Component RuleComponent in Component.ChildNodes)
                {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(RuleComponent.Contents);
                XmlNode Rule = Doc.DocumentElement;
                foreach (XmlNode Condition in XmlHelper.ChildNodes(Rule, "condition"))
                    {
                    string Contents = Condition.OuterXml;

                    foreach (XmlNode Category in XmlHelper.ChildNodes(Rule, "category"))
                        {
                        foreach (XmlNode Prop in XmlHelper.ChildNodes(Category, ""))
                            {
                            string MacroToLookFor = "[" + Prop.Name + "]";
                            Contents = Contents.Replace(MacroToLookFor, Prop.InnerText);
                            }
                        }
                    Contents = Contents.Replace("<condition ", "<rule ");
                    Contents = Contents.Replace("</condition>", "</rule>");
                    XmlDocument RuleDoc = new XmlDocument();
                    RuleDoc.LoadXml(Contents);
                    XmlNode NewRule = RuleDoc.DocumentElement;

                    string RuleCondition = XmlHelper.Name(NewRule);
                    string NewName = XmlHelper.Name(Rule) + " - " + RuleCondition;
                    XmlHelper.SetName(RuleDoc.DocumentElement, NewName);
                    XmlHelper.SetAttribute(RuleDoc.DocumentElement, "condition", RuleCondition);
                    ParentNode.AppendChild(ParentNode.OwnerDocument.ImportNode(NewRule, true));
                    }
                }
            return ParentNode;
            }
        #endregion 

        }
    }
