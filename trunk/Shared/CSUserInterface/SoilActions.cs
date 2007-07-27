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
                Dialog.Title = "Provide a filename to save the new soils file to";
                if (Dialog.ShowDialog() == DialogResult.OK)
                    {
                    StreamWriter Out = new StreamWriter(Dialog.FileName);
                    Out.WriteLine("<folder name=\"Soils\" version=\"" + ApsimFile.APSIMChangeTool.CurrentVersion.ToString() + "\"/>");
                    Out.Close();
                    Controller.FileOpen(Dialog.FileName);
                    }
                }
            }
        public static void AddSoil(BaseController Controller)
            {
            Soil NewSoil = new Soil(new APSIMData("soil", "NewSoil"));
            Controller.ApsimData.Add(Controller.SelectedPath, NewSoil.Data.XML);
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
                    APSIMData NewData = new APSIMData();
                    NewData.LoadFromFile(FileName);
                    APSIMChangeTool.Upgrade(NewData);
                    Controller.ApsimData.Add(Controller.SelectedPath, NewData.XML);
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
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                APSIMData ForeignSoils;
                if (!File.Exists(Dialog.FileName))
                    {
                    ForeignSoils = new APSIMData("soils", "");
                    }
                else
                    {
                    ForeignSoils = new APSIMData();
                    ForeignSoils.LoadFromFile(Dialog.FileName);
                    }

                foreach (string SelectedPath in Controller.SelectedPaths)
                    ForeignSoils.Add(Controller.ApsimData.AllData.Find(SelectedPath));
                ForeignSoils.SetAttribute("version", ApsimFile.APSIMChangeTool.CurrentVersion.ToString());
                ForeignSoils.SaveToFile(Dialog.FileName);
                MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.",
                                "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
        public static void ExportToPar(BaseController Controller)
            {
            SaveFileDialog Dialog = new SaveFileDialog();
            Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
            Dialog.Title = "Enter a .par file to export to";
            if (Dialog.ShowDialog() == DialogResult.OK)
                {
                File.Delete(Dialog.FileName);
                foreach (string SelectedPath in Controller.SelectedPaths)
                    ExportToPar(Controller.ApsimData.AllData.Find(SelectedPath), Dialog.FileName, Controller);
                MessageBox.Show("Soils have been exported to '" + Dialog.FileName + "'", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }
        private static void ExportToPar(APSIMData Data, string FileName, BaseController Controller)
            {
            if (Data.Type.ToLower() == "folder")
                {
                foreach (APSIMData Child in Data.get_Children(null))
                    ExportToPar(Child, FileName, Controller);
                }
            else
                {
                Soil SoilToExport = new Soil(Data);
                SoilToExport.ExportToPar(FileName, SoilToExport.Name, true);
                }
            }
        public static void ExportToSpreadsheet(BaseController Controller)
            {
            SaveFileDialog Dialog = new SaveFileDialog();
            Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*";
            Dialog.Title = "Enter a spreadsheet file to export to";
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
                CheckSoils(Controller.ApsimData.AllData.Find(SelectedPath), ref ErrorMessage);
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
        private static void CheckSoils(APSIMData Data, ref string ErrorMessage)
            {
            if (Data.Type.ToLower() == "soil")
                {
                Soil ThisSoil = new Soil(Data);
                string Errors = ThisSoil.CheckForErrors();
                if (Errors != "")
                    ErrorMessage += "\r\n" + ThisSoil.Name + "\r\n" + StringManip.IndentText(Errors, 6);
                }
            else if (Data.Type.ToLower() == "folder")
                {
                foreach (APSIMData Child in Data.get_Children(null))
                    CheckSoils(Child, ref ErrorMessage);
                }
            }

        public static void SortSoils(BaseController Controller)
            {
            Cursor.Current = Cursors.WaitCursor;
            Controller.ApsimData.Sort(Controller.SelectedPath);
            Cursor.Current = Cursors.Default;
            }


        public static void Version(BaseController Controller)
            {
            MessageBox.Show(VersionString(), "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        public static string VersionString()
            {
            System.Version Version = Assembly.GetEntryAssembly().GetName().Version;
            return "Version " + Version.Major.ToString() + "." + Version.Minor.ToString()
                    + " (" + Version.Build.ToString() + "." + Version.Revision.ToString() + ")";
            }
        public static void SoilCropManagement(BaseController Controller)
            {
            Soil MySoil = new Soil(Controller.Data.Parent);

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
                MySoil.SetCropOrder(Crops);
                Controller.Explorer.RefreshCurrentView();
                }
            }
        public static void ChangePHUnits(BaseController Controller)
            {
            Soil MySoil = new Soil(Controller.Data);
            if (MySoil.PHStoredAsWater())
                MySoil.PHCaCl = MySoil.PH;
            else
                MySoil.PH = MySoil.PHCaCl;
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
            WaterUI SoilUI = (WaterUI) Controller.Explorer.CurrentView;

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
        }
    }
