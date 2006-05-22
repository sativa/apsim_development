using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.IO;
using VBGeneral;
using CSGeneral;
using ChangeTool;
using System.Reflection;

namespace APSoil
	{
	//-----------------------------
	// Apsoil main form.
	// ----------------------------
	public class MainForm : System.Windows.Forms.Form
		{
		private ApsoilController Apsoil;
        private ExplorerUI SoilExplorer;
		private System.Windows.Forms.OpenFileDialog ImportSpreadsheetDialog;
		private System.Windows.Forms.FolderBrowserDialog FolderBrowserDialog;
		private System.Windows.Forms.OpenFileDialog ImportParDialog;
		private System.Windows.Forms.SaveFileDialog ParExportDialog;
		private System.Windows.Forms.SaveFileDialog SimExportDialog;
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.OpenFileDialog ImportSoilsDialog;
		private System.Windows.Forms.OpenFileDialog openFileDialog2;
        private System.Windows.Forms.SaveFileDialog ExportSoilsDialog;
        private System.Windows.Forms.SaveFileDialog ExportSpreadsheetDialog;
        private ToolStripContainer ToolStripContainer;
        private ToolStrip toolStrip1;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripButton CutButton;
        private ToolStripButton CopyButton;
        private ToolStripButton PasteButton;
        private ToolStripSeparator toolStripSeparator2;
        private ToolStripButton HelpSmallButton;
        private ToolStripButton SaveAsButton;
        private ToolStripButton NewButton;
        private ToolStripButton OpenButton;
        private ToolStripButton SaveButton;
        private ToolStripButton PrintButton;
        private ToolStripButton CheckSoilsButton;
        private ToolStripButton SortButton;
        private ToolStripDropDownButton InsertButton;
        private ToolStripDropDownButton ImportButton;
        private ToolStripDropDownButton ExportButton;
        private ToolStripMenuItem InsertNewFolder;
        private ToolStripMenuItem InsertNewSoil;
        private ToolStripMenuItem InsertNewSample;
        private ToolStripSeparator toolStripSeparator3;
        private ToolStripMenuItem ImportSoilsFile;
        private ToolStripMenuItem ImportParFile;
        private ToolStripMenuItem ImportSpreadsheet;
        private ToolStripMenuItem ImportW2File;
        private ToolStripMenuItem ExportSoilsFile;
        private ToolStripMenuItem ExportParFile;
        private ToolStripMenuItem ExportSpreadsheet;
        private ImageList SmallImages;
		private string CommandLineFileName;

		#region Constructor / Destructor / Main
		public MainForm()
			{
            Xceed.Chart.Licenser.LicenseKey = "CHT40-N4AAF-77UTD-6ANA";
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if (components != null) 
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
		}
		
		[STAThread]	static void Main(string[] args) 
			{
			Application.EnableVisualStyles();
			Application.DoEvents();

			Application.DoEvents();
			MainForm Main = new MainForm();
			if (args.Length == 1)
				Main.LoadFile(args[0]);
			Application.Run(Main);
			Application.DoEvents();

			Application.DoEvents();
			}
		#endregion

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
        this.components = new System.ComponentModel.Container();
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
        this.ToolStripContainer = new System.Windows.Forms.ToolStripContainer();
        this.toolStrip1 = new System.Windows.Forms.ToolStrip();
        this.NewButton = new System.Windows.Forms.ToolStripButton();
        this.OpenButton = new System.Windows.Forms.ToolStripButton();
        this.SaveButton = new System.Windows.Forms.ToolStripButton();
        this.SaveAsButton = new System.Windows.Forms.ToolStripButton();
        this.PrintButton = new System.Windows.Forms.ToolStripButton();
        this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
        this.CutButton = new System.Windows.Forms.ToolStripButton();
        this.CopyButton = new System.Windows.Forms.ToolStripButton();
        this.PasteButton = new System.Windows.Forms.ToolStripButton();
        this.HelpSmallButton = new System.Windows.Forms.ToolStripButton();
        this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
        this.InsertButton = new System.Windows.Forms.ToolStripDropDownButton();
        this.InsertNewFolder = new System.Windows.Forms.ToolStripMenuItem();
        this.InsertNewSoil = new System.Windows.Forms.ToolStripMenuItem();
        this.InsertNewSample = new System.Windows.Forms.ToolStripMenuItem();
        this.ImportButton = new System.Windows.Forms.ToolStripDropDownButton();
        this.ImportSoilsFile = new System.Windows.Forms.ToolStripMenuItem();
        this.ImportParFile = new System.Windows.Forms.ToolStripMenuItem();
        this.ImportSpreadsheet = new System.Windows.Forms.ToolStripMenuItem();
        this.ImportW2File = new System.Windows.Forms.ToolStripMenuItem();
        this.ExportButton = new System.Windows.Forms.ToolStripDropDownButton();
        this.ExportSoilsFile = new System.Windows.Forms.ToolStripMenuItem();
        this.ExportParFile = new System.Windows.Forms.ToolStripMenuItem();
        this.ExportSpreadsheet = new System.Windows.Forms.ToolStripMenuItem();
        this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
        this.CheckSoilsButton = new System.Windows.Forms.ToolStripButton();
        this.SortButton = new System.Windows.Forms.ToolStripButton();
        this.ImportSpreadsheetDialog = new System.Windows.Forms.OpenFileDialog();
        this.FolderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
        this.ImportParDialog = new System.Windows.Forms.OpenFileDialog();
        this.ParExportDialog = new System.Windows.Forms.SaveFileDialog();
        this.ImportSoilsDialog = new System.Windows.Forms.OpenFileDialog();
        this.SimExportDialog = new System.Windows.Forms.SaveFileDialog();
        this.openFileDialog2 = new System.Windows.Forms.OpenFileDialog();
        this.ExportSoilsDialog = new System.Windows.Forms.SaveFileDialog();
        this.ExportSpreadsheetDialog = new System.Windows.Forms.SaveFileDialog();
        this.SmallImages = new System.Windows.Forms.ImageList(this.components);
        this.ToolStripContainer.TopToolStripPanel.SuspendLayout();
        this.ToolStripContainer.SuspendLayout();
        this.toolStrip1.SuspendLayout();
        this.SuspendLayout();
        // 
        // ToolStripContainer
        // 
        // 
        // ToolStripContainer.ContentPanel
        // 
        this.ToolStripContainer.ContentPanel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
        this.ToolStripContainer.ContentPanel.Size = new System.Drawing.Size(784, 531);
        this.ToolStripContainer.Dock = System.Windows.Forms.DockStyle.Fill;
        this.ToolStripContainer.Location = new System.Drawing.Point(0, 0);
        this.ToolStripContainer.Name = "ToolStripContainer";
        this.ToolStripContainer.Size = new System.Drawing.Size(784, 578);
        this.ToolStripContainer.TabIndex = 0;
        this.ToolStripContainer.Text = "toolStripContainer1";
        // 
        // ToolStripContainer.TopToolStripPanel
        // 
        this.ToolStripContainer.TopToolStripPanel.Controls.Add(this.toolStrip1);
        // 
        // toolStrip1
        // 
        this.toolStrip1.Dock = System.Windows.Forms.DockStyle.None;
        this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.NewButton,
            this.OpenButton,
            this.SaveButton,
            this.SaveAsButton,
            this.PrintButton,
            this.toolStripSeparator1,
            this.CutButton,
            this.CopyButton,
            this.PasteButton,
            this.HelpSmallButton,
            this.toolStripSeparator2,
            this.InsertButton,
            this.ImportButton,
            this.ExportButton,
            this.toolStripSeparator3,
            this.CheckSoilsButton,
            this.SortButton});
        this.toolStrip1.Location = new System.Drawing.Point(3, 0);
        this.toolStrip1.Name = "toolStrip1";
        this.toolStrip1.Size = new System.Drawing.Size(771, 47);
        this.toolStrip1.TabIndex = 0;
        // 
        // NewButton
        // 
        this.NewButton.Image = global::APSoil.Properties.Resources.document_new1;
        this.NewButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.NewButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.NewButton.Name = "NewButton";
        this.NewButton.Size = new System.Drawing.Size(49, 44);
        this.NewButton.Text = "&New...";
        this.NewButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.NewButton.Click += new System.EventHandler(this.OnNewFileClick);
        // 
        // OpenButton
        // 
        this.OpenButton.Image = global::APSoil.Properties.Resources.folder_document1;
        this.OpenButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.OpenButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.OpenButton.Name = "OpenButton";
        this.OpenButton.Size = new System.Drawing.Size(54, 44);
        this.OpenButton.Text = "&Open...";
        this.OpenButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.OpenButton.Click += new System.EventHandler(this.OnOpenFileClick);
        // 
        // SaveButton
        // 
        this.SaveButton.Image = global::APSoil.Properties.Resources.disk_blue1;
        this.SaveButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.SaveButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.SaveButton.Name = "SaveButton";
        this.SaveButton.Size = new System.Drawing.Size(40, 44);
        this.SaveButton.Text = "&Save";
        this.SaveButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.SaveButton.Click += new System.EventHandler(this.OnSaveFileClick);
        // 
        // SaveAsButton
        // 
        this.SaveAsButton.Image = global::APSoil.Properties.Resources.disk_blue_window1;
        this.SaveAsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.SaveAsButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.SaveAsButton.Name = "SaveAsButton";
        this.SaveAsButton.Size = new System.Drawing.Size(69, 44);
        this.SaveAsButton.Text = "Save &as...";
        this.SaveAsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.SaveAsButton.Click += new System.EventHandler(this.OnSaveAsClick);
        // 
        // PrintButton
        // 
        this.PrintButton.Image = global::APSoil.Properties.Resources.printer;
        this.PrintButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.PrintButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.PrintButton.Name = "PrintButton";
        this.PrintButton.Size = new System.Drawing.Size(50, 44);
        this.PrintButton.Text = "Print...";
        this.PrintButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.PrintButton.Click += new System.EventHandler(this.OnPrintClick);
        // 
        // toolStripSeparator1
        // 
        this.toolStripSeparator1.Name = "toolStripSeparator1";
        this.toolStripSeparator1.Size = new System.Drawing.Size(6, 47);
        // 
        // CutButton
        // 
        this.CutButton.Image = global::APSoil.Properties.Resources.cut1;
        this.CutButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.CutButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.CutButton.Name = "CutButton";
        this.CutButton.Size = new System.Drawing.Size(31, 44);
        this.CutButton.Text = "&Cut";
        this.CutButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.CutButton.Click += new System.EventHandler(this.OnCutClick);
        // 
        // CopyButton
        // 
        this.CopyButton.Image = global::APSoil.Properties.Resources.copy1;
        this.CopyButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.CopyButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.CopyButton.Name = "CopyButton";
        this.CopyButton.Size = new System.Drawing.Size(40, 44);
        this.CopyButton.Text = "Co&py";
        this.CopyButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.CopyButton.Click += new System.EventHandler(this.OnCopyClick);
        // 
        // PasteButton
        // 
        this.PasteButton.Image = global::APSoil.Properties.Resources.paste1;
        this.PasteButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.PasteButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.PasteButton.Name = "PasteButton";
        this.PasteButton.Size = new System.Drawing.Size(43, 44);
        this.PasteButton.Text = "&Paste";
        this.PasteButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.PasteButton.Click += new System.EventHandler(this.OnPasteClick);
        // 
        // HelpSmallButton
        // 
        this.HelpSmallButton.Image = global::APSoil.Properties.Resources.help21;
        this.HelpSmallButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.HelpSmallButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.HelpSmallButton.Name = "HelpSmallButton";
        this.HelpSmallButton.Size = new System.Drawing.Size(55, 44);
        this.HelpSmallButton.Text = "&Version";
        this.HelpSmallButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.HelpSmallButton.ToolTipText = "Version number";
        this.HelpSmallButton.Click += new System.EventHandler(this.OnHelpClick);
        // 
        // toolStripSeparator2
        // 
        this.toolStripSeparator2.Name = "toolStripSeparator2";
        this.toolStripSeparator2.Size = new System.Drawing.Size(6, 47);
        // 
        // InsertButton
        // 
        this.InsertButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.InsertNewFolder,
            this.InsertNewSoil,
            this.InsertNewSample});
        this.InsertButton.Image = global::APSoil.Properties.Resources.add;
        this.InsertButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.InsertButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.InsertButton.Name = "InsertButton";
        this.InsertButton.Size = new System.Drawing.Size(54, 44);
        this.InsertButton.Text = "Insert";
        this.InsertButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.InsertButton.ToolTipText = "Version number";
        // 
        // InsertNewFolder
        // 
        this.InsertNewFolder.Image = global::APSoil.Properties.Resources.folder_new;
        this.InsertNewFolder.Name = "InsertNewFolder";
        this.InsertNewFolder.Size = new System.Drawing.Size(159, 22);
        this.InsertNewFolder.Text = "New folder";
        this.InsertNewFolder.Click += new System.EventHandler(this.OnInsertNewFolderClick);
        // 
        // InsertNewSoil
        // 
        this.InsertNewSoil.Image = global::APSoil.Properties.Resources.shovel_new;
        this.InsertNewSoil.Name = "InsertNewSoil";
        this.InsertNewSoil.Size = new System.Drawing.Size(159, 22);
        this.InsertNewSoil.Text = "New soil";
        this.InsertNewSoil.Click += new System.EventHandler(this.OnInsertNewSoilClick);
        // 
        // InsertNewSample
        // 
        this.InsertNewSample.Image = global::APSoil.Properties.Resources.box_new;
        this.InsertNewSample.Name = "InsertNewSample";
        this.InsertNewSample.Size = new System.Drawing.Size(159, 22);
        this.InsertNewSample.Text = "New sample";
        this.InsertNewSample.Click += new System.EventHandler(this.OnInsertNewSampleClick);
        // 
        // ImportButton
        // 
        this.ImportButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ImportSoilsFile,
            this.ImportParFile,
            this.ImportSpreadsheet,
            this.ImportW2File});
        this.ImportButton.Image = global::APSoil.Properties.Resources.import1;
        this.ImportButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.ImportButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.ImportButton.Name = "ImportButton";
        this.ImportButton.Size = new System.Drawing.Size(59, 44);
        this.ImportButton.Text = "Import";
        this.ImportButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.ImportButton.ToolTipText = "Version number";
        // 
        // ImportSoilsFile
        // 
        this.ImportSoilsFile.Image = global::APSoil.Properties.Resources.document;
        this.ImportSoilsFile.Name = "ImportSoilsFile";
        this.ImportSoilsFile.Size = new System.Drawing.Size(221, 22);
        this.ImportSoilsFile.Text = "From another .soils file";
        this.ImportSoilsFile.Click += new System.EventHandler(this.OnImportSoilsClick);
        // 
        // ImportParFile
        // 
        this.ImportParFile.Image = global::APSoil.Properties.Resources.par_file;
        this.ImportParFile.Name = "ImportParFile";
        this.ImportParFile.Size = new System.Drawing.Size(221, 22);
        this.ImportParFile.Text = "From a .par file";
        this.ImportParFile.Click += new System.EventHandler(this.OnImportParClick);
        // 
        // ImportSpreadsheet
        // 
        this.ImportSpreadsheet.Image = global::APSoil.Properties.Resources.excel;
        this.ImportSpreadsheet.Name = "ImportSpreadsheet";
        this.ImportSpreadsheet.Size = new System.Drawing.Size(221, 22);
        this.ImportSpreadsheet.Text = "From a spreadsheet";
        this.ImportSpreadsheet.Click += new System.EventHandler(this.OnImportSpreadsheetClick);
        // 
        // ImportW2File
        // 
        this.ImportW2File.Image = global::APSoil.Properties.Resources.w2_file;
        this.ImportW2File.Name = "ImportW2File";
        this.ImportW2File.Size = new System.Drawing.Size(221, 22);
        this.ImportW2File.Text = "From .&w2 / .n2";
        this.ImportW2File.Click += new System.EventHandler(this.OnImportW2Click);
        // 
        // ExportButton
        // 
        this.ExportButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ExportSoilsFile,
            this.ExportParFile,
            this.ExportSpreadsheet});
        this.ExportButton.Image = global::APSoil.Properties.Resources.export2;
        this.ExportButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.ExportButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.ExportButton.Name = "ExportButton";
        this.ExportButton.Size = new System.Drawing.Size(57, 44);
        this.ExportButton.Text = "Export";
        this.ExportButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.ExportButton.ToolTipText = "Version number";
        // 
        // ExportSoilsFile
        // 
        this.ExportSoilsFile.Image = global::APSoil.Properties.Resources.document;
        this.ExportSoilsFile.Name = "ExportSoilsFile";
        this.ExportSoilsFile.Size = new System.Drawing.Size(206, 22);
        this.ExportSoilsFile.Text = "To &another .soils file";
        this.ExportSoilsFile.Click += new System.EventHandler(this.OnExportSoilsClick);
        // 
        // ExportParFile
        // 
        this.ExportParFile.Image = global::APSoil.Properties.Resources.par_file;
        this.ExportParFile.Name = "ExportParFile";
        this.ExportParFile.Size = new System.Drawing.Size(206, 22);
        this.ExportParFile.Text = "To a .&par file";
        this.ExportParFile.Click += new System.EventHandler(this.OnExportParClick);
        // 
        // ExportSpreadsheet
        // 
        this.ExportSpreadsheet.Image = global::APSoil.Properties.Resources.excel;
        this.ExportSpreadsheet.Name = "ExportSpreadsheet";
        this.ExportSpreadsheet.Size = new System.Drawing.Size(206, 22);
        this.ExportSpreadsheet.Text = "To a &spreadsheet";
        this.ExportSpreadsheet.Click += new System.EventHandler(this.OnExportSpreadsheetClick);
        // 
        // toolStripSeparator3
        // 
        this.toolStripSeparator3.Name = "toolStripSeparator3";
        this.toolStripSeparator3.Size = new System.Drawing.Size(6, 47);
        // 
        // CheckSoilsButton
        // 
        this.CheckSoilsButton.CheckOnClick = true;
        this.CheckSoilsButton.Image = global::APSoil.Properties.Resources.check2;
        this.CheckSoilsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.CheckSoilsButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.CheckSoilsButton.Name = "CheckSoilsButton";
        this.CheckSoilsButton.Size = new System.Drawing.Size(75, 44);
        this.CheckSoilsButton.Text = "Check soils";
        this.CheckSoilsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.CheckSoilsButton.ToolTipText = "Version number";
        this.CheckSoilsButton.Click += new System.EventHandler(this.OnCheckSoilsClick);
        // 
        // SortButton
        // 
        this.SortButton.Image = global::APSoil.Properties.Resources.sort_ascending;
        this.SortButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
        this.SortButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        this.SortButton.Name = "SortButton";
        this.SortButton.Size = new System.Drawing.Size(65, 44);
        this.SortButton.Text = "Sort soils";
        this.SortButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
        this.SortButton.ToolTipText = "Version number";
        this.SortButton.Click += new System.EventHandler(this.OnSortSoilsClick);
        // 
        // ImportSpreadsheetDialog
        // 
        this.ImportSpreadsheetDialog.DefaultExt = "xls";
        this.ImportSpreadsheetDialog.Filter = "XLS files (*.xls)|*.xls";
        this.ImportSpreadsheetDialog.Title = "Import soils from spreadsheet";
        // 
        // FolderBrowserDialog
        // 
        this.FolderBrowserDialog.Description = "Select the folder containing your w2, n2, p2 files";
        this.FolderBrowserDialog.ShowNewFolderButton = false;
        // 
        // ImportParDialog
        // 
        this.ImportParDialog.DefaultExt = "par";
        this.ImportParDialog.DereferenceLinks = false;
        this.ImportParDialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
        this.ImportParDialog.InitialDirectory = "c:\\";
        this.ImportParDialog.Multiselect = true;
        this.ImportParDialog.Title = "Select one or more PAR files";
        // 
        // ParExportDialog
        // 
        this.ParExportDialog.DefaultExt = "par";
        this.ParExportDialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
        this.ParExportDialog.Title = "Enter output file name for soil";
        // 
        // ImportSoilsDialog
        // 
        this.ImportSoilsDialog.DefaultExt = "soils";
        this.ImportSoilsDialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*";
        this.ImportSoilsDialog.Multiselect = true;
        this.ImportSoilsDialog.Title = "Select one or more .soils files to import";
        // 
        // SimExportDialog
        // 
        this.SimExportDialog.DefaultExt = "sim";
        this.SimExportDialog.Filter = "Sim files (*.sim)|*.sim|All files (*.*)|*.*";
        this.SimExportDialog.Title = "Enter output file name for soil";
        // 
        // ExportSoilsDialog
        // 
        this.ExportSoilsDialog.DefaultExt = "soils";
        this.ExportSoilsDialog.Filter = "Soils database (*.soils)|*.soils";
        this.ExportSoilsDialog.OverwritePrompt = false;
        this.ExportSoilsDialog.Title = "Select a soils database file to send the selected soils to.";
        // 
        // ExportSpreadsheetDialog
        // 
        this.ExportSpreadsheetDialog.DefaultExt = "xls";
        this.ExportSpreadsheetDialog.Filter = "XLS files (*.xls)|*.xls";
        this.ExportSpreadsheetDialog.Title = "Select a spreadsheet file to send the selected soils to.";
        // 
        // SmallImages
        // 
        this.SmallImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("SmallImages.ImageStream")));
        this.SmallImages.TransparentColor = System.Drawing.Color.Transparent;
        this.SmallImages.Images.SetKeyName(0, "");
        this.SmallImages.Images.SetKeyName(1, "folder.png");
        this.SmallImages.Images.SetKeyName(2, "");
        this.SmallImages.Images.SetKeyName(3, "");
        this.SmallImages.Images.SetKeyName(4, "box.png");
        // 
        // MainForm
        // 
        this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
        this.CausesValidation = false;
        this.ClientSize = new System.Drawing.Size(784, 578);
        this.Controls.Add(this.ToolStripContainer);
        this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
        this.KeyPreview = true;
        this.Name = "MainForm";
        this.Text = "Apsoil";
        this.Closing += new System.ComponentModel.CancelEventHandler(this.MainForm_Closing);
        this.Load += new System.EventHandler(this.MainForm_Load);
        this.ToolStripContainer.TopToolStripPanel.ResumeLayout(false);
        this.ToolStripContainer.TopToolStripPanel.PerformLayout();
        this.ToolStripContainer.ResumeLayout(false);
        this.ToolStripContainer.PerformLayout();
        this.toolStrip1.ResumeLayout(false);
        this.toolStrip1.PerformLayout();
        this.ResumeLayout(false);

		}
		#endregion

		#region Startup methods
		public void LoadFile(string FileName)
			{
			// Load the specified file. Called when a command line arg is used.
			CommandLineFileName = FileName.Replace("\"", "");
			}

		private void MainForm_Load(object sender, System.EventArgs e)
			{
			SplashForm Splash = new SplashForm();
			Splash.Setup(this.VersionString());
			Splash.ShowDialog();
            ToolStripContainer.Size = new Size(ToolStripContainer.ContentPanel.Width, 103);

			// Form has been loaded - set everything up
		
			Apsoil = new ApsoilController(".soils", "Soils files (*.soils)|*.soils|" + 
											"All files (*.*)|*.*", 
											"Apsoil",
											SmallImages);
			Apsoil.NewDataEvent += new ApsoilController.NotifyEventHandler(OnNewDataEvent);
			Apsoil.SelectionChangedEvent += new ApsoilController.NotifyEventHandler(SetFunctionality);
			Apsoil.DataChangedEvent += new ApsoilController.NotifyEventHandler(SetFunctionality);

			// Show the Simulation Explorer.
			SoilExplorer = new ExplorerUI(this, Apsoil);
			SoilExplorer.Dock = DockStyle.Fill;
			SoilExplorer.Parent = ToolStripContainer.ContentPanel;
			SoilExplorer.Visible = true;
			SoilExplorer.ExpandAll = false;
			SortButton.Checked = (APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "Soil", "SortByName") == "yes");
            SoilExplorer.SortAll = SortButton.Checked;

			// Load up the file from the command line if necessary.
			if (CommandLineFileName != null)
				Apsoil.FileOpen(CommandLineFileName);

			SetFunctionality();
			}
		#endregion

		#region Application level events
		private void OnNewDataEvent()
			{
			// New data has entered the system.
			// This is usually caused by FileNew,
			// FileOpen etc.

			APSIMChangeTool.Upgrade(Apsoil.AllData);
			SetFunctionality();
			}

		private void SetFunctionality()
			{
			// User has changed something e.g. selection / new data.
			// Enable / Disable bits of functionality as 
			// required. i.e. ensure program is in a 
			// consistant state.
			bool SomethingInTree = (Apsoil.AllData != null && Apsoil.AllData.ChildList(null).Count > 0);

			bool OnlySoilsSelected = Apsoil.SomethingIsSelected;
			foreach (APSIMData SelectedData in Apsoil.SelectedData)
				OnlySoilsSelected = (SelectedData.Type.ToLower() == "soil");

			SaveButton.Enabled = Apsoil.AllowChanges;
			SaveButton.Enabled = Apsoil.AllowChanges;

			CutButton.Enabled = Apsoil.AllowCut;
			CopyButton.Enabled = Apsoil.AllowCopy;
			PasteButton.Enabled = Apsoil.AllowPaste;

			bool FolderIsSelected = false;
			if (Apsoil.SelectedPaths.Count == 1)
				{
				APSIMData SelectedData = (APSIMData) Apsoil.SelectedData[0];
				string SelectedType = SelectedData.Type.ToLower();
				FolderIsSelected = (SelectedType == "soils" || SelectedType == "folder");
				}
			ImportSpreadsheet.Enabled = FolderIsSelected && Apsoil.AllowChanges;
			ImportParFile.Enabled = FolderIsSelected && Apsoil.AllowChanges;
			ImportSoilsFile.Enabled = FolderIsSelected && Apsoil.AllowChanges;
			ImportW2File.Enabled = FolderIsSelected && Apsoil.AllowChanges;
			ExportParFile.Enabled = OnlySoilsSelected;
			ExportSoilsFile.Enabled = OnlySoilsSelected;
			CheckSoilsButton.Enabled = SomethingInTree;
			SortButton.Enabled = SomethingInTree;
			PrintButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			InsertNewFolder.Enabled = (Apsoil.AllowChanges && Apsoil.AllowInsertFolder);
			InsertNewSoil.Enabled = (Apsoil.AllowChanges && Apsoil.AllowInsertSoil);
			InsertNewSample.Enabled = (Apsoil.AllowChanges && Apsoil.AllowInsertSample);
			}

		private void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
			{
			// User is closing down - save our work.
			e.Cancel = !Apsoil.FileSaveAfterPrompt();
			}

		#endregion

		#region Button event handlers

        private void OnNewFileClick(object sender, EventArgs e)
			{
			APSIMData NewData = new APSIMData("soils", "");
			Apsoil.FileNew(NewData);
			}

		private void OnOpenFileClick(object sender, EventArgs e)
			{
			Apsoil.FileOpen();
			}

		private void OnSaveFileClick(object sender, EventArgs e)
			{
			Apsoil.FileSave();
			}

		private void OnSaveAsClick(object sender, EventArgs e)
			{
			Apsoil.FileSaveAs();
			}


        private void OnPrintClick(object sender, EventArgs e)
            {
            Apsoil.Print();
            }

        private void OnCutClick(object sender, EventArgs e)
            {
            Apsoil.Cut();
            }

        private void OnCopyClick(object sender, EventArgs e)
            {
            Apsoil.Copy();
            }

        private void OnPasteClick(object sender, EventArgs e)
            {
            Apsoil.Paste();
            }

        private void OnHelpClick(object sender, EventArgs e)
            {
            MessageBox.Show(VersionString(), "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }

        private string VersionString()
            {
            System.Version Version = Assembly.GetExecutingAssembly().GetName().Version;
            return "Version " + Version.Major.ToString() + "." + Version.Minor.ToString()
                    + " (" + Version.Build.ToString() + "." + Version.Revision.ToString() + ")";
            }

        private void OnInsertNewFolderClick(object sender, EventArgs e)
            {
            Apsoil.InsertFolder();
            }

        private void OnInsertNewSoilClick(object sender, EventArgs e)
            {
            Apsoil.InsertSoil();
            }

        private void OnInsertNewSampleClick(object sender, EventArgs e)
            {
            Apsoil.InsertSample();
            }

        private void OnImportSoilsClick(object sender, EventArgs e)
            {
            try
                {
                if (ImportSoilsDialog.ShowDialog() == DialogResult.OK)
                    {
                    foreach (string File in ImportSoilsDialog.FileNames)
                        {
                        APSIMData NewData = new APSIMData();
                        NewData.LoadFromFile(File);
                        APSIMChangeTool.Upgrade(NewData);
                        Apsoil.AddXMLToSelected(NewData.XML);
                        }
                    }
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            SoilExplorer.Refresh();
            }
 
        private void OnImportParClick(object sender, EventArgs e)
            {
            try
                {
                if (ImportParDialog.ShowDialog() == DialogResult.OK)
                    {
                    foreach (string File in ImportParDialog.FileNames)
                        ParFileImporter.ImportParFile(File, Apsoil);
                    }
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

        private void OnImportSpreadsheetClick(object sender, EventArgs e)
            {
            try
                {
                if (ImportSpreadsheetDialog.ShowDialog() == DialogResult.OK)
                    SoilSpreadsheet.ImportFromFile(ImportSpreadsheetDialog.FileName, Apsoil);
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

        private void OnImportW2Click(object sender, EventArgs e)
            {
            try
                {
                if (FolderBrowserDialog.ShowDialog() == DialogResult.OK)
                    {
                    string[] Files = Directory.GetFiles(FolderBrowserDialog.SelectedPath, "*.w2");
                    foreach (string File in Files)
                        ParFileImporter.ImportW2N2P2(File, Apsoil);
                    }
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

        private void OnExportSoilsClick(object sender, EventArgs e)
            {
            // User has clicked on Export selected soils to another .soils file.
            if (ExportSoilsDialog.ShowDialog() == DialogResult.OK)
                {
                APSIMData ForeignSoils;
                if (!File.Exists(ExportSoilsDialog.FileName))
                    {
                    ForeignSoils = new APSIMData("soils", "");
                    }
                else
                    {
                    ForeignSoils = new APSIMData();
                    ForeignSoils.LoadFromFile(ExportSoilsDialog.FileName);
                    }

                foreach (APSIMData SelectedData in Apsoil.SelectedData)
                    ForeignSoils.Add(SelectedData);
                ForeignSoils.SaveToFile(ExportSoilsDialog.FileName);
                MessageBox.Show("Soils have been exported to: " + ExportSoilsDialog.FileName, "For your information",
                                MessageBoxButtons.OK, MessageBoxIcon.Information);
                }
            }

        private void OnExportParClick(object sender, EventArgs e)
            {
            // User has clicked on export selected file to par
            try
                {
                if (ParExportDialog.ShowDialog() == DialogResult.OK)
                    {
                    File.Delete(ParExportDialog.FileName);
                    foreach (APSIMData SelectedData in Apsoil.SelectedData)
                        {
                        Soil SoilToExport = new Soil(SelectedData);
                        string Errors = SoilToExport.CheckForErrors();
                        if (Errors != "")
                            {
                            ErrorMessageForm ErrorForm = new ErrorMessageForm();
                            ErrorForm.SetText(Errors);
                            ErrorForm.ShowDialog();
                            }
                        else
                            {
                            SoilToExport.ExportToPar(ParExportDialog.FileName, SoilToExport.Name, true);
                            }
                        }
                    }
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }

            }

        private void OnExportSpreadsheetClick(object sender, EventArgs e)
            {
            // User has clicked on export selected file to spreadsheet
            try
                {
                if (ExportSpreadsheetDialog.ShowDialog() == DialogResult.OK)
                    {
                    File.Delete(ExportSpreadsheetDialog.FileName);
                    APSIMData SoilsToExport = new APSIMData("soils", "");
                    foreach (APSIMData SelectedData in Apsoil.SelectedData)
                        SoilsToExport.Add(SelectedData);
                    SoilSpreadsheet.ExportToFile(ExportSpreadsheetDialog.FileName, SoilsToExport);
                    MessageBox.Show("Soils have been successfully exported to '" + ExportSpreadsheetDialog.FileName + "'",
                                    "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
                    }
                }
            catch (Exception err)
                {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }

        private void OnCheckSoilsClick(object sender, EventArgs e)
            {
            // User wants to check all soils for consistency
            Cursor.Current = Cursors.WaitCursor;
            string ErrorMessage = "";
            Apsoil.CheckAllSoils(Apsoil.AllData, ref ErrorMessage);
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

        private void OnSortSoilsClick(object sender, EventArgs e)
            {
            // User has changed the sort menu item.
            string SortValue = "no";
            if (SortButton.Checked)
                SortValue = "yes";
            APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "soil", "SortByName", SortValue);
            SoilExplorer.SortAll = SortButton.Checked;
            }


    #endregion







		}
	}
