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
		private System.Windows.Forms.ImageList SmallImages;
		internal System.Windows.Forms.ImageList ButtonImageList;
		private System.Windows.Forms.Panel MainPanel;
		private System.Windows.Forms.OpenFileDialog ImportSpreadsheetDialog;
		private System.Windows.Forms.FolderBrowserDialog FolderBrowserDialog;
		private System.Windows.Forms.OpenFileDialog ImportParDialog;
		private System.Windows.Forms.SaveFileDialog ParExportDialog;
		private System.Windows.Forms.SaveFileDialog SimExportDialog;
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.OpenFileDialog ImportSoilsDialog;
		private System.Windows.Forms.OpenFileDialog openFileDialog2;
		private System.Windows.Forms.SaveFileDialog ExportSoilsDialog;
		private System.Windows.Forms.Panel MainToolbarPanel;
		private System.Windows.Forms.ToolBar MainToolBar;
		private System.Windows.Forms.ToolBarButton FileButton;
		private System.Windows.Forms.ToolBarButton ImportButton;
		private System.Windows.Forms.ToolBarButton ExportButton;
		private System.Windows.Forms.Panel ExportToolBarPanel;
		private System.Windows.Forms.Panel ImportToolBarPanel;
		private System.Windows.Forms.Panel FileToolBarPanel;
		private System.Windows.Forms.Panel SoilsToolBarPanel;
		private System.Windows.Forms.ToolBarButton SoilsButton;
		private System.Windows.Forms.Splitter splitter1;
		private System.Windows.Forms.Panel SoilPanel1;
		private System.Windows.Forms.ToolBar SoilToolBar1;
		private System.Windows.Forms.ToolBarButton CutButton;
		private System.Windows.Forms.ToolBarButton CopyButton;
		private System.Windows.Forms.ToolBarButton PasteButton;
		private System.Windows.Forms.Label SoilLabel1;
		private System.Windows.Forms.Panel SoilPanel2;
		private System.Windows.Forms.Label SoilLabel2;
		private System.Windows.Forms.ToolBar SoilToolBar2;
		private System.Windows.Forms.ToolBarButton AddCropButton;
		private System.Windows.Forms.ToolBarButton DeleteCropButton;
		private System.Windows.Forms.ToolBarButton ReorderCropButton;
		private System.Windows.Forms.Panel SoilPanel3;
		private System.Windows.Forms.Label SoilLabel3;
		private System.Windows.Forms.Splitter splitter2;
		private System.Windows.Forms.ToolBar SoilToolBar3;
		private System.Windows.Forms.ToolBarButton AddNoteButton;
		private System.Windows.Forms.ToolBarButton DeleteNoteButton;
		private System.Windows.Forms.Splitter splitter3;
		private System.Windows.Forms.Panel FilePanel1;
		private System.Windows.Forms.Label FileLabel1;
		private System.Windows.Forms.ToolBar FileToolBar1;
		private System.Windows.Forms.Splitter splitter4;
		private System.Windows.Forms.Panel FilePanel2;
		private System.Windows.Forms.Label FileLabel2;
		private System.Windows.Forms.ToolBar FileToolBar;
		private System.Windows.Forms.ToolBarButton VersionButton;
		private System.Windows.Forms.Splitter splitter5;
		private System.Windows.Forms.ToolBarButton NewFileButton;
		private System.Windows.Forms.ToolBarButton OpenFileButton;
		private System.Windows.Forms.ToolBarButton SaveFileButton;
		private System.Windows.Forms.ToolBarButton SaveFileAsButton;
		private System.Windows.Forms.Panel ToolPanel1;
		private System.Windows.Forms.ToolBar ToolToolBar;
		private System.Windows.Forms.Label ToolLabel1;
		private System.Windows.Forms.ToolBarButton CheckSoilsButton;
		private System.Windows.Forms.ToolBarButton SortButton;
		private System.Windows.Forms.Panel ImportPanel1;
		private System.Windows.Forms.Label ImportLabel1;
		private System.Windows.Forms.ToolBar ImportToolBar;
		private System.Windows.Forms.ToolBarButton FromSoilsButton;
		private System.Windows.Forms.ToolBarButton FromParButton;
		private System.Windows.Forms.ToolBarButton FromSpreadsheetButton;
		private System.Windows.Forms.ToolBarButton Fromw2n2Button;
		private System.Windows.Forms.Splitter splitter6;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.ToolBar ExportToolBar;
		private System.Windows.Forms.ToolBarButton ToSoilsButton;
		private System.Windows.Forms.ToolBarButton ToParButton;
		private System.Windows.Forms.Splitter splitter7;
		private System.Windows.Forms.Panel PrintPanel;
		private System.Windows.Forms.ToolBar PrintToolBar;
		private System.Windows.Forms.Label PrintLabel;
		private System.Windows.Forms.ToolBarButton PrintButton;
		private System.Windows.Forms.Splitter splitter8;
		private string CommandLineFileName;

		#region Constructor / Destructor / Main
		public MainForm()
			{
		    //Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA";
			Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A";

			//
			// Required for Windows Form Designer support
			//
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
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
			this.SmallImages = new System.Windows.Forms.ImageList(this.components);
			this.ButtonImageList = new System.Windows.Forms.ImageList(this.components);
			this.MainPanel = new System.Windows.Forms.Panel();
			this.ImportSpreadsheetDialog = new System.Windows.Forms.OpenFileDialog();
			this.FolderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
			this.ImportParDialog = new System.Windows.Forms.OpenFileDialog();
			this.ParExportDialog = new System.Windows.Forms.SaveFileDialog();
			this.ImportSoilsDialog = new System.Windows.Forms.OpenFileDialog();
			this.SimExportDialog = new System.Windows.Forms.SaveFileDialog();
			this.openFileDialog2 = new System.Windows.Forms.OpenFileDialog();
			this.ExportSoilsDialog = new System.Windows.Forms.SaveFileDialog();
			this.SoilsToolBarPanel = new System.Windows.Forms.Panel();
			this.ToolPanel1 = new System.Windows.Forms.Panel();
			this.ToolToolBar = new System.Windows.Forms.ToolBar();
			this.CheckSoilsButton = new System.Windows.Forms.ToolBarButton();
			this.SortButton = new System.Windows.Forms.ToolBarButton();
			this.ToolLabel1 = new System.Windows.Forms.Label();
			this.splitter3 = new System.Windows.Forms.Splitter();
			this.SoilPanel3 = new System.Windows.Forms.Panel();
			this.SoilToolBar3 = new System.Windows.Forms.ToolBar();
			this.AddNoteButton = new System.Windows.Forms.ToolBarButton();
			this.DeleteNoteButton = new System.Windows.Forms.ToolBarButton();
			this.SoilLabel3 = new System.Windows.Forms.Label();
			this.splitter2 = new System.Windows.Forms.Splitter();
			this.SoilPanel2 = new System.Windows.Forms.Panel();
			this.SoilToolBar2 = new System.Windows.Forms.ToolBar();
			this.AddCropButton = new System.Windows.Forms.ToolBarButton();
			this.DeleteCropButton = new System.Windows.Forms.ToolBarButton();
			this.ReorderCropButton = new System.Windows.Forms.ToolBarButton();
			this.SoilLabel2 = new System.Windows.Forms.Label();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.SoilPanel1 = new System.Windows.Forms.Panel();
			this.SoilToolBar1 = new System.Windows.Forms.ToolBar();
			this.CutButton = new System.Windows.Forms.ToolBarButton();
			this.CopyButton = new System.Windows.Forms.ToolBarButton();
			this.PasteButton = new System.Windows.Forms.ToolBarButton();
			this.SoilLabel1 = new System.Windows.Forms.Label();
			this.splitter8 = new System.Windows.Forms.Splitter();
			this.PrintPanel = new System.Windows.Forms.Panel();
			this.PrintToolBar = new System.Windows.Forms.ToolBar();
			this.PrintButton = new System.Windows.Forms.ToolBarButton();
			this.PrintLabel = new System.Windows.Forms.Label();
			this.ExportToolBarPanel = new System.Windows.Forms.Panel();
			this.splitter7 = new System.Windows.Forms.Splitter();
			this.panel1 = new System.Windows.Forms.Panel();
			this.ExportToolBar = new System.Windows.Forms.ToolBar();
			this.ToSoilsButton = new System.Windows.Forms.ToolBarButton();
			this.ToParButton = new System.Windows.Forms.ToolBarButton();
			this.label1 = new System.Windows.Forms.Label();
			this.ImportToolBarPanel = new System.Windows.Forms.Panel();
			this.splitter6 = new System.Windows.Forms.Splitter();
			this.ImportPanel1 = new System.Windows.Forms.Panel();
			this.ImportToolBar = new System.Windows.Forms.ToolBar();
			this.FromSoilsButton = new System.Windows.Forms.ToolBarButton();
			this.FromParButton = new System.Windows.Forms.ToolBarButton();
			this.FromSpreadsheetButton = new System.Windows.Forms.ToolBarButton();
			this.Fromw2n2Button = new System.Windows.Forms.ToolBarButton();
			this.ImportLabel1 = new System.Windows.Forms.Label();
			this.FileToolBarPanel = new System.Windows.Forms.Panel();
			this.splitter5 = new System.Windows.Forms.Splitter();
			this.FilePanel2 = new System.Windows.Forms.Panel();
			this.FileToolBar = new System.Windows.Forms.ToolBar();
			this.VersionButton = new System.Windows.Forms.ToolBarButton();
			this.FileLabel2 = new System.Windows.Forms.Label();
			this.splitter4 = new System.Windows.Forms.Splitter();
			this.FilePanel1 = new System.Windows.Forms.Panel();
			this.FileToolBar1 = new System.Windows.Forms.ToolBar();
			this.NewFileButton = new System.Windows.Forms.ToolBarButton();
			this.OpenFileButton = new System.Windows.Forms.ToolBarButton();
			this.SaveFileButton = new System.Windows.Forms.ToolBarButton();
			this.SaveFileAsButton = new System.Windows.Forms.ToolBarButton();
			this.FileLabel1 = new System.Windows.Forms.Label();
			this.MainToolbarPanel = new System.Windows.Forms.Panel();
			this.MainToolBar = new System.Windows.Forms.ToolBar();
			this.FileButton = new System.Windows.Forms.ToolBarButton();
			this.SoilsButton = new System.Windows.Forms.ToolBarButton();
			this.ImportButton = new System.Windows.Forms.ToolBarButton();
			this.ExportButton = new System.Windows.Forms.ToolBarButton();
			this.SoilsToolBarPanel.SuspendLayout();
			this.ToolPanel1.SuspendLayout();
			this.SoilPanel3.SuspendLayout();
			this.SoilPanel2.SuspendLayout();
			this.SoilPanel1.SuspendLayout();
			this.PrintPanel.SuspendLayout();
			this.ExportToolBarPanel.SuspendLayout();
			this.panel1.SuspendLayout();
			this.ImportToolBarPanel.SuspendLayout();
			this.ImportPanel1.SuspendLayout();
			this.FileToolBarPanel.SuspendLayout();
			this.FilePanel2.SuspendLayout();
			this.FilePanel1.SuspendLayout();
			this.MainToolbarPanel.SuspendLayout();
			this.SuspendLayout();
			// 
			// SmallImages
			// 
			this.SmallImages.ImageSize = new System.Drawing.Size(16, 16);
			this.SmallImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("SmallImages.ImageStream")));
			this.SmallImages.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// ButtonImageList
			// 
			this.ButtonImageList.ImageSize = new System.Drawing.Size(24, 24);
			this.ButtonImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ButtonImageList.ImageStream")));
			this.ButtonImageList.TransparentColor = System.Drawing.SystemColors.Control;
			// 
			// MainPanel
			// 
			this.MainPanel.CausesValidation = false;
			this.MainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.MainPanel.Location = new System.Drawing.Point(0, 312);
			this.MainPanel.Name = "MainPanel";
			this.MainPanel.Size = new System.Drawing.Size(848, 266);
			this.MainPanel.TabIndex = 0;
			this.MainPanel.TabStop = true;
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
			// SoilsToolBarPanel
			// 
			this.SoilsToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue;
			this.SoilsToolBarPanel.Controls.Add(this.ToolPanel1);
			this.SoilsToolBarPanel.Controls.Add(this.splitter3);
			this.SoilsToolBarPanel.Controls.Add(this.SoilPanel3);
			this.SoilsToolBarPanel.Controls.Add(this.splitter2);
			this.SoilsToolBarPanel.Controls.Add(this.SoilPanel2);
			this.SoilsToolBarPanel.Controls.Add(this.splitter1);
			this.SoilsToolBarPanel.Controls.Add(this.SoilPanel1);
			this.SoilsToolBarPanel.Controls.Add(this.splitter8);
			this.SoilsToolBarPanel.Controls.Add(this.PrintPanel);
			this.SoilsToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.SoilsToolBarPanel.Location = new System.Drawing.Point(0, 96);
			this.SoilsToolBarPanel.Name = "SoilsToolBarPanel";
			this.SoilsToolBarPanel.Size = new System.Drawing.Size(848, 74);
			this.SoilsToolBarPanel.TabIndex = 20;
			this.SoilsToolBarPanel.Visible = false;
			// 
			// ToolPanel1
			// 
			this.ToolPanel1.BackColor = System.Drawing.Color.Transparent;
			this.ToolPanel1.Controls.Add(this.ToolToolBar);
			this.ToolPanel1.Controls.Add(this.ToolLabel1);
			this.ToolPanel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.ToolPanel1.Location = new System.Drawing.Point(583, 0);
			this.ToolPanel1.Name = "ToolPanel1";
			this.ToolPanel1.Size = new System.Drawing.Size(152, 74);
			this.ToolPanel1.TabIndex = 25;
			// 
			// ToolToolBar
			// 
			this.ToolToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ToolToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																						   this.CheckSoilsButton,
																						   this.SortButton});
			this.ToolToolBar.Divider = false;
			this.ToolToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
			this.ToolToolBar.DropDownArrows = true;
			this.ToolToolBar.ImageList = this.ButtonImageList;
			this.ToolToolBar.Location = new System.Drawing.Point(0, 20);
			this.ToolToolBar.Name = "ToolToolBar";
			this.ToolToolBar.ShowToolTips = true;
			this.ToolToolBar.Size = new System.Drawing.Size(152, 48);
			this.ToolToolBar.TabIndex = 16;
			this.ToolToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// CheckSoilsButton
			// 
			this.CheckSoilsButton.ImageIndex = 23;
			this.CheckSoilsButton.Text = "Chec&k soils";
			this.CheckSoilsButton.ToolTipText = "Ensure soils are ok to run in APSIM";
			// 
			// SortButton
			// 
			this.SortButton.ImageIndex = 24;
			this.SortButton.Text = "&Sort soils";
			this.SortButton.ToolTipText = "Sort soils in the tree below";
			// 
			// ToolLabel1
			// 
			this.ToolLabel1.BackColor = System.Drawing.Color.SteelBlue;
			this.ToolLabel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.ToolLabel1.ForeColor = System.Drawing.Color.White;
			this.ToolLabel1.Location = new System.Drawing.Point(0, 0);
			this.ToolLabel1.Name = "ToolLabel1";
			this.ToolLabel1.Size = new System.Drawing.Size(152, 20);
			this.ToolLabel1.TabIndex = 15;
			this.ToolLabel1.Text = "Tools";
			this.ToolLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// splitter3
			// 
			this.splitter3.BackColor = System.Drawing.Color.LightGray;
			this.splitter3.Enabled = false;
			this.splitter3.Location = new System.Drawing.Point(582, 0);
			this.splitter3.Name = "splitter3";
			this.splitter3.Size = new System.Drawing.Size(1, 74);
			this.splitter3.TabIndex = 24;
			this.splitter3.TabStop = false;
			// 
			// SoilPanel3
			// 
			this.SoilPanel3.BackColor = System.Drawing.Color.Transparent;
			this.SoilPanel3.Controls.Add(this.SoilToolBar3);
			this.SoilPanel3.Controls.Add(this.SoilLabel3);
			this.SoilPanel3.Dock = System.Windows.Forms.DockStyle.Left;
			this.SoilPanel3.Location = new System.Drawing.Point(433, 0);
			this.SoilPanel3.Name = "SoilPanel3";
			this.SoilPanel3.Size = new System.Drawing.Size(149, 74);
			this.SoilPanel3.TabIndex = 22;
			// 
			// SoilToolBar3
			// 
			this.SoilToolBar3.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.SoilToolBar3.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.AddNoteButton,
																							this.DeleteNoteButton});
			this.SoilToolBar3.Divider = false;
			this.SoilToolBar3.Dock = System.Windows.Forms.DockStyle.Fill;
			this.SoilToolBar3.DropDownArrows = true;
			this.SoilToolBar3.ImageList = this.ButtonImageList;
			this.SoilToolBar3.Location = new System.Drawing.Point(0, 20);
			this.SoilToolBar3.Name = "SoilToolBar3";
			this.SoilToolBar3.ShowToolTips = true;
			this.SoilToolBar3.Size = new System.Drawing.Size(149, 48);
			this.SoilToolBar3.TabIndex = 19;
			this.SoilToolBar3.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// AddNoteButton
			// 
			this.AddNoteButton.ImageIndex = 29;
			this.AddNoteButton.Text = "&Add note";
			this.AddNoteButton.ToolTipText = "Add a note to the grid below";
			// 
			// DeleteNoteButton
			// 
			this.DeleteNoteButton.ImageIndex = 30;
			this.DeleteNoteButton.Text = "&Delete note";
			this.DeleteNoteButton.ToolTipText = "Delete all notes from the selected cells.";
			// 
			// SoilLabel3
			// 
			this.SoilLabel3.BackColor = System.Drawing.Color.SteelBlue;
			this.SoilLabel3.Dock = System.Windows.Forms.DockStyle.Top;
			this.SoilLabel3.ForeColor = System.Drawing.Color.White;
			this.SoilLabel3.Location = new System.Drawing.Point(0, 0);
			this.SoilLabel3.Name = "SoilLabel3";
			this.SoilLabel3.Size = new System.Drawing.Size(149, 20);
			this.SoilLabel3.TabIndex = 15;
			this.SoilLabel3.Text = "Notes";
			this.SoilLabel3.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// splitter2
			// 
			this.splitter2.BackColor = System.Drawing.Color.LightGray;
			this.splitter2.Enabled = false;
			this.splitter2.Location = new System.Drawing.Point(432, 0);
			this.splitter2.Name = "splitter2";
			this.splitter2.Size = new System.Drawing.Size(1, 74);
			this.splitter2.TabIndex = 23;
			this.splitter2.TabStop = false;
			// 
			// SoilPanel2
			// 
			this.SoilPanel2.BackColor = System.Drawing.Color.Transparent;
			this.SoilPanel2.Controls.Add(this.SoilToolBar2);
			this.SoilPanel2.Controls.Add(this.SoilLabel2);
			this.SoilPanel2.Dock = System.Windows.Forms.DockStyle.Left;
			this.SoilPanel2.Location = new System.Drawing.Point(186, 0);
			this.SoilPanel2.Name = "SoilPanel2";
			this.SoilPanel2.Size = new System.Drawing.Size(246, 74);
			this.SoilPanel2.TabIndex = 20;
			// 
			// SoilToolBar2
			// 
			this.SoilToolBar2.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.SoilToolBar2.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.AddCropButton,
																							this.DeleteCropButton,
																							this.ReorderCropButton});
			this.SoilToolBar2.Divider = false;
			this.SoilToolBar2.Dock = System.Windows.Forms.DockStyle.Fill;
			this.SoilToolBar2.DropDownArrows = true;
			this.SoilToolBar2.ImageList = this.ButtonImageList;
			this.SoilToolBar2.Location = new System.Drawing.Point(0, 20);
			this.SoilToolBar2.Name = "SoilToolBar2";
			this.SoilToolBar2.ShowToolTips = true;
			this.SoilToolBar2.Size = new System.Drawing.Size(246, 48);
			this.SoilToolBar2.TabIndex = 17;
			this.SoilToolBar2.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// AddCropButton
			// 
			this.AddCropButton.ImageIndex = 26;
			this.AddCropButton.Text = "&Add crop";
			this.AddCropButton.ToolTipText = "Add a crop to the selected soil";
			// 
			// DeleteCropButton
			// 
			this.DeleteCropButton.ImageIndex = 27;
			this.DeleteCropButton.Text = "&Delete crops";
			this.DeleteCropButton.ToolTipText = "Delete a crop from the selected soil";
			// 
			// ReorderCropButton
			// 
			this.ReorderCropButton.ImageIndex = 28;
			this.ReorderCropButton.Text = "&Reorder crops";
			this.ReorderCropButton.ToolTipText = "Reorder crops in selected soil";
			// 
			// SoilLabel2
			// 
			this.SoilLabel2.BackColor = System.Drawing.Color.SteelBlue;
			this.SoilLabel2.Dock = System.Windows.Forms.DockStyle.Top;
			this.SoilLabel2.ForeColor = System.Drawing.Color.White;
			this.SoilLabel2.Location = new System.Drawing.Point(0, 0);
			this.SoilLabel2.Name = "SoilLabel2";
			this.SoilLabel2.Size = new System.Drawing.Size(246, 20);
			this.SoilLabel2.TabIndex = 15;
			this.SoilLabel2.Text = "Crops";
			this.SoilLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// splitter1
			// 
			this.splitter1.BackColor = System.Drawing.Color.LightGray;
			this.splitter1.Enabled = false;
			this.splitter1.Location = new System.Drawing.Point(185, 0);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(1, 74);
			this.splitter1.TabIndex = 21;
			this.splitter1.TabStop = false;
			// 
			// SoilPanel1
			// 
			this.SoilPanel1.BackColor = System.Drawing.Color.Transparent;
			this.SoilPanel1.Controls.Add(this.SoilToolBar1);
			this.SoilPanel1.Controls.Add(this.SoilLabel1);
			this.SoilPanel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.SoilPanel1.Location = new System.Drawing.Point(49, 0);
			this.SoilPanel1.Name = "SoilPanel1";
			this.SoilPanel1.Size = new System.Drawing.Size(136, 74);
			this.SoilPanel1.TabIndex = 19;
			// 
			// SoilToolBar1
			// 
			this.SoilToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.SoilToolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.CutButton,
																							this.CopyButton,
																							this.PasteButton});
			this.SoilToolBar1.Divider = false;
			this.SoilToolBar1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.SoilToolBar1.DropDownArrows = true;
			this.SoilToolBar1.ImageList = this.ButtonImageList;
			this.SoilToolBar1.Location = new System.Drawing.Point(0, 20);
			this.SoilToolBar1.Name = "SoilToolBar1";
			this.SoilToolBar1.ShowToolTips = true;
			this.SoilToolBar1.Size = new System.Drawing.Size(136, 48);
			this.SoilToolBar1.TabIndex = 16;
			this.SoilToolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// CutButton
			// 
			this.CutButton.ImageIndex = 3;
			this.CutButton.Text = "Cu&t";
			this.CutButton.ToolTipText = "Cut the selected soil(s) to the clipboard";
			// 
			// CopyButton
			// 
			this.CopyButton.ImageIndex = 4;
			this.CopyButton.Text = "&Copy";
			this.CopyButton.ToolTipText = "Copy the seleted soil(s) to the clipboard";
			// 
			// PasteButton
			// 
			this.PasteButton.ImageIndex = 5;
			this.PasteButton.Text = "&Paste";
			this.PasteButton.ToolTipText = "Paste the soils from the clipboard";
			// 
			// SoilLabel1
			// 
			this.SoilLabel1.BackColor = System.Drawing.Color.SteelBlue;
			this.SoilLabel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.SoilLabel1.ForeColor = System.Drawing.Color.White;
			this.SoilLabel1.Location = new System.Drawing.Point(0, 0);
			this.SoilLabel1.Name = "SoilLabel1";
			this.SoilLabel1.Size = new System.Drawing.Size(136, 20);
			this.SoilLabel1.TabIndex = 15;
			this.SoilLabel1.Text = "Clipboard";
			this.SoilLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// splitter8
			// 
			this.splitter8.BackColor = System.Drawing.Color.LightGray;
			this.splitter8.Enabled = false;
			this.splitter8.Location = new System.Drawing.Point(48, 0);
			this.splitter8.Name = "splitter8";
			this.splitter8.Size = new System.Drawing.Size(1, 74);
			this.splitter8.TabIndex = 27;
			this.splitter8.TabStop = false;
			// 
			// PrintPanel
			// 
			this.PrintPanel.BackColor = System.Drawing.Color.Transparent;
			this.PrintPanel.Controls.Add(this.PrintToolBar);
			this.PrintPanel.Controls.Add(this.PrintLabel);
			this.PrintPanel.Dock = System.Windows.Forms.DockStyle.Left;
			this.PrintPanel.Location = new System.Drawing.Point(0, 0);
			this.PrintPanel.Name = "PrintPanel";
			this.PrintPanel.Size = new System.Drawing.Size(48, 74);
			this.PrintPanel.TabIndex = 26;
			// 
			// PrintToolBar
			// 
			this.PrintToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.PrintToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.PrintButton});
			this.PrintToolBar.Divider = false;
			this.PrintToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
			this.PrintToolBar.DropDownArrows = true;
			this.PrintToolBar.ImageList = this.ButtonImageList;
			this.PrintToolBar.Location = new System.Drawing.Point(0, 20);
			this.PrintToolBar.Name = "PrintToolBar";
			this.PrintToolBar.ShowToolTips = true;
			this.PrintToolBar.Size = new System.Drawing.Size(48, 48);
			this.PrintToolBar.TabIndex = 16;
			this.PrintToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// PrintButton
			// 
			this.PrintButton.ImageIndex = 31;
			this.PrintButton.Text = "&Print";
			this.PrintButton.ToolTipText = "Print the current soil";
			// 
			// PrintLabel
			// 
			this.PrintLabel.BackColor = System.Drawing.Color.SteelBlue;
			this.PrintLabel.Dock = System.Windows.Forms.DockStyle.Top;
			this.PrintLabel.ForeColor = System.Drawing.Color.White;
			this.PrintLabel.Location = new System.Drawing.Point(0, 0);
			this.PrintLabel.Name = "PrintLabel";
			this.PrintLabel.Size = new System.Drawing.Size(48, 20);
			this.PrintLabel.TabIndex = 15;
			this.PrintLabel.Text = "Printer";
			this.PrintLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// ExportToolBarPanel
			// 
			this.ExportToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue;
			this.ExportToolBarPanel.Controls.Add(this.splitter7);
			this.ExportToolBarPanel.Controls.Add(this.panel1);
			this.ExportToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.ExportToolBarPanel.Location = new System.Drawing.Point(0, 240);
			this.ExportToolBarPanel.Name = "ExportToolBarPanel";
			this.ExportToolBarPanel.Size = new System.Drawing.Size(848, 72);
			this.ExportToolBarPanel.TabIndex = 19;
			this.ExportToolBarPanel.Visible = false;
			// 
			// splitter7
			// 
			this.splitter7.BackColor = System.Drawing.Color.LightGray;
			this.splitter7.Enabled = false;
			this.splitter7.Location = new System.Drawing.Point(176, 0);
			this.splitter7.Name = "splitter7";
			this.splitter7.Size = new System.Drawing.Size(1, 72);
			this.splitter7.TabIndex = 26;
			this.splitter7.TabStop = false;
			// 
			// panel1
			// 
			this.panel1.BackColor = System.Drawing.Color.Transparent;
			this.panel1.Controls.Add(this.ExportToolBar);
			this.panel1.Controls.Add(this.label1);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.panel1.Location = new System.Drawing.Point(0, 0);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(176, 72);
			this.panel1.TabIndex = 21;
			// 
			// ExportToolBar
			// 
			this.ExportToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ExportToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							 this.ToSoilsButton,
																							 this.ToParButton});
			this.ExportToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
			this.ExportToolBar.DropDownArrows = true;
			this.ExportToolBar.ImageList = this.ButtonImageList;
			this.ExportToolBar.Location = new System.Drawing.Point(0, 20);
			this.ExportToolBar.Name = "ExportToolBar";
			this.ExportToolBar.ShowToolTips = true;
			this.ExportToolBar.Size = new System.Drawing.Size(176, 50);
			this.ExportToolBar.TabIndex = 16;
			this.ExportToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// ToSoilsButton
			// 
			this.ToSoilsButton.ImageIndex = 21;
			this.ToSoilsButton.Text = "To &another .soils file";
			this.ToSoilsButton.ToolTipText = "Export soils to another .soils file";
			// 
			// ToParButton
			// 
			this.ToParButton.ImageIndex = 22;
			this.ToParButton.Text = "To .&par";
			this.ToParButton.ToolTipText = "Export soils to an APSIM .par file";
			// 
			// label1
			// 
			this.label1.BackColor = System.Drawing.Color.SteelBlue;
			this.label1.Dock = System.Windows.Forms.DockStyle.Top;
			this.label1.ForeColor = System.Drawing.Color.White;
			this.label1.Location = new System.Drawing.Point(0, 0);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(176, 20);
			this.label1.TabIndex = 15;
			this.label1.Text = "Export soils";
			this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// ImportToolBarPanel
			// 
			this.ImportToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue;
			this.ImportToolBarPanel.Controls.Add(this.splitter6);
			this.ImportToolBarPanel.Controls.Add(this.ImportPanel1);
			this.ImportToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.ImportToolBarPanel.Location = new System.Drawing.Point(0, 170);
			this.ImportToolBarPanel.Name = "ImportToolBarPanel";
			this.ImportToolBarPanel.Size = new System.Drawing.Size(848, 70);
			this.ImportToolBarPanel.TabIndex = 18;
			this.ImportToolBarPanel.Visible = false;
			// 
			// splitter6
			// 
			this.splitter6.BackColor = System.Drawing.Color.LightGray;
			this.splitter6.Enabled = false;
			this.splitter6.Location = new System.Drawing.Point(408, 0);
			this.splitter6.Name = "splitter6";
			this.splitter6.Size = new System.Drawing.Size(1, 70);
			this.splitter6.TabIndex = 25;
			this.splitter6.TabStop = false;
			// 
			// ImportPanel1
			// 
			this.ImportPanel1.BackColor = System.Drawing.Color.Transparent;
			this.ImportPanel1.Controls.Add(this.ImportToolBar);
			this.ImportPanel1.Controls.Add(this.ImportLabel1);
			this.ImportPanel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.ImportPanel1.Location = new System.Drawing.Point(0, 0);
			this.ImportPanel1.Name = "ImportPanel1";
			this.ImportPanel1.Size = new System.Drawing.Size(408, 70);
			this.ImportPanel1.TabIndex = 20;
			// 
			// ImportToolBar
			// 
			this.ImportToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ImportToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							 this.FromSoilsButton,
																							 this.FromParButton,
																							 this.FromSpreadsheetButton,
																							 this.Fromw2n2Button});
			this.ImportToolBar.Divider = false;
			this.ImportToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
			this.ImportToolBar.DropDownArrows = true;
			this.ImportToolBar.ImageList = this.ButtonImageList;
			this.ImportToolBar.Location = new System.Drawing.Point(0, 20);
			this.ImportToolBar.Name = "ImportToolBar";
			this.ImportToolBar.ShowToolTips = true;
			this.ImportToolBar.Size = new System.Drawing.Size(408, 48);
			this.ImportToolBar.TabIndex = 16;
			this.ImportToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// FromSoilsButton
			// 
			this.FromSoilsButton.ImageIndex = 20;
			this.FromSoilsButton.Text = "From &another .soils file";
			this.FromSoilsButton.ToolTipText = "Import soils from another .soils file";
			// 
			// FromParButton
			// 
			this.FromParButton.ImageIndex = 18;
			this.FromParButton.Text = "From .&par";
			this.FromParButton.ToolTipText = "Import soils from an APSIM .par file";
			// 
			// FromSpreadsheetButton
			// 
			this.FromSpreadsheetButton.ImageIndex = 17;
			this.FromSpreadsheetButton.Text = "From &spreadsheet";
			this.FromSpreadsheetButton.ToolTipText = "Import soils from a spreadsheet";
			// 
			// Fromw2n2Button
			// 
			this.Fromw2n2Button.ImageIndex = 19;
			this.Fromw2n2Button.Text = "From .&w2 / .n2";
			this.Fromw2n2Button.ToolTipText = "Import soils from .w2 / .n2 files";
			// 
			// ImportLabel1
			// 
			this.ImportLabel1.BackColor = System.Drawing.Color.SteelBlue;
			this.ImportLabel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.ImportLabel1.ForeColor = System.Drawing.Color.White;
			this.ImportLabel1.Location = new System.Drawing.Point(0, 0);
			this.ImportLabel1.Name = "ImportLabel1";
			this.ImportLabel1.Size = new System.Drawing.Size(408, 20);
			this.ImportLabel1.TabIndex = 15;
			this.ImportLabel1.Text = "Import soils";
			this.ImportLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// FileToolBarPanel
			// 
			this.FileToolBarPanel.BackColor = System.Drawing.Color.LightSteelBlue;
			this.FileToolBarPanel.Controls.Add(this.splitter5);
			this.FileToolBarPanel.Controls.Add(this.FilePanel2);
			this.FileToolBarPanel.Controls.Add(this.splitter4);
			this.FileToolBarPanel.Controls.Add(this.FilePanel1);
			this.FileToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.FileToolBarPanel.Location = new System.Drawing.Point(0, 25);
			this.FileToolBarPanel.Name = "FileToolBarPanel";
			this.FileToolBarPanel.Size = new System.Drawing.Size(848, 71);
			this.FileToolBarPanel.TabIndex = 17;
			// 
			// splitter5
			// 
			this.splitter5.BackColor = System.Drawing.Color.LightGray;
			this.splitter5.Enabled = false;
			this.splitter5.Location = new System.Drawing.Point(334, 0);
			this.splitter5.Name = "splitter5";
			this.splitter5.Size = new System.Drawing.Size(1, 71);
			this.splitter5.TabIndex = 23;
			this.splitter5.TabStop = false;
			// 
			// FilePanel2
			// 
			this.FilePanel2.BackColor = System.Drawing.Color.Transparent;
			this.FilePanel2.Controls.Add(this.FileToolBar);
			this.FilePanel2.Controls.Add(this.FileLabel2);
			this.FilePanel2.Dock = System.Windows.Forms.DockStyle.Left;
			this.FilePanel2.Location = new System.Drawing.Point(273, 0);
			this.FilePanel2.Name = "FilePanel2";
			this.FilePanel2.Size = new System.Drawing.Size(61, 71);
			this.FilePanel2.TabIndex = 22;
			// 
			// FileToolBar
			// 
			this.FileToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.FileToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																						   this.VersionButton});
			this.FileToolBar.Divider = false;
			this.FileToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
			this.FileToolBar.DropDownArrows = true;
			this.FileToolBar.ImageList = this.ButtonImageList;
			this.FileToolBar.Location = new System.Drawing.Point(0, 20);
			this.FileToolBar.Name = "FileToolBar";
			this.FileToolBar.ShowToolTips = true;
			this.FileToolBar.Size = new System.Drawing.Size(61, 48);
			this.FileToolBar.TabIndex = 16;
			this.FileToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// VersionButton
			// 
			this.VersionButton.ImageIndex = 25;
			this.VersionButton.Text = "&Version";
			// 
			// FileLabel2
			// 
			this.FileLabel2.BackColor = System.Drawing.Color.SteelBlue;
			this.FileLabel2.Dock = System.Windows.Forms.DockStyle.Top;
			this.FileLabel2.ForeColor = System.Drawing.Color.White;
			this.FileLabel2.Location = new System.Drawing.Point(0, 0);
			this.FileLabel2.Name = "FileLabel2";
			this.FileLabel2.Size = new System.Drawing.Size(61, 20);
			this.FileLabel2.TabIndex = 15;
			this.FileLabel2.Text = "About";
			this.FileLabel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// splitter4
			// 
			this.splitter4.BackColor = System.Drawing.Color.LightGray;
			this.splitter4.Enabled = false;
			this.splitter4.Location = new System.Drawing.Point(272, 0);
			this.splitter4.Name = "splitter4";
			this.splitter4.Size = new System.Drawing.Size(1, 71);
			this.splitter4.TabIndex = 21;
			this.splitter4.TabStop = false;
			// 
			// FilePanel1
			// 
			this.FilePanel1.BackColor = System.Drawing.Color.Transparent;
			this.FilePanel1.Controls.Add(this.FileToolBar1);
			this.FilePanel1.Controls.Add(this.FileLabel1);
			this.FilePanel1.Dock = System.Windows.Forms.DockStyle.Left;
			this.FilePanel1.Location = new System.Drawing.Point(0, 0);
			this.FilePanel1.Name = "FilePanel1";
			this.FilePanel1.Size = new System.Drawing.Size(272, 71);
			this.FilePanel1.TabIndex = 20;
			// 
			// FileToolBar1
			// 
			this.FileToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.FileToolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																							this.NewFileButton,
																							this.OpenFileButton,
																							this.SaveFileButton,
																							this.SaveFileAsButton});
			this.FileToolBar1.Divider = false;
			this.FileToolBar1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.FileToolBar1.DropDownArrows = true;
			this.FileToolBar1.ImageList = this.ButtonImageList;
			this.FileToolBar1.Location = new System.Drawing.Point(0, 20);
			this.FileToolBar1.Name = "FileToolBar1";
			this.FileToolBar1.ShowToolTips = true;
			this.FileToolBar1.Size = new System.Drawing.Size(272, 48);
			this.FileToolBar1.TabIndex = 16;
			this.FileToolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// NewFileButton
			// 
			this.NewFileButton.ImageIndex = 1;
			this.NewFileButton.Text = "&New file";
			this.NewFileButton.ToolTipText = "Create a new soils file";
			// 
			// OpenFileButton
			// 
			this.OpenFileButton.ImageIndex = 0;
			this.OpenFileButton.Text = "&Open file";
			this.OpenFileButton.ToolTipText = "Open a new soils file";
			// 
			// SaveFileButton
			// 
			this.SaveFileButton.ImageIndex = 2;
			this.SaveFileButton.Text = "&Save file";
			this.SaveFileButton.ToolTipText = "Save the current file";
			// 
			// SaveFileAsButton
			// 
			this.SaveFileAsButton.ImageIndex = 16;
			this.SaveFileAsButton.Text = "Save &As file";
			this.SaveFileAsButton.ToolTipText = "Save file under new name";
			// 
			// FileLabel1
			// 
			this.FileLabel1.BackColor = System.Drawing.Color.SteelBlue;
			this.FileLabel1.Dock = System.Windows.Forms.DockStyle.Top;
			this.FileLabel1.ForeColor = System.Drawing.Color.White;
			this.FileLabel1.Location = new System.Drawing.Point(0, 0);
			this.FileLabel1.Name = "FileLabel1";
			this.FileLabel1.Size = new System.Drawing.Size(272, 20);
			this.FileLabel1.TabIndex = 15;
			this.FileLabel1.Text = "File management";
			this.FileLabel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			// 
			// MainToolbarPanel
			// 
			this.MainToolbarPanel.BackColor = System.Drawing.Color.Gainsboro;
			this.MainToolbarPanel.Controls.Add(this.MainToolBar);
			this.MainToolbarPanel.Dock = System.Windows.Forms.DockStyle.Top;
			this.MainToolbarPanel.ForeColor = System.Drawing.SystemColors.Highlight;
			this.MainToolbarPanel.Location = new System.Drawing.Point(0, 0);
			this.MainToolbarPanel.Name = "MainToolbarPanel";
			this.MainToolbarPanel.Size = new System.Drawing.Size(848, 25);
			this.MainToolbarPanel.TabIndex = 16;
			// 
			// MainToolBar
			// 
			this.MainToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.MainToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																						   this.FileButton,
																						   this.SoilsButton,
																						   this.ImportButton,
																						   this.ExportButton});
			this.MainToolBar.ButtonSize = new System.Drawing.Size(75, 30);
			this.MainToolBar.Divider = false;
			this.MainToolBar.DropDownArrows = true;
			this.MainToolBar.Location = new System.Drawing.Point(0, 0);
			this.MainToolBar.Name = "MainToolBar";
			this.MainToolBar.ShowToolTips = true;
			this.MainToolBar.Size = new System.Drawing.Size(848, 26);
			this.MainToolBar.TabIndex = 4;
			this.MainToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right;
			this.MainToolBar.Wrappable = false;
			this.MainToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ButtonClick);
			// 
			// FileButton
			// 
			this.FileButton.Pushed = true;
			this.FileButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.FileButton.Text = "&File";
			// 
			// SoilsButton
			// 
			this.SoilsButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.SoilsButton.Text = "Soi&ls";
			// 
			// ImportButton
			// 
			this.ImportButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.ImportButton.Text = "&Import";
			// 
			// ExportButton
			// 
			this.ExportButton.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.ExportButton.Text = "&Export";
			// 
			// MainForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.CausesValidation = false;
			this.ClientSize = new System.Drawing.Size(848, 578);
			this.Controls.Add(this.MainPanel);
			this.Controls.Add(this.ExportToolBarPanel);
			this.Controls.Add(this.ImportToolBarPanel);
			this.Controls.Add(this.SoilsToolBarPanel);
			this.Controls.Add(this.FileToolBarPanel);
			this.Controls.Add(this.MainToolbarPanel);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.KeyPreview = true;
			this.Name = "MainForm";
			this.Text = "Apsoil";
			this.Closing += new System.ComponentModel.CancelEventHandler(this.MainForm_Closing);
			this.Load += new System.EventHandler(this.MainForm_Load);
			this.SoilsToolBarPanel.ResumeLayout(false);
			this.ToolPanel1.ResumeLayout(false);
			this.SoilPanel3.ResumeLayout(false);
			this.SoilPanel2.ResumeLayout(false);
			this.SoilPanel1.ResumeLayout(false);
			this.PrintPanel.ResumeLayout(false);
			this.ExportToolBarPanel.ResumeLayout(false);
			this.panel1.ResumeLayout(false);
			this.ImportToolBarPanel.ResumeLayout(false);
			this.ImportPanel1.ResumeLayout(false);
			this.FileToolBarPanel.ResumeLayout(false);
			this.FilePanel2.ResumeLayout(false);
			this.FilePanel1.ResumeLayout(false);
			this.MainToolbarPanel.ResumeLayout(false);
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
			SoilExplorer.Parent = MainPanel;
			SoilExplorer.Visible = true;
			SoilExplorer.ExpandAll = false;
			SortButton.Pushed = (APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "Soil", "SortByName") == "yes");
			SoilExplorer.SortAll = SortButton.Pushed;

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

			SaveFileButton.Enabled = Apsoil.AllowFileSave;
			SaveFileAsButton.Enabled = Apsoil.AllowFileSaveAs;
			CutButton.Enabled = Apsoil.AllowCut;
			CopyButton.Enabled = Apsoil.AllowCopy;
			PasteButton.Enabled = Apsoil.AllowPaste;

			FromSpreadsheetButton.Enabled = Apsoil.AllowChanges;
			FromParButton.Enabled = Apsoil.AllowChanges;
			FromSoilsButton.Enabled = Apsoil.AllowChanges;
			Fromw2n2Button.Enabled = Apsoil.AllowChanges;
			ToParButton.Enabled = OnlySoilsSelected;
			ToSoilsButton.Enabled = OnlySoilsSelected;
			CheckSoilsButton.Enabled = SomethingInTree;
			SortButton.Enabled = SomethingInTree;
			AddCropButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			DeleteCropButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			ReorderCropButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			AddNoteButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			DeleteNoteButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			PrintButton.Enabled = (OnlySoilsSelected && Apsoil.SelectedData.Count == 1);
			}

		private void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
			{
			// User is closing down - save our work.
			e.Cancel = !Apsoil.FileSaveAfterPrompt();
			}

		private void ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
			{
			if (e.Button.Parent == MainToolBar)
				{
				// Use has clicked a top level menu button
				foreach (ToolBarButton Button in MainToolBar.Buttons)
					{
					if (Button != e.Button)
						Button.Pushed = false;
					}
				e.Button.Pushed = true;

				FileToolBarPanel.Visible = FileButton.Pushed;
				ImportToolBarPanel.Visible = ImportButton.Pushed;
				ExportToolBarPanel.Visible = ExportButton.Pushed;
				SoilsToolBarPanel.Visible = SoilsButton.Pushed;
				}
			else if (e.Button == NewFileButton)
				{
				APSIMData NewData = new APSIMData("soils", "");
				NewData.Add(new Soil(new APSIMData("soil", "Blank soil")).Data);
				Apsoil.FileNew(NewData);
				}
			else if (e.Button == OpenFileButton)
				Apsoil.FileOpen();
			else if (e.Button == SaveFileButton)
				Apsoil.FileSave();
			else if (e.Button == SaveFileAsButton)
				Apsoil.FileSaveAs();
			else if (e.Button == CutButton)
				Apsoil.Cut();			
			else if (e.Button == CopyButton)
				Apsoil.Copy();			
			else if (e.Button == PasteButton)
				Apsoil.Paste();
			else if (e.Button == VersionButton)
				{
				System.Version Version = Assembly.GetExecutingAssembly().GetName().Version;
				string msg = "Version " + Version.Major.ToString() + "." + Version.Minor.ToString() + "\r\n";
                       msg += "(Internal #: " + Version.Build.ToString() + "." + Version.Revision.ToString() + ")";
  				MessageBox.Show(msg, "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information);
				}
			else if (e.Button == FromSpreadsheetButton)
				ImportFromSpreadsheet();
			else if (e.Button == FromParButton)
				ImportFromPar();
			else if (e.Button == FromSoilsButton)
				ImportFromSoils();
			else if (e.Button == Fromw2n2Button)
				ImportFromW2N2();
			else if (e.Button == ToParButton)
				ExportToPar();
			else if (e.Button == ToSoilsButton)
				ExportToSoils();
			else if (e.Button == CheckSoilsButton)
				CheckForErrors();
			else if (e.Button == SortButton)
				Sort();
			else if (e.Button == AddCropButton)
				Apsoil.AddCrop();
			else if (e.Button == DeleteCropButton)
				Apsoil.DeleteCrop();
			else if (e.Button == ReorderCropButton)
				Apsoil.ReorderCrops();
			else if (e.Button == AddNoteButton)
				Apsoil.AddNote();
			else if (e.Button == DeleteNoteButton)
				Apsoil.DeleteNote();
			else if (e.Button == PrintButton)
				Apsoil.Print();
			}

		#endregion

		#region Import methods

		private void ImportFromSpreadsheet()
			{
			try
				{
				if (ImportSpreadsheetDialog.ShowDialog() == DialogResult.OK)
					SoilSpreadsheetImporter.ImportFromFile(ImportSpreadsheetDialog.FileName, Apsoil.AllData);
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			SoilExplorer.Refresh();
			}                  

		private void ImportFromW2N2()
			{
			try
				{
				if (FolderBrowserDialog.ShowDialog() == DialogResult.OK)
					{
					string[] Files = Directory.GetFiles(FolderBrowserDialog.SelectedPath, "*.w2");
					foreach (string File in Files)
                        ParFileImporter.ImportW2N2P2(File, Apsoil.AllData);
					}
				}
			 catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			SoilExplorer.Refresh();
			}

		private void ImportFromPar()
			{
			try
				{
				if (ImportParDialog.ShowDialog() == DialogResult.OK)
					{
					foreach (string File in ImportParDialog.FileNames)
                        ParFileImporter.ImportParFile(File, Apsoil.AllData);
					}
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			SoilExplorer.Refresh();
			}

		private void ImportFromSoils()
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
						Apsoil.AllData.Add(NewData);
						}
					}
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			SoilExplorer.Refresh();
			}

		#endregion

		#region Export methods
		private void ExportToPar()
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

		private void ExportToSoils()
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

		#endregion

		#region Tools methods
		private void CheckForErrors()
			{
			// User wants to check all soils for consistency
			Cursor.Current = Cursors.WaitCursor;
			string ErrorMessage = "";
			CheckAllSoils(Apsoil.AllData, ref ErrorMessage);
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

		private void CheckAllSoils(APSIMData Data, ref string ErrorMessage)
			{
			// Check all soils and return an error message string
			foreach (APSIMData Child in Data.get_Children(null))
				{
				if (Child.Type.ToLower() == "folder" || Child.Type.ToLower() == "soils")
					CheckAllSoils(Child, ref ErrorMessage);
				else if (Child.Type.ToLower() == "soil")
					{
					Soil ThisSoil = new Soil(Child);
					string Errors = ThisSoil.CheckForErrors();
					if (Errors != "")
						ErrorMessage += "\r\n" + ThisSoil.Name + "\r\n" + StringManip.IndentText(Errors, 6);
					}
				}
			}

		private void Sort()
			{
			// User has changed the sort menu item.
			SortButton.Pushed = !SortButton.Pushed;
			string SortValue = "no";
			if (SortButton.Pushed)
				SortValue = "yes";
			APSIMSettings.INIWrite(APSIMSettings.ApsimIniFile(), "soil", "SortByName", SortValue);
			SoilExplorer.SortAll = SortButton.Pushed;
			}

		#endregion	

		#region Keyboard shortcut handling
		private bool LookForShortCutOnControl(Control Parent, int KeyValue)
			{
			foreach (Control c in Parent.Controls)
				{
				Panel panel = c as Panel;
				if (panel != null)
					{
					if (LookForShortCutOnControl(panel, KeyValue))
						return true;
					}
				else
					{
					ToolBar toolbar = c as ToolBar;
					if (toolbar != null && toolbar.Visible)
						{
						foreach (ToolBarButton Button in toolbar.Buttons)
							{
							int PosShortCut = Button.Text.IndexOf("&");
							if (PosShortCut != -1)
								{
								char ShortCutKey = Button.Text[PosShortCut+1];
								if (ShortCutKey > 'a')
									ShortCutKey = (char) (ShortCutKey - 32);
								if (ShortCutKey == KeyValue)
									{
									ButtonClick(null, new ToolBarButtonClickEventArgs(Button));
									return true;
									}
								}
							}
						}
					}
				}
			return false;
			}

		protected override bool ProcessDialogKey(Keys KeyData)
			{
			if ((KeyData & Keys.Alt) == Keys.Alt)
				{
				int Key = (int) (KeyData & ~Keys.Alt);
				if (LookForShortCutOnControl(this, Key))
					return true;
				}
			return base.ProcessDialogKey(KeyData);
			}
		#endregion


		}
	}
