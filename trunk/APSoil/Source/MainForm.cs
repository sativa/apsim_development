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

namespace APSoil
	{
	//-----------------------------
	// APSoil main form.
	// ----------------------------
	public class MainForm : System.Windows.Forms.Form
		{
		private ExplorerUI SoilExplorer;
		private UIManager UserInterfaceManager;
		
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem menuItem5;
		private System.Windows.Forms.MenuItem FileOpen;
		private System.Windows.Forms.MenuItem FileSave;
		private System.Windows.Forms.MenuItem FileSaveAs;
		private System.Windows.Forms.MenuItem FileExit;
		private System.Windows.Forms.MenuItem FileNew;
		private System.Windows.Forms.ImageList SmallImages;
		internal System.Windows.Forms.ImageList ButtonImageList;
		private System.Windows.Forms.Panel MainPanel;
		internal System.Windows.Forms.ToolBar ToolBar;
		internal System.Windows.Forms.ToolBarButton FileNewButton;
		internal System.Windows.Forms.ToolBarButton FileOpenButton;
		internal System.Windows.Forms.ToolBarButton FileSaveButton;
		internal System.Windows.Forms.ToolBarButton Separator1;
		internal System.Windows.Forms.ToolBarButton CutButton;
		internal System.Windows.Forms.ToolBarButton CopyButton;
		internal System.Windows.Forms.ToolBarButton PasteButton;
		private System.Windows.Forms.ToolBarButton ImportButton;
		private System.Windows.Forms.ContextMenu ImportMenu;
		private System.Windows.Forms.MenuItem ImportFromSpreadsheet;
		private System.Windows.Forms.MenuItem menuItem2;
		private System.Windows.Forms.MenuItem menuItem4;
		private System.Windows.Forms.OpenFileDialog ImportSpreadsheetDialog;
		private System.Windows.Forms.MenuItem ImportMenuItem;
		private System.ComponentModel.IContainer components;

		// ------------------
		// constructor
		// ------------------
		public MainForm()
			{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
	        Xceed.Grid.Licenser.LicenseKey = "GRD22-KTL57-34ZF5-W4JA";
		    Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA";
			Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A";
			}


        // -----------------------------------
		// Clean up any resources being used.
		// -----------------------------------
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
		
		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.menuItem1 = new System.Windows.Forms.MenuItem();
			this.FileNew = new System.Windows.Forms.MenuItem();
			this.FileOpen = new System.Windows.Forms.MenuItem();
			this.FileSave = new System.Windows.Forms.MenuItem();
			this.FileSaveAs = new System.Windows.Forms.MenuItem();
			this.menuItem5 = new System.Windows.Forms.MenuItem();
			this.ImportMenuItem = new System.Windows.Forms.MenuItem();
			this.menuItem4 = new System.Windows.Forms.MenuItem();
			this.menuItem2 = new System.Windows.Forms.MenuItem();
			this.FileExit = new System.Windows.Forms.MenuItem();
			this.SmallImages = new System.Windows.Forms.ImageList(this.components);
			this.ButtonImageList = new System.Windows.Forms.ImageList(this.components);
			this.MainPanel = new System.Windows.Forms.Panel();
			this.ToolBar = new System.Windows.Forms.ToolBar();
			this.FileNewButton = new System.Windows.Forms.ToolBarButton();
			this.FileOpenButton = new System.Windows.Forms.ToolBarButton();
			this.FileSaveButton = new System.Windows.Forms.ToolBarButton();
			this.ImportButton = new System.Windows.Forms.ToolBarButton();
			this.ImportMenu = new System.Windows.Forms.ContextMenu();
			this.ImportFromSpreadsheet = new System.Windows.Forms.MenuItem();
			this.Separator1 = new System.Windows.Forms.ToolBarButton();
			this.CutButton = new System.Windows.Forms.ToolBarButton();
			this.CopyButton = new System.Windows.Forms.ToolBarButton();
			this.PasteButton = new System.Windows.Forms.ToolBarButton();
			this.ImportSpreadsheetDialog = new System.Windows.Forms.OpenFileDialog();
			this.SuspendLayout();
			// 
			// mainMenu1
			// 
			this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.menuItem1});
			// 
			// menuItem1
			// 
			this.menuItem1.Index = 0;
			this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.FileNew,
																					  this.FileOpen,
																					  this.FileSave,
																					  this.FileSaveAs,
																					  this.menuItem5,
																					  this.ImportMenuItem,
																					  this.menuItem2,
																					  this.FileExit});
			this.menuItem1.Text = "&File";
			// 
			// FileNew
			// 
			this.FileNew.Index = 0;
			this.FileNew.Text = "&New";
			this.FileNew.Click += new System.EventHandler(this.FileNew_Click);
			// 
			// FileOpen
			// 
			this.FileOpen.Index = 1;
			this.FileOpen.Text = "&Open...";
			this.FileOpen.Click += new System.EventHandler(this.FileOpen_Click);
			// 
			// FileSave
			// 
			this.FileSave.Index = 2;
			this.FileSave.Text = "&Save...";
			this.FileSave.Click += new System.EventHandler(this.FileSave_Click);
			// 
			// FileSaveAs
			// 
			this.FileSaveAs.Index = 3;
			this.FileSaveAs.Text = "Save &as...";
			this.FileSaveAs.Click += new System.EventHandler(this.FileSaveAs_Click);
			// 
			// menuItem5
			// 
			this.menuItem5.Index = 4;
			this.menuItem5.Text = "-";
			// 
			// ImportMenuItem
			// 
			this.ImportMenuItem.Index = 5;
			this.ImportMenuItem.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																						   this.menuItem4});
			this.ImportMenuItem.Text = "&Import";
			// 
			// menuItem4
			// 
			this.menuItem4.Index = 0;
			this.menuItem4.Text = "From &Spreadsheet";
			this.menuItem4.Click += new System.EventHandler(this.ImportFromSpreadsheet_Click);
			// 
			// menuItem2
			// 
			this.menuItem2.Index = 6;
			this.menuItem2.Text = "-";
			// 
			// FileExit
			// 
			this.FileExit.Index = 7;
			this.FileExit.Text = "E&xit";
			this.FileExit.Click += new System.EventHandler(this.FileExit_Click);
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
			this.MainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.MainPanel.Location = new System.Drawing.Point(0, 36);
			this.MainPanel.Name = "MainPanel";
			this.MainPanel.Size = new System.Drawing.Size(800, 509);
			this.MainPanel.TabIndex = 0;
			// 
			// ToolBar
			// 
			this.ToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ToolBar.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																					   this.FileNewButton,
																					   this.FileOpenButton,
																					   this.FileSaveButton,
																					   this.ImportButton,
																					   this.Separator1,
																					   this.CutButton,
																					   this.CopyButton,
																					   this.PasteButton});
			this.ToolBar.ButtonSize = new System.Drawing.Size(62, 30);
			this.ToolBar.DropDownArrows = true;
			this.ToolBar.ImageList = this.ButtonImageList;
			this.ToolBar.Location = new System.Drawing.Point(0, 0);
			this.ToolBar.Name = "ToolBar";
			this.ToolBar.ShowToolTips = true;
			this.ToolBar.Size = new System.Drawing.Size(800, 36);
			this.ToolBar.TabIndex = 3;
			this.ToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right;
			this.ToolBar.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ToolBar_ButtonClick);
			// 
			// FileNewButton
			// 
			this.FileNewButton.ImageIndex = 0;
			this.FileNewButton.Text = "New";
			this.FileNewButton.ToolTipText = "Create a new soils database";
			// 
			// FileOpenButton
			// 
			this.FileOpenButton.ImageIndex = 1;
			this.FileOpenButton.Text = "Open";
			this.FileOpenButton.ToolTipText = "Open an existing soils database";
			// 
			// FileSaveButton
			// 
			this.FileSaveButton.ImageIndex = 2;
			this.FileSaveButton.Text = "Save";
			this.FileSaveButton.ToolTipText = "Save current soils";
			// 
			// ImportButton
			// 
			this.ImportButton.DropDownMenu = this.ImportMenu;
			this.ImportButton.ImageIndex = 15;
			this.ImportButton.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
			this.ImportButton.Text = "Import";
			// 
			// ImportMenu
			// 
			this.ImportMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					   this.ImportFromSpreadsheet});
			// 
			// ImportFromSpreadsheet
			// 
			this.ImportFromSpreadsheet.Index = 0;
			this.ImportFromSpreadsheet.Text = "From &spreadsheet";
			this.ImportFromSpreadsheet.Click += new System.EventHandler(this.ImportFromSpreadsheet_Click);
			// 
			// Separator1
			// 
			this.Separator1.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// CutButton
			// 
			this.CutButton.Enabled = false;
			this.CutButton.ImageIndex = 3;
			this.CutButton.Text = "Cut";
			// 
			// CopyButton
			// 
			this.CopyButton.Enabled = false;
			this.CopyButton.ImageIndex = 4;
			this.CopyButton.Text = "Copy";
			// 
			// PasteButton
			// 
			this.PasteButton.Enabled = false;
			this.PasteButton.ImageIndex = 5;
			this.PasteButton.Text = "Paste";
			// 
			// ImportSpreadsheetDialog
			// 
			this.ImportSpreadsheetDialog.DefaultExt = "xls";
			this.ImportSpreadsheetDialog.Filter = "XLS files (*.xls)|*.xls";
			this.ImportSpreadsheetDialog.Title = "Import soils from spreadsheet";
			// 
			// MainForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(800, 545);
			this.Controls.Add(this.MainPanel);
			this.Controls.Add(this.ToolBar);
			this.Menu = this.mainMenu1;
			this.Name = "MainForm";
			this.Text = "APSoil";
			this.Load += new System.EventHandler(this.MainForm_Load);
			this.ResumeLayout(false);

		}
		#endregion

		// -----------------------------------------
		// The main entry point for the application.
		// -----------------------------------------
		[STAThread]
		static void Main() 
			{
			Application.EnableVisualStyles();
			Application.DoEvents();

			Application.DoEvents();
			Application.Run(new MainForm());
			Application.DoEvents();

			Application.DoEvents();
			}


		// ----------------------------------------
		// Form has been loaded - set everything up
		// ----------------------------------------
		private void MainForm_Load(object sender, System.EventArgs e)
			{
			UserInterfaceManager = new UIManager(SmallImages);

			// Show the Simulation Explorer.
			SoilExplorer = new ExplorerUI();
			SoilExplorer.ApplicationSettings = UserInterfaceManager;
			SoilExplorer.TopLevel = false;
			SoilExplorer.Dock = DockStyle.Fill;
			SoilExplorer.Parent = MainPanel;
			SoilExplorer.Visible = true;
			SoilExplorer.Setup(this, "Soils files (*.soils)|*.soils|" + 
								 	 "All files (*.*)|*.*", 
									 ".soils", "apsoil");
			SoilExplorer.DataTreeCaption = "Empty soils database";
			SetFunctionality();
			}


		// ----------------------------
		// User has clicked on file new
		// ----------------------------
		private void FileNew_Click(object sender, System.EventArgs e)
			{
			APSIMData NewData = new APSIMData("soils", "");
			NewData.Add(new Soil(new APSIMData("soil", "Blank soil")).Data);
			SoilExplorer.FileNew(NewData);
			SetFunctionality();
			}	

		
		// --------------------------------
	    // User has clicked on file open
		// --------------------------------
		private void FileOpen_Click(object sender, System.EventArgs e)
			{
			SoilExplorer.FileOpen();
			APSIMChangeTool.Upgrade(SoilExplorer.Data);
			SoilExplorer.Refresh();
			SetFunctionality();
			}


		// --------------------------------
		// User has clicked on file save
		// --------------------------------
		private void FileSave_Click(object sender, System.EventArgs e)
			{
			SoilExplorer.FileSave();
			SetFunctionality();
			}


		// --------------------------------
		// User has clicked on File SaveAs
		// --------------------------------
		private void FileSaveAs_Click(object sender, System.EventArgs e)
			{
			SoilExplorer.FileSaveAs();
			SetFunctionality();
			}


		// --------------------------------
		// User has clicked on File Exit
		// --------------------------------
		private void FileExit_Click(object sender, System.EventArgs e)
			{
			SoilExplorer.FileSave();
			Close();
			}


		// ---------------------------------------
		// User has clicked on a toolbar button
		// ---------------------------------------
		private void ToolBar_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
			{
			if (e.Button == FileNewButton)
				FileNew_Click(sender, e);
			else if (e.Button == FileOpenButton)
				FileOpen_Click(sender, e);
			else if (e.Button == FileSaveButton)
				FileSave_Click(sender, e);
			SetFunctionality();
			}

		// ---------------------------------------
		// User has clicked on import spreadsheet.
		// ---------------------------------------
		private void ImportFromSpreadsheet_Click(object sender, System.EventArgs e)
			{
			try
				{
				ImportSpreadsheetDialog.InitialDirectory = Path.GetDirectoryName(SoilExplorer.FileName);
				if (ImportSpreadsheetDialog.ShowDialog() == DialogResult.OK)
					{
					SoilSpreadsheetImporter.ImportFromFile(ImportSpreadsheetDialog.FileName, SoilExplorer.Data);
					MessageBox.Show("Soils successfully imported into current file.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
					}
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			SoilExplorer.Refresh();
			}


		// -------------------------------------------
		// Enable / Disable bits of functionality as 
		// required. i.e. ensure program is in a 
		// consistant state.
		// -------------------------------------------
		void SetFunctionality()
			{
			ImportButton.Enabled = (Text != "APSoil");
			ImportMenuItem.Enabled = ImportButton.Enabled;
			}



		}
	}
