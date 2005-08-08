using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.IO;
using CSGeneral;
using VBGeneral;
using APSoil;
using ChangeTool;

namespace SoilPicker
	{
	// ---------------------------------------
	// Used by APSFront to pick a soil
	// ---------------------------------------
	public class MainForm : System.Windows.Forms.Form
		{
		private ExplorerUI SoilExplorer;
		private UIManager UserInterfaceManager;
		private string CommandLineFileName;
		private string DefaultSoil;
		private bool QuitImmediately = false;

		private System.Windows.Forms.Panel RightPanel;
		private System.Windows.Forms.Button OkButton;
		private System.Windows.Forms.Panel MainPanel;
		private System.Windows.Forms.Splitter splitter1;
		private System.Windows.Forms.Button CancelBut;
		private System.Windows.Forms.ImageList SmallImages;
		private System.Windows.Forms.Button BrowseButton;
		private System.ComponentModel.IContainer components;

		// -----------------------------------------
		// The main entry point for the application.
		// -----------------------------------------
		public MainForm()
			{
			InitializeComponent();
	        Xceed.Grid.Licenser.LicenseKey = "GRD22-KTL57-34ZF5-W4JA";
		    Xceed.SmartUI.Licenser.LicenseKey = "SUN31-9TL57-SUXL5-F4BA";
			Xceed.Chart.Licenser.LicenseKey = "CHT30-YTL57-0UXLJ-145A";
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

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
			this.RightPanel = new System.Windows.Forms.Panel();
			this.CancelBut = new System.Windows.Forms.Button();
			this.OkButton = new System.Windows.Forms.Button();
			this.MainPanel = new System.Windows.Forms.Panel();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.SmallImages = new System.Windows.Forms.ImageList(this.components);
			this.BrowseButton = new System.Windows.Forms.Button();
			this.RightPanel.SuspendLayout();
			this.SuspendLayout();
			// 
			// RightPanel
			// 
			this.RightPanel.Controls.Add(this.BrowseButton);
			this.RightPanel.Controls.Add(this.CancelBut);
			this.RightPanel.Controls.Add(this.OkButton);
			this.RightPanel.Dock = System.Windows.Forms.DockStyle.Right;
			this.RightPanel.Location = new System.Drawing.Point(816, 0);
			this.RightPanel.Name = "RightPanel";
			this.RightPanel.Size = new System.Drawing.Size(104, 534);
			this.RightPanel.TabIndex = 0;
			// 
			// CancelBut
			// 
			this.CancelBut.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.CancelBut.Location = new System.Drawing.Point(16, 48);
			this.CancelBut.Name = "CancelBut";
			this.CancelBut.TabIndex = 1;
			this.CancelBut.Text = "&Cancel";
			this.CancelBut.Click += new System.EventHandler(this.CancelBut_Click);
			// 
			// OkButton
			// 
			this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.OkButton.Location = new System.Drawing.Point(16, 16);
			this.OkButton.Name = "OkButton";
			this.OkButton.TabIndex = 0;
			this.OkButton.Text = "&Ok";
			this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
			// 
			// MainPanel
			// 
			this.MainPanel.Dock = System.Windows.Forms.DockStyle.Fill;
			this.MainPanel.Location = new System.Drawing.Point(0, 0);
			this.MainPanel.Name = "MainPanel";
			this.MainPanel.Size = new System.Drawing.Size(816, 534);
			this.MainPanel.TabIndex = 1;
			// 
			// splitter1
			// 
			this.splitter1.Dock = System.Windows.Forms.DockStyle.Right;
			this.splitter1.Location = new System.Drawing.Point(813, 0);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(3, 534);
			this.splitter1.TabIndex = 2;
			this.splitter1.TabStop = false;
			// 
			// SmallImages
			// 
			this.SmallImages.ImageSize = new System.Drawing.Size(16, 16);
			this.SmallImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("SmallImages.ImageStream")));
			this.SmallImages.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// BrowseButton
			// 
			this.BrowseButton.Location = new System.Drawing.Point(16, 104);
			this.BrowseButton.Name = "BrowseButton";
			this.BrowseButton.TabIndex = 2;
			this.BrowseButton.Text = "&Browse";
			this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);
			// 
			// MainForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(920, 534);
			this.Controls.Add(this.splitter1);
			this.Controls.Add(this.MainPanel);
			this.Controls.Add(this.RightPanel);
			this.Name = "MainForm";
			this.Text = "Soil selection form";
			this.Load += new System.EventHandler(this.MainForm_Load);
			this.RightPanel.ResumeLayout(false);
			this.ResumeLayout(false);

		}
		#endregion


		[STAThread]
		static void Main(string[] args) 
			{
			Application.EnableVisualStyles();
			Application.DoEvents();
			MainForm Main = new MainForm();
			if (args.Length == 2)
				Main.LoadFile(args[0], args[1], false);
			else if (args.Length == 3)
				Main.LoadFile(args[0], args[1], true);

			Application.Run(Main);
			}



		// ----------------------------------------------------------------
		// Load the specified file. Called when a command line arg is used.
		// ----------------------------------------------------------------
		public void LoadFile(string FileName, string SoilName, bool QuitImmediately)
			{
			CommandLineFileName = FileName.Replace("\"", "");
			DefaultSoil = SoilName.Replace("\"", "");
			this.QuitImmediately = QuitImmediately;
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
			SoilExplorer.ExpandAll = false;
			SoilExplorer.Setup(this, "Soils files (*.soils)|*.soils|" + 
								 	 "All files (*.*)|*.*", 
									 ".soils", "apsoil");
			SoilExplorer.DataTreeCaption = "Empty soils database";

			// Load up the file from the command line if necessary.
			try
				{
				if (CommandLineFileName != null)
					{
					SoilExplorer.FileOpen(CommandLineFileName);
					APSIMChangeTool.Upgrade(SoilExplorer.Data);
					SoilExplorer.Refresh();
					SoilExplorer.SelectNode(DefaultSoil);
					if (SoilExplorer.GetSelectedData() == null)
						SoilExplorer.SelectFirstNodeOfType("soil");
					}
				if (QuitImmediately)
				OkButton_Click(null, null);
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message + "\r\nFile: " + CommandLineFileName, "Error");
				}
			}


		// ---------------------------------------------------
		// User has clicked on browse. Open a new soils. file.
		// ---------------------------------------------------
		private void BrowseButton_Click(object sender, System.EventArgs e)
			{
			if (SoilExplorer.FileOpen())
				{
				APSIMChangeTool.Upgrade(SoilExplorer.Data);
				SoilExplorer.Refresh();
				SoilExplorer.SelectFirstNodeOfType("soil");
				}
			}


		// ---------------------------------------------------
		// User has clicked on Ok.
		// ---------------------------------------------------
		private void OkButton_Click(object sender, System.EventArgs e)
			{
			// write soil to a temporary file.
			Soil SelectedSoil = new Soil(SoilExplorer.GetSelectedData());
			string OutputFileName = Path.GetTempPath() + "\\temp.par";
			SelectedSoil.ExportToPar(OutputFileName);

			// Read in contents of our temporary file.
			StreamReader FileIn = new StreamReader(OutputFileName);
			string Contents = FileIn.ReadToEnd();
			FileIn.Close();
			FileIn = null;

			Contents = Contents.Replace("[soil.", "[run%.");
			int PosSoilN = Contents.IndexOf("[run%.soiln2.parameters]");
			if (PosSoilN == -1)
				MessageBox.Show("Bad soil file format for soil " + SelectedSoil.Name);
			else
				{
				string W2FileName = Path.GetTempPath() + "\\temp.w2";
				StreamWriter w2 = new StreamWriter(W2FileName);
				w2.WriteLine("!Title = " + SelectedSoil.Name);
				w2.WriteLine("[*attributes]");
				w2.WriteLine("   module_usage  = soil_water");
				w2.WriteLine("   must_have     = soil_water");
				w2.WriteLine("[*contents]");
				w2.WriteLine(Contents.Substring(0, PosSoilN));

				string N2FileName = Path.GetTempPath() + "\\temp.n2";
				StreamWriter n2 = new StreamWriter(N2FileName);
				n2.WriteLine("!Title = " + SelectedSoil.Name);
				n2.WriteLine("[*attributes]");
				n2.WriteLine("   module_usage  = soil_nitrogen");
				n2.WriteLine("   must_have     = soil_nitrogen");
				n2.WriteLine("[*contents]");

				int PosSoilP = Contents.IndexOf("[run%.soilp.parameters]");
				if (PosSoilP == -1)
					n2.WriteLine(Contents.Substring(PosSoilN));
				else
					{
					n2.WriteLine(Contents.Substring(PosSoilN, PosSoilP-PosSoilN));

					string P2FileName = Path.GetTempPath() + "\\temp.p2";
					StreamWriter p2 = new StreamWriter(P2FileName);
					p2.WriteLine("!Title = " + SelectedSoil.Name);
					p2.WriteLine("[*attributes]");
					p2.WriteLine("   module_usage  = soil_phosphorus");
					p2.WriteLine("   must_have     = soil_phosphorus");
					p2.WriteLine("[*contents]");
					p2.WriteLine(Contents.Substring(PosSoilN));
					p2.Close();
					p2 = null;
					}

				w2.Close();
				w2 = null;
				n2.Close();
				n2 = null;
				Close();
				}
			}


		// ---------------------------------------------------
		// User has clicked on cancel.
		// ---------------------------------------------------
		private void CancelBut_Click(object sender, System.EventArgs e)
			{
			Close();
			}


	}
}
