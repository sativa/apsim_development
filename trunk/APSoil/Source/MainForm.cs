using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.IO;
using VBGeneral;
using CSGeneral;
using CSUserInterface;
using VBUserInterface;
using ApsimFile;
using System.Reflection;
using System.Collections.Specialized;
using Soils;

namespace APSoil
	{
	//-----------------------------
	// Apsoil main form.
	// ----------------------------
	public class MainForm : System.Windows.Forms.Form
		{
		private BaseController Apsoil;
        private ExplorerUI SoilExplorer;
        private System.ComponentModel.IContainer components;
        private ToolStripContainer ToolStripContainer;
        private ToolStrip MainToolStrip;
        private ImageList SmallImages;
		private string CommandLineFileName;

		#region Constructor / Destructor / Main
		public MainForm()
			{
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
        this.MainToolStrip = new System.Windows.Forms.ToolStrip();
        this.SmallImages = new System.Windows.Forms.ImageList(this.components);
        this.ToolStripContainer.TopToolStripPanel.SuspendLayout();
        this.ToolStripContainer.SuspendLayout();
        this.SuspendLayout();
        // 
        // ToolStripContainer
        // 
        // 
        // ToolStripContainer.ContentPanel
        // 
        this.ToolStripContainer.ContentPanel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
        this.ToolStripContainer.ContentPanel.Size = new System.Drawing.Size(852, 553);
        this.ToolStripContainer.Dock = System.Windows.Forms.DockStyle.Fill;
        this.ToolStripContainer.Location = new System.Drawing.Point(0, 0);
        this.ToolStripContainer.Name = "ToolStripContainer";
        this.ToolStripContainer.Size = new System.Drawing.Size(852, 578);
        this.ToolStripContainer.TabIndex = 0;
        this.ToolStripContainer.Text = "toolStripContainer1";
        // 
        // ToolStripContainer.TopToolStripPanel
        // 
        this.ToolStripContainer.TopToolStripPanel.Controls.Add(this.MainToolStrip);
        // 
        // MainToolStrip
        // 
        this.MainToolStrip.Dock = System.Windows.Forms.DockStyle.None;
        this.MainToolStrip.Location = new System.Drawing.Point(3, 0);
        this.MainToolStrip.Name = "MainToolStrip";
        this.MainToolStrip.Size = new System.Drawing.Size(111, 25);
        this.MainToolStrip.TabIndex = 0;
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
        this.ClientSize = new System.Drawing.Size(852, 578);
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
			Splash.Setup(SoilActions.VersionString());
			Splash.ShowDialog();
            ToolStripContainer.Size = new Size(ToolStripContainer.ContentPanel.Width, 103);

			// Form has been loaded - set everything up
			Apsoil = new BaseController(this, "Apsoil");

			// Show the Simulation Explorer.
			SoilExplorer = new ExplorerUI(Apsoil);
            SoilExplorer.Dock = DockStyle.Fill;
			SoilExplorer.Parent = ToolStripContainer.ContentPanel;
            SoilExplorer.OnLoad(Apsoil);
			SoilExplorer.Visible = true;
            Apsoil.ApsimData.NewDataEvent += new ApsimFile.ApsimFile.VoidDelegate(OnNewData);
            Apsoil.Explorer = SoilExplorer;

			// Load up the file from the command line if necessary.
            if (CommandLineFileName != null)
                {
                APSIMData FileData = new APSIMData();
                if (FileData.LoadFromFile(CommandLineFileName))
                    Apsoil.FileOpen(CommandLineFileName);
                }
            Apsoil.ProvideToolStrip(MainToolStrip, "MainToolBar");
			}
		#endregion

        private void OnNewData()
            {
            SoilExplorer.ExpandAllFolders();
            }

		private void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
			{
			// User is closing down - save our work.
			e.Cancel = !Apsoil.FileSaveAfterPrompt();
			}

		#region Button event handlers








    #endregion







		}
	}
