using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ApsimFile;
using System.IO;
using CSUserInterface;

namespace ApsimRun
   {
   public partial class SimulationRunnerForm : Form
      {
      private SimulationRunner Runner;
      private Configuration Config = new Configuration("ApsimRun");
      private string[] Args;
      private bool FirstPaint = true;
      private bool AutoClose = false;
      private bool FromGUI = false;
      private bool InDirectoryScan = false;

      /// <summary>
      /// Constructor
      /// </summary>
      public SimulationRunnerForm(string[] Args)
         {
         InitializeComponent();

         Runner = new SimulationRunner(this);
         Runner.SimulationUpdated += OnUpdate;
         string NumCPUsString = Config.Setting("NumCPUs");
         if (NumCPUsString != "")
             Runner.NumCPUs = Convert.ToInt32(NumCPUsString);
         NumCPUs.Value = Runner.NumCPUs;
         NumCPUs.ValueChanged += OnNumCPUsChanged;
         this.Args = Args;

         // Position window correctly.
         if (Config.Setting("Top") != "")
            {
            Top = Convert.ToInt32(Config.Setting("Top"));
            Left = Convert.ToInt32(Config.Setting("Left"));
            }
         if (Config.Setting("Minimised") == "yes")
            WindowState = FormWindowState.Minimized;
         try
            {
            this.PerformanceCounter = new System.Diagnostics.PerformanceCounter();
            this.PerformanceCounter.CategoryName = "Processor";
            this.PerformanceCounter.CounterName = "% Processor Time";
            this.PerformanceCounter.InstanceName = "_Total";
            }
         catch
            {
            this.PerformanceCounter = null;
            }

         }

      /// <summary>
      /// Form has loaded so now we can process any command line arguments.
      /// </summary>
      /// 
      private void OnShown(object sender, EventArgs e)
         {
         }

      public void AddFromGUI(string ResponseFileName)
         {
         try
            {
            ResponseFileName = ResponseFileName.Remove(0, 1);
            ResponseFileName = ResponseFileName.Replace("\"", "");
            StreamReader In = new StreamReader(ResponseFileName);
            string FileName = In.ReadLine();
            Runnable FileToRun = new ApsimFile.ApsimFile(new Configuration("apsimui"), FileName);
            List<string> SimsToRun = new List<string>();
            string Line = In.ReadLine();
            while (Line != "" && Line != null)
               {
               SimsToRun.Add(Line);
               Line = In.ReadLine();
               }
            FileToRun.SimulationsToRun = SimsToRun;

            if (RunButton.Text == "Run")
               Runner.Clear();
            if (FileToRun.SimulationsToRun.Count > 0)
               Runner.Add(FileToRun);
            Total.Text = Runner.Count.ToString();
            if (RunButton.Text == "Run")
               OnRunClick(null, null);
            In.Close();
            }
         catch (Exception ex)
            {
            MessageBox.Show(ex.Message);
            }
         }

      /// <summary>
      /// Add a bunch of simulation files or directories to the run queue.
      /// </summary>
      /// <param name="files">The files or directories to run</param>
      public void Add(string[] files)
         {
         try
            {
            if (RunButton.Text == "Run")
               Runner.Clear();

            bool JustDoIt = false;

            foreach (string FileName in files)
               {
               if (FileName == "/auto")
                  JustDoIt = true;
               else if (FileName == "/autoclose")
                  AutoClose = true;
               else if (FileName[0] == '@')
                  AddFromGUI(FileName);
               else if (Directory.Exists(FileName))
                  {
                  Cursor.Current = Cursors.WaitCursor;
                  InDirectoryScan = true;
                  AddDirectory(FileName);
                  InDirectoryScan = false;
                  Cursor.Current = Cursors.Default;
                  }
               else if (Path.GetExtension(FileName).ToLower() == ".txt")
                  {
                  StreamReader Txt = new StreamReader(FileName);
                  string Contents = Txt.ReadToEnd();
                  string[] Lines = Contents.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                  Add(Lines);
                  Txt.Close();
                  return;
                  }
               else
                  AddFile(FileName, JustDoIt);
               }
            }
         catch (Exception ex)
            {
            MessageBox.Show(ex.Message);
            }
         }

      /// <summary>
      /// Recursively add a directory of simulations to the run queue.
      /// </summary>
      /// <param name="DirectoryName">Directory name to search in</param>
      private void AddDirectory(string DirectoryName)
         {
         foreach (string FileName in Directory.GetFiles(DirectoryName))
            {
            string Extension = Path.GetExtension(FileName).ToLower();
            if (Extension == ".con" || Extension == ".apsim" || Extension == ".sim")
               AddFile(FileName, true);
            }
         foreach (string ChildDirectoryName in Directory.GetDirectories(DirectoryName))
            {
            if (ChildDirectoryName != ".svn")
               AddDirectory(ChildDirectoryName);
            }
         }

      /// <summary>
      /// Add the specified simulation file to the run queue.
      /// </summary>
      /// <param name="FileName">Simulation file to add</param>
      private void AddFile(string FileName, bool JustDoIt)
         {
         Runnable FileToRun = null;
         if (Path.GetExtension(FileName).ToLower() == ".con")
            FileToRun = new ConFile(FileName);
         else if (Path.GetExtension(FileName).ToLower() == ".apsim")
            FileToRun = new ApsimFile.ApsimFile(new Configuration("apsimui"), FileName);
         else if (Path.GetExtension(FileName).ToLower() == ".sim")
            FileToRun = new ApsimFile.SimFile(FileName);
         else
            throw new Exception("Unknown simulation file type: " + FileName);

         // Display a selection form if there are more than 1 simulations and this isn't an AutoRun
         if (FileToRun.SimulationsToRun.Count > 1 && !JustDoIt)
            {
            SelectionForm Form = new SelectionForm(FileToRun);
            Form.ShowDialog();
            }

         if (FileToRun.SimulationsToRun.Count > 0)
            Runner.Add(FileToRun);
         Total.Text = Runner.Count.ToString();
         Application.DoEvents();
         }

      /// <summary>
      /// Called whenever the runner updates the progress of a simulation.
      /// </summary>
      /// <param name="SimulationName">The name of the simulation that is being updated</param>
      /// <param name="PercentDone">The percent done of the specified simulation</param>
      /// <param name="OverallPercent">The overall progress of the simulation queue</param>
      private void OnUpdate(Detail Simulation, int PercentDone, int OverallPercent)
         {
         OverallPercent = Math.Min(OverallPercent, 100);
         ProgressBar.Value = OverallPercent;
         if (PercentDone == 100)
            {
            Completed.Text = Runner.NumberCompleted.ToString();
            if (Simulation.HasErrors)
               NumberWithErrors.Text = Increment(NumberWithErrors.Text);
            if (Simulation.HasWarnings)
               NumberWithWarnings.Text = Increment(NumberWithWarnings.Text);
            }
         if (OverallPercent >= 100 && RunButton.Text == "Stop" && !InDirectoryScan)
            {
            string WavFileName = Config.Setting("ApsimFinishedWAVFileName");
            if (File.Exists(WavFileName))
               {
               System.Media.SoundPlayer Player = new System.Media.SoundPlayer(WavFileName);
               Player.Play();
               }
            RunButton.Text = "Run";
            RunButton.ImageIndex = 0;
            ReportButton.Visible = true;
            if (AutoClose)
               Close();
            }
         Completed.Text = Runner.NumberCompleted.ToString();
         this.Text = OverallPercent.ToString() + "% complete";
         }

      private string Increment(string Value)
         {
         return Convert.ToString(Convert.ToInt32(Value) + 1);
         }

      private void OnRunClick(object sender, EventArgs e)
         {
         if (RunButton.Text == "Run")
            {
            Total.Text = Runner.Count.ToString();
            NumberWithErrors.Text = "0";
            NumberWithWarnings.Text = "0";
            ProgressBar.Value = 0;

            RunButton.Text = "Stop";
            RunButton.ImageIndex = 1;
            Runner.Run();
            ReportButton.Visible = false;

            }
         else
            {
            RunButton.Text = "Run";
            RunButton.ImageIndex = 0;
            Runner.Stop();
            ReportButton.Visible = true;
            }
         }

      private void OnNumCPUsChanged(object sender, EventArgs e)
         {
         Runner.NumCPUs = (int) NumCPUs.Value;
         Config.SetSetting("NumCPUs", NumCPUs.Value.ToString());
         }
 
      private void OnClosing(object sender, FormClosingEventArgs e)
         {
         Runner.Stop();
         Runner.Close();
         if (WindowState == FormWindowState.Minimized)
            Config.SetSetting("Minimised", "yes");
         else
            {
            Config.SetSetting("Minimised", "no");
            Config.SetSetting("Top", Top.ToString());
            Config.SetSetting("Left", Left.ToString());
            }
         }
      private double[] values = new double[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      private void OnTimerTick(object sender, EventArgs e)
         {
         try
            {
            if (PerformanceCounter != null)
               {
               Array.Copy(values, 1, values, 0, 9);
               values[9] = PerformanceCounter.NextValue();
               PerformanceSeries.Clear();
               PerformanceSeries.Add(values);
               }
            }
         catch
            {

            }
         }

      private void OnDragEnter(object sender, DragEventArgs e)
         {
         // If the data is a file, display the copy cursor.
         if (e.Data.GetDataPresent(DataFormats.FileDrop))
            e.Effect = DragDropEffects.Copy;
         else
            e.Effect = DragDropEffects.None;
         }

      private void OnDragDrop(object sender, DragEventArgs e)
         {
         // Handle FileDrop data.
         if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
            // Assign the file names to a string array, in 
            // case the user has selected multiple files.
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
            Add(files);
            }
         }

      private void OnReportClick(object sender, EventArgs e)
         {
         SimulationRunnerReportForm Form = new SimulationRunnerReportForm();
         Form.Populate(Runner.SimulationDetails);
         Form.ShowDialog();
         }

      /// <summary>
      /// We need to override the windows message proc to intercept our WM_COPYDATA message. This is 
      /// sent when a second instance of ApsimRun is created with a command line argument. The lpData
      /// field of that WM_COPYDATA structure will contain the command line.
      /// </summary>
      /// <param name="m"></param>
      protected override void WndProc(ref Message m)
         {
         if (m.Msg == SingleApplicationInstance.WM_COPYDATA)
            {
            string[] files = { SingleApplicationInstance.ProcessWM_COPYDATA(m) };
            Activate();
            Add(files);
            }
         base.WndProc(ref m);
         }

      private void OnPaint(object sender, PaintEventArgs e)
         {
         if (FirstPaint && Args != null)
            {
            FirstPaint = false;
            Add(Args);
            }
         }

      private void OnKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Escape)
            Visible = false;
         }


      }
   }