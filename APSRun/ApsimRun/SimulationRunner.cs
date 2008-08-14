using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Windows.Forms;
using System.ComponentModel;
using System.Threading;
using ApsimFile;

namespace ApsimRun
   {
   public class Detail
      {
      public string Name;
      public string FileName;
      public bool HasErrors = false;
      public bool HasWarnings = false;
      public bool IsCompleted = false;
      public string SummaryFileName;
      }

   /// <summary> 
   /// An internal class for encapsulating a single run of APSIM
   /// It contains the external Runnable object, plus the summary
   /// file being written to. 
   /// </summary>
   class SingleRun
      {
      public Runnable SimulationFile;
      public StreamWriter SummaryFile;
      public Detail Details = new Detail();
      public SingleRun(Runnable SimulationFile, string Name)
         {
         this.SimulationFile = SimulationFile;
         Details.Name = Name;
         Details.FileName = SimulationFile.FileName;
         }
      public string SimFileName;
      internal string PrepareToRun()
         {
         string Messages;
         
         SimulationFile.WriteSimFile(Details.Name, out SimFileName, out Messages);
         Details.SummaryFileName = SimFileName.Replace(".sim", ".sum");
         if (File.Exists(Details.SummaryFileName))
            File.Delete(Details.SummaryFileName);
         SummaryFile = new StreamWriter(Details.SummaryFileName);

         return Messages;
         }
      internal void IsCompleted()
         {
         Details.IsCompleted = true;
         if (SimulationFile.DeleteSimOnceRunCompleted)
            File.Delete(SimFileName);
         }
      internal void Close()
         {
         if (SimulationFile.DeleteSimOnceRunCompleted && File.Exists(SimFileName))
            File.Delete(SimFileName);
         }
      internal void WriteToSummaryFile(string Line)
         {
         try
            {
            SummaryFile.WriteLine(Line);
            }
         catch (Exception)
            {
            // This is bad. It means that the associated apsim.exe has been closed but
            // we're still being passed stdout stuff.
            SummaryFile = new StreamWriter(Details.SummaryFileName, true);
            SummaryFile.WriteLine(Line);
            SummaryFile.Close();
            }
         }
      }

   /// <summary>
   /// This is the main simulation runner class. It takes runnable objects, adds
   /// them to the run queue, and schedules them to execute when it can. It is
   /// multi-CPU aware and is capable of running several in parallel.
   /// </summary>
   public class SimulationRunner
      {
      private int NumCPUsToUse = 1;
      private List<SingleRun> Simulations = new List<SingleRun>();
      private int NextIndex = -1;
      private int NumCompleted = 0;
      private ISynchronizeInvoke MainThread;
      private bool Stopped = false;
      private bool KillThread = false;
      private Thread WorkerThread;
      private object LockObject = new object();
      private int NumApsimsRunning = 0;

      public delegate void WriteDelegate(Detail Simulation, string Line);
      public delegate void UpdateDelegate(Detail Simulation, int PercentDone, int OverallPercent);
      public event WriteDelegate StdOutWritten;
      public event WriteDelegate StdErrWritten;
      public event UpdateDelegate SimulationUpdated;

      /// <summary>
      /// Constructor.
      /// </summary>
      /// <param name="MainThread">An object implementing the
      /// ISynchronizeInvoke interface.  All events will be delivered
      /// through this object, ensuring that they are delivered to the
      /// correct thread.</param>
      public SimulationRunner(ISynchronizeInvoke MainThread)
         {
         this.MainThread = MainThread;
         string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
         if (NumberOfProcesses != null && NumberOfProcesses != "")
            NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
         WorkerThread = new Thread(DoWork);
         WorkerThread.Start();
         }
      public void Close()
         {
         NumApsimsRunning = 0;
         KillThread = true;
         for (int i = 0; i < NextIndex; i++)
            Simulations[i].Close();
         }

      public void Clear()
         {
         lock (LockObject)
            {
            Simulations.Clear();
            NumApsimsRunning = 0;
            NumCompleted = 0;
            NextIndex = -1;
            }
         }

      /// <summary>
      /// Adds the specified simulation set to the run queue. The object
      /// will be automatically run when a free CPU becomes available.
      /// </summary>
      /// <param name="SimulationSet"></param>
      public void Add(Runnable SimulationSet)
         {
         lock (LockObject)
            {
            foreach (string Name in SimulationSet.SimulationsToRun)
               Simulations.Add(new SingleRun(SimulationSet, Name));
            }
         }

      /// <summary>
      /// Returns a list of simulations and details that are currently in the queue.
      /// </summary>
      public List<Detail> SimulationDetails
         {
         get
            {
            List<Detail> Queue = new List<Detail>();
            foreach (SingleRun Run in Simulations)
               Queue.Add(Run.Details);
            return Queue;
            }
         }

      /// <summary>
      /// The number of simulations currently in the queue.
      /// </summary>
      public int Count
         {
         get
            {
            return Simulations.Count;
            }
         }

      /// <summary>
      /// Return the number of completed simulations.
      /// </summary>
      public int NumberCompleted
         {
         get
            {
            return NumCompleted;
            }
         }

      /// <summary>
      /// Get or set the number of CPU's that APSIM is allowed to use.
      /// </summary>
      public int NumCPUs
         {
         get
            {
            return NumCPUsToUse;
            }
         set
            {
            NumCPUsToUse = value;
            }
         }

      /// <summary>
      /// Run the simulations in the queue
      /// </summary>
      public void Run()
         {
         lock (LockObject)
            {
            Stopped = false;
            NumApsimsRunning = 0;
            NumCompleted = 0;
            NextIndex = -1;
            }
         }


      /// <summary> 
      /// Main worker thread for keeping APSIM busy.
      /// </summary>
      private void DoWork()
         {
         while (!KillThread)
            {
            SingleRun SimulationToRun = GetNextSimulationToRun();
            if (SimulationToRun != null)
               {
               ProcessCaller ApsimProcess = new ProcessCaller(MainThread);
               ApsimProcess.Tag = SimulationToRun;
               try
                  {
                  string Messages = SimulationToRun.PrepareToRun();

                  ApsimProcess.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\\apsim.exe";
                  ApsimProcess.Arguments = "\"" + SimulationToRun.SimFileName + "\"";
                  ApsimProcess.WorkingDirectory = Path.GetDirectoryName(SimulationToRun.SimFileName);
                  ApsimProcess.AllFinished += OnApsimExited;
                  ApsimProcess.StdOutReceived += OnStdOut;
                  ApsimProcess.StdErrReceived += OnStdError;

                  // Do something with any messages during the create .sim bit.
                  if (Messages != "")
                     {
                     CSGeneral.DataReceivedEventArgs arg = new CSGeneral.DataReceivedEventArgs(Messages);
                     OnStdOut(ApsimProcess, arg);
                     }

                  lock (LockObject)
                     {
                     if (!Stopped)
                        {
                        InvokeUpdatedEvent(SimulationToRun.Details, 0);
                        NumApsimsRunning++;
                        ApsimProcess.Start();
                        }
                     }
                  }
              
               catch (Exception ex) 
                  {
                  SimulationToRun.Details.HasErrors = true;
                  CSGeneral.DataReceivedEventArgs Arg = new CSGeneral.DataReceivedEventArgs(ex.Message);
                  OnStdError(ApsimProcess, Arg);
                  CloseJob(SimulationToRun);
                  }
               }
            Thread.Sleep(100);
            }
         }
      private SingleRun GetNextSimulationToRun()
         {
         lock (LockObject)
            {
            if (NextIndex + 1 < Simulations.Count && NumApsimsRunning < NumCPUsToUse)
               {
               NextIndex++;
               return Simulations[NextIndex];
               }
            else
               return null;
            }
         }

      /// <summary>
      /// Stop all APSIM threads immediately.
      /// </summary>
      public void Stop()
         {
         lock (LockObject)
            {
            Stopped = true;
            NextIndex = Simulations.Count;
            }
         foreach (Process P in Process.GetProcesses())
            {
            if (P.ProcessName == "apsim")
               {
               P.Kill();
               }
            }
         }

      /// <summary>
      /// A handler for an APSIM process writting to stdout.
      /// </summary>
      private void OnStdOut(object sender, CSGeneral.DataReceivedEventArgs e)
         {
         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;
         Simulation.WriteToSummaryFile(e.Text);
         InvokeStdOutEvent(Simulation.Details, e.Text);
         }

      /// <summary>
      /// A handler for an APSIM process writting to stderr.
      /// </summary>
      private void OnStdError(object sender, CSGeneral.DataReceivedEventArgs e)
         {
         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;

         // Look for a percent complete
         if (e.Text.Length > 0)
            {
            if (e.Text[0] == '%')
               {
               int Percent = Convert.ToInt32(e.Text.Substring(1));
               if (Percent >= 0 && Percent < 100)
                  InvokeUpdatedEvent(Simulation.Details, Percent);
               }
            else
               {
               if (e.Text.IndexOf("APSIM  Fatal  Error") != -1)
                  Simulation.Details.HasErrors = true;
               else if (e.Text.IndexOf("APSIM Warning Error") != -1)
                  Simulation.Details.HasWarnings = true;
               InvokeStdErrEvent(Simulation.Details, e.Text);
               }
            }
         }

      /// <summary>
      /// A handler for when an APSIM process terminates.
      /// </summary>
      private void OnApsimExited(object sender)
         {
         // APSIM has finished running, so we need to close the summary file
         // and attempt to run the next simulation

         ProcessCaller Process = (ProcessCaller)sender;
         SingleRun Simulation = (SingleRun)Process.Tag;
         CloseJob(Simulation);
         lock (LockObject)
            {
            NumApsimsRunning--;
            }
         }
      private void CloseJob(SingleRun Simulation)
         {
         try
            {
            Simulation.SummaryFile.Close();
            }
         catch (Exception )
            {
            }
         Simulation.IsCompleted();
         lock (LockObject)
            {
            NumCompleted++;
            }
         InvokeUpdatedEvent(Simulation.Details, 100);
         }

      /// <summary>
      /// Invokes the event stdout callback
      /// </summary>
      private void InvokeStdOutEvent(Detail SimulationDetail, string Line)
         {
         if (StdOutWritten != null)
            {
            object[] args = new object[] { SimulationDetail, Line };
            MainThread.Invoke(StdOutWritten, args);
            }
         }

      /// <summary>
      /// Invokes the event stderr callback
      /// </summary>
      private void InvokeStdErrEvent(Detail SimulationDetail, string Line)
         {
         if (StdErrWritten != null)
            {
            object[] args = new object[] { SimulationDetail, Line };
            MainThread.Invoke(StdErrWritten, args);
            }
         }

      /// <summary>
      /// Invokes the updated event callback
      /// </summary>
      private void InvokeUpdatedEvent(Detail SimulationDetail, int PercentDone)
         {
         if (SimulationUpdated != null)
            {
            double PercentPerSimulation = 100.0 / Simulations.Count;
            int OverallPercent = (int)(PercentDone / 100.0 * PercentPerSimulation + NumCompleted * PercentPerSimulation);

            object[] args = new object[] { SimulationDetail, PercentDone, OverallPercent };
            MainThread.Invoke(SimulationUpdated, args);
            }
         }


      }
   }
