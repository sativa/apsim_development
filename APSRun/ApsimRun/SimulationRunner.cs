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
      private ISynchronizeInvoke MainThread;
      private int NumApsimsRunning = 0;
      private bool Stopped = false;

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
         }

      public void Clear()
         {
         Simulations.Clear();
         }

      /// <summary>
      /// Adds the specified simulation set to the run queue. The object
      /// will be automatically run when a free CPU becomes available.
      /// </summary>
      /// <param name="SimulationSet"></param>
      public void Add(Runnable SimulationSet)
         {
         foreach (string Name in SimulationSet.SimulationsToRun)
            Simulations.Add(new SingleRun(SimulationSet, Name));
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
         Stopped = false;
         NextIndex = -1;
         RunNext();
         }


      /// <summary> 
      /// Run the next simulation in the run queue.
      /// </summary>
      private void RunNext()
         {
         while (NextIndex + 1 < Simulations.Count && NumApsimsRunning < NumCPUsToUse && !Stopped)
            {
            Monitor.Enter(this);
            NumApsimsRunning++; 
            NextIndex++;
            Monitor.Exit(this);
            try
               {
               string Messages = Simulations[NextIndex].PrepareToRun();

               ProcessCaller ApsimProcess = new ProcessCaller(MainThread);
               ApsimProcess.FileName = Path.GetDirectoryName(Application.ExecutablePath) + "\\apsim.exe";
               ApsimProcess.Arguments = "\"" + Simulations[NextIndex].SimFileName + "\"";
               ApsimProcess.WorkingDirectory = Path.GetDirectoryName(Simulations[NextIndex].SimFileName);
               ApsimProcess.StdOutClosed += OnApsimExited;
               ApsimProcess.StdOutReceived += OnStdOut;
               ApsimProcess.StdErrReceived += OnStdError;
               ApsimProcess.Tag = NextIndex;

               // Do something with any messages during the create .sim bit.
               if (Messages != "")
                  {
                  CSGeneral.DataReceivedEventArgs arg = new CSGeneral.DataReceivedEventArgs(Messages);
                  OnStdOut(ApsimProcess, arg);
                  }

               ApsimProcess.Start();
               }
           
            catch (Exception ex) 
               {
               Simulations[NextIndex].Details.HasErrors = true;
               InvokeStdErrEvent(Simulations[NextIndex].Details, ex.Message);
               InvokeUpdatedEvent(Simulations[NextIndex].Details, 100);
               Run();
               }
            }
         }

      /// <summary>
      /// Stop all APSIM threads immediately.
      /// </summary>
      public void Stop()
         {
         Stopped = true;
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
         int ProcessIndex = Process.Tag;
         Simulations[ProcessIndex].SummaryFile.WriteLine(e.Text);

         InvokeStdOutEvent(Simulations[NextIndex].Details, e.Text);
         }

      /// <summary>
      /// A handler for an APSIM process writting to stderr.
      /// </summary>
      private void OnStdError(object sender, CSGeneral.DataReceivedEventArgs e)
         {
         ProcessCaller Process = (ProcessCaller)sender;
         int ProcessIndex = Process.Tag;

         // Look for a percent complete
         if (e.Text[0] == '%')
            {
            int Percent = Convert.ToInt32(e.Text.Substring(1));
            if (Percent >= 0 && Percent < 100)
               InvokeUpdatedEvent(Simulations[ProcessIndex].Details, Percent);
            }
         else
            {
            if (e.Text.IndexOf("APSIM  Fatal  Error") != -1)
               Simulations[ProcessIndex].Details.HasErrors = true;
            InvokeStdErrEvent(Simulations[ProcessIndex].Details, e.Text);
            }
         }

      /// <summary>
      /// A handler for when an APSIM process terminates.
      /// </summary>
      private void OnApsimExited(object Sender)
         {
         // APSIM has finished running, so we need to close the summary file
         // and attempt to run the next simulation

         ProcessCaller Process = (ProcessCaller)Sender;
         int ProcessIndex = Process.Tag;

         Simulations[ProcessIndex].SummaryFile.Close();
         Simulations[ProcessIndex].IsCompleted();
         InvokeUpdatedEvent(Simulations[ProcessIndex].Details, 100);

         Monitor.Enter(this);
         NumApsimsRunning--;
         Monitor.Exit(this);

         RunNext();
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
            int OverallPercent = (int) (PercentDone / 100.0 * PercentPerSimulation  + NextIndex * PercentPerSimulation);

            object[] args = new object[] { SimulationDetail, PercentDone, OverallPercent };
            MainThread.Invoke(SimulationUpdated, args);
            }
         }


      }
   }
