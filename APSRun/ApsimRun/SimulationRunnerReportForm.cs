using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ApsimFile;
using System.Diagnostics;

namespace ApsimRun
   {
   public partial class SimulationRunnerReportForm : Form
      {
      private List<Detail> Simulations;
      public SimulationRunnerReportForm()
         {
         InitializeComponent();
         }

      public void Populate(List<Detail> Simulations)
         {
         this.Simulations = Simulations;

         ListViewGroup Group = null;
         foreach (Detail Simulation in Simulations)
            {
            if (Group == null || Group.Header != Simulation.FileName)
               {
               Group = new ListViewGroup(Simulation.FileName);
               SimulationList.Groups.Add(Group);
               }
            ListViewItem Item = new ListViewItem(Group);
            Item.Text = Simulation.Name;
            if (Simulation.IsCompleted)
               {
               Item.ImageIndex = 3;
               if (Simulation.HasErrors)
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("ERRORS");
                  Sub.ForeColor = Color.Red;
                  Sub.Font = SystemFonts.CaptionFont;
                  }
               else
                  Item.SubItems.Add("No errors");
               if (Simulation.HasWarnings)
                  {
                  System.Windows.Forms.ListViewItem.ListViewSubItem Sub = Item.SubItems.Add("WARNINGS");
                  Sub.ForeColor = Color.Pink;
                  Sub.Font = SystemFonts.CaptionFont;
                  }
               else
                  Item.SubItems.Add("No warnings");
               }
            SimulationList.Items.Add(Item);
            }
         }

      private void OnDoubleClick(object sender, MouseEventArgs e)
         {
         ListViewItem ClickedItem = SimulationList.GetItemAt(e.X, e.Y);
         if (ClickedItem != null)
            {
            string SummaryFileName = Simulations[ClickedItem.Index].SummaryFileName;
            Process.Start(SummaryFileName);
            }
         }



      }
   }