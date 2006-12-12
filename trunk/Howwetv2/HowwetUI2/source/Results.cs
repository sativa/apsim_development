using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace APSRU.Howwet
    {
    public partial class Results : APSRU.Howwet.PageTemplate
        {
        public Results()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Your results";
            this.SecondaryHeadingLabel = "Results";
            }

        private void simulationGraphs_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            Go(typeof(Graphs));
            }

        private void nRequirement_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            Go(typeof(NRequirement));
            }

        private void viewReport_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {

            }

        #region IEventListener Members

        public override void OnNotification(IHowwetModel publisher)
            {
            //Water
            startSoilWater.Text = publisher.StartPAW.ToString("f0"); 
            fallowRainfall.Text = publisher.FallowRainfall.ToString("f0") ;
            fallowEvaporation.Text = publisher.FallowEvaporation.ToString("f0");
            fallowRunoff.Text = publisher.FallowRunoff.ToString("f0");
            drainage.Text = publisher.FallowDrainage.ToString("f0");
            endSoilWater.Text = publisher.EndPAW.ToString("f0");
            gainSoilWater.Text = publisher.GainPAW.ToString("f0");
            waterEfficiency.Text = publisher.FallowEfficiency.ToString("f0");

            //Nitrogen
            startSoilNitrate.Text = publisher.StartSoilNitrate.ToString("f0");
            endSoilNitrate.Text = publisher.EndSoilNitrate.ToString("f0");
            gainNitrate.Text = publisher.GainNitrate.ToString("f0");
            }

        #endregion

       
       
        }
    }

