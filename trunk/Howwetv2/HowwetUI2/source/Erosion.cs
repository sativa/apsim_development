using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using APSRU.Model.Howwet;


namespace APSRU.Howwet
    {
    public partial class Erosion : Form
        {

        private static Erosion instance = null;
        public bool isLoaded = false;
        private SimulationIn simulationObject;
        public delegate void ErosionValuesChanged(String slope, String slopeLength, String erodibilty);
        public event ErosionValuesChanged ErosionChangedEvent;

        public Erosion()
            {
            InitializeComponent();
            }
       
        public static Erosion Instance
            {
            get
                {
                if (Erosion.instance == null) Erosion.instance = new Erosion();
                return Erosion.instance;
                }
            }

        public void loadObject(SimulationIn data)
            {
            isLoaded = true;
            simulationObject = data;
            slope.Text = simulationObject.ErosionSlope;
            slopeLength.Text = simulationObject.ErosionSlopeLength;
            erodibilty.Text = simulationObject.ErosionErodibilty;
            }

        private void Erosion_FormClosing(object sender, FormClosingEventArgs e)
            {
            instance = null;
            }

        private void SaveButton_Click(object sender, EventArgs e)
            {
            ErosionChangedEvent(slope.Text, slopeLength.Text, erodibilty.Text);
            this.Close();
            }

        private void closeButton_Click(object sender, EventArgs e)
            {
            this.Close();
            }

        

        }
    }