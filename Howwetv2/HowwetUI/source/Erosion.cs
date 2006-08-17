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
        public Erosion(SimulationIn simulationObject)
            {
            InitializeComponent();
            slope.Text = simulationObject.ErosionSlope;
            slopeLength.Text = simulationObject.ErosionSlopeLength;
            erodibilty.Text = simulationObject.ErosionErodibilty;
            }
        }
    }