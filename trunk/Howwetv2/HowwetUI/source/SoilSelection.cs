using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;
using CSGeneral;


namespace APSRU.Howwet
    {
    public partial class SoilSelection : Form
        {

        public delegate void SoilSelected(APSIMData soil);
        public event SoilSelected SoilSelectedEvent;

        private APSIMData soilsObject;
        private ApsoilController Apsoil;
        public SoilSelection(String fileName)
            {
            InitializeComponent();
            soilsObject = new APSIMData();
            soilsObject.LoadFromFile(fileName);
            }

        private void SoilSelection_Load(object sender, EventArgs e)
            {
            this.dataTree1.ExpandAll = false;
            Apsoil = new ApsoilController(".soils", "Soils files (*.soils)|*.soils|" +
                                            "All files (*.*)|*.*",
                                                "Apsoil",
                                                imageList1);
            Apsoil.AllData = soilsObject;
            dataTree1.Controller = Apsoil;
            }
        private void dataTree1_DoubleClickEvent()
            {
            if (Apsoil.Data.Type == "Soil")
                {
                SoilSelectedEvent(Apsoil.Data);
                this.Close();
                }
            }

       
        }
    }