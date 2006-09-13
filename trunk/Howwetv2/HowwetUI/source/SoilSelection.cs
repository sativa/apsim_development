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
        private static SoilSelection instance = null;
        public bool isLoaded = false;
        public delegate void SoilSelected(APSIMData soil);
        public event SoilSelected SoilSelectedEvent;

        private APSIMData soilsObject;
        private ApsoilController Apsoil;

        public static SoilSelection Instance
            {
            get
                {
                if (SoilSelection.instance == null)SoilSelection.instance = new SoilSelection();
                return SoilSelection.instance;
                }
            }

        public SoilSelection()
            {
            InitializeComponent();
            }

        public void loadObject(String fileName)
            {
            soilsObject = new APSIMData();
            soilsObject.LoadFromFile(fileName);
            isLoaded = true;
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
            if (Apsoil.Data.Type == "soil")
                {
                SoilSelectedEvent(Apsoil.Data);
                this.Close();
                }
            }

        private void SoilSelection_FormClosing(object sender, FormClosingEventArgs e)
            {
            instance = null;
            }

        private void SoilCloseButton_Click(object sender, EventArgs e)
            {
            this.Close();
            }

        private void SaveCloseButton_Click(object sender, EventArgs e)
            {
            if (Apsoil.Data.Type == "Soil")
                {
                SoilSelectedEvent(Apsoil.Data);
                }
            this.Close();
            }
        }
    }