using System;
using System.IO;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;

namespace APSRU.Howwet
    {
    public partial class Home : APSRU.Howwet.PageTemplate
        {

        private String soilFileFullName="";
        public Home()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Pick a soil and metfile that best represent your location";
            this.SecondaryHeadingLabel = "Startup";           
            }


        private void newSimulation_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            
            }

        private void openSimulation_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {

            }

        private void saveSimulation_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {

            }


        private void nextStepButton_Click(object sender, EventArgs e)
            {
            Go(typeof(FallowSettings));
            }

        private void editMetfile_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            if (!(this.Explorer.metObject == null))
                {
                if (!RainfallEditor.Instance.isLoaded)
                    {
                    RainfallEditor.Instance.loadObject(this.Explorer.metObject);
                    }
                RainfallEditor.Instance.Focus();
                RainfallEditor.Instance.Show();
                }
            else
                {
                MessageBox.Show("Please select a Met file to edit");
                } 
            }
        
        private void selectSoil_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            if (!(soilFileFullName == ""))
                {
                showSoilTree(soilFileFullName);
                }
            else
                {
                MessageBox.Show("Please select a Soil File first");
                }
            }

        private void showSoilTree(String soilFilePath)
            {
            if (!SoilSelection.Instance.isLoaded)
                {
                SoilSelection.Instance.loadObject(soilFilePath);
                SoilSelection.Instance.SoilSelectedEvent += new SoilSelection.SoilSelected(soilForm_SoilSelectedEvent);
                }
            SoilSelection.Instance.Focus();
            SoilSelection.Instance.Show();
            }

        void soilForm_SoilSelectedEvent(APSIMData soil)
            {
            this.Explorer.LoadSoil(soil);
            }

        private void browseSoilFile_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.Title = "Browse for Soil File";
            openDialog.Filter = "Soils files (*.soils)|*.soils";
            openDialog.ShowDialog();
            if (!(openDialog.FileName == ""))
                {
                this.Explorer.LoadSoilFile(openDialog.FileName);
                showSoilTree(soilFileFullName);
                }
            }

        private void browseMetfile_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
            //Select a met file
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.Title = "Browse for Met File";
            openDialog.Filter = "Met files (*.met)|*.met";
            openDialog.ShowDialog();
            if (!(openDialog.FileName == ""))
                {
                this.Explorer.LoadMetFile(openDialog.FileName);
                }
            }

        private void regionList_SelectedValueChanged(object sender, EventArgs e)
            {
            this.Explorer.LoadRegion((String)regionList.SelectedItem);
            }

        private void displayRegionList(ArrayList regions)
            {
            if (!(regions == null))
                {
                regionList.Items.Clear();
                for (int i = 0; i < regions.Count; i++)
                    {
                    APSRU.Model.Howwet.Region region = (APSRU.Model.Howwet.Region)regions[i];
                    regionList.Items.Add(region.Name);
                    }
                this.regionList.SelectedValueChanged -= new System.EventHandler(this.regionList_SelectedValueChanged);
                regionList.SelectedIndex = 0;
                this.regionList.SelectedValueChanged += new System.EventHandler(this.regionList_SelectedValueChanged);
                }
            }
    
        #region IEventListener Members

        public override void OnNotification(IHowwetModel publisher) 
            {
            soilFileName.Text=publisher.SoilFileName;
            soilFileFullName = publisher.SoilFileFullName;
            soilName.Text=publisher.SoilName;
            txtMetFile.Text=publisher.MetFileName;
            displayRegionList(publisher.RegionList);
            }

        #endregion
       
        }
    }

