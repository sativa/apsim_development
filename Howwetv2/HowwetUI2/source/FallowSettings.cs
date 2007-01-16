using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using APSRU.Model.Howwet;

namespace APSRU.Howwet
    {
    public partial class FallowSettings : APSRU.Howwet.PageTemplate
        {
        public FallowSettings()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Adjust your fallow simulation settings";
            this.SecondaryHeadingLabel = "Fallow Settings";

            }

        private void initialWaterPercent_ValueChanged(object sender, EventArgs e)
            {
            if (!this.Explorer.inUpdate)
                {
                this.Explorer.UpdateInitialWater(Convert.ToInt32(initialWaterPercent.Value));
                }
            }

        private void initialWater_TextChanged(object sender, EventArgs e)
            {
            if (!this.Explorer.inUpdate)
                {
              //  this.Explorer.UpdateInitialWater(Convert.ToDouble(initialWater.Text));
                }
            }

        private void displayCoverCropList(ArrayList crops)
            {
            if (!(crops == null))
                {
                coverCropList.Items.Clear();
                for (int i = 0; i < crops.Count; i++)
                    {
                    CoverCrop crop = (CoverCrop)crops[i];
                    coverCropList.Items.Add(crop.Name);
                    }
                this.coverCropList.SelectedValueChanged -= new System.EventHandler(this.coverCropList_SelectedValueChanged);
                coverCropList.SelectedIndex = 0;
                this.coverCropList.SelectedValueChanged += new System.EventHandler(this.coverCropList_SelectedValueChanged);
                }
            }

        //On selecting the cover crop 
        private void coverCropList_SelectedValueChanged(object sender, EventArgs e)
            {
            this.Explorer.UpdateCoverType((String)coverCropList.SelectedItem);
            }

        private void displayProposedCropList(String[] crops)
            {
            proposedCropList.Items.Clear();
           // String[] crops = simulationObject.Soil.Crops;
            if (!(crops==null))
                {
                for (int i = 0; i < crops.Length; i++)
                    {
                    proposedCropList.Items.Add(crops[i]);
                    }
                proposedCropList.SelectedIndex = 0;
                }
            else
                {
                proposedCropList.Text = "Using LL15";
                }
            }

        //After selecting a proposed corp subtract the corp from the esw to get PAWC 
        private void proposedCropList_SelectedValueChanged(object sender, System.EventArgs e)
            {
           // String selectedCrop = (String)proposedCropList.SelectedItem;
          //  if (selectedCrop == "Using LL15")
          //      {
          //      cLL = simulationObject.Soil.LL15;
          //      }
          //  else
          //      {
          //      cLL = simulationObject.Soil.LL(selectedCrop);
          //      }
         //   endPAW.Text = result.calcPAWEnd(cLL).ToString("f0");
         //   calculateNitrogenRequirement();
            }

        private void erosionOptions_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
            {
                {
                if (!Erosion.Instance.isLoaded)
                    {
                    Erosion.Instance.loadObject(this.Explorer.simulationObject);
                    Erosion.Instance.ErosionChangedEvent += new Erosion.ErosionValuesChanged(Instance_ErosionChangedEvent);
                    }
                Erosion.Instance.Focus();
                Erosion.Instance.Show();
                }
            }

        void Instance_ErosionChangedEvent(string slope, string slopeLength, string erodibilty)
            {
            this.Explorer.UpdateErosion(slope, slopeLength, erodibilty);
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

        private void StartDatePicker_ValueChanged(object sender, EventArgs e)
            {
            this.Explorer.UpdateStartFallowDate(StartDatePicker.Value);
            }

        private void EndDatePicker_ValueChanged(object sender, EventArgs e)
            {
            this.Explorer.UpdateEndFallowDate(EndDatePicker.Value);
            }

        private void coverStartPercent_ValueChanged(object sender, EventArgs e)
            {
            this.Explorer.UpdateStartCover(coverStartPercent.Value);
            }

        private void coverEndPercent_ValueChanged(object sender, EventArgs e)
            {
            this.Explorer.UpdateEndCover(coverEndPercent.Value);
            }

        private void button1_Click(object sender, EventArgs e)
            {
            MessageBox.Show(this.Explorer.simulationObject.Data.XML);
            }

        private void runSimulation_Click(object sender, EventArgs e)
            {
            this.Explorer.SaveAllData();

            if (this.Explorer.ExecuteAPSIM())
                {
                this.Explorer.BuildResultObject();
                }
            Go(typeof(Results));
            }

        #region IEventListener Members

        public override void OnNotification(IHowwetModel publisher)
            {
            soilName.Text = publisher.SoilName;
            soilRegion.Text=publisher.SoilRegion;
            ocDepthLabel.Text = publisher.OcDepthFirstLayer;
            organicCarbonContent.Text = publisher.OrganicCarbonContent.ToString("f0");
            soilDepth.Text = publisher.SoilDepth.ToString("f0");
            PAWC.Text = publisher.PAWC.ToString("f0");
            initialWater.Text = publisher.InitialWater.ToString("f0");
            initialWaterPercent.Value = publisher.InitialWaterPercent;
            initialNitrogen.Text = publisher.InitialNitrogen.ToString("f0");
            displayCoverCropList(publisher.CoverTypeCropList);
            displayProposedCropList(publisher.CropToGrowList);
            EndDatePicker.MaxDate = publisher.FallowDateEnd;
            StartDatePicker.MinDate = publisher.FallowDateStart;
            StartDatePicker.Value=publisher.FallowDateStart;
            EndDatePicker.Value = publisher.FallowDateEnd;
            coverStartPercent.Value=publisher.CoverStart;
            coverEndPercent.Value=publisher.CoverEnd;
            }
        #endregion
      
        
        }
    }

