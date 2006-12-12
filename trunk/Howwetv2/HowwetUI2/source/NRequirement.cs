using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace APSRU.Howwet
    {
    public partial class NRequirement : APSRU.Howwet.PageTemplate
        {
        public NRequirement()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Nitrogen Reuirement";
            this.SecondaryHeadingLabel = "NRequirement";   
            }

        #region IEventListener Members

        public override void OnNotification(IHowwetModel publisher)
            {
          
            }

        #endregion
        }
    }

