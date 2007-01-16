using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace APSRU.Howwet
    {
    public partial class Graphs : APSRU.Howwet.PageTemplate
        {
        private DataTable chartData;
        private DateTime fallowStartDate;
        private DateTime fallowEndDate;

        public Graphs()
            {
            InitializeComponent();
            this.PrimaryHeadingLabel = "Graphs";
            this.SecondaryHeadingLabel = "Graphs";   
            }
  


#region IEventListener Members

        public override void OnNotification(IHowwetModel publisher)
            {
            chartData=publisher.ChartData;
            fallowStartDate=publisher.FallowDateStart;
            fallowEndDate = publisher.FallowDateEnd;
           // DisplayCharts();
            }

        #endregion

    }
        }
 

