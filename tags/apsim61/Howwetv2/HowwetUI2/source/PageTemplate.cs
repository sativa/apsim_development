using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

using APSRU.Error;


namespace APSRU.Howwet
    {
    public partial class PageTemplate :APSRU.Howwet.Page
	{
        private String primaryHeadingLabel;
        private String secondaryHeadingLabel;
        

		public PageTemplate()
		{
			InitializeComponent();
		}

        public String PrimaryHeadingLabel
            {
            get { return primaryHeadingLabel; }
            set { primaryHeadingLabel = value; }
            }

        public String SecondaryHeadingLabel
            {
            get { return secondaryHeadingLabel; }
            set { secondaryHeadingLabel = value; }
            }

        protected override void OnPaint(PaintEventArgs e)
            {
            base.OnPaint(e);
            this.textHeader1.HeaderText = primaryHeadingLabel;
            this.secondaryLabel.Text = secondaryHeadingLabel;
            }


        private void startupLabel_Click(object sender, EventArgs e)
            {
            Go(typeof(Home));
            }

        private void fallowSettingsLabel_Click(object sender, EventArgs e)
            {
            Go(typeof(FallowSettings));
            }

        private void resultsLabel_Click(object sender, EventArgs e)
            {
            Go(typeof(Results));
            }
    }
    }
