using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.ComponentModel.Design;

namespace APSRU.UIControls
    {
    [Designer("System.Windows.Forms.Design.ParentControlDesigner, System.Design", typeof(IDesigner))] 
    public partial class HowwetPageTemplate : UserControl
        {
        private String tempText;
        private String leftText;
        public HowwetPageTemplate()
            {
            InitializeComponent();
            }

        public String Text1
            {
            get { return tempText; }
            set { tempText = value; }
            }

        public String LeftText
            {
            get { return leftText; }
            set { leftText = value; }
            }

        protected override void OnPaint(PaintEventArgs e)
            {
            textHeader1.HeaderText = tempText;
            leftLabel.Text = leftText; ;
            base.OnPaint(e);
            }
      
               
        }
    }
