using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace APSRU.Howwet
    {
    public partial class Page : UserControl,IEventListener
        {
        private Explorer explorer = null;
        
        public Page()
            {
            InitializeComponent();
            }

        public virtual void Go(Page page)
            {
            explorer.Go(page);
            }

        public virtual void Go(Type pageType)
            {
            explorer.Go(pageType);
            }

        public Explorer Explorer
            {
            get { return explorer; }
            set { explorer = value; }
            }

        #region IEventListener Members

        public virtual void OnNotification(IHowwetModel publisher)
            {
            }

        #endregion

        }
    }
