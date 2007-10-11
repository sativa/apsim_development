using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;

namespace Graph
    {
    public partial class ApsimFileReaderUI : BaseView
        {
        private APSIMData Data;
        private ChartPageUI ParentUI;
        public ApsimFileReaderUI()
            {
            InitializeComponent();
            }

        public override void OnLoad(BaseController Controller, string NodePath)
            {
            base.OnLoad(Controller, NodePath);
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();
            Data = Controller.ApsimData.Find(NodePath);
            FileList.Lines = Data.get_Values("FileName");
            GroupBox.Text = Name;
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
            }
        
        private void OnBrowseButtonClick(object sender, EventArgs e)
            {
            if (OpenFileDialog.ShowDialog() == DialogResult.OK)
                FileList.Lines = OpenFileDialog.FileNames;
            }

        private void OnTextChanged(object sender, EventArgs e)
            {
            Data.set_Values("FileName", FileList.Lines);
            ParentUI.DoRefresh(Data);
            }


        }
    }

