using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBUserInterface;
using VBGeneral;
using CSGeneral;

namespace Graph
    {
    public partial class ApsimFileReaderUI : BaseView
        {
        private ChartPageUI ParentUI;
        public ApsimFileReaderUI()
            {
            InitializeComponent();
            }

        protected override void OnLoad()
            {
            ParentUI = (ChartPageUI)Parent;
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();
            List<string> FileNames = XmlHelper.Values(Data, "FileName");
            string[] AllLines = new string[FileNames.Count];
            FileList.Lines = AllLines;
            GroupBox.Text = Name;
            }
        protected override void OnSave()
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
            List<string> FileNames = new List<string>();
            FileNames.AddRange(FileList.Lines);
            XmlHelper.SetValues(Data, "FileName", FileNames);
            PublishViewChanged(Data);
            }


        }
    }

