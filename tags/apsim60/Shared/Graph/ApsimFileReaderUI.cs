using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
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

            FileList.TextChanged -= OnTextChanged;
            List<string> FileNames = XmlHelper.Values(Data, "FileName");
            string[] AllLines = new string[FileNames.Count];
            FileNames.CopyTo(AllLines);
            FileList.Lines = AllLines;
            GroupBox.Text = Name;
            FileList.TextChanged += OnTextChanged;
            }
   
        private void OnBrowseButtonClick(object sender, EventArgs e)
            {
            if (OpenFileDialog.ShowDialog() == DialogResult.OK)
                {
                string[] Files = OpenFileDialog.FileNames;
                for (int i = 0; i != Files.Length; i++)
                    {
                    Files[i] = Files[i].Replace(Directory.GetCurrentDirectory() + "\\", "");
                    Files[i] = Files[i].Replace(APSIMSettings.ApsimDirectory(), "%apsuite");
                    }
                FileList.Lines = Files;
                }
            }

        private void OnTextChanged(object sender, EventArgs e)
            {
            List<string> FileNames = new List<string>();
            FileNames.AddRange(FileList.Lines);
            XmlHelper.SetValues(Data, "FileName", FileNames);
            PublishViewChanged();
            }



        }
    }
