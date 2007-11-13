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
using System.Xml;

namespace Graph
    {
    public partial class SOIUI : VBUserInterface.BaseView
        {
        public SOIUI()
            {
            InitializeComponent();
            }

        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();
            GroupBox.Text = Name;

            FileNameEdit.TextChanged -= OnFileNameChanged;
            MonthDropDown.TextChanged -= OnMonthChanged;
            PhaseList.ItemCheck -= OnPhaseItemCheck;

            FileNameEdit.Text = XmlHelper.Value(Data, "FileName");
            MonthDropDown.Text = XmlHelper.Value(Data, "Month");

            foreach (string PhaseName in XmlHelper.Values(Data, "Phase"))
                {
                int Index = PhaseList.Items.IndexOf(PhaseName);
                if (Index == -1)
                    Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "Phase", PhaseName));
                else
                    PhaseList.SetItemChecked(Index, true);
                }


            FileNameEdit.TextChanged += OnFileNameChanged;
            MonthDropDown.TextChanged += OnMonthChanged;
            PhaseList.ItemCheck += OnPhaseItemCheck;
            }

        private void OnFileNameChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "FileName", FileNameEdit.Text);
            PublishViewChanged();
            }

        private void OnPhaseItemCheck(object sender, ItemCheckEventArgs e)
            {
            if (e.NewValue == CheckState.Checked)
                {
                XmlNode NewPhase = XmlHelper.CreateNode(Data.OwnerDocument, "Phase", "");
                NewPhase.InnerText = PhaseList.Items[e.Index].ToString();
                Data.AppendChild(NewPhase);
                }
            else
                Data.RemoveChild(XmlHelper.ChildByTypeAndValue(Data, "Phase", PhaseList.Items[e.Index].ToString()));
            PublishViewChanged();
            }

        private void OnMonthChanged(object sender, EventArgs e)
            {
            XmlHelper.SetValue(Data, "Month", MonthDropDown.Text);
            PublishViewChanged();
            }        
        }
    }

