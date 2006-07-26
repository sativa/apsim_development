using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using VBGeneral;


namespace APSRU.Howwet
    {
    public partial class SoilSelection : Form
        {
        public SoilSelection()
            {
            InitializeComponent();
            TreeNode tn = new TreeNode("Top");
            treeView1.Nodes.Add(tn);

           
           
            }

        private void treeView1_Click(object sender,EventArgs e )
            {
            TreeNode tnode = new TreeNode("test");
              treeView1.SelectedNode.Nodes.Add(tnode);
              treeView1.ExpandAll();

            }

       

        }
    }