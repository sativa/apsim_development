using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace CSGeneral
	{

	public class ReorderForm : System.Windows.Forms.Form
		{
		private System.Windows.Forms.Button UpButton;
		private System.Windows.Forms.Button DownButton;
		private System.Windows.Forms.Button CanButton;
		private System.Windows.Forms.ListBox ListBox;
		private System.Windows.Forms.Button OkButton;
		private System.ComponentModel.Container components = null;

		#region Constructor / Dispose
		public ReorderForm()
			{
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if(components != null)
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ReorderForm));
			this.ListBox = new System.Windows.Forms.ListBox();
			this.UpButton = new System.Windows.Forms.Button();
			this.DownButton = new System.Windows.Forms.Button();
			this.OkButton = new System.Windows.Forms.Button();
			this.CanButton = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// ListBox
			// 
			this.ListBox.Location = new System.Drawing.Point(16, 8);
			this.ListBox.Name = "ListBox";
			this.ListBox.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
			this.ListBox.Size = new System.Drawing.Size(232, 303);
			this.ListBox.TabIndex = 0;
			this.ListBox.SelectedIndexChanged += new System.EventHandler(this.ListBox_SelectedIndexChanged);
			// 
			// UpButton
			// 
			this.UpButton.Image = ((System.Drawing.Image)(resources.GetObject("UpButton.Image")));
			this.UpButton.Location = new System.Drawing.Point(256, 128);
			this.UpButton.Name = "UpButton";
			this.UpButton.Size = new System.Drawing.Size(32, 32);
			this.UpButton.TabIndex = 1;
			this.UpButton.Click += new System.EventHandler(this.UpButton_Click);
			// 
			// DownButton
			// 
			this.DownButton.Image = ((System.Drawing.Image)(resources.GetObject("DownButton.Image")));
			this.DownButton.Location = new System.Drawing.Point(256, 168);
			this.DownButton.Name = "DownButton";
			this.DownButton.Size = new System.Drawing.Size(32, 32);
			this.DownButton.TabIndex = 2;
			this.DownButton.Click += new System.EventHandler(this.DownButton_Click);
			// 
			// OkButton
			// 
			this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.OkButton.Location = new System.Drawing.Point(304, 8);
			this.OkButton.Name = "OkButton";
			this.OkButton.TabIndex = 3;
			this.OkButton.Text = "Ok";
			this.OkButton.Click += new System.EventHandler(this.OkButton_Click);
			// 
			// CanButton
			// 
			this.CanButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.CanButton.Location = new System.Drawing.Point(304, 48);
			this.CanButton.Name = "CanButton";
			this.CanButton.TabIndex = 4;
			this.CanButton.Text = "Cancel";
			this.CanButton.Click += new System.EventHandler(this.CanButton_Click);
			// 
			// ReorderForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(386, 338);
			this.Controls.Add(this.CanButton);
			this.Controls.Add(this.OkButton);
			this.Controls.Add(this.DownButton);
			this.Controls.Add(this.UpButton);
			this.Controls.Add(this.ListBox);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Name = "ReorderForm";
			this.Text = "Reorder form";
			this.ResumeLayout(false);

		}
		#endregion
		#endregion

		public void SetItems(string[] Items)
			{	
			ListBox.Items.AddRange(Items);
			SetFunctionality();
			}

		public string[] GetItems()
			{	
			string[] Items = new string[ListBox.Items.Count];
			ListBox.Items.CopyTo(Items, 0);
			return Items;
			}


		private void UpButton_Click(object sender, System.EventArgs e)
			{
			int InsertIndex = ListBox.SelectedIndices[0] - 1;
			string[] SelectedItems = new string[ListBox.SelectedIndices.Count];
			ListBox.SelectedItems.CopyTo(SelectedItems, 0);

			// remove old items.
			foreach (string Item in SelectedItems)
				ListBox.Items.Remove(Item);

			// insert new items.
			foreach (string Item in SelectedItems)
				{
				ListBox.Items.Insert(InsertIndex, Item);
				InsertIndex++;
				}

			// select new items.
			foreach (string Item in SelectedItems)
				ListBox.SetSelected(ListBox.Items.IndexOf(Item), true);
			SetFunctionality();
			}

		private void DownButton_Click(object sender, System.EventArgs e)
			{
			int InsertIndex = ListBox.SelectedIndices[ListBox.SelectedIndices.Count-1] + 2;
			string[] SelectedItems = new string[ListBox.SelectedIndices.Count];
			ListBox.SelectedItems.CopyTo(SelectedItems, 0);

			// remove old items.
			foreach (string Item in SelectedItems)
				ListBox.Items.Remove(Item);

			// insert new items.
			InsertIndex -= SelectedItems.Length;
			foreach (string Item in SelectedItems)
				{
				ListBox.Items.Insert(InsertIndex, Item);
				InsertIndex++;
				}

			// select new items.
			foreach (string Item in SelectedItems)
				ListBox.SetSelected(ListBox.Items.IndexOf(Item), true);
			SetFunctionality();
			}

		private void SetFunctionality()
			{
			UpButton.Enabled = (ListBox.SelectedIndices.Count > 0 && ListBox.SelectedIndices[0] != 0);
			DownButton.Enabled = (ListBox.SelectedIndices.Count > 0 && ListBox.SelectedIndices[ListBox.SelectedIndices.Count-1] != ListBox.Items.Count-1);
			OkButton.Enabled = (ListBox.SelectedIndices.Count > 0);
			}

		private void ListBox_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			SetFunctionality();
			}

		private void CanButton_Click(object sender, System.EventArgs e)
			{
			Close();
			}

		private void OkButton_Click(object sender, System.EventArgs e)
			{
			Close();
			}
	}
}
