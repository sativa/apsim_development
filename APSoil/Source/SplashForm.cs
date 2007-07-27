using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace APSoil
	{
	public class SplashForm : System.Windows.Forms.Form
		{
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.Label VersionLabel;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Button button1;
        private Label label3;
        private Label label2;
        private Label label5;
        private Label label4;
		private System.ComponentModel.Container components = null;

		public SplashForm()
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
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SplashForm));
        this.panel1 = new System.Windows.Forms.Panel();
        this.button1 = new System.Windows.Forms.Button();
        this.VersionLabel = new System.Windows.Forms.Label();
        this.label1 = new System.Windows.Forms.Label();
        this.label2 = new System.Windows.Forms.Label();
        this.label3 = new System.Windows.Forms.Label();
        this.label4 = new System.Windows.Forms.Label();
        this.label5 = new System.Windows.Forms.Label();
        this.panel1.SuspendLayout();
        this.SuspendLayout();
        // 
        // panel1
        // 
        this.panel1.BackColor = System.Drawing.SystemColors.Window;
        this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
        this.panel1.Controls.Add(this.label5);
        this.panel1.Controls.Add(this.label4);
        this.panel1.Controls.Add(this.label3);
        this.panel1.Controls.Add(this.label2);
        this.panel1.Controls.Add(this.button1);
        this.panel1.Controls.Add(this.VersionLabel);
        this.panel1.Controls.Add(this.label1);
        this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
        this.panel1.Location = new System.Drawing.Point(0, 370);
        this.panel1.Name = "panel1";
        this.panel1.Size = new System.Drawing.Size(634, 240);
        this.panel1.TabIndex = 3;
        // 
        // button1
        // 
        this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
        this.button1.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
        this.button1.Location = new System.Drawing.Point(268, 204);
        this.button1.Name = "button1";
        this.button1.Size = new System.Drawing.Size(88, 23);
        this.button1.TabIndex = 5;
        this.button1.Text = "Acknowledge";
        // 
        // VersionLabel
        // 
        this.VersionLabel.AutoSize = true;
        this.VersionLabel.BackColor = System.Drawing.SystemColors.Window;
        this.VersionLabel.Font = new System.Drawing.Font("Comic Sans MS", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.VersionLabel.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
        this.VersionLabel.Location = new System.Drawing.Point(181, 9);
        this.VersionLabel.Name = "VersionLabel";
        this.VersionLabel.Size = new System.Drawing.Size(52, 15);
        this.VersionLabel.TabIndex = 4;
        this.VersionLabel.Text = "Version: ";
        this.VersionLabel.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
        // 
        // label1
        // 
        this.label1.BackColor = System.Drawing.SystemColors.Window;
        this.label1.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
        this.label1.Font = new System.Drawing.Font("Comic Sans MS", 21.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.label1.ForeColor = System.Drawing.Color.Blue;
        this.label1.Location = new System.Drawing.Point(3, 0);
        this.label1.Name = "label1";
        this.label1.Size = new System.Drawing.Size(184, 32);
        this.label1.TabIndex = 3;
        this.label1.Text = "APSoil";
        this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
        // 
        // label2
        // 
        this.label2.AutoSize = true;
        this.label2.BackColor = System.Drawing.SystemColors.Window;
        this.label2.Font = new System.Drawing.Font("Comic Sans MS", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.label2.ForeColor = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
        this.label2.Location = new System.Drawing.Point(3, 37);
        this.label2.Name = "label2";
        this.label2.Size = new System.Drawing.Size(116, 15);
        this.label2.TabIndex = 6;
        this.label2.Text = "Important Disclaimer";
        this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
        // 
        // label3
        // 
        this.label3.AutoSize = true;
        this.label3.BackColor = System.Drawing.SystemColors.Window;
        this.label3.Font = new System.Drawing.Font("Comic Sans MS", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.label3.ForeColor = System.Drawing.Color.Black;
        this.label3.Location = new System.Drawing.Point(3, 52);
        this.label3.Name = "label3";
        this.label3.Size = new System.Drawing.Size(288, 15);
        this.label3.TabIndex = 7;
        this.label3.Text = "Be advised that the data and information in APSoil are:";
        this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
        // 
        // label4
        // 
        this.label4.AutoSize = true;
        this.label4.BackColor = System.Drawing.SystemColors.Window;
        this.label4.Font = new System.Drawing.Font("Comic Sans MS", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.label4.ForeColor = System.Drawing.Color.Black;
        this.label4.Location = new System.Drawing.Point(20, 67);
        this.label4.Name = "label4";
        this.label4.Size = new System.Drawing.Size(591, 45);
        this.label4.TabIndex = 8;
        this.label4.Text = resources.GetString("label4.Text");
        this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
        // 
        // label5
        // 
        this.label5.AutoSize = true;
        this.label5.BackColor = System.Drawing.SystemColors.Window;
        this.label5.Font = new System.Drawing.Font("Comic Sans MS", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.label5.ForeColor = System.Drawing.Color.Black;
        this.label5.Location = new System.Drawing.Point(3, 120);
        this.label5.Name = "label5";
        this.label5.Size = new System.Drawing.Size(610, 75);
        this.label5.TabIndex = 9;
        this.label5.Text = resources.GetString("label5.Text");
        this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
        // 
        // SplashForm
        // 
        this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
        this.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("$this.BackgroundImage")));
        this.ClientSize = new System.Drawing.Size(634, 610);
        this.ControlBox = false;
        this.Controls.Add(this.panel1);
        this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
        this.MaximizeBox = false;
        this.MinimizeBox = false;
        this.Name = "SplashForm";
        this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
        this.panel1.ResumeLayout(false);
        this.panel1.PerformLayout();
        this.ResumeLayout(false);

		}
		#endregion

		public void Setup(string version)
			{
			VersionLabel.Text = version;
			}
	
		}
	}
