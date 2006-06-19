using System;
using System.Windows.Forms;
using System.IO;
using VBGeneral;
using CSGeneral;

namespace CSGeneral
	{

	public class ApsoilController : BaseController
		{
		private ImageList MyImageList;
		public event NotifyEventHandler AddCropEvent;
		public event NotifyEventHandler DeleteCropEvent;
		public event NotifyEventHandler ReorderCropEvent;
		public event NotifyEventHandler AddNoteEvent;
		public event NotifyEventHandler DeleteNoteEvent;
		public event NotifyEventHandler PrintEvent;


		// -----------
		// constructor
		// -----------
		public ApsoilController(string DefaultExtension,
								string DialogFilter, 
								string FrequentListSection,
								ImageList images)	
			: base(DefaultExtension, DialogFilter, FrequentListSection)
			{
			MyImageList = images;
			}


		// ----------------------------------------------
		// Provide access to our imagelist of small icons
		// ----------------------------------------------
		public override ImageList SmallImageList
			{
			get
				{
				return MyImageList;
				}
			}


		// --------------------------------------------
		// Return access to an imagelist of small icons
		// --------------------------------------------
		public override int SmallImageIndex(string ComponentName)
			{
			if (ComponentName.ToLower() == "soil")
				return 0;
			else if (ComponentName.ToLower() == "initwater")
				return 2;
			else if (ComponentName.ToLower() == "initnitrogen")
				return 3;
			else if (ComponentName.ToLower() == "folder" || ComponentName.ToLower() == "soils")
				return 1;
			else if (ComponentName.ToLower() == "sample")
				return 4;
			else
				return -1;
			}

		// -------------------------------------------------
		// Return true if the specified component is visible
		// to the user.
		// -------------------------------------------------
		public override bool IsComponentVisible(string ComponentName)
			{
			string ComponentNameLowerCase = ComponentName.ToLower();
			return (ComponentNameLowerCase == "soils" || 
				    ComponentNameLowerCase == "soil" || 
				    ComponentNameLowerCase == "sample" ||
					ComponentNameLowerCase == "initwater" || 
					ComponentNameLowerCase == "initnitrogen" ||
					ComponentNameLowerCase == "folder");

			}


		public override bool AllowFileOpenWrite(string FullFileName)
			{
			string FileName = Path.GetFileName(FullFileName).ToLower();
			if (FileName.Substring(0, 6) == "apsru-" || FileName.Substring(0, 4) == "npd-" || FileName.Substring(0, 3) == "ap-")
				{
                string Password = InputDialog.InputBox("Enter password:", "This file is password protected", "", true);
                if (Password == "soilinfo")
                    return true;
                else if (Password == "")
                    return false;
                else
					{
					MessageBox.Show("Password incorrect", "Error",MessageBoxButtons.OK, MessageBoxIcon.Error);
					return false;
					}

				}
			else
				return true;
			}
		// -------------------------------------------------
		// Return true if the specified component type can
		// be added as a child to the specified parent type.
		// -------------------------------------------------
		public override bool AllowComponentAdd(string ChildComponentType, string ParentComponentType)
			{
			if (ChildComponentType.ToLower() == "soil")
            	return (ParentComponentType.ToLower() == "soils" || ParentComponentType.ToLower() == "folder");
			else if (ChildComponentType.ToLower() == "initwater" || ChildComponentType.ToLower() == "initnitrogen")
				return (ParentComponentType.ToLower() == "soil");
			else if (ChildComponentType.ToLower() == "sample")
				return (ParentComponentType.ToLower() == "soil");
			return true;
			}


		// -------------------------------------------------
		// Open a new user interface based on the specified
		// component type.
		// -------------------------------------------------
		public override BaseView CreateUI(string UIType)
			{
            switch (UIType.ToLower())
				{
				case "soil"         : return new SoilUI();
				case "sample"       : return new SampleUI();
				case "initwater"    : return new InitWaterUI();
				case "initnitrogen" : return new InitNitrogenUI();
				default             : return null;
				}
			}

		public void AddCrop()
			{
			string NewCropName = InputDialog.InputBox("Enter the name of the new crop:", "New crop", "", false);
			if (NewCropName != "")
				{
				Soil MySoil = new Soil(SelectedData[0] as APSIMData);
				MySoil.AddCrop(NewCropName);
				AddCropEvent();
				}
			}

		public void DeleteCrop()
			{
			string CropNameToDelete = InputDialog.InputBox("Enter the name of the crop to delete:", "New crop", "", false);
			if (CropNameToDelete != "")
				{
				try
					{
					Soil MySoil = new Soil(SelectedData[0] as APSIMData);
					MySoil.DeleteCrop(CropNameToDelete);
					DeleteCropEvent();
					}
				catch (Exception err)
					{
					MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
					}
				}
			}

		public void ReorderCrops()
			{
			Soil MySoil = new Soil(SelectedData[0] as APSIMData);

			ReorderForm Form = new ReorderForm();
			Form.SetItems(MySoil.Crops);
			if (Form.ShowDialog() == DialogResult.OK)
				{
				string[] Crops = Form.GetItems();
				MySoil.SetCropOrder(Crops);
				ReorderCropEvent();
				}
			}

		public void AddNote()
			{
			AddNoteEvent();
			}

		public void DeleteNote()
			{
			DeleteNoteEvent();
			}

		public void Print()
			{
			PrintEvent();
			}


		public bool AllowInsertFolder
			{
			get {return (SelectedData.Count == 1 && AllowComponentAdd("folder", ((APSIMData)SelectedData[0]).Type));}
			}
		public bool AllowInsertSoil
			{
			get {return (SelectedData.Count == 1 && AllowComponentAdd("soil", ((APSIMData)SelectedData[0]).Type));}
			}
		public bool AllowInsertSample
			{
			get {return (SelectedData.Count == 1 && AllowComponentAdd("sample", ((APSIMData)SelectedData[0]).Type));}
			}
		public void InsertFolder()
			{
			if (AllowInsertFolder)
				AddXMLToSelected("<folder name=\"NewFolder\"/>");
			}

		public void InsertSoil()
			{
            if (AllowInsertSoil)
                {
                Soil NewSoil = new Soil(new APSIMData("soil", "NewSoil"));
                AddXMLToSelected(NewSoil.Data.XML);
                }
			}

		public void InsertSample()
			{
			if (AllowInsertSample)
				AddXMLToSelected("<sample name=\"NewSample\"/>");
			}

        public void CheckAllSoils(APSIMData Data, ref string ErrorMessage)
            {
            // Check all soils and return an error message string
            foreach (APSIMData Child in Data.get_Children(null))
                {
                if (Child.Type.ToLower() == "folder" || Child.Type.ToLower() == "soils")
                    CheckAllSoils(Child, ref ErrorMessage);
                else if (Child.Type.ToLower() == "soil")
                    {
                    Soil ThisSoil = new Soil(Child);
                    string Errors = ThisSoil.CheckForErrors();
                    if (Errors != "")
                        ErrorMessage += "\r\n" + ThisSoil.Name + "\r\n" + StringManip.IndentText(Errors, 6);
                    }
                }
            }

		}
	}

