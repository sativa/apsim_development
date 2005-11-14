using System;
using System.Windows.Forms;
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
			else
				return 1;
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
				case "initwater"    : return new InitWaterUI();
				case "initnitrogen" : return new InitNitrogenUI();
				default             : return null;
				}
			}

		public void AddCrop()
			{
			string NewCropName = InputDialog.InputBox("Enter the name of the new crop:", "New crop", "");
			if (NewCropName != "")
				{
				Soil MySoil = new Soil(SelectedData[0] as APSIMData);
				MySoil.AddCrop(NewCropName);
				AddCropEvent();
				}
			}

		public void DeleteCrop()
			{
			string CropNameToDelete = InputDialog.InputBox("Enter the name of the crop to delete:", "New crop", "");
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

		}
	}

