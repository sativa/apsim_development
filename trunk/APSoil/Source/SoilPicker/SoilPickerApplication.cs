using System;
using System.Windows.Forms;
using VBGeneral;
using CSGeneral;

namespace APSoil
	{
	// -------------------------------------
	// Summary description for UIManager.
	// -------------------------------------
	public class SoilPickerController : BaseController
		{
		private ImageList MyImageList;


		// -----------
		// constructor
		// -----------
		public SoilPickerController(string DefaultExtension,
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

		}
	}

