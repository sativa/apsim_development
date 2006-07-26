using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.Data;
using VBGeneral;
using CSGeneral;

namespace APSRU.Translator.Howwet
    {
    public class HowwetUtility
        {
        private String fileLocation="c:\\robert\\data\\howwettemplate.xml";

        private APSIMData apsimData = new APSIMData();

        public StringCollection GetListOfSoils(String fileName)
            {
            apsimData.LoadFromFile(fileName);
            StringCollection soilList = apsimData.ChildList("Soil");
            return soilList;
            }

        public APSIMData GetSoil(string soilName)
            {
            APSIMData soil = new APSIMData();
            return apsimData.Child(soilName);
            }

        public APSIMData ReadTemplateFile()
            {
            APSIMData template = new APSIMData();
            template.LoadFromFile(this.fileLocation);
            return template;
            }
       
        private String GetTemplateFileName()
            {
            return APSIMSettings.INIRead(fileLocation, "setup", "templatefile");
            }

       
        }
    }
