using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Model.Howwet
    {
    //CoverCrop Enity object class
    public class CoverCrop
        {
        private String name;
        private double specificArea;
        private int cnr;

        public String Name
            {
            set { name = value; }
            get { return name; }
            }
        public double SpecificArea
            {
            set { specificArea = value; }
            get { return specificArea; }
            }
        public int Cnr
            {
            set { cnr = value; }
            get { return cnr; }
            }
        }
    }
