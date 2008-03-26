using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Model.Howwet
    {
    //CropNDemand Enity object class
    public class CropNDemand
        {
        private String name;
        private double protein_target;
        private double fraction_of_n_in_protein;
        private double n_uptake_efficiency;

        public String Name
            {
            set { name = value; }
            get { return name; }
            }

        public double Protein_target
            {
            set { protein_target = value; }
            get { return protein_target; }
            }

        public double Fraction_of_n_in_protein
            {
            set { fraction_of_n_in_protein = value; }
            get { return fraction_of_n_in_protein; }
            }

        public double N_uptake_efficiency
            {
            set { n_uptake_efficiency = value; }
            get { return n_uptake_efficiency; }
            }
        }
    }
