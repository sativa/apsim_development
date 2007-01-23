using System;
using System.Collections.Generic;
using System.Text;

namespace APSRU.Model
    {
    //Region Enity object class
    public class Region
        {
        private String name;
        private String description;
        private String[] averageMonthlyRain;
        private String[] averageMonthlyRadiation;
        private String[] averageMonthlyMaxT;
        private String[] averageMonthlyMinT;

        public String Name
            {
            set { name = value; }
            get { return name; }
            }
        public String Description
            {
            set { description = value; }
            get { return description; }
            }
        public String[] AverageMonthlyRain
            {
            set { averageMonthlyRain = value; }
            get { return averageMonthlyRain; }
            }
        public String[] AverageMonthlyRadiation
            {
            set { averageMonthlyRadiation = value; }
            get { return averageMonthlyRadiation; }
            }
        public String[] AverageMonthlyMaxT
            {
            set { averageMonthlyMaxT = value; }
            get { return averageMonthlyMaxT; }
            }
        public String[] AverageMonthlyMinT
            {
            set { averageMonthlyMinT = value; }
            get { return averageMonthlyMinT; }
            }
        }
    }
