using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Data;

namespace APSRU.Howwet
    {
    public interface IHowwetModel
        {
        //Startup page
        string SoilFileName {get;set;}
        string SoilName { get;set;}
        string MetFileName { get;set;}
        string SoilFileFullName { get;set;}
        ArrayList RegionList { get;set;}
        string Region { get;set;}
        
        //Fallow Settings Page
        string SoilRegion { get;set;}
        String OcDepthFirstLayer { get;set;}
        double OrganicCarbonContent { get;set;}
        double SoilDepth { get;set;}
        double PAWC { get;set;}
        double InitialNitrogen { get;set;}
        double InitialWater { get;set;}
        int InitialWaterPercent { get;set;}
        string CropToGrow { get;set;}
        ArrayList CoverTypeCropList { get;set;}
        string[] CropToGrowList { get;set;}
        decimal CoverStart { get;set;}
        decimal CoverEnd { get;set;}
        DateTime FallowDateStart { get;set;}
        DateTime FallowDateEnd { get;set;}

        //Results Page

        double StartPAW { get;set;}
        double FallowRainfall { get;set;}
        double FallowEvaporation { get;set;}
        double FallowRunoff { get;set;}
        double FallowDrainage { get;set;}
        double EndPAW { get;set;}
        double GainPAW { get;set;}
        double FallowEfficiency { get;set;}
        double StartSoilNitrate { get;set;}
        double EndSoilNitrate { get;set;}
        double GainNitrate { get;set;}

        //Nitrogen Requirement

       // double InCropRainfall { get;set;}
       // double DaysToMaturity { get;set;}
       // double ThresholdWater { get;set;}
       // double WUE { get;set;}
       // double Yield { get;set;}
       // double NDemand { get;set;}
       // double NGap { get;set;}

        //Graphs

        DataTable ChartData { get;set;}

        void RegisterListener(IEventListener listener);
        void UnregisterListener(IEventListener listener);
        void NotifyListeners();
        }
    }
