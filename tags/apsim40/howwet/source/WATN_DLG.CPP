/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         watn_dlg.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Water_n_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <consts.rh>
#include "watn_dlg.h"
#include <stdio.h>
#include "so_what.h"
#include <gobjsx\texttble.h>
#include "Rainfall\HowwetRainfall.h"

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Water_n_dlg, TDialog)
//{{Water_n_dlgRSP_TBL_BEGIN}}
    EV_EN_CHANGE(IDC_EDIT1, Fill_results_boxes),
    EV_EN_CHANGE(IDC_EDIT2, In_crop_rain_change),
    EV_EN_CHANGE(IDC_EDIT3, Days_to_maturity_change),
    EV_EN_CHANGE(IDC_EDIT4, Fill_results_boxes),
    EV_EN_CHANGE(IDC_EDIT5, Fill_results_boxes),
    EV_EN_CHANGE(IDC_EDIT6, Fill_results_boxes),
    EV_EN_CHANGE(IDC_EDIT7, Fill_results_boxes),
    EV_CBN_SELCHANGE(IDC_COMBOBOX1, Fill_results_boxes),
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO10, Help10),
    EV_BN_CLICKED(INFO11, Help11),
    EV_BN_CLICKED(INFO2, Help2),
    EV_BN_CLICKED(INFO3, Help3),
    EV_BN_CLICKED(INFO4, Help4),
    EV_BN_CLICKED(INFO5, Help5),
    EV_BN_CLICKED(INFO6, Help6),
    EV_BN_CLICKED(INFO7, Help7),
    EV_BN_CLICKED(INFO8, Help8),
    EV_BN_CLICKED(INFO9, Help9),
    EV_WM_CTLCOLOR,
  EV_BN_CLICKED(PRINT, Print),
//{{Water_n_dlgRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Water_n_dlg Implementation}}


Water_n_dlg::Water_n_dlg (TWindow* parent,
                          Parameters* Param_p,
                          Simul* Simul_p,
                          RainfallFile* rainfallF,
                          TResId resId,
                          TModule* module) :
    TDialog(parent, resId, module)
{
    // INSERT>> Your constructor code here.

    Param_ptr = Param_p;
    Simul_ptr = Simul_p;
    rainfallFile = rainfallF;
    In_fill_results = false;

   User_stored_moisture = new TEdit(this, IDC_EDIT1, 20);
	In_crop_rain = new TEdit(this, IDC_EDIT2, 20);
	Days_to_maturity = new TEdit(this, IDC_EDIT3, 20);
   Threshold_water = new TEdit(this, IDC_EDIT4, 20);
	Water_use_efficiency = new TEdit(this, IDC_EDIT5, 20);
	Protein = new TEdit(this, IDC_EDIT6, 20);
	User_NO3_supply = new TEdit(this, IDC_EDIT7, 20);

	NO3_Perc_tonne = new TComboBox(this, IDC_COMBOBOX1);

   Stored_moisture = new TStatic(this, IDC_STATIC1);
   Total_water1 = new TStatic(this, IDC_STATIC3);
   Total_water2 = new TStatic(this, IDC_STATIC4);
   Expected_yield = new TStatic(this, IDC_STATIC5);
   Howwet_NO3_supply = new TStatic(this, IDC_STATIC6);
	NO3_gap = new TStatic(this, IDC_STATIC7);
	Howwet_NO3_demand = new TStatic(this, IDC_STATIC8);
	Need_surplus = new TStatic(this, IDC_STATIC9);
   NO3_Perc_tonne_num = -1;

   }


Water_n_dlg::~Water_n_dlg ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}

// *******************************************************************
      void Water_n_dlg::Fill_results_boxes(void)  {
// *******************************************************************

//  Short description:
//    fill in the howwet no3 supply box.

//  Notes:

//  Changes:
//    DPH 13/4/96

//  Internal variables
   char St[50];

// -------------------- Executable code section ----------------------

   if (!In_fill_results)
      {
      In_fill_results = true;

      // calculate fallow moisture.

      float Fallow_moisture_storage;
      User_stored_moisture->GetText(St, sizeof(St));
      if (strlen(St) == 0)
         {
         Fallow_moisture_storage = Simul_ptr->Get_final_soil_water();
         sprintf (St, "%5.0f", Fallow_moisture_storage);
         Stored_moisture->SetText(St);
         }
      else
         {
         Fallow_moisture_storage = atof(St);
         Stored_moisture->SetText("");
         }

      // try and calculate in crop rainfall.

      float In_crop_rn = -1.0;
      In_crop_rain->GetText(St, sizeof(St));
      if (strlen(St) == 0)
         {
         Days_to_maturity->GetText(St, sizeof(St));
         if (strlen(St) > 0)
            {
            int Days_to_mat = atoi(St);

            // calculate incrop rainfall.

            GDate Start_date;
            Start_date = Param_ptr->Get_end_date();
            GDate End_date;
            End_date = Start_date;
            End_date += Days_to_mat;
            In_crop_rn = Param_ptr->Get_average_rain(Start_date, End_date);

            sprintf (St, "%5.0f", In_crop_rn);
            }
         else
            strcpy(St, "");
         }
      if (strlen(St) > 0)
         In_crop_rn = atof(St);

      In_crop_rain->SetText(St);

      // calculate total water supply.

      float Total_water_supply = -1.0;
      if (In_crop_rn >= 0)
         {
         Total_water_supply = Fallow_moisture_storage + In_crop_rn;
         sprintf (St, "%5.0f", Total_water_supply);
         }
      else
         strcpy(St, "");
      Total_water1->SetText(St);
      Total_water2->SetText(St);

      // look for a threshold water

      Threshold_water->GetText(St, sizeof(St));
      if (strlen(St) > 0 && Total_water_supply > 0)
         Total_water_supply -= atof(St);

      // calculate expected yield

      float Expected_yld = -1.0;
      Water_use_efficiency->GetText(St, sizeof(St));
      if (strlen(St) > 0 && Total_water_supply >= 0)
         {
         float WUE = atof(St);
         Expected_yld = Total_water_supply * WUE / 1000;
         sprintf (St, "%5.1f", Expected_yld);
         }
      else
         strcpy(St, "");
      Expected_yield->SetText(St);

      // calculate nitrate needed.

      float Prot = -1.0;
      float kgno3_perc_t = -1.0;
      Protein->GetText(St, sizeof(St));
      if (strlen(St) > 0)
         Prot = atof(St);

      if (NO3_Perc_tonne->GetSelIndex() >= 0)
         {
         String_array* List;
         Param_ptr->Get_kgno3_perc_tonne_list(&List);
         strcpy (St, (*List)[NO3_Perc_tonne->GetSelIndex()].c_str());
         kgno3_perc_t = atof(St);
         }

      // update nitrate demand field.

      float no3_demand = -1.0;
      if (Expected_yld >= 0 && Prot >= 0 && kgno3_perc_t >= 0)
         {
         no3_demand = Expected_yld * Prot * kgno3_perc_t;
         sprintf(St,
                 "%5.1f",
                 no3_demand);
         }
      else
         strcpy(St, "");
      Howwet_NO3_demand->SetText(St);

      // calculate no3 supply.

      float no3_supply;
      User_NO3_supply->GetText(St, sizeof(St));
      if (strlen(St) > 0)
         {
         no3_supply = atof(St);
         strcpy(St, "");
         }
      else
         {
         no3_supply = Simul_ptr->Get_final_nitrate();
         sprintf (St, "%5.1f", no3_supply);
         }
      Howwet_NO3_supply->SetText(St);

      // update nitrate gap field.

      if (no3_demand >= 0)
         {
         sprintf(St,
                  "%6.1f",
                  no3_supply - no3_demand);

         if (no3_supply - no3_demand >= 0)
            Need_surplus->SetText("Surplus");
         else
            Need_surplus->SetText("Needed");
         }
      else
         {
         strcpy(St, "");
         Need_surplus->SetText("");
         }

      NO3_gap->SetText(St);

      In_fill_results = false;
      }
   }

// *******************************************************************
      void Water_n_dlg::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


    TDialog::SetupWindow();

   // INSERT>> Your code here.

   // fill the kgno3/%/tonne list box.

   String_array* List;
   Param_ptr->Get_kgno3_perc_tonne_list(&List);
   for (int Index = 0;
        Index < List->GetItemsInContainer();
        Index++)
      {
      size_t Pos = (*List)[Index].find ("- ");
      if (Pos == NPOS)
         NO3_Perc_tonne->AddString((*List)[Index].c_str());
      else
         NO3_Perc_tonne->AddString((*List)[Index].substr(Pos + 2).c_str());
      }
   if (NO3_Perc_tonne_num >= 0)
      NO3_Perc_tonne->SetSelIndex(NO3_Perc_tonne_num);
   else
      NO3_Perc_tonne->SetSelIndex(0);

   Fill_results_boxes();

   // fill edit boxes.

	if (User_stored_moisture_st != "")
      User_stored_moisture->SetText(User_stored_moisture_st.c_str());

   if (In_crop_rain_st != "")
      In_crop_rain->SetText(In_crop_rain_st.c_str());

	if (Days_to_maturity_st != "")
	   Days_to_maturity->SetText(Days_to_maturity_st.c_str());

	if (Threshold_water_st != "")
   	Threshold_water->SetText(Threshold_water_st.c_str());

	if (Water_use_efficiency_st != "")
	   Water_use_efficiency->SetText(Water_use_efficiency_st.c_str());

	if (Protein_st != "")
   	Protein->SetText(Protein_st.c_str());

	if (User_NO3_supply_st != "")
   	User_NO3_supply->SetText(User_NO3_supply_st.c_str());
   }

void Water_n_dlg::Help1 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 1).Execute();

}


void Water_n_dlg::Help10 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 10).Execute();
}


void Water_n_dlg::Help11 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 11).Execute();
}


void Water_n_dlg::Help2 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 2).Execute();
}


void Water_n_dlg::Help3 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 3).Execute();
}


void Water_n_dlg::Help4 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 4).Execute();
}


void Water_n_dlg::Help5 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 5).Execute();
}


void Water_n_dlg::Help6 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 6).Execute();
}


void Water_n_dlg::Help7 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 7).Execute();
}


void Water_n_dlg::Help8 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 8).Execute();
}


void Water_n_dlg::Help9 ()
{
    // INSERT>> Your code here.

   So_what_dlg(this, 10, 9).Execute();
}

// ------------------------------------------------------------------
//  Short description:
//    The days to maturity edit box has changed.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
void Water_n_dlg::Days_to_maturity_change (void)
   {
   if (!In_fill_results)
      {
      In_fill_results = true;
      In_crop_rain->SetText("");
      In_fill_results = false;
      Fill_results_boxes();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    The in crop rainfall edit box has changed.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
void Water_n_dlg::In_crop_rain_change (void)
   {
   if (!In_fill_results)
      {
      In_fill_results = true;
      Days_to_maturity->SetText("");
      In_fill_results = false;

      uint Start_pos, End_pos;
      In_crop_rain->GetSelection(Start_pos, End_pos);
      Fill_results_boxes();
      In_crop_rain->SetSelection(Start_pos, End_pos);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Windows is letting us change the colour of a control.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
HBRUSH Water_n_dlg::EvCtlColor (HDC dc, HWND hWndChild, uint ctlType)
   {
   HBRUSH result;

   result = TDialog::EvCtlColor(dc, hWndChild, ctlType);

   // INSERT>> Your code here.

   // get the NO3_gap variable and if negative then make control red

   if (hWndChild == NO3_gap->HWindow)
      {
      char St[20];
      NO3_gap->GetText(St, sizeof St);
      if (atof(St) < 0)
         ::SetTextColor (dc, TColor(255, 0, 0));
      }

   return result;
   }


void Water_n_dlg::Print()
   {
   // INSERT>> Your code here.

   // create rectangles.

   Table_rect_item R1(4, 1, 70, 8);
   Table_rect_item R2(4, 8, 70, 15);
   Table_rect_item R3(4, 15, 70, 22);
   Table_rect_item R4(4, 22, 70, 29);
   Table_rect_item R5(4, 29, 70, 36);

   Text_table Our_table;            // our table.
   Our_table.Add_item(&R1);
   Our_table.Add_item(&R2);
   Our_table.Add_item(&R3);
   Our_table.Add_item(&R4);
   Our_table.Add_item(&R5);

   char St[150];
   char Word1[30];
   char Word2[30];
   char Word3[30];
   char Word4[30];
   char Word5[30];

   char rainTitle[200];
   getTitle(rainfallFile, rainTitle);

   // create header box.

   Table_text_item H1(5, 2, GString("Site name : "));
   Our_table.Add_item(&H1);
   Table_text_item H2(20, 2, GString(Param_ptr->Get_site_name()));
   Our_table.Add_item(&H2);

   Table_text_item H3(5, 3, GString("Rainfall name : "));
   Our_table.Add_item(&H3);
   Table_text_item H4(20, 3, (char*)&rainTitle[0]);
   Our_table.Add_item(&H4);

   Table_text_item H5(5, 4, GString("For period : "));
   Our_table.Add_item(&H5);

   Param_ptr->Get_start_date().Write(Word1);
   Param_ptr->Get_end_date().Write(Word2);
   sprintf(St, "%s to %s", Word1, Word2);

   Table_text_item H6(20, 4, GString(St));
   Our_table.Add_item(&H6);

   Table_text_item H7(5, 5, GString("Soil type : "));
   Our_table.Add_item(&H7);
   Table_text_item H8(20, 5, GString(Param_ptr->Get_soil_name()));
   Our_table.Add_item(&H8);

   Table_text_item H9(5, 6, GString("Cover : "));
   Our_table.Add_item(&H9);
   sprintf(St, "%4.0f%% to %4.0f%%",
      Param_ptr->Get_start_soil_cover(),
      Param_ptr->Get_end_soil_cover());

   Table_text_item H10(20, 6, GString(St));
   Our_table.Add_item(&H10);

   Table_text_item H11(5, 7, GString("Organic Carbon : "));
   Our_table.Add_item(&H11);
   sprintf (St, "%4.1f%%", Param_ptr->Get_calc_organic_carbon());
   Table_text_item H12(20, 7, GString(St));
   Our_table.Add_item(&H12);

   // create all text items.

   Table_text_item T1(5, 9, GString("1.  WATER SUPPLY"));
   Our_table.Add_item(&T1);

   Table_text_item T2(5, 11, GString("Stored water    +    In crop rain    =    Total water available"));
   Our_table.Add_item(&T2);

   Stored_moisture->GetText (Word2, sizeof Word2);
   if (strlen (Word2) == 0)
      {
      strcpy (Word1, "Your estimate");
      User_stored_moisture->GetText (Word2, sizeof Word2);
      }
   else
      strcpy (Word1, "Howwet estimate");

   In_crop_rain->GetText (Word3, sizeof Word3);
   Total_water1->GetText (Word4, sizeof Word4);
   sprintf (St, "%s %smm    +    %smm    =    %smm",
            Word1, Word2, Word3, Word4);
   Table_text_item T3(5, 13, GString(St));
   Our_table.Add_item(&T3);



   Table_text_item T4(5, 16, GString("2.  EXPECTED YIELD"));
   Our_table.Add_item(&T4);

   Table_text_item T5(5, 18, GString("Total water - Threshold = Effective water x WUE = Yield"));
   Our_table.Add_item(&T5);

   Total_water2->GetText (Word1, sizeof Word1);
   Threshold_water->GetText (Word2, sizeof Word2);
   sprintf (Word3, "%5.0f", atof(Word1) - atof(Word2));
   Water_use_efficiency->GetText(Word4, sizeof Word4);
   Expected_yield->GetText(Word5, sizeof Word5);
   float Yld = atof(Word5);
   Yld *= 1000;
   sprintf (St, "%smm  -  %smm  =  %smm  x  %skg/ha/mm  = %6.1fkg/ha",
            Word1, Word2, Word3, Word4, Yld);

   Table_text_item T6(5, 20, GString(St));
   Our_table.Add_item(&T6);



   Table_text_item T7(5, 23, GString("3.  NITROGEN NEEDED"));
   Our_table.Add_item(&T7);

   Table_text_item T8(5, 25, GString("Target yield x Target protein x Protein mult. = Nitrate needed"));
   Our_table.Add_item(&T8);

   Expected_yield->GetText(Word1, sizeof Word1);
   Protein->GetText(Word2, sizeof Word2);

   String_array* List;
   Param_ptr->Get_kgno3_perc_tonne_list(&List);
   strcpy (Word3, (*List)[NO3_Perc_tonne->GetSelIndex()].c_str());

   Howwet_NO3_demand->GetText(Word4, sizeof Word4);

   sprintf (St, "%6.1fkg/ha  x  %s%%  x  [%s]  =  %skg/ha",
            Yld, Word2, Word3, Word4);

   Table_text_item T9(5, 27, GString(St));
   Our_table.Add_item(&T9);



   Table_text_item T10(5, 30, GString("4.  NITRATE GAP"));
   Our_table.Add_item(&T10);

   Table_text_item T11(5, 32, GString("Nitrate available  -  Nitrate needed  =  Nitrate gap"));
   Our_table.Add_item(&T11);

   Howwet_NO3_supply->GetText(Word2, sizeof Word2);
   if (strlen(Word1) == 0)
      {
      strcpy (Word1, "Howwet estimate");
      User_NO3_supply->GetText(Word2, sizeof Word2);
      }
   else
      strcpy (Word1, "Your estimate");
   Howwet_NO3_demand->GetText(Word3, sizeof Word3);
   NO3_gap->GetText(Word4, sizeof Word4);
   float gap = atof (Word4);
   if (gap < 0)
      strcpy(Word5, "need more N");
   else if (gap > 0)
      strcpy(Word5, "excess N");
   else
      strcpy(Word5, "");

   sprintf (St, "%s %s  -  %s  =  %s kg/ha - %s",
            Word1, Word2, Word3, Word4, Word5);
   Table_text_item T12(5, 34, GString(St));
   Our_table.Add_item(&T12);

   GDate Current_date;
   Current_date.Set_to_current();
   char Date_str[30];
   Current_date.Write (Date_str);
   GString Current_date_st;
   Current_date_st = "Date printed : ";
   Current_date_st += Date_str;

   Table_text_item T13(5, 37, Current_date_st);
   Our_table.Add_item(&T13);

   // Send chart to printer.

   TPrinter Printer;

   Text_table_printout Printout("Howwet results table", &Our_table);
   Printer.Print(this, Printout, TRUE);

   }


bool Water_n_dlg::CanClose()
{
  bool result;

  result = TDialog::CanClose();

  // INSERT>> Your code here.
   if (result)
      {
      // fill edit boxes.
      char St[100];
      User_stored_moisture->GetText(St, sizeof St);
      User_stored_moisture_st = St;

      In_crop_rain->GetText(St, sizeof St);
      In_crop_rain_st = St;

      Days_to_maturity->GetText(St, sizeof St);
      Days_to_maturity_st = St;

      Threshold_water->GetText(St, sizeof St);
      Threshold_water_st = St;

      Water_use_efficiency->GetText(St, sizeof St);
      Water_use_efficiency_st = St;

      Protein->GetText(St, sizeof St);
      Protein_st = St;

      User_NO3_supply->GetText(St, sizeof St);
      User_NO3_supply_st = St;

      NO3_Perc_tonne_num = NO3_Perc_tonne->GetSelIndex();
      }

  return result;
}

