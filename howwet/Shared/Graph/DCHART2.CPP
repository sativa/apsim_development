#include <graph\global.h>
#pragma hdrstop

#include <graph\dseries.h>
#include <graph\daxis.h>
#include <gobjs\gdatecol.h>
#include <gobjs\grealcol.h>

// *******************************************************************
       void Replace_macros
                  (GString& Macro,
                   const char *X_heading,
                   const char *X_unit,
                   const char *Y_heading,
                   const char *Y_unit,
                   const char *Title)  {
// *******************************************************************

//  Short description:
//    Replace any '%' macros from Macro string with one of the other
//    strings.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString St;

// -------------------- Executable code section ----------------------

   Macro.Replace("%xu", X_unit);
   Macro.Replace("%yu", Y_unit);
   Macro.Replace("%col", X_heading);

   St = X_heading;
   size_t Pos = St.find(";");
   if (Pos != NPOS)
      St.remove(Pos);
   Macro.Replace("%x", St);

   St = Y_heading;
   Pos = St.find(";");
   if (Pos != NPOS)
      St.remove(Pos);
   Macro.Replace("%y", St);

   Macro.Replace("%tit", Title);
   }

// *******************************************************************
       void Check_axis_scale_object(DAxis *Axis_ptr,
                                            GColumn& Data)  {
// *******************************************************************

//  Short description:
//    Check to make sure the axis is setup for this data.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Label;                   // Label from label scale object

// -------------------- Executable code section ----------------------

   // Determine if this column of data is a numeric one.

   bool Is_date = (TYPESAFE_DOWNCAST(&Data, GDate_column) != NULL);
   bool Is_numeric = (TYPESAFE_DOWNCAST(&Data, GReal_column) != NULL);

   // Determine whether we need to create a scale object for this
   // axis or not.

   if (!Axis_ptr->Has_axis_scale())
      {
      if (Is_numeric)
         Axis_ptr->Set_axis_scale(new GMin_max_scale);
      else if (Is_date)
         Axis_ptr->Set_axis_scale(new GDate_scale);
      else
         Axis_ptr->Set_axis_scale(new GLabel_scale);
      }

   // If we're dealing with a label scale for this axis, then we
   // need to make sure our data exists in label scale.

   GLabel_scale *Label_scale_ptr = TYPESAFE_DOWNCAST
                        (&(Axis_ptr->Axis_scale()), GLabel_scale);

   if (Label_scale_ptr != NULL)
      {
      // Yes we have a label scale.

      bool Add_labels = FALSE;
      if (Label_scale_ptr->Get_num_ticks() == 0)
         Add_labels = TRUE;

      if (Add_labels)
         {

         // Add all data strings into label scale.

         for (int Label_number = 0;
              Label_number < Data.Get_num_elements();
              Label_number++)
            {
            Label_scale_ptr->Add_label(GString(Data.Get_char(Label_number)));
            }
         }
      }
   }

// *******************************************************************
       void Conv_stacked_data_to_absolute
         (GColumns& Data)  {
// *******************************************************************

//  Short description:
//    Convert the data array passed in into absolute numbers by adding all
//    columns to previous column (ignoring columns 0 and 1)

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   for (int Column_index = 2;
        Column_index < Data.Get_num_columns();
        Column_index++)
      {
      GColumn& Col = *Data[Column_index];
      GColumn& Previous_col = *Data[Column_index - 1];

      for (int Data_index = 0;
           Data_index < Col.Get_num_elements();
           Data_index++)
         {
         Col.AddAt_real(Data_index, Col.Get_real(Data_index) +
                                    Previous_col.Get_real(Data_index));
         }
      }
   }

