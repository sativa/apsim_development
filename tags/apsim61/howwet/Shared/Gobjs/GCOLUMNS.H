#if !defined(__GCOLUMNS_H)
#define __GCOLUMNS_H

#include <gobjs\gcolumn.h>
#include <assert.h>
#include <classlib\arrays.h>

class GColumns_array_dummy;


// *******************************************************************
       class GOBJS_CLASS_EXPORT GColumns : public TStreamableBase {
// *******************************************************************

//  Short description:
//      Object for storing a series of columns of data

//  Notes:
//    When this object is streamed, the actual data is only written
//    when the "write_all_data" flag is set.  

//  Changes:
//      DPH 5/2/96

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      GColumn * *Column_list;          // list of all columns.
      int Num_columns;                 // number of columns in array
      int Max_columns;                 // maximum number of columns
      bool Own_all_data;               // do we own all data? default = TRUE
      char Title[MAX_TITLE_SIZE];      // title of gcolumns.
      float Minimum;                   // minimum value in all columns.
      float Maximum;                   // maximum value in all columns.
      bool Write_all_data;             // write all data ?

      // data pointers.

      int *Column_number_list;         // column numbers of each column
      int *Table_number_list;          // tables numbers of each column

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GColumns(void)
         {
         Own_all_data = TRUE;
         Max_columns = 0;
         Num_columns = 0;
         };                            // constructor
      virtual ~GColumns(void);         // destructor

      virtual void Init(int Max_items);
                                       // initialise object
      
      int operator==(const GColumns& )
         {return TRUE;}                // equality operator

      GColumn* operator []( int loc )
         {
         assert (loc < Num_columns);
         Num_columns = max(Num_columns, loc);
         return Column_list[loc];
         }
      void Flush(void);                // flush all items in columns.
      void Delete_all_columns(void);   // delete all columns in array.
      int Get_num_columns (void)
         {
         return Num_columns;
         };                            // return number of elements in array.
      const char *Get_title(void)
         {
         return Title;
         };                            // title of columns.
      void Add(GColumn* Col, int Column_number = -1, int Table_number = -1)
         {
         assert (Num_columns < Max_columns);
         assert (Col != NULL);
         AddAt(Col, Num_columns, Column_number, Table_number);
         Num_columns++;
         };                            // add a column into object.  Use the 2 data
                                       // pointers if you are going to call
                                       // refresh_data.
      void AddAt(GColumn* Col, int loc, int Table_number = -1, int Column_number = -1);
                                       // add a column at a specified point.

      void Set_own_all_data (bool Own_data = TRUE)
         {
         Own_all_data = Own_data;
         };                            // set the own_all_data field.
      void Set_title (const char *Tit)
         {
         strncpy(Title, Tit, MAX_TITLE_SIZE);
         }
      float Get_minimum (void)
         {
         return Minimum;
         };                            // return minimum value
      float Get_maximum (void)
         {
         return Maximum;
         };                            // return maximum value
      bool Get_write_all_data(void)
         {
         return Write_all_data;
         };                            // return the write all data flag.
      void Set_write_all_data(bool Write)
         {
         Write_all_data = Write;
         };                            // set the write all data flag.
      void Refresh_data (GColumns_array_dummy& Arr_dummy);
                                       // refresh all data columns from the
                                       // data passed in.

      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GColumns, 1);
   };

typedef TIArrayAsVector <GColumns> GColumns_array;

class GColumns_array_dummy
   {
   public:
      GColumns_array* Arr_ptr;
   };


#endif