//---------------------------------------------------------------------------

#ifndef FactorH
#define FactorH

#include <graphics.hpp>
#include <general\string_functions.h>
#include <vector>
class AddInBase;

//---------------------------------------------------------------------------
// The factor class captures the idea of a 'factor' as used in APSIM Outlook.
// It contains a factor name, a corresponding image and a value
// applicable to that factor.  It also keeps track of the name of the
// add-in that it belongs to.
//
// Changes: DAH prototype 11/12/00
//          DAH 23/1/01
//          DPH slightly reworked to move getValues to addin

class Factor {

   public:
      Factor(Graphics::TBitmap* img,
             std::string nm,
             std::string val,
             AddInBase* addin)
      : image(img), name(nm), value(val), addIn(addin)
      {
      }

      // equality operator
      bool operator== (const std::string rhsName) const
         { return Str_i_Eq(name, rhsName); }

      // getters:
      const Graphics::TBitmap* getImage(void) const { return image; }
      std::string              getName(void)  const { return name;  }
      std::string              getValue(void) const { return value; }

      // setters:
      void setImage(const Graphics::TBitmap* i) {image = i;}
      void setName(std::string& n)              {name  = n;}
      void setValue(std::string& v)             {value = v;}
      
      // Set a factor value.
      void setValue(const std::string& val)   { value = val; }

      // get a pointer to add-in
      const AddInBase* getAddIn(void)  {return addIn;}
      void       setAddIn(const AddInBase* addin) {addIn = addin;}

   protected:
      std::string name;
      std::string value;
      const AddInBase* addIn;
      const Graphics::TBitmap* image;
      std::vector<std::string> values;
};

//---------------------------------------------------------------------------
#endif
