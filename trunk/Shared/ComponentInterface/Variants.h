 //---------------------------------------------------------------------------

#ifndef VariantsH
#define VariantsH
#include "Vector.h"
#include "Variant.h"
namespace protocol {
class TypeConverter;

#pragma warn -inl
// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates one or more variant variables as
//     returned by the returnValue message.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
class Variants
   {
   public:
      Variants(Component* parent, const Type& t)
         : registeredType(t), typeConverter(NULL)
         {
         }
      ~Variants(void)
         {
         delete typeConverter;
         empty();
         }
      unsigned int size(void) const {return variants.size();}
      void empty(void)
         {
         for (unsigned int i = 0; i < variants.size(); i++)
            delete variants[i];
         variants.empty();
         }
      void addVariant(const Variant& variant)
         {
         variants.push_back(new Variant(variant));
         variants[variants.size()-1]->setTypeConverter(typeConverter);
         }

      Variant* getVariant(unsigned int variantIndex)
         {
         if (variantIndex < variants.size())
            return variants[variantIndex];
         else
            return NULL;
         }
      void setTypeConverter(TypeConverter* typeConv)
         {
         typeConverter = typeConv;
         }

   private:
      const Type registeredType;
      vector<Variant*> variants;
      TypeConverter* typeConverter;

   };
#pragma warn .inl

} // namespace protocol
#endif
