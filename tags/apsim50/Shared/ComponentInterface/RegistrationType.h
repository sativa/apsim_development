//---------------------------------------------------------------------------
#ifndef RegistrationTypeH
#define RegistrationTypeH

#include <string>
//---------------------------------------------------------------------------
// Encapsulates a registration type.
//---------------------------------------------------------------------------
class __declspec(dllexport) RegistrationType
   {
   public:
      enum Type {get=1,         respondToGet=2,
                 set=8,         respondToSet=3,
                 event=5,       respondToEvent=6,
                                respondToGetSet=4,
                                invalid=100};

      RegistrationType(void) : regType(invalid) {}
      RegistrationType(Type t) : regType(t) { }
      RegistrationType(const std::string& st);

      std::string asString();
      RegistrationType opposite();
      RegistrationType::Type type() const {return regType;}
      bool isPassive();

      bool operator ==(const RegistrationType& rhs)
         {return rhs.regType == regType;}
      bool operator !=(const RegistrationType& rhs)
         {return rhs.regType != regType;}
   private:
      Type regType;
   };

#endif