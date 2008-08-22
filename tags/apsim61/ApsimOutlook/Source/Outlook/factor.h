//---------------------------------------------------------------------------
#ifndef FactorH
#define FactorH

#include <vector>
//---------------------------------------------------------------------------
// The factor class captures the idea of a 'factor' as used in APSIM Outlook.
// It contains a factor name, and a value applicable to that factor.
//---------------------------------------------------------------------------
class Factor
   {
   public:
      Factor(const std::string& nm, const std::string& val)
      : name(nm), value(val)
         { }
      Factor(const std::string& state)
         {
         setState(state);
         }

      // equality operator
      bool operator== (const std::string rhsName) const
         { return (strcmpi(name.c_str(), rhsName.c_str()) == 0); }

      // getters:
      std::string getName(void)  const { return name;  }
      std::string getValue(void) const { return value; }

      // setters:
      void setName(const std::string& n)  {name  = n;}
      void setValue(const std::string& v) {value = v;}

      // return the factor as a state string.
      std::string getState(void) const;

   protected:
      std::string name;
      std::string value;

      void setState(const std::string& state);

};

//---------------------------------------------------------------------------
#endif
