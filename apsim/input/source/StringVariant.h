//---------------------------------------------------------------------------

#ifndef StringVariantH
#define StringVariantH
#include <string>
#include <vector>
namespace protocol {
   class QueryValueData;
   class QuerySetValueData;
   class Component;
   }
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a single string based variant variable
//     The variant has a name, type, and value(s).  When more
//     than one value is present, the values are treated
//     as an array.
//
//     Array indexing is zero based.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
class StringVariant
   {
   public:
      StringVariant(void) : parent(NULL) { }
      StringVariant(protocol::Component* p) : parent(p) { }
      StringVariant(protocol::Component* parent, const std::string& name);
      StringVariant(protocol::Component* parent,
                    const std::string& name,
                    const std::string& units,
                    const std::string& value);
      void addValue(const std::string& value, unsigned arrayIndex);
      void addValues(const std::string& valueString);
      unsigned numValues(void) {return values.size();}

      void sendVariable(protocol::QueryValueData& queryData);
      void setVariable(protocol::QuerySetValueData& setValueData);
      bool asInteger(int& value);
      bool asLogical(bool& value);
      bool asFloat(float& value);
      unsigned doRegistration(void);

      bool operator ==(const std::string& n)
         {return (name == n);}
      bool operator !=(const std::string& n)
         {return (name != n);}
      std::string getName(void) const {return name;}

   private:
      protocol::Component* parent;
      std::string name;
      std::string units;
      std::string typeString;
      typedef std::vector<std::string> Values;
      Values values;
      unsigned int regID;

      enum Type {Real, Integer, String, RealArray, IntegerArray, StringArray};
      Type type;

      void determineType(void);
   };
#endif
