//---------------------------------------------------------------------------

#ifndef CMPDataH
#define CMPDataH
//---------------------------------------------------------------------------
#include <string>
#include <vector>
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/Interfaces.h>
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/TypeConverter.h>
#include <boost/function.hpp>

void getKindAndArray(const std::string& ddml,
                     std::string& kind, bool& isArray);

// -------------------------------------------------------------------
// A wrapper class for passing builtin types via the CMP
// -------------------------------------------------------------------
template <class T>
class CMPBuiltIn : public IPackableData
   {
   private:
      T& variable;
   public:
      CMPBuiltIn(T& value) : variable(value) {ddml = DDML(value);}

      virtual void setValue(const std::vector<std::string>& values)
         {
         TypeConverter(values, variable, NULL);
         }
      virtual unsigned memorySize()
         {return ::memorySize(variable);}
      virtual void pack(MessageData& messageData)
         {::pack(messageData, variable);}
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {
         bool sourceIsArray;
         string sourceKind;
         getKindAndArray(sourceDDML, sourceKind, sourceIsArray);
         if (sourceKind == "")
            ::unpack(messageData, variable);  // must be a structure - no conversion.
         else
            {
            if (sourceIsArray)
               {
               if (sourceKind == "integer4")
                  {
                  std::vector<int> value;
                  if (typeid(variable) == typeid(std::vector<int>) && arraySpecifier == NULL)
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     TypeConverter(value, variable, arraySpecifier);
                     }
                  }
               else if (sourceKind == "single")
                  {
                  std::vector<float> value;
                  if (typeid(variable) == typeid(std::vector<float>) && arraySpecifier == NULL)
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     TypeConverter(value, variable, arraySpecifier);
                     }
                  }
               else if (sourceKind == "double")
                  {
                  std::vector<double> value;
                  if (typeid(variable) == typeid(std::vector<double>) && arraySpecifier == NULL)
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     TypeConverter(value, variable, arraySpecifier);
                     }
                  }
               else if (sourceKind == "string")
                  {
                  std::vector<std::string> value;
                  if (typeid(variable) == typeid(std::vector<std::string>) && arraySpecifier == NULL)
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     TypeConverter(value, variable, arraySpecifier);
                     }
                  }
               else
                  throw runtime_error("Bad kind found in ddml: " + sourceDDML);
               }
            else
               {
               if (arraySpecifier)
                  throw runtime_error("Cannot use an array specifier (e.g. sw(2-6)) on scalar data");

               if (sourceKind == "integer4")
                  {
                  if (typeid(variable) == typeid(int))
                     ::unpack(messageData, variable);
                  else
                     {
                     int value;
                     ::unpack(messageData, value);
                     TypeConverter(value, variable);
                     }
                  }
               else if (sourceKind == "single")
                  {
                  if (typeid(variable) == typeid(float))
                     ::unpack(messageData, variable);
                  else
                     {
                     float value;
                     ::unpack(messageData, value);
                     TypeConverter(value, variable);
                     }
                  }
               else if (sourceKind == "double")
                  {
                  if (typeid(variable) == typeid(double))
                     ::unpack(messageData, variable);
                  else
                     {
                     double value;
                     ::unpack(messageData, value);
                     TypeConverter(value, variable);
                     }
                  }
               else if (sourceKind == "string")
                  {
                  if (typeid(variable) == typeid(std::string))
                     ::unpack(messageData, variable);
                  else
                     {
                     std::string value;
                     ::unpack(messageData, value);
                     TypeConverter(value, variable);
                     }
                  }
               else
                  throw runtime_error("Bad kind found in ddml: " + sourceDDML);
               }
            }
         }

   };

// -------------------------------------------------------------------
// bounding function templates + a bounded version of CMPBuiltIn
// -------------------------------------------------------------------
template <class T>
void performBoundCheck(const std::string& name, const std::vector<T>& values, T lower, T upper)
   {
   for (unsigned i = 0; i != values.size(); i++)
      performBoundCheck(name, values[i], lower, upper);
   }

template <class T>
void performBoundCheck(const std::string& name, T value, T lower, T upper)
   {
   if (value < lower || value > upper)
      printf("Value of variable is out of bounds.\n"
             "Variable: %s\n"
             "Value: %16.7f\n"
             "Bounds: %16.7f to %16.7f", /* WRONG WRONG WRONG THIS IS REALLY REALLY WRONG */
             name.c_str(), value, lower, upper);
   }

template <class T, class B>
class CMPBuiltInBounded : public CMPBuiltIn<T>
   {
   private:
      T& variable;
      B lowerBound;
      B upperBound;
      std::string name;
   public:
      CMPBuiltInBounded(const std::string& variableName,  T& value, B lower, B upper)
         : name(variableName), variable(value), lowerBound(lower), upperBound(upper),
           CMPBuiltIn<T>(value) { }

      virtual void setValue(const std::vector<std::string>& values)
         {
         CMPBuiltIn<T>::setValue(values);
         performBoundCheck(name, variable, lowerBound, upperBound);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {
         CMPBuiltIn<T>::unpack(messageData, sourceDDML, arraySpecifier);
         performBoundCheck(name, variable, lowerBound, upperBound);
         }
   };

// -------------------------------------------------------------------
// A wrapper class for passing non builtin types via the CMP
// -------------------------------------------------------------------
template <class T>
class CMPType : public IPackableData
   {
   private:
      T& variable;
   public:
      CMPType(T& value) : variable(value) {ddml = DDML(value);}

      virtual void setValue(const std::vector<std::string>& values)
         {
         throw runtime_error("Cannot set the value of a type");
         }
      virtual unsigned memorySize()
         {return ::memorySize(variable);}
      virtual void pack(MessageData& messageData)
         {::pack(messageData, variable);}
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {::unpack(messageData, variable);}

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class CMPMethod1 : public IPackableData
   {
   private:
      FT getter;
      FT setter;
      T dummy;
   public:
      CMPMethod1(FT& fn, bool isGetter)
         {
         if (isGetter)
            getter = fn;
         else
            setter = fn;
         ddml = DDML(dummy);
         }
      CMPMethod1(FT& getFn, FT& setFn) : getter(getFn), setter(setFn)
         {ddml = DDML(dummy);}

      virtual void setValue(const std::vector<std::string>& values)
         {
         throw runtime_error("Cannot call setValue on a function");
         }

      virtual unsigned memorySize()
         {
         if (getter.empty())
            throw runtime_error("Cannot call pack on a setter function");
         else
            {
            getter(dummy);
            return ::memorySize(dummy);
            }
         }
      virtual void pack(MessageData& messageData)
         {
         ::pack(messageData, dummy);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {
         if (setter.empty())
            throw runtime_error("Cannot call unpack on a getter function");
         else
            {
            ::unpack(messageData, dummy);
            setter(dummy);
            }
         }

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events that take no data ie. null events.
// -------------------------------------------------------------------
class CMPMethod0 : public IPackableData
   {
   private:
      boost::function0<void> fn;
      Null null;
   public:
      CMPMethod0(boost::function0<void>& fn)
         {
         this->fn = fn;
         ddml = DDML(null);
         }
      virtual void setValue(const std::vector<std::string>& values)
         {
         throw runtime_error("Cannot call setValue on a Null method");
         }

      virtual unsigned memorySize()
         {
         return ::memorySize(null);
         }
      virtual void pack(MessageData& messageData)
         {
         throw runtime_error("Cannot call pack on a Null method");
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML,
                          ArraySpecifier* arraySpecifier)
         {
         fn();
         }
      virtual void invoke()
         {
         fn();
         }

   };

#endif
