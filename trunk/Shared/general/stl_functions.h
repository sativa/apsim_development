//---------------------------------------------------------------------------
#ifndef STL functionsH
#define STL functionsH

#include <functional>
#include <general\string_functions.h>
// ------------------------------------------------------------------
//  Short description:
//    locates an object in a container and returns the numerical
//    location within that container.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class InputIterator, class T>
int location_number (InputIterator first, InputIterator last, const T& value)
   {
   int index = 0;
   while (first != last && value != *first)
      {
      ++first;
      index++;
      }
   if (first == last)
      return -1;
   else
      return index;
   }


// ------------------------------------------------------------------
//  Short description:
//    Find an item in stl container.  This function is a replacement for the
//    find function in algorithm.h.  The released function cannot handle
//    containers of pointers properly.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class InputIterator, class T>
InputIterator Pfind (InputIterator first, InputIterator last, const T& value)
{
    while (first != last && *(*first) != *value)
        ++first;
    return first;
}

// ------------------------------------------------------------------
//  Short description:
//    Destroys all elements in a container.  Works with
//    containers of pointers.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT, class T>
void Destroy_pointers (CT& container, const T )
   {
   while (!container.empty())
      {
		T Ptr = container.back();
      container.pop_back();
		delete Ptr;
		}
   }

template <class CT, class T>
class string2double_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      string2double_and_store (CT& c) : container(c) {}
      void operator() (T& arg)
         {container.push_back (atof(arg.c_str()));}
   };

template <class CT, class T>
class double2string_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
      int NumDecPlaces;
   public:
      double2string_and_store (CT& c, int numdecplaces)
         : container(c), NumDecPlaces(numdecplaces) {}
      void operator() (T& arg)
         {container.push_back (ftoa(arg, NumDecPlaces));}
   };

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void String2double (CT1& source, CT2& dest)
   {
   dest.erase(dest.begin(), dest.end());
   string2double_and_store<CT2, std::string> convert(dest);
   std::for_each(source.begin(), source.end(), convert);
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a container of doubles to a container of strings with the
//    specified number of decimal places.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void Double2string (CT1& source, CT2& dest, int NumDecPlaces = 5)
   {
   dest.erase(dest.begin(), dest.end());
   double2string_and_store<CT2, double> convert(dest, NumDecPlaces);
   std::for_each(source.begin(), source.end(), convert);
   }


template <class CT, class T>
class Add_if_unique : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      Add_if_unique (CT& c) : container(c) {}
      void operator() (T& arg)
         {
         if (std::find(container.begin(), container.end(), arg) == container.end())
            container.push_back (arg);
         }
   };

template <class CT, class T>
class Get_name_and_store_function
   {
   private:
      CT& Container;
   public:
      Get_name_and_store_function(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.Get_name());
         };
   };

template <class CT, class T>
class Get_filename_and_store_function
   {
   private:
      CT& Container;
   public:
      Get_filename_and_store_function(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.Get_filename());
         };
   };

template <class CT, class T>
class GetNameAndStoreFunction
   {
   private:
      CT& Container;
   public:
      GetNameAndStoreFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.GetName());
         };
   };
template <class CT, class T>
class PGetNameFunction
   {
   private:
      CT& Container;
   public:
      PGetNameFunction(CT& container)
         : Container (container)
         { }

      void operator () (T* arg)
         {
         Container.push_back (arg->getName());
         };
   };

template <class CT, class T>
class GetNameFunction
   {
   private:
      CT& Container;
   public:
      GetNameFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.getName());
         };
   };

template <class CT, class T>
class GetFilenameAndStoreFunction
   {
   private:
      CT& Container;
   public:
      GetFilenameAndStoreFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.GetFilename());
         };
   };

template <class T>
class Find_by_name_predicate
   {
   private:
      std::string Name;
   public:
      Find_by_name_predicate(const char* name)
         : Name(name)
         { }

      bool operator () (T& arg)
         {
         return (strcmpi(arg.Get_name().c_str(), Name.c_str()) == 0);
         };
   };

template <class T>
class Find_by_filename_predicate
   {
   private:
      std::string File_name;
   public:
      Find_by_filename_predicate(const char* file_name)
         : File_name(file_name)
         { }

      bool operator () (T& arg)
         {
         return (strcmpi(arg.Get_filename().c_str(), File_name.c_str()) == 0);
         };
   };

template <class T>
class CallbackFunction
   {
   public:
      virtual ~CallbackFunction(void) { };
      virtual void callback(T x) = 0;
   };
template <class T>
class ConstCallbackFunction
   {
   public:
      virtual ~ConstCallbackFunction(void) { };
      virtual void callback(T x) const = 0;
   };

template <class CT, class T>
class GetNameCallback : public CallbackFunction<T>
   {
   public:
      CT& C;
      GetNameCallback(CT& c) : C(c) { }

      virtual void callback(T t) {C.push_back(t.getName());}
   };
template <class CT, class T>
class PGetNameCallback : public CallbackFunction<T*>
   {
   public:
      CT& C;
      PGetNameCallback(CT& c) : C(c) { }

      virtual void callback(T* t) {C.push_back(t->getName());}
   };

template <class T>
class PEqualToName
   {
   private:
      std::string name;
   public:
      PEqualToName(const std::string& n)
         : name(n) {}

      bool operator () (T* arg)
         {return (stricmp(arg->getName().c_str(), name.c_str()) == 0);};
   };
template <class T>
class EqualToName
   {
   private:
      std::string name;
   public:
      EqualToName(const std::string& n)
         : name(n) {}

      bool operator () (T& arg)
         {return (stricmp(arg.getName().c_str(), name.c_str()) == 0);};
   };


#endif
