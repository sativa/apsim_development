//---------------------------------------------------------------------------
#ifndef STL functionsH
#define STL functionsH

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
		T* Ptr = container.back();
      container.pop_back();
		delete Ptr;
		}
   }

#endif
