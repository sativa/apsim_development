#ifndef VectorH
#define VectorH
#include "MessageData.h"
namespace protocol {
class Component;

void tooManyError(unsigned int maxCount);
void rangeError(unsigned int index, unsigned int maxCount);

// ------------------------------------------------------------------
//  Short description:
//     This template class is a vector container that mimics the
//     STL vector.  We can't use the STL vector because when
//     statically linking to FORTRAN the STL isn't found.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
template <class T>
class vector
   {
   public:
      vector(unsigned maxItems = 50)
         : maxCount(maxItems), count(0), weOwnArray(true)
         {
         array = new T[maxItems];
         }
      vector(T* ptr, unsigned numValues, unsigned maxItems)
         : maxCount(maxItems), count(numValues), weOwnArray(false)
         {
         array = ptr;
         }
      vector(T* ptr1, T* ptr2) : weOwnArray(false)
         {
         array = ptr1;
         count = maxCount = (unsigned) (ptr2 - ptr1);
         if (count <= 0) {tooManyError(count);}
         }
      virtual ~vector(void)
         {
         if (weOwnArray)
            delete [] array;
         }
      void empty(void)
         {
         count = 0;
         }

      void push_back(T data)
         {
         count++;
         if (count <= maxCount)
            array[count-1] = data;
         else
            tooManyError(maxCount);
         }

      unsigned int size(void) const {return count;}
      T& operator[](unsigned int index)
         {
         if (index < count)
            return array[index];
         else
            {
            rangeError(index, maxCount);
            return array[0];
            }
         }
      const T operator[](unsigned int index) const
         {
         if (index < count)
            return array[index];
         else
            {
            rangeError(index, maxCount);
            return array[0];
            }
         }
      void erase(unsigned int index)
         {
         if (index < count)
            {
            if (index == count-1)
               count--;
            }
         else
            rangeError(index, maxCount);
         }


   private:
      unsigned int count;
      unsigned int maxCount;
      bool weOwnArray;
      T* array;

   };
#pragma warn -inl
// VECTOR type
template <class T>
inline MessageData& operator<<(MessageData& messageData, const vector<T>& values)
   {
   messageData << (int)values.size();
   for (unsigned int i = 0; i < values.size(); i++)
      messageData << values[i];
   return messageData;
   }
template <class T>
inline MessageData& operator>>(MessageData& messageData, vector<T>& values)
   {
   unsigned int numValues;
   messageData >> (int) numValues;
   values.empty();
   for (unsigned int i = 0; i < numValues; i++)
      {
      T value;
      messageData >> value;
      values.push_back(value);
      }

   return messageData;
   };
template <class T>
inline unsigned int memorySize(const vector<T>& values)
   {
   unsigned size = 4;
   if (values.size() > 0)
      size += values.size() * memorySize(values[0]);
   return size;
   }
#pragma warn .inl

} // namespace protocol

#endif
