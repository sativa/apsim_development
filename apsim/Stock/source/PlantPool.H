
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef PlantPool_H
#define PlantPool_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

class PlantPool
{
   friend ostream &operator<<(ostream &, const PlantPool &);
	public:												// member functions
		PlantPool();			// default constructor
            PlantPool(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		PlantPool(const PlantPool &PlantPool); 						// copy constructor
		const PlantPool &operator=(const PlantPool &other);		// Assigment operator

            PlantPool operator* (const PlantPool &y) const;  // Command
            PlantPool operator* (const float &y) const;  // Command
            PlantPool operator/ (const PlantPool &y) const;  // Command
            PlantPool operator/ (const float &y) const;  // Command
            PlantPool multiply (const PlantPool &y);  // Command
            PlantPool divide (PlantPool &dividend, PlantPool &divisor, float default_value);  // Command
            float divide (float dividend, float divisor, float default_value) const;  // Command
            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		float total() const;  	// Query

		virtual void display(ostream &os = cout) const;	// display function

#if TEST_PlantPool
		virtual ~PlantPool();							// destructor
#else
		virtual ~PlantPool();
#endif

      struct PlantPartType
         {
         float leaf;
         float stem;
         };

         PlantPartType green;
         PlantPartType senesced;
         PlantPartType dead;
	private:
};

#endif
