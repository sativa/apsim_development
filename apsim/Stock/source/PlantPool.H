
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

class PlantPartType
{
   friend ostream &operator<<(ostream &, const PlantPartType &);
   public:
      PlantPartType();
      PlantPartType(float leafInit, float stemInit);
	PlantPartType(const PlantPartType &PlantPartType); 						// copy constructor
	const PlantPartType &operator=(const PlantPartType &other);		// Assigment operator

      PlantPartType operator- (const PlantPartType &y) const;  // Command
      PlantPartType operator+ (const PlantPartType &y) const;  // Command
      virtual ~PlantPartType();							// destructor

      float leaf;
      float stem;

   private:
};

class PlantPool
{
   friend ostream &operator<<(ostream &, const PlantPool &);
	public:												// member functions
		PlantPool();			// default constructor
            PlantPool(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
		PlantPool(const PlantPool &PlantPool); 						// copy constructor
		const PlantPool &operator=(const PlantPool &other);		// Assigment operator

            PlantPool operator- (const PlantPool &y) const;  // Command
            PlantPool operator+ (const PlantPool &y) const;  // Command
            PlantPool operator* (const PlantPool &y) const;  // Command
            PlantPool operator* (const float &y) const;  // Command
            PlantPool operator/ (const PlantPool &y) const;  // Command
            PlantPool operator/ (const float &y) const;  // Command
            PlantPool multiply (const PlantPool &y);  // Command
            PlantPool divide (PlantPool &dividend, PlantPool &divisor, float default_value);  // Command
            float divide (float dividend, float divisor, float default_value) const;  // Command
            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
            void setValue(PlantPartType green, PlantPartType senesced, PlantPartType dead);
		float total() const;  	// Query

		virtual void display(ostream &os = cout) const;	// display function

		virtual ~PlantPool();							// destructor


         PlantPartType green;
         PlantPartType senesced;
         PlantPartType dead;
	private:
};

#endif
