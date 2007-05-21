#pragma once

#include "IComponent.h"
#include "ApsimComponent.h"

// ---------------------------------------------
// This class is a base class for all components
// ---------------------------------------------
class Component : public IComponent
	{
	public:
		Component(const std::string& dllFileName);
		~Component(void);

	protected:
		ApsimComponent* component;
	};

