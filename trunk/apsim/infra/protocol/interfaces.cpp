#include "interfaces.h"

bool componentResponded;
void ICoordinator::setComponentResponded(bool responded) {componentResponded = responded;}
bool ICoordinator::getComponentResponded(void) {return componentResponded;}
