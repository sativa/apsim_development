#pragma once

__gc class RegisteredEventHandlers
	{
	public:
		static unsigned add(IEvent* event) 
			{
			if (numEvents == MAX_NUM_EVENTS)
				throw new Exception("Too many event handlers have been registered");
			events[numEvents] = event;
			numEvents++;
			return numEvents-1;
			}
		static IEvent* at(unsigned index)
			{
			if (index >= numEvents)
				throw new Exception("Invalid event registeration id");
			return events[index];
			}
	
	private:
		static const unsigned MAX_NUM_EVENTS = 50;
		static IEvent __pin * events[] = new IEvent* [MAX_NUM_EVENTS];
		static unsigned numEvents = 0;
			
	};