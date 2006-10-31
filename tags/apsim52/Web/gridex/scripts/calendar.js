///////////////////////////////////////////////////////////
// JavaScript API (1.5.1011.0)
// Copyright by Janus Systems S.A.
// 1998 - 2005
//////////////////////////////////////////////////////////
function getCalendarCombo(id)
{
	var calendar =  null;
	calendar = eval("calendar = " + id + ";"); 
	if(calendar == null)
		throw new Error("invalid calendar combo with id '" + id + "'");
		
	return calendar;  
}
function clickMonthCaption(td, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.ShowScrollableMonths(td); 
}
function onClickDay(td, id, month)
{
	var calendar = getCalendarCombo(id);
	calendar.onClickDay(td, month); 
}
function onBlurDateInput(id)
{
	var calendar = getCalendarCombo(id);
	calendar.onBlurDateInput(); 
}
function onClickDateInput(span, input, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onClickDateInput(span, input); 
}
function onChangeDateInput(id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onChangeDateInput(); 
}
function onKeyDownDateInput(id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onKeyDownDateInput(); 
}
function onClickToday(button, id, postback)
{
	var calendar = getCalendarCombo(id); 
	calendar.onClickToday(button, postback); 
}
function onClickNone(button, id, postback)
{
	var calendar = getCalendarCombo(id); 
	calendar.onClickNone(button, postback); 
}
function onClickMonth(tr, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onClickMonth(tr); 
}
function onMouseOverToday(button, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOverToday(button); 
}
function onMouseOutToday(button, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOutToday(button); 
}
function onMouseOverNone(button, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOverNone(button); 
}
function onMouseOutNone(button, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOutNone(button); 
}
function onMouseOverMonth(tr, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOverMonth(tr); 
}
function onMouseOutMonth(tr, id)
{	
	var calendar = getCalendarCombo(id);
	calendar.onMouseOutMonth(tr); 
}
function onSelectStartMonths()
{
	window.event.cancelBubble = true;
	window.event.returnValue = false; 
	return false; 
}
function showPreviousMonth(td, id)
{
	var calendar = getCalendarCombo(id); 	
	calendar.ShowPreviousMonth(); 
}
function showNextMonth(td, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.ShowNextMonth(); 
}
function onMouseOverDropDown(td, id)
{
	var calendar = getCalendarCombo(id);
	calendar.onMouseOverDropDown(td); 
}
function onMouseOutDropDown(td, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.onMouseOutDropDown(td); 
}	
function showDropDown(td, id)
{
	var calendar = getCalendarCombo(id); 
	calendar.ShowDropDown(td, getPixelLeft(td), getPixelTop(td)); 
}